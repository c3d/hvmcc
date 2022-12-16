#![allow(unused_variables)]
use pyo3::prelude::*;
use pyo3::exceptions::PyException;
//use pyo3::Python;
use rustpython_parser::ast::*;
use imp_hvm::{Imp, Expr as Exp, Program, Procedure, Oper};
use num_traits::cast::ToPrimitive;

pyo3::create_exception!(compiler, NotSupported, PyException);

fn not_supported<A>(msg: &str) -> PyResult<A> {
  Err(NotSupported::new_err(String::from(msg)))
}

pub trait Compile<S> {
  fn compile(&self) -> PyResult<S>;
}

// just throw away the locations
impl<T,S> Compile<T> for Located<S>
where
  S: Compile<T>
{
  fn compile(&self) -> PyResult<T> {
    let Located {location: _, node, end_location: _, custom: _} = self;
    node.compile()
  }
}

impl Compile<Program> for Mod {
  fn compile(&self) -> PyResult<Program> {
    match self {
      // if it is a module, we compile it, expecting functions in the top level
      Mod::Module { body, type_ignores: _ } => {
        let procs = body.iter().map(|x| x.compile()).collect::<Result<Vec<Procedure>, PyErr>>();
        Ok(Program(procs?))
      },
      // else, raise error.
      Mod::Expression { body:_ } | Mod::Interactive { body:_ } => not_supported("Only modules can be compiled."),
      // i dont know exactly what this means.
      Mod::FunctionType { argtypes, returns } => todo!(), // i'm not sure what this is supposed to be.
    }
  }
}



impl Compile<Imp> for StmtKind {
  fn compile(&self) -> PyResult<Imp> {
    /// Compiles a possibly multi-expression target into
    /// a string. Right now, only single variable targets are supported.
    fn compile_target(target: &Vec<Located<ExprKind>>) -> PyResult<String> {
      // if vec has only 1 element, with only 1 variable
      // we can convert it into assert
      if let [Located{ location:_, node: ExprKind::Name { id, ctx: _ }, end_location: _, custom: _ }] = target.as_slice() {
        Ok(id.clone())
      }
      // otherwise, not supported.
      else {
        not_supported("Multiple targets are not supported yet.")
      }
    }
    
    match self {
      // SUPPORTED STATEMENTS:
      StmtKind::FunctionDef { name, args, body, decorator_list, returns, type_comment } => todo!(),
      StmtKind::Return { value } => {
        let value = match value {
          Some(value) => value.compile()?,
          None => Exp::Unit,
        };
        Ok(Imp::Return { value })
      },
      StmtKind::Assign { targets, value, type_comment } => {
        let name = compile_target(targets)?;
        let expr = value.compile()?;
        Ok(Imp::Assignment { name, expr })
      },      
      StmtKind::For { target, iter, body, orelse, type_comment } => {
        let target = compile_target(&vec![*target.clone()])?;
        let iterator = iter.compile()?;
        let body = body.compile()?;
        let else_case = orelse.compile()?;
        Ok(Imp::ForInElse { target, iterator, body, else_case })
      },
      StmtKind::While { test, body, orelse } => {
        let condition = test.compile()?;
        let body = body.compile()?;
        let else_case = orelse.compile()?;
        Ok(Imp::WhileElse { condition, body, else_case }) 
      },
      StmtKind::Expr { value } => {
        let expr = value.compile()?;
        Ok(Imp::Expression { expr })
      },
      StmtKind::If { test, body, orelse } => {
        let condition = test.compile()?;
        let true_case = body.compile()?;
        let false_case = orelse.compile()?;
        Ok(Imp::IfElse { condition, true_case, false_case })
      },
      StmtKind::Pass => Ok(Imp::Pass),
      StmtKind::Break => Ok(Imp::Break),
      StmtKind::Continue => Ok(Imp::Continue),
      // to implement match we still need rustpython's parser support
      // i need to look into it a bit later.
      StmtKind::Match { subject, cases } => todo!(),
      StmtKind::AugAssign { target, op, value } => {
        let name = compile_target(&vec![*target.clone()])?; // ugly
        let left = Box::new(Exp::Var { name: name.clone() });
        let op   = op.compile()?;
        let right = Box::new(value.compile()?);
        let expr = Exp::BinOp { op, left, right };
        Ok(Imp::Assignment { name, expr })
      },
      StmtKind::Assert { test, msg } => not_supported("Assert statement is not supported yet."),
      // NOT SUPPORTED:
      StmtKind::AsyncFunctionDef { name, args, body, decorator_list, returns, type_comment } => not_supported("Async functions are not suported."),
      StmtKind::ClassDef { name, bases, keywords, body, decorator_list } => not_supported("Class definitions are not supported."),
      StmtKind::AsyncFor { target, iter, body, orelse, type_comment } => not_supported("Async For loops are not supported"),
      StmtKind::Delete { targets } => not_supported("Delete statement is not supported."),
      StmtKind::AnnAssign { target, annotation, value, simple } => not_supported("Annotated assignment is not supported yet."),
      StmtKind::Raise { exc, cause } => not_supported("Raise statement is not supported yet."),
      StmtKind::With { items, body, type_comment } => not_supported("With statement is not supported yet."),
      StmtKind::AsyncWith { items, body, type_comment } => not_supported("Async with statement is not supported yet."),
      StmtKind::Try { body, handlers, orelse, finalbody } => not_supported("Try-catch statement is not supported yet."),
      StmtKind::Import { names } => not_supported("Import statements are not supported yet."),
      StmtKind::ImportFrom { module, names, level } => not_supported("Import-from statement is not supported yet."),
      StmtKind::Global { names } => not_supported("Global statement is not supported."),
      StmtKind::Nonlocal { names } => not_supported("Nonlocal statement is not supported yet."),
    }
  }
}


impl Compile<Procedure> for StmtKind {
  fn compile(&self) -> PyResult<Procedure> {
    match self {
      StmtKind::FunctionDef { name, args, body, decorator_list, returns, type_comment } => {
        fn args_to_vector(arguments: &Arguments) -> Vec<String> {
          let mut names = vec![];
          for loc in &arguments.args {
            let ArgData { arg, annotation:_, type_comment:_ } = &loc.node;
            names.push(arg.clone());
          }
          names
        }
        let name = name.clone();
        let args = args_to_vector(args);
        let body = body.compile()?;
        // fmap into result in order to avoid a match.
        Ok(Procedure { name, args, body })
      }
      _ => {
        not_supported("Only function definitions are accepted in the top level.")
      }
    }
  }
}

impl Compile<Vec<Imp>> for Vec<Stmt> {
  fn compile(&self) -> PyResult<Vec<Imp>> {
    // this is kinda scuffed, i think it can be refactored into simpler code
    // but i wont try to do it right now.
    if self.len() == 0 {
      Ok(vec![Imp::Pass])
    }
    else {
      self.iter().map(|x| x.compile()).collect()
    }
  }
}

// impl ToImp<Box<Term>> for PatternKind {
//   fn to_imp(&self, ctx: &mut Ctx) -> PyResult<Box<Term>> {
//     match self {
//       PatternKind::MatchValue { value } => value.to_imp(ctx),
//       PatternKind::MatchSingleton { value } => value.to_imp(ctx),
//       PatternKind::MatchSequence { patterns } => todo!(),
//       PatternKind::MatchMapping { keys, patterns, rest } => todo!(),
//       PatternKind::MatchClass { cls, patterns, kwd_attrs, kwd_patterns } => todo!(),
//       PatternKind::MatchStar { name } => {
//         let name = match name {
//           Some(name) => name.clone(),
//           None       => String::from("_")
//         };
//         Ok(Box::new(Term::Var { name }))
//       },
//       PatternKind::MatchAs { pattern, name } => todo!(),
//       PatternKind::MatchOr { patterns } => todo!(),
//     }
//   }
// }

// impl ToImp<()> for MatchCase {
//   fn to_imp(&self, ctx: &mut Ctx) -> PyResult<()> {
//     let MatchCase { pattern, guard, body } = self;
//     let target = pattern.to_imp(ctx)?;
//     let body = body.to_imp(ctx)?;
//     let lhs = Box::new(Term::Ctr { name: ctx.curr_rule.clone(), args: ctx.vars.values().cloned().collect()});
//     let rhs = body;
//     let rule = Rule { lhs, rhs };
//     ctx.file.rules.push(rule);
//     Ok(())
//   }
// }

impl Compile<Exp> for Constant {
  fn compile(&self) -> PyResult<Exp> {
    match self {
      Constant::None => Ok(Exp::Unit),
      Constant::Bool(boolean) => {
        let numb = if *boolean { 1 } else { 0 };
        Ok(Exp::Unsigned { numb })
      },
      Constant::Str(_) => todo!(),
      Constant::Bytes(_) => todo!(),
      Constant::Int(numb) => {
        let numb = match numb.to_u64() {
          Some(num) => Ok(num),
          None      => not_supported(&format!("Number does not fit in U60: {numb}")),
        }?;
        Ok(Exp::Unsigned { numb })
      },
      Constant::Tuple(tup) => {
        let args = tup.iter().map(|x| x.compile()).collect::<PyResult<Vec<Exp>>>()?;
        Ok(Exp::Ctr { name: String::from("Tup"), args })
      },
      Constant::Float(float) => {
        let numb = *float;
        Ok(Exp::Float { numb })
      },
      Constant::Complex { real, imag } => todo!(),
      Constant::Ellipsis => todo!(),
    }
  }
}

// i think this should be more general.
impl Compile<(String, Exp)> for Comprehension {
  fn compile(&self) -> PyResult<(String, Exp)> {
    let Comprehension { target, iter, ifs, is_async } = self;
    if let Exp::Var { name } = target.compile()? {
      let iter = iter.compile()?;
      Ok((name, iter))
    }
    else {
      not_supported("Multiple targets in comprehension is not supported yet.")
    }
  }
}

impl Compile<Oper> for Operator {
  fn compile(&self) -> PyResult<Oper> {
     match self {
       Operator::Add    => Ok(Oper::Add),
       Operator::Sub    => Ok(Oper::Sub),
       Operator::Mult   => Ok(Oper::Mul),
       Operator::Div    => Ok(Oper::Div),
       Operator::Mod    => Ok(Oper::Mod),
       Operator::LShift => Ok(Oper::Shl),
       Operator::RShift => Ok(Oper::Shr),
       Operator::BitOr  => Ok(Oper::Or ),
       Operator::BitXor => Ok(Oper::Xor),
       Operator::BitAnd => Ok(Oper::And),
       _ => not_supported("Operation not supported yet.")
     }
  }
}

impl Compile<Oper> for Cmpop {
  fn compile(&self) -> PyResult<Oper> {
    match self {
      Cmpop::Eq    => Ok(Oper::Eql),
      Cmpop::NotEq => Ok(Oper::Neq),
      Cmpop::Lt    => Ok(Oper::Ltn),
      Cmpop::LtE   => Ok(Oper::Lte),
      Cmpop::Gt    => Ok(Oper::Gtn),
      Cmpop::GtE   => Ok(Oper::Gte),
      _ => not_supported("Comparison operator not supported")
    }
  }
}

impl Compile<Exp> for ExprKind {
  fn compile(&self) -> PyResult<Exp> {
    match self {
      ExprKind::BoolOp { op, values } => todo!(),
      ExprKind::NamedExpr { target, value } => todo!(),
      ExprKind::BinOp { left, op, right } => {
        let left  = Box::new(left.compile()?);
        let right = Box::new(right.compile()?);
        let op = op.compile()?;
        Ok(Exp::BinOp { op, left, right })
      },
      ExprKind::UnaryOp { op, operand } => {
        let value = operand.compile()?;
        match op {
          Unaryop::Invert => todo!(),
          Unaryop::Not => todo!(),
          Unaryop::UAdd => todo!(),
          Unaryop::USub => {
            let term = Exp::BinOp { op: Oper::Sub, left: Box::new(Exp::Unsigned { numb: 0 }), right: Box::new(value) };
            Ok(term)
          },
        }
      }
      ExprKind::Lambda { args, body } => todo!(),
      ExprKind::IfExp { test, body, orelse } => todo!(),
      ExprKind::Dict { keys, values } => todo!(),
      ExprKind::Set { elts } => todo!(),
      ExprKind::ListComp { elt, generators } => {
        let mut expr = elt.compile()?;
        for gen in generators {
          let (var, iter) = gen.compile()?;
          let lamd = Exp::Lambda { var, body: Box::new(expr) };
          let name = String::from("List.fold");
          let args = vec![lamd, iter];
          expr = Exp::FunCall { name , args };
        }
        Ok(expr)
      },
      ExprKind::SetComp { elt, generators } => todo!(),
      ExprKind::DictComp { key, value, generators } => todo!(),
      ExprKind::GeneratorExp { elt, generators } => todo!(),
      ExprKind::Await { value } => todo!(),
      ExprKind::Yield { value } => todo!(),
      ExprKind::YieldFrom { value } => todo!(),
      ExprKind::Compare { left, ops, comparators } => {
        let left = left.compile()?;
        let mut comp = comparators.iter().map(|x| x.compile()).collect::<PyResult<Vec<Exp>>>()?;
        comp.insert(0, left);
        let ops = ops.iter().map(|x| x.compile()).collect::<PyResult<Vec<Oper>>>()?;
        let comparisons = comp.windows(2) // zips (a[n], a[n+1])
          .zip(ops) // zips ((comp[n], comp[n+1]), op[n])
          .map(|(w, op)| Exp::BinOp { op, left: Box::new(w[0].clone()), right:Box::new(w[1].clone()) });
        // TODO: split comparisons into head, tail, and use head as base for fold.
        let term = comparisons
          .fold(Exp::Unsigned {numb:1}, |acc, val|
                Exp::BinOp { op: Oper::And, left: Box::new(acc), right: Box::new(val) }
          );
        Ok(term)
      }
      ExprKind::Call { func, args, keywords } => {
        let call = match func.compile()? {
          Exp::Var { name } => {
            let args = args.iter().map(|x| x.compile()).collect::<PyResult<Vec<Exp>>>()?;
            Exp::FunCall { name, args }
          },
          term => {
            let mut expr = term;
            for arg in args {
              let argm = Box::new(arg.compile()?);
              expr = Exp::App { expr: Box::new(expr), argm };
            }
            expr
          }
        };
        Ok(call)
      },
      ExprKind::FormattedValue { value, conversion, format_spec } => todo!(),
      ExprKind::JoinedStr { values } => todo!(),
      ExprKind::Constant { value, kind } => value.compile(),
      ExprKind::Attribute { value, attr, ctx } => todo!(),
      ExprKind::Subscript { value, slice, ctx } => todo!(),
      ExprKind::Starred { value, ctx } => todo!(),
      ExprKind::Name { id, ctx } => {
        let name = id.clone();
        Ok(Exp::Var { name })
      },
      ExprKind::List { elts, ctx } => todo!(),
      ExprKind::Tuple { elts, ctx } => {
        let exprs = elts.iter().map(|x| x.compile()).collect::<PyResult<Vec<Exp>>>()?;
        let tuple = Exp::Ctr {name: "Tuple".into(), args: exprs};
        Ok(tuple)
      },
      ExprKind::Slice { lower, upper, step } => todo!(),
    }
  }
}

// trait ToPy {
//   fn readback(self) -> PyObject;
// }

// impl ToPy for Term {
//   fn readback(self) -> PyObject {
//     match self {
//         Term::Var { name } => todo!(),
//         Term::Dup { nam0, nam1, expr, body } => panic!("Dup should not appear in result"),
//         Term::Sup { val0, val1 } => panic!("Sup should not appear in result"),
//         Term::Let { name, expr, body } => todo!(),
//         Term::Lam { name, body } => todo!(),
//         Term::App { func, argm } => todo!(),
//         Term::Ctr { name, args } => todo!(),
//         Term::U6O { numb } => todo!(),
//         Term::F6O { numb } => todo!(),
//         Term::Op2 { oper, val0, val1 } => todo!(),
//     }
//   }
// }
