use imp_hvm::fun::Expr as ImpExpr;
use imp_hvm::imp::{Imp, Program as ImpProgram};
use imp_hvm::{to_fun::unbound_in_stmt, CaseStmt};
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use swc_ecma_ast::{
  BinExpr, BinaryOp, BindingIdent, BlockStmt, Bool, CallExpr, Class, ClassDecl, Constructor, Decl,
  Expr as JsExpr, ExprOrSpread, ExprStmt, ForStmt, Ident, IfStmt, LabeledStmt, Lit, Module,
  ModuleItem, ParenExpr, Pat, Program as JsProgram, ReturnStmt, Script, Stmt, SwitchCase,
  SwitchStmt, VarDecl, VarDeclOrExpr, VarDeclarator, WhileStmt,
};

#[derive(Debug)]
pub enum JsErr {
  NotSupported { feature: String },
  NotImplemented { feature: String },
  CtrNotDefined { name: String },
}

pub type JSResult<S> = Result<S, JsErr>;

pub struct Ctx {
  ctrs: HashMap<String, Vec<String>>, // ctrs
}

impl Ctx {
  pub fn new() -> Self {
    let ctrs = HashMap::new();
    Ctx { ctrs }
  }
  pub fn default() -> Self {
    Self::new()
  }
  fn add_ctr(&mut self, name: String, args: Vec<String>) {
    self.ctrs.insert(name, args);
  }

  fn get_ctr(&self, name: &String) -> JSResult<&Vec<String>> {
    self.ctrs.get(name).ok_or_else(|| JsErr::CtrNotDefined { name: name.clone() })
  }
}

#[inline(always)]
fn id_to_string(ident: &Ident) -> String {
  (*ident.sym).into()
}

pub trait Compile<S> {
  fn compile(&self, ctx: &mut Ctx) -> JSResult<S>;
}

impl Compile<ImpProgram> for JsProgram {
  fn compile(&self, ctx: &mut Ctx) -> JSResult<ImpProgram> {
    match self {
      // literally the same code, but rust does not let me unify them.
      // because body types are different.
      JsProgram::Module(Module { body, .. }) => {
        let body = Box::new(body.compile(ctx)?);
        let main = Imp::ProcedureDef { name: "Main".into(), args: vec![], body };
        Ok(ImpProgram(vec![main]))
      }
      JsProgram::Script(Script { body, .. }) => {
        let body = Box::new(body.compile(ctx)?);
        let main = Imp::ProcedureDef { name: "Main".into(), args: vec![], body };
        Ok(ImpProgram(vec![main]))
      }
    }
  }
}

impl Compile<Imp> for ModuleItem {
  fn compile(&self, ctx: &mut Ctx) -> JSResult<Imp> {
    match self {
      ModuleItem::ModuleDecl(_) => todo!(),
      ModuleItem::Stmt(stmt) => stmt.compile(ctx),
    }
  }
}

impl<T> Compile<Imp> for Vec<T>
where
  T: Compile<Imp>,
{
  fn compile(&self, ctx: &mut Ctx) -> JSResult<Imp> {
    if self.is_empty() {
      Ok(Imp::Pass)
    } else if let [stmt] = self.as_slice() {
      Ok(stmt.compile(ctx)?)
    } else {
      let stmts = self.iter().map(|x| x.compile(ctx)).collect::<JSResult<Vec<Imp>>>()?;
      Ok(Imp::Block { stmts })
    }
  }
}

impl Compile<Imp> for Stmt {
  fn compile(&self, ctx: &mut Ctx) -> JSResult<Imp> {
    match self {
      Stmt::Block(BlockStmt { stmts, .. }) => stmts.compile(ctx),
      Stmt::With(_) => todo!(),
      Stmt::Return(ReturnStmt { arg, .. }) => {
        let value = match arg {
          Some(value) => value.compile(ctx)?,
          None => ImpExpr::Unit,
        };
        Ok(Imp::Return { value })
      }
      Stmt::Labeled(LabeledStmt { label, body, .. }) => {
        let name = id_to_string(label);
        let stmt = Box::new(body.compile(ctx)?);
        Ok(Imp::Label { name, stmt })
      }
      Stmt::If(IfStmt { test, cons, alt, .. }) => {
        let condition = test.compile(ctx)?;
        let true_case = Box::new(cons.compile(ctx)?);
        let false_case =
          Box::new(if let Some(stmt) = alt { stmt.compile(ctx)? } else { Imp::Pass });
        Ok(Imp::IfElse { condition, true_case, false_case })
      }
      Stmt::Switch(SwitchStmt { discriminant, cases: switch_cases, .. }) => {
        if let Some(match_stmt) = switch_to_destructuring_match(discriminant, switch_cases, ctx)? {
          Ok(match_stmt)
        } else {
          let expr = discriminant.compile(ctx)?;
          let mut default = Imp::Pass;
          let mut cases = Vec::with_capacity(switch_cases.len());
          for case in switch_cases {
            if let Some(expr) = &case.test {
              let matched = expr.compile(ctx)?;
              let body = case.cons.compile(ctx)?;
              cases.push(CaseStmt { matched, body });
            } else {
              // TODO: what if there is more than one default?
              // should be an error.
              default = case.cons.compile(ctx)?;
            }
          }
          Ok(Imp::MatchStmt { expr, cases, default: Box::new(default) })
        }
      }
      Stmt::Throw(_) => todo!(),
      Stmt::Try(_) => todo!(),
      Stmt::While(WhileStmt { test, body, .. }) => {
        let condition = test.compile(ctx)?;
        let body = Box::new(body.compile(ctx)?);
        let else_case = Box::new(Imp::Pass);
        Ok(Imp::WhileElse { condition, body, else_case })
      }
      Stmt::DoWhile(_) => todo!(),
      Stmt::For(ForStmt { init, test, update, body, .. }) => {
        if let (Some(init), Some(test), Some(update)) = (init, test, update) {
          let initialize = Box::new(init.compile(ctx)?);
          let condition = test.compile(ctx)?;
          let afterthought = Box::new(Imp::Expression { expr: update.compile(ctx)? });
          let body = Box::new(body.compile(ctx)?);
          let else_case = Box::new(Imp::Pass);
          let for_else = Imp::ForElse { initialize, condition, afterthought, body, else_case };
          Ok(for_else)
        } else {
          Err(JsErr::NotSupported { feature: "Uncomplete for-loop".into() })
        }
      }
      Stmt::ForIn(_) => todo!(),
      Stmt::ForOf(_) => todo!(),
      Stmt::Decl(decl) => decl.compile(ctx),
      Stmt::Debugger(_) => todo!(),
      Stmt::Expr(ExprStmt { expr, .. }) => {
        let expr = expr.compile(ctx)?;
        Ok(Imp::Expression { expr })
      }
      Stmt::Break(_) => Ok(Imp::Break),
      Stmt::Continue(_) => Ok(Imp::Continue),
      Stmt::Empty(_) => Ok(Imp::Pass),
    }
  }
}

/// Transforms a switch of the form
/// ```
/// switch (true) {
///   case a instanceof Foo:
///     caseFoo
///   case b instanceof Bar:
///     caseBar
/// }
/// ```
/// into
/// ```
/// Match (Tuple2 a b) {
///   (Tuple2 (Foo x y) b) = caseFoo
///   (Tuple2 a   (Bar z)) = caseBar
/// }
/// ```
/// given that `Foo`, `Bar` have been previously declared.
fn switch_to_destructuring_match(
  discriminant: &JsExpr,
  cases: &[SwitchCase],
  ctx: &mut Ctx,
) -> JSResult<Option<Imp>> {
  struct Case {
    binds: HashMap<String, ImpExpr>,
    body: Imp,
  }
  fn is_valid_case(expr: &JsExpr) -> Option<Vec<(String, String)>> {
    if let JsExpr::Bin(BinExpr { op, left, right, .. }) = expr {
      match (op, *left.clone(), *right.clone()) {
        (BinaryOp::InstanceOf, JsExpr::Ident(var), JsExpr::Ident(ctr_name)) => {
          Some(vec![(id_to_string(&var), id_to_string(&ctr_name))])
        }
        (BinaryOp::LogicalAnd, left, right) => {
          let left = is_valid_case(&left)?;
          let right = is_valid_case(&right)?;
          // concat left and right
          Some(left.into_iter().chain(right.into_iter()).collect())
        }
        _ => None,
      }
    } else {
      None
    }
  }

  // matched expression must be `true`
  if let JsExpr::Lit(Lit::Bool(Bool { value, .. })) = discriminant {
    if *value {
      let mut unfinished_cases: Vec<Case> = Vec::with_capacity(cases.len());
      // iterate through the cases, and for each case
      // save the variables in the form of
      // `(bound_var -> constructor)`
      // in a hashmap, together with the corresponding body
      for case in cases {
        if let Some(expr) = &case.test {
          if let Some(binds) = is_valid_case(expr) {
            let mut bound_args = HashMap::new();
            for (var, ctr_name) in binds {
              let args = ctx.get_ctr(&ctr_name)?.iter().map(|x| ImpExpr::Var{name:format!("{var}.{x}")}).collect();
              let ctr = ImpExpr::Ctr { name: ctr_name, args};
              bound_args.insert(var, ctr);
            }
            let body = case.cons.compile(ctx)?;
            unfinished_cases.push(Case { binds: bound_args, body });
          } else {
            return Ok(None);
          }
        }
        // TODO: add default case
      }
      // figure out with
      let total_args = unfinished_cases
        .iter()
        .flat_map(|Case { binds, body }| {
          binds
            .iter()
            .map(|(name, _)| name.clone())
            .chain(unbound_in_stmt(body).into_iter())
        })
        .collect::<HashSet<_>>();
      let mut match_cases = vec![];
      for Case { mut binds, body } in unfinished_cases.into_iter() {
        let mut args = vec![];
        for var in total_args.iter() {
          match binds.entry((*var).clone()) {
            Entry::Occupied(occ) => {
              let (_, ctr) = occ.remove_entry();
              args.push(ctr);
            }
            Entry::Vacant(vac) => {
              args.push(ImpExpr::Var { name: vac.into_key() });
            }
          }
        }
        // TODO: create unique name for every switch case
        let expr = ImpExpr::Ctr { name: "Switch".into(), args };
        let case = CaseStmt { matched: expr, body };
        match_cases.push(case);
      }
      let args = total_args.into_iter().map(|name| ImpExpr::Var { name }).collect();
      // TODO: same unique name
      let expr = ImpExpr::Ctr { name: "Switch".into(), args };
      let match_stmt = Imp::MatchStmt { expr, cases: match_cases, default: Box::new(Imp::Pass) };
      return Ok(Some(match_stmt));
    }
  };
  Ok(None)
}

impl Compile<ImpExpr> for JsExpr {
  fn compile(&self, ctx: &mut Ctx) -> JSResult<ImpExpr> {
    match self {
      JsExpr::This(_) => todo!(),
      JsExpr::Array(_) => todo!(),
      JsExpr::Object(_) => todo!(),
      JsExpr::Fn(_) => todo!(),
      JsExpr::Unary(_) => todo!(),
      JsExpr::Update(_) => todo!(),
      JsExpr::Bin(_) => todo!(),
      JsExpr::Assign(_) => todo!(),
      JsExpr::Member(_) => todo!(),
      JsExpr::SuperProp(_) => todo!(),
      JsExpr::Cond(_) => todo!(),
      JsExpr::Call(CallExpr { callee, args, .. }) => match callee {
        swc_ecma_ast::Callee::Super(_) => todo!(),
        swc_ecma_ast::Callee::Import(_) => todo!(),
        swc_ecma_ast::Callee::Expr(func) => {
          let call = match func.compile(ctx)? {
            ImpExpr::Var { name } => {
              let args = args.iter().map(|x| x.compile(ctx)).collect::<JSResult<Vec<ImpExpr>>>()?;
              ImpExpr::FunCall { name, args }
            }
            term => {
              let mut expr = term;
              for arg in args {
                let argm = Box::new(arg.compile(ctx)?);
                expr = ImpExpr::App { expr: Box::new(expr), argm };
              }
              expr
            }
          };
          Ok(call)
        }
      },
      JsExpr::New(_) => todo!(),
      JsExpr::Seq(_) => todo!(),
      JsExpr::Ident(ident) => {
        let name = id_to_string(ident);
        let var = ImpExpr::Var { name };
        Ok(var)
      }
      JsExpr::Lit(literal) => literal.compile(ctx),
      JsExpr::Tpl(_) => todo!(),
      JsExpr::TaggedTpl(_) => todo!(),
      JsExpr::Arrow(_) => todo!(),
      JsExpr::Class(_) => todo!(),
      JsExpr::Yield(_) => todo!(),
      JsExpr::MetaProp(_) => todo!(),
      JsExpr::Await(_) => todo!(),
      JsExpr::Paren(ParenExpr { expr, .. }) => expr.compile(ctx),
      JsExpr::JSXMember(_) => todo!(),
      JsExpr::JSXNamespacedName(_) => todo!(),
      JsExpr::JSXEmpty(_) => todo!(),
      JsExpr::JSXElement(_) => todo!(),
      JsExpr::JSXFragment(_) => todo!(),
      JsExpr::TsTypeAssertion(_) => todo!(),
      JsExpr::TsConstAssertion(_) => todo!(),
      JsExpr::TsNonNull(_) => todo!(),
      JsExpr::TsAs(_) => todo!(),
      JsExpr::TsInstantiation(_) => todo!(),
      JsExpr::TsSatisfies(_) => todo!(),
      JsExpr::PrivateName(_) => todo!(),
      JsExpr::OptChain(_) => todo!(),
      JsExpr::Invalid(_) => todo!(),
    }
  }
}

impl Compile<ImpExpr> for Lit {
  fn compile(&self, _ctx: &mut Ctx) -> JSResult<ImpExpr> {
    match self {
      Lit::Str(_) => todo!(),
      Lit::Bool(Bool { value, .. }) => {
        let numb = if *value { 1 } else { 0 };
        Ok(ImpExpr::Unsigned { numb })
      }
      Lit::Null(_) => Ok(ImpExpr::Unit),
      Lit::Num(number) => {
        let numb = number.value;
        Ok(ImpExpr::Float { numb })
      }
      Lit::BigInt(_) => todo!(),
      Lit::Regex(_) => todo!(),
      Lit::JSXText(_) => todo!(),
    }
  }
}

impl Compile<Imp> for VarDeclOrExpr {
  fn compile(&self, ctx: &mut Ctx) -> JSResult<Imp> {
    match self {
      VarDeclOrExpr::VarDecl(vardecl) => vardecl.compile(ctx),
      VarDeclOrExpr::Expr(expr) => {
        let expr = expr.compile(ctx)?;
        Ok(Imp::Expression { expr })
      }
    }
  }
}

impl Compile<ImpExpr> for ExprOrSpread {
  fn compile(&self, ctx: &mut Ctx) -> JSResult<ImpExpr> {
    if let Some(_span) = self.spread {
      Err(JsErr::NotSupported { feature: "Spread operator".into() })
    } else {
      self.expr.compile(ctx)
    }
  }
}

impl Compile<Imp> for Decl {
  fn compile(&self, ctx: &mut Ctx) -> JSResult<Imp> {
    match self {
      Decl::Class(ClassDecl { ident, class, .. }) => {
        let name = id_to_string(ident);
        let args = class.compile(ctx)?;
        println!("{name} {:?}", args);
        ctx.add_ctr(name, args);
        Ok(Imp::Pass)
      }
      Decl::Fn(_) => todo!(),
      Decl::Var(var_decl) => var_decl.compile(ctx),
      Decl::TsInterface(_) => todo!(),
      Decl::TsTypeAlias(_) => todo!(),
      Decl::TsEnum(_) => todo!(),
      Decl::TsModule(_) => todo!(),
    }
  }
}

impl Compile<Imp> for VarDecl {
  fn compile(&self, ctx: &mut Ctx) -> JSResult<Imp> {
    if let [VarDeclarator { name, init, .. }] = self.decls.as_slice() {
      if let (Pat::Ident(BindingIdent { id, .. }), Some(val)) = (name, init) {
        let name = id_to_string(id);
        let expr = val.compile(ctx)?;
        let assign = Imp::Assignment { name, expr };
        Ok(assign)
      } else {
        Err(JsErr::NotSupported { feature: "Lefthand-side expression".into() })
      }
    } else {
      Err(JsErr::NotImplemented { feature: "Multiple assignment".into() })
    }
  }
}

impl Compile<Vec<String>> for Class {
  fn compile(&self, _ctx: &mut Ctx) -> JSResult<Vec<String>> {
    use swc_ecma_ast::ClassMember;
    use swc_ecma_ast::ParamOrTsParamProp;
    let mut args = vec![];
    for class_member in self.body.iter() {
      match class_member {
        ClassMember::Constructor(Constructor { params, .. }) => {
          // TODO: validate constructor body.
          for param in params {
            if let ParamOrTsParamProp::Param(param) = param {
              if let Pat::Ident(ident) = &param.pat {
                args.push(id_to_string(ident));
              }
            }
          }
        }
        _ => todo!(),
      }
    }
    Ok(args)
  }
}
