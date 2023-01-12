use imp_hvm::fun::Expr as ImpExpr;
use imp_hvm::imp::{Imp, Program as ImpProgram};
use swc_ecma_ast::{
  BindingIdent, BlockStmt, Bool, CallExpr, Decl, Expr as JsExpr, ExprOrSpread, ExprStmt, ForStmt,
  Ident, LabeledStmt, Lit, Module, ModuleItem, ParenExpr, Pat, Program as JsProgram, ReturnStmt,
  Script, Stmt, VarDecl, VarDeclOrExpr, VarDeclarator, IfStmt,
};

#[derive(Debug)]
pub enum JsErr {
  NotSupported { feature: String },
  NotImplemented { feature: String },
}

pub type JSResult<S> = Result<S, JsErr>;

pub trait Compile<S> {
  fn compile(&self) -> JSResult<S>;
}

impl Compile<ImpProgram> for JsProgram {
  fn compile(&self) -> JSResult<ImpProgram> {
    match self {
      // literally the same code, but rust does not let me unify them.
      // because body types are different.
      JsProgram::Module(Module { body, .. }) => {
        let body = Box::new(body.compile()?);
        let main = Imp::ProcedureDef { name: "Main".into(), args: vec![], body };
        Ok(ImpProgram(vec![main]))
      }
      JsProgram::Script(Script { body, .. }) => {
        let body = Box::new(body.compile()?);
        let main = Imp::ProcedureDef { name: "Main".into(), args: vec![], body };
        Ok(ImpProgram(vec![main]))
      }
    }
  }
}

impl Compile<Imp> for ModuleItem {
  fn compile(&self) -> JSResult<Imp> {
    match self {
      ModuleItem::ModuleDecl(_) => todo!(),
      ModuleItem::Stmt(stmt) => stmt.compile(),
    }
  }
}

impl<T> Compile<Imp> for Vec<T>
where
  T: Compile<Imp>,
{
  fn compile(&self) -> JSResult<Imp> {
    if self.is_empty() {
      Ok(Imp::Pass)
    } else if let [stmt] = self.as_slice() {
      Ok(stmt.compile()?)
    } else {
      let stmts = self.iter().map(|x| x.compile()).collect::<JSResult<Vec<Imp>>>()?;
      Ok(Imp::Block { stmts })
    }
  }
}

impl Compile<Imp> for Stmt {
  fn compile(&self) -> JSResult<Imp> {
    match self {
      Stmt::Block(BlockStmt { stmts, .. }) => {
        stmts.compile()
      }
      Stmt::With(_) => todo!(),
      Stmt::Return(ReturnStmt { arg, .. }) => {
        let value = match arg {
          Some(value) => value.compile()?,
          None => ImpExpr::Unit,
        };
        Ok(Imp::Return { value })
      }
      Stmt::Labeled(LabeledStmt { label, body, .. }) => {
        let name: String = (*label.clone().sym).into();
        let stmt = Box::new(body.compile()?);
        Ok(Imp::Label { name, stmt })
      }
      Stmt::If(IfStmt { test, cons, alt, .. }) => {
        let condition = test.compile()?;
        let true_case = Box::new(cons.compile()?);
        let false_case = Box::new(if let Some(stmt) = alt {
          stmt.compile()?
        } else {
          Imp::Pass
        });
        Ok(Imp::IfElse { condition, true_case, false_case })
      },
      Stmt::Switch(_) => todo!(),
      Stmt::Throw(_) => todo!(),
      Stmt::Try(_) => todo!(),
      Stmt::While(_) => todo!(),
      Stmt::DoWhile(_) => todo!(),
      Stmt::For(ForStmt { init, test, update, body, .. }) => {
        if let (Some(init), Some(test), Some(update)) = (init, test, update) {
          let initialize = Box::new(init.compile()?);
          let condition = test.compile()?;
          let afterthought = Box::new(Imp::Expression { expr: update.compile()? });
          let body = Box::new(body.compile()?);
          let else_case = Box::new(Imp::Pass);
          let for_else = Imp::ForElse { initialize, condition, afterthought, body, else_case };
          Ok(for_else)
        } else {
          Err(JsErr::NotSupported { feature: "For-loop".into() })
        }
      }
      Stmt::ForIn(_) => todo!(),
      Stmt::ForOf(_) => todo!(),
      Stmt::Decl(decl) => decl.compile(),
      Stmt::Debugger(_) => todo!(),
      Stmt::Expr(ExprStmt { expr, .. }) => {
        let expr = expr.compile()?;
        Ok(Imp::Expression { expr })
      }
      Stmt::Break(_) => Ok(Imp::Break),
      Stmt::Continue(_) => Ok(Imp::Continue),
      Stmt::Empty(_) => Ok(Imp::Pass),
    }
  }
}

impl Compile<ImpExpr> for JsExpr {
  fn compile(&self) -> JSResult<ImpExpr> {
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
      JsExpr::Call(CallExpr { callee, args, type_args, .. }) => match callee {
        swc_ecma_ast::Callee::Super(_) => todo!(),
        swc_ecma_ast::Callee::Import(_) => todo!(),
        swc_ecma_ast::Callee::Expr(func) => {
          let call = match func.compile()? {
            ImpExpr::Var { name } => {
              let args = args.iter().map(|x| x.compile()).collect::<JSResult<Vec<ImpExpr>>>()?;
              ImpExpr::FunCall { name, args }
            }
            term => {
              let mut expr = term;
              for arg in args {
                let argm = Box::new(arg.compile()?);
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
      JsExpr::Ident(Ident { sym, .. }) => {
        let name: String = (**sym).into();
        let var = ImpExpr::Var { name };
        Ok(var)
      }
      JsExpr::Lit(literal) => literal.compile(),
      JsExpr::Tpl(_) => todo!(),
      JsExpr::TaggedTpl(_) => todo!(),
      JsExpr::Arrow(_) => todo!(),
      JsExpr::Class(_) => todo!(),
      JsExpr::Yield(_) => todo!(),
      JsExpr::MetaProp(_) => todo!(),
      JsExpr::Await(_) => todo!(),
      JsExpr::Paren(ParenExpr { expr, .. }) => expr.compile(),
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
  fn compile(&self) -> JSResult<ImpExpr> {
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
  fn compile(&self) -> JSResult<Imp> {
    match self {
      VarDeclOrExpr::VarDecl(vardecl) => {
        vardecl.compile()
      },
      VarDeclOrExpr::Expr(expr) => {
        let expr = expr.compile()?;
        Ok(Imp::Expression { expr })
      },
    }
  }
}

impl Compile<ImpExpr> for ExprOrSpread {
  fn compile(&self) -> JSResult<ImpExpr> {
    if let Some(_span) = self.spread {
      Err(JsErr::NotSupported { feature: "Spread operator".into() })
    } else {
      self.expr.compile()
    }
  }
}

impl Compile<Imp> for Decl {
  fn compile(&self) -> JSResult<Imp> {
    match self {
      Decl::Class(_) => todo!(),
      Decl::Fn(_) => todo!(),
      Decl::Var(var_decl) => var_decl.compile(),
      Decl::TsInterface(_) => todo!(),
      Decl::TsTypeAlias(_) => todo!(),
      Decl::TsEnum(_) => todo!(),
      Decl::TsModule(_) => todo!(),
    }
  }
}

impl Compile<Imp> for VarDecl {
  fn compile(&self) -> JSResult<Imp> {
    if let [VarDeclarator { name, init, .. }] = self.decls.as_slice() {
      if let (Pat::Ident(BindingIdent { id, .. }), Some(val)) = (name, init) {
        let name = (*id.sym).into();
        let expr = val.compile()?;
        let assign = Imp::Assignment { name, expr };
        Ok(assign)
      } else {
        Err(JsErr::NotSupported { feature: "Lefthand-side expression".into() })
      }
    } else {
      Err(JsErr::NotImplemented { feature: "Multiple assignment".into()})
    }
  }
}
