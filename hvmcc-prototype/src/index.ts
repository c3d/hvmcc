import * as peg from "./parser"

type Variable = string

interface Block {
  getPreds(): Block[]
}

class Phi {

  public name: Variable
  public block: Block
  public operands: Array<Operand>
  public users: Set<Phi>

  constructor(name: Variable, block: Block) {
    this.name = name
    this.block = block
    this.operands = []
    this.users = new Set()
  }

  public appendOperand(operand: Operand) {
    this.operands.push(operand)
    if(operand instanceof Phi) {
      this.users.add(operand)
    }
  }

  public replaceBy(operand: Operand) {
    console.log(["Replace by!", this, operand])
  }

}

class Undef {
  
}

type Value = never

type Operand = Phi | Undef | Value

class BraunConverter {

  public currentDef: Map<string, Map<Block, Operand>>
  public incompletePhis: Map<Block, Map<string, Phi>>
  public sealedBlocks: Array<Block>

  constructor() {
    this.currentDef = new Map()
    this.incompletePhis = new Map()
    this.sealedBlocks = []
  }

  public writeVariable(variable: Variable, block: Block, value: Operand): void {
    console.log(["writeVariable", variable, block, value])
    if(!this.currentDef.has(variable)) {
      this.currentDef.set(variable, new Map())
    }
    this.currentDef.get(variable)?.set(block, value)
  }

  public readVariable(variable: Variable, block: Block): Phi | Undef {
    console.log(["readVariable", variable, block])
    if(this.currentDef.has(variable)) {
      if(this.currentDef.get(variable)?.has(block)) {
        return this.currentDef.get(variable)?.get(block)!
      }
    }
    return this.readVariableRecursive(variable, block)
  }

  public readVariableRecursive(variable: Variable, block: Block): Phi | Undef {
    var value;
    if(!(this.sealedBlocks.includes(block))) {
      value = new Phi(variable, block)
      if(!this.incompletePhis.has(block)) {
        this.incompletePhis.set(block, new Map())
      }
      this.incompletePhis.get(block)?.set(variable, value)
    } else {
      var preds = block.getPreds()
      if(preds.length == 1) {
        value = this.readVariable(variable, preds[0])
      } else {
        value = new Phi(variable, block)
        this.writeVariable(variable, block, value)
        value = this.addPhiOperands(variable, value)
      }
    }
    this.writeVariable(variable, block, value)
    return value
  }

  public addPhiOperands(variable: Variable, phi: Phi): Phi | Undef {
    for(var pred of phi.block.getPreds()) {
      phi.appendOperand(this.readVariable(variable, pred))
    }

    return this.tryRemoveTrivialPhi(phi)
  }

  public tryRemoveTrivialPhi(phi: Phi): Phi | Undef {
    var same = null
    for(var operand of phi.operands) {
      if(operand == same || operand == phi) {
        continue;
      }
      if(same != null) {
        return phi
      }
      same = operand;
    }
    if(same == null) {
      // The phi is unrecheable or in the start block
      same = new Undef();
    }

    // Remember all users except the phi itself
    var users = phi.users
    users.delete(phi)

    // Reroute all uses of phi to same and remove phi
    phi.replaceBy(same)

    // Try to recursively remove all phi users, which might have become trivial
    for(var user of users) {
      if(user instanceof Phi) {
       this.tryRemoveTrivialPhi(user)
      }
    }

    return same;
  }

  public sealBlock(block: Block) {
    if(this.incompletePhis.has(block)) {
      var phis = this.incompletePhis.get(block)
      var keys = phis?.keys()!
      for(var variable of keys) {
        this.addPhiOperands(variable, phis?.get(variable)!)
      }
      this.incompletePhis.delete(block)
    }
    this.sealedBlocks.push(block)
  }

}

var ssa = new BraunConverter()

type Stmt = ["assign", string, Expr]
          | ["if", Expr, Stmt[], Stmt[]]
type Expr = number
          | string
          | ["+", Expr, Expr]

class BasicBlock implements Block {

  private preds: BasicBlock[]
  private assignments: Array<[Variable, Expr]>

  constructor(...blocks: BasicBlock[]) {
    this.preds = []
    for(var block of blocks) {
      this.preds.push(block)
    }
    this.assignments = []
  }

  public getPreds(): Block[] {
    return this.preds
  }

  public addAssignment(v: Variable, e: Expr) {
    console.log(["adding assignment to block", v, e])
    this.assignments.push([v, e])
  }

}

class Converter {

  private names: Map<Variable, number>
  private blocks: BasicBlock[]

  constructor() {
    this.names = new Map()
    this.blocks = []
  }

  private newBlock(...preds: BasicBlock[]) {
    var block = new BasicBlock(...preds);
    this.blocks.push(block)
    return block
  }

  public makeSealedBlock(...preds: BasicBlock[]) {
    var block = this.newBlock(...preds)
    ssa.sealBlock(block)
    return block
  }

  private genNewName(v: Variable): Variable {
    var n = this.names.get(v) ?? 1
    this.names.set(v, n + 1)
    return `${v}'${n}`
  }

  public convStmtList(code: Stmt[], block: BasicBlock): BasicBlock {
    if(code.length == 0) {
      return block
    }
    let [car, ...cdr] = code
    return this.convStmtList(cdr, this.convStmt(car, block))
  }

  public convStmt(stmt: Stmt, block: BasicBlock): BasicBlock {
    switch(stmt[0]) {
      case 'assign': {
        console.log(stmt)
        var expr = this.convExpr(stmt[2], block)

        var name = this.genNewName(stmt[1])
        block.addAssignment(name, expr)

        ssa.writeVariable(stmt[1], block, name)
        return block;
      }
      case 'if': {
        const t = this.convStmtList(stmt[2], this.makeSealedBlock(block))
        const e = this.convStmtList(stmt[3], this.makeSealedBlock(block))
        return this.makeSealedBlock(t, e);
      }
    }
  }

  public convExpr(expr: Expr, block: BasicBlock): any {
    if(typeof expr === 'string') {
      return ssa.readVariable(expr, block)
    }
    if(typeof expr === 'number') {
      return expr
    }
    switch(expr[0]) {
      case '+': {
        var lhs = this.convExpr(expr[1], block)
        var rhs = this.convExpr(expr[2], block)
        return ['+', lhs, rhs]
      }
    }
  }

  public done() {
    for(var block of this.blocks) {
      ssa.sealBlock(block)
    }
  }

}

var converter = new Converter()
var code: Stmt[] = peg.parse("x=10;if(10){}else{};y=x+5;")
const block = converter.convStmtList(code, converter.makeSealedBlock())

// converter.done();

// console.dir(ssa, { depth: null })
// console.dir(converter, { depth: null })

console.dir(converter["blocks"], { depth: null })
