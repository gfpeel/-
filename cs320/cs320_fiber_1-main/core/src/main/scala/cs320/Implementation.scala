package cs320

import Value._

object Implementation extends Template {

  def interp(expr: Expr): Value = helper(expr, Map())
  def helper(expr: Expr, env: Env): Value = expr match {
    case Id(x) => lookup(x, env)
    case IntE(n) => IntV(n)
    case BooleanE(bool) => BooleanV(bool)
    case Add(l, r) => add(helper(l, env), helper(r, env))
    case Mul(l, r) => mul(helper(l, env), helper(r, env))
    case Div(l, r) => eq(helper(Add(l, r), env), helper(l, env)) match {
      case BooleanV(true) => error(s"divied by 0")
      case v => div(helper(l, env), helper(r, env))
    }
    case Mod(l, r) => eq(helper(Add(l, r), env), helper(l, env)) match {
      case BooleanV(true) => error(s"divied by 0")
      case v => mod(helper(l, env), helper(r, env))
    }
    case Eq(l, r) => eq(helper(l, env), helper(r, env))
    case Lt(l, r) => lt(helper(l, env), helper(r, env))
    case If(e_1, e_2, e_3) => helper(e_1, env) match {
      case BooleanV(true) => helper(e_2, env)
      case BooleanV(false) => helper(e_3, env)
      case v => error(s"If: not boolean: $v")
    }
    case TupleE(expr_list) => TupleV(maketuple(expr_list, env))
    case Proj(e, i) => helper(e, env) match {
      case TupleV(listv) =>
        if(listv.length < i) error(s"Proj: not enough long: $i")
        else ithelement(listv, i)
      case v => error(s"Proj: not Tuple: $v")
    }
    case NilE => NilV
    case ConsE(e_1, e_2) => helper(e_2, env) match {
      case ConsV(ch, ct) => ConsV(helper(e_1, env), ConsV(ch, ct))
      case NilV => ConsV(helper(e_1, env), NilV)
      case v => error(s"Conse: not cons: $v")
    }
    case Empty(expr) => helper(expr, env) match {
      case ConsV(ch, ct) => BooleanV(false)
      case NilV => BooleanV(true)
      case v => error(s"isEmpty: not cons: $v")
    }
    case Head(expr) => helper(expr, env) match {
      case ConsV(ch, ct) => ch
      case NilV => error(s"Head: empty list")
      case v => error(s"Head: not cons: $v")
    }
    case Tail(expr) => helper(expr, env) match {
      case ConsV(ch, ct) => ct
      case NilV => error(s"Head: empty list")
      case v => error(s"Head: not cons: $v")
    }
    case Val(x, expr, body) =>
      helper(body, env + (x -> helper(expr, env)))
    case Fun(paralist, body) => CloV(paralist, body, env)
    case RecFuns(funlist, body) =>
      helper(body, recur(funlist, env))
    case App(expr, arguments) => helper(expr, env) match {
      case CloV(paralist, body, fenv) =>
        if (paralist.length == arguments.length)
          helper(body, appenv(paralist, maketuple(arguments, env), fenv))
        else
          error(s"app: not same length")
      case v => error(s"app: not Clov")
    }
    case Test(expr, typ) => ftyp(typ, expr, env)
  }

  def lookup(x: String, env: Env): Value =
    env.getOrElse(x, error(s"free identifier: $x"))

  def IntVOP(op: (BigInt, BigInt) => BigInt): (Value, Value) => Value = (_, _) match {
    case (IntV(m), IntV(n)) => IntV(op(m, n))
    case (x, y) => error(s"Intcal: not both numbers: $x, $y")
  }
  val add = IntVOP(_ + _)
  val mul = IntVOP(_ * _)
  val div = IntVOP(_ / _)
  val mod = IntVOP(_ % _)

  def ConVOP(op: (BigInt, BigInt) => Boolean): (Value, Value) => Value = (_, _) match {
    case (IntV(m), IntV(n)) => BooleanV(op(m, n))
    case (x,y) => error(s"condition: not both numbers: $x, $y")
  }
  val eq = ConVOP(_ == _)
  val lt = ConVOP(_ < _)

  def maketuple(expr_list: List[Expr], env: Env): List[Value] = expr_list match {
    case head :: tail => helper(head, env) :: maketuple(tail, env)
    case Nil => Nil
  }

  def ithelement(listv: List[Value], i: Int): Value = listv match {
    case head :: tail => if(i == 1) head else ithelement(tail, i-1)
    case Nil => error(s"ithelement: not enough long")
  }

  def appenv(paralist: List[String], arguments: List[Value], env: Env): Env =
    paralist match {
      case parahead :: paratail => arguments match {
        case arguhead :: argutail => appenv(paratail, argutail, env) + (parahead -> arguhead)
      }
      case Nil => env
    }
  def ftyp(t: Type, e: Expr, env: Env): Value = t match {
    case IntT => helper(e, env) match {
      case IntV(n) => BooleanV(true)
      case v => BooleanV(false)
    }
    case BooleanT => helper(e, env) match {
      case BooleanV(n) => BooleanV(true)
      case v => BooleanV(false)
    } 
    case TupleT => helper(e, env) match {
      case TupleV(v) => BooleanV(true)
      case v => BooleanV(false)
    } 
    case ListT => helper(e, env) match {
      case ConsV(h, t) => BooleanV(true)
      case NilV => BooleanV(true)
      case v => BooleanV(false)
    } 
    case FunctionT => helper(e, env) match {
      case CloV(p, b, env) => BooleanV(true)
      case v => BooleanV(false)
    } 
    case x => error(s"not Type: $t")
  }

    def recur(funlist: List[FunDef], env: Env) : Env = {
    val clovlist = fundef2clov(funlist, env)
    val nenv = rec_nenv(clovlist, funlist, env)
    clovlist.map(_.env = nenv)
    nenv
  }
  def fundef2clov(funlist: List[FunDef], env: Env): List[CloV] = funlist match {
    case head :: tail =>
      CloV(head.parameters, head.body, env) :: fundef2clov(tail, env)
    case Nil => Nil
  }
  def rec_nenv(clovlist: List[Value], funlist: List[FunDef], env: Env): Env = clovlist match {
    case clovhead :: clovtail => funlist match {
      case head :: tail =>
        rec_nenv(clovtail, tail, env) + (head.name -> clovhead)
    }
    case Nil => env
  }
}
