package cs320

import Value._

trait Handler
case class ExHandler(expr_h: Expr, env_h: Env, k_h: Cont, pre_h: Handler) extends Handler
case object EmptyH extends Handler

object Implementation extends Template {

  def interp(expr: Expr): Value = semihelper(expr, Map(), EmptyH)
  def semihelper(expr: Expr, env: Env, h: Handler) = helper(expr, env, h, x => x)
  def helper(expr: Expr, env: Env, h: Handler, k: Cont): Value = expr match {
    case Id(x) => k(lookup(x, env))
    case IntE(n) => k(IntV(n))
    case BooleanE(bool) => k(BooleanV(bool))
    case Add(l, r) => helper(l, env, h, lv => helper(r, env, h, rv => k(add(lv, rv))))
    case Mul(l, r) => helper(l, env, h, lv => helper(r, env, h, rv => k(mul(lv, rv))))
    case Div(l, r) => helper(Eq(Add(l, r), r), env, h, iszero =>
        iszero match {
          case BooleanV(true) => error(s"divied by 0")
          case v => helper(l, env, h, lv => helper(r, env, h, rv => k(div(lv, rv))))
    })
    case Mod(l, r) => helper(Eq(Add(l, r), r), env, h, iszero =>
        iszero match {
          case BooleanV(true) => error(s"divied by 0")
          case v => helper(l, env, h, lv => helper(r, env, h, rv => k(mod(lv, rv))))
    })
    case Eq(l, r) => helper(l, env, h, lv => helper(r, env, h, rv => k(eq(lv, rv))))
    case Lt(l, r) => helper(l, env, h, lv => helper(r, env, h, rv => k(lt(lv, rv))))
    case If(con, tb, fb) => helper(con, env, h, conV => 
        conV match {
          case BooleanV(true) => helper(tb, env, h, k)
          case BooleanV(false) => helper(fb, env, h, k)
          case v => error(s"not boolean: $con")
    })
    case TupleE(expr_list) => k(TupleV(maketuple(expr_list, env)))
    case Proj(e, i) => helper(e, env, h, istuple => 
      istuple match {
        case TupleV(listv) =>
          if(listv.length < i) error(s"Proj: not enough long: $i")
          else k(ithelement(listv, i))
        case v => error(s"Proj: not Tuple: $v")
    })
    case NilE => k(NilV)
    case ConsE(e_1, e_2) => helper(e_1, env, h , v_1 =>
      helper(e_2, env, h, v_2 => v_2 match {
        case ConsV(ch, ct) => k(ConsV(v_1, v_2))
        case NilV => k(ConsV(v_1, v_2))
        case v => error(s"Conse: not cons: $v")
    }))
    case Empty(expr) => helper(expr, env, h, iscons =>
      iscons match {
        case NilV => k(BooleanV(true))
        case ConsV(ch, ct) => k(BooleanV(false))
        case v => error(s"not cons: $v")
    })
    case Head(expr) => helper(expr, env, h, iscons =>
      iscons match {
        case ConsV(ch, ct) => k(ch)
        case NilV => error(s"empty list")
        case v => error(s"Head: not ConsV: $v")
    })
    case Tail(expr) => helper(expr, env, h, iscons =>
      iscons match {
        case ConsV(ch, ct) => k(ct)
        case NilV => error(s"empty list")
        case v => error(s"Head: not ConsV: $v")
    })
    case Val(x, e_1, e_2) =>
      helper(e_1, env, h, v_1 =>
        helper(e_2, env + (x -> v_1), h, v_2 =>
        k(v_2)
    ))
    case Vcc(x, b) => helper(b, env + (x -> ContV(k)), h, k)
    case Fun(paralist, body) => k(CloV(paralist, body, env))
    case RecFuns(funlist, body) => helper(body, recur(funlist, env), h, k)
    case App(expr, arguments) => app(expr, arguments, env, h, k)
    /*helper(expr, env, h, fv =>
      fv match {
        case CloV(paralist, body, fenv) =>
          if (paralist.length == arguments.length)
            helper(body, appenv(paralist, maketuple(arguments, env), fenv), h, k)
          else
            error(s"app: not same length")
        case ContV(newk) => arguments match {
          case firstexpr :: Nil => helper(firstexpr, env, h, fv => newk(fv))
          case v => error(s"App: not one argument: $v")
        }
        case v => error(s"app: not Clov")
      }
    )*/
    case Test(expr, typ) => k(ftyp(typ, expr, env, h, k))
    case Throw(e) => helper(e, env, h, v =>
      h match {
        case ExHandler(eh, envh, kh, pre_h) =>
          helper(eh, envh, pre_h, vh =>
            vh match {
                case CloV(para: List[String], ec, envc) => para match {
                  case x :: Nil => helper(ec, envc + (x -> v), pre_h, kh)
                  case notfit => error(s"more than two parameters: $notfit")
                }
                case ContV(newk) => newk(v)
                case notfit => error(s"throw: not clov or contv: $notfit")
            }
          )
        case v => error(s"throw: no Exception handler: $v")
      })
    case Try(expr, eh) => helper(expr, env, ExHandler(eh, env, k, h), k)
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

  def maketuple(expr_list: List[Expr], env: Env, h: Handler): List[Value] = expr_list match {
    case head :: tail => semihelper(head, env, h) :: maketuple(tail, env, h)
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
  def ftyp(t: Type, e: Expr, env: Env, h: Handler, k: Cont): Value = t match {
    case IntT => helper(e, env, h, value => value match{
      case IntV(n) => k(BooleanV(true))
      case v => k(BooleanV(false))
    })
    case BooleanT => helper(e, env, h, value => value match{
      case BooleanV(b) => k(BooleanV(true))
      case v => k(BooleanV(false))
    })
    case TupleT => helper(e, env, h, value => value match{
      case TupleV(v) => k(BooleanV(true))
      case v => k(BooleanV(false))
    })
    case ListT => helper(e, env, h, value => value match{
      case ConsV(ch, ct) => k(BooleanV(true))
      case NilV => k(BooleanV(true))
      case v => k(BooleanV(false))
    })
    case FunctionT => helper(e, env, h, value => value match {
      case CloV(p, b, env) => k(BooleanV(true))
      case ContV(cont) => k(BooleanV(true))
      case v => k(BooleanV(false))      
    })
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



  def app(expr: Expr, arguments: List[Expr], env: Env, h: Handler, k: Cont): Value = {
    helper(expr, env, h, fv => {
val a = maketuple(argument, env)
    fv match {
      case CloV(paralist, body, fenv) =>
        if (paralist.length == arguments.length)
          helper(body, appenv(paralist, maketuple(arguments, env), fenv), h, k)
        else
          error(s"app: not same length")
      case ContV(newk) => arguments match {
        case firstexpr :: Nil => helper(firstexpr, env, h, fv => newk(fv))
        case v => error(s"App: not one argument: $v")
      }
      case v => error(s"app: not Clov")
    }
    
    })
  }
}
