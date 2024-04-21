package cs320

object Implementation extends Template {

  def typeCheck(e: Typed.Expr): Typed.Type = T.typeCheck(e)

  def interp(e: Untyped.Expr): Untyped.Value = U.interp(e)

  object T {
    import Typed._
    
    trait AddType
    case class TypeScheme(arguments: List[VarT], sctype: Type) extends AddType
    case class TypeDef() extends AddType
    case class TEnv(
      tname: Map[String, TypeDef],
      ts: Map[String, (TypeScheme, Boolean)],
      tv: Map[VarT, Type]
    ) {
      def add(x: String, t: TypeDef): TEnv = {
        TEnv(tname + (x -> t), ts, tv)
      }
      def add(x: String, s: (TypeScheme, Boolean)): TEnv = {
        TEnv(tname, ts + (x -> s), tv)
      }

      def add(x: String, t: Type): TEnv ={
        TEnv(tname, ts, tv + (x -> t))
      }
      def contains(x: String): Boolean =
        tv.contains(x)
    }

    def typeCheck(expr: Expr): Type = thelper(expr, (Map(), Map(), Map()))

    def thelper(expr: Expr, env: Tenv): Type = expr match {
      case Id(name, targs) =>
        targs.map(wfType(_, env))
        val (typsch, mt) = env.ts(name)
        if (targs.length == (typsch.arguments).length) substitute(typsch.sctype, typsch.arguments, targs) else error("diff#: Id")
      // integer
      case IntE(value) => IntT
      // boolean
      case BooleanE(value) => BooleanT
      // unit
      case UnitE => UnitT
      // addition
      case Add(left, right) => if ((thelper(left, tenv), thelper(right, tenv)) == (NumT, NumT)) NumT else error("not NumT")
      // multiplication
      case Mul(left, right) => if ((thelper(left, tenv), thelper(right, tenv)) == (NumT, NumT)) NumT else error("not NumT")
      // division
      case Div(left, right) => if ((thelper(left, tenv), thelper(right, tenv)) == (NumT, NumT)) NumT else error("not NumT")
      // modulo
      case Mod(left, right) => if ((thelper(left, tenv), thelper(right, tenv)) == (NumT, NumT)) NumT else error("not NumT")
      // equal-to
      case Eq(left, right) => if ((thelper(left, tenv), thelper(right, tenv)) == (NumT, NumT)) BooleanT else error("not NumT")
      // less-than
      case Lt(left, right) => if ((thelper(left, tenv), thelper(right, tenv)) == (NumT, NumT)) BooleanT else error("not NumT")
      // sequence
      case Sequence(left, right) =>
        wfType(thelper(left, tenv))
        thelper(right, tenv)
      case If(cond, texpr, fexpr) =>
        if (thelper(cond, tenv) == BooleanT) {
          val ttyp = thelper(texpr, tenv)
          if (ttyp == thelper(fexpr, tenv)) ttyp else error("If: diff type")
        }
        else ("If: not BooleanT")
      case Val(mut, name, typ, expr, body) =>
        typ match {
          case Some(x) =>
            wfType(x, env)
            if (x == thelper(expr, env)) {
              val typsch = TypeScheme(Nil, x)
              thelper(body, env.add(name, (typsch, mut)))
            }
            else error("val: diff type")
          case None =>
            val typsch = TypeScheme(Nil, thelper(expr, env))
            thelper(body, env.add(name, (typsch, mut)))
        }
      case RecBinds(defs: List[RecDef], body: Expr) =>
        val ntenv = tyrec(defs, env)
        defs.map(wfType(_, ntenv))
        val typ = thelper(body, ntenv)
        wfType(typ, ntenv)
        typ
      case Fun(params, body) =>
        val typs = params.map(_._2)
        typs.map(wfType(_, env))
        val typ = thelper(body, pairs2tenv(params, env))
        ArrowT(typs, typ)
      case Assign(name, expr) =>
        env.st(name) match {
          case (TypeScheme(Nil, sctype), true) => sctype match {
            case thelper(expr, env) => UnitT
            case v => error("assign: diff typ")
          }
          case w => error("assign: not correct typsheme")
        }
      case App(fun: Expr, args: List[Expr]) =>
        thelper(fun, env) match {
          case ArrowT(ptypes: List[Type], rtype: Type) =>
            if (ptypes.length == args.length) {
              val typs = args.map(thelper(_, env))
              if (typs.diff(ptypes) == Nil) rtype else error("app: diff typ")
            }
            else error("app: diff #")
          case w => error("app: not fun")
        }
      case Match(expr: Expr, cases: List[Case]) =>
        thelper(expr, env) match {
          case AppT(t: String, targs: List[Type]) => env.tname(t) match {
            case TypeDef(name: String, tparams: List[String], variants: List[Variant]) =>
              if (tparams.length == targs.length) {
                if (cases.length == variants.length) {
                  allsame(typcase(cases, targs, tparams, variants, env))
                }
                else error("match: diff#")
              }
              else error("match: diff #")
            case w => error("no type")
          }
          case w => error("match: not appt")
        }
    }

    def allsame(typs: List[Type]): Type = typs match {
      case h :: Nil => h
      case h :: t => if (h == t.apply(0)) allsame(t) else error("allsame: diff type")
    }

    def typcase(cases: List[Case], targs: List[Type], tparams: List[String], variants: List[Variant], env: TEnv): Type[List] = cases match {
      case Case(variant: String, names: List[String], body: Expr) :: t =>
        if (tparams.length == variants.length) {
          findvariant(variant, variants) match {
            case Variant(name, params) =>
              if (targs.length == params.length) {
                val newtyplist = params.map(substitute(_, tparams, targs))
                thelper(body, schemes2tenv(names, newtyplist, env)) :: typcase(t, targs, tparams, variants, env)
              }
              else error("typcase: diff#")
            case v => error("wtf")
          }
        }
        else error("typcase: diff#")
      case Nil => Nil
    }

    def findvariant(x: String, variants: List[Variant]): Variant = variants match {
      case Variant(name, params) :: t => if (name == x) Variant(name, params) else findvariant(x, t)
      case Nil => error("findvariant: no variant")
    }

    def tyrec(defs: List[RecDef], env: TEnv): TEnv = defs match {
      case h :: t => h match {
        case Lazy(name, typ, expr) => {
          wfType(typ, env)
          if (thelper(expr, env) == typ) {
            tyrec(t, env.add(name, (TypeScheme(Nil, typ), false)))
          }
          else error("rec_lazy: diff type")
        }
        case RecFun(name, tparams, params, rtype, body) => {
          tparams.map(if(env.contains(VarT(_))) error("tyrec_recfun: tparams in domain of tenv"))
          val ntenv = string2tenv(tparams, env)
          val typs = params.map(_._2)
          typs.map(wfType(_, ntenv))
          ntenv = pairs2tenv(params, ntenv)
          if (rtype == thelper(body, ntenv)) {
            val varts = tparams.map(VarT(_))
            tyrec(t, env.add(name, (TypeScheme(varts, ArrowT(typs, rtype)), false)))
          }
          else error("tyrec_recfun: diff type")
        }
        case TypeDef(name: String, tparams: List[String], variants: List[Variant]) => {
          tparams.map(if(env.contains(VarT(_))) error("tyrec_recfun: tparams in domain of tenv"))
          val ntenv = string2tenv(tparams, env)
          wfvariants(variants, ntenv)
          ntenv = env.add(name, TypeDef(name, tparams, variants))
          ntenv = typedef2tenv(name, tparams, variants, ntenv)
          tyrec(t, ntenv)
        }
      }
      case Nil => env
    }

    def schemes2tenv(names: List[String], typs: List[Type], env: TEnv): TEnv = (names, typs) match {
      case (hx, ht) :: (tx, tt) => schemes2tenv(tx, tt, env.add(hx, (TypeScheme(Nil, ht), false)))
      case Nil => env
    }

    def string2tenv(strings: List[String], env: TEnv): TEnv = strings match {
      case h :: t => string2vart(t, env.add(VarT(h), UnitT))
      case Nil => env
    }
    def pairs2tenv(pairs: List[(String, Type)], env: TEnv): TEnv = pairs match {
      case (hs, ht) :: (ts, tt) => pairs2tenv((ts, tt), env.add(hs, (TypeScheme(Nil, ht), false)))
      case Nil => env
    }
    def wfvariants(variants: List[Variant], env: TEnv): Unit = variants match {
      case Variant(name, params) :: t =>
        params.map(wfType(_, env))
        wfvariants(t, env)
      case Nil =>
    }
    def typedef2tenv(t: String, tparams: List[String], variants: List[Variant], env: TEnv): TEnv = variants match {
      case Variant(name, params) :: t => params match {
        case Nil => ypedef2tenv(t, variants, env.add(name, (TypeScheme(tparams.map(VarT(_)), AppT(t, tparams.map(VarT(_)))), false)))
        case l => typedef2tenv(t, variants, env.add(name, (TypeScheme(tparams.map(VarT(_)), ArrowT(params, AppT(t, tparams.map(VarT(_))))), false)))
      }
      case Nil => env
    }

    def wfType(t: Type, env: TEnv): Unit = t match {
      case AppT(name, targs) =>
        targs.map(wfType(_, env))
        env.tname(name) match {
          case TypeDef(id, tparams, variants) => if (tparams.length == targs.length) 0 else error("diff#: appt")
          case v => error(s"free id: $name")
        }
      case VarT(name) => env.tv(name)
      case NumT =>
      case UnitT =>
      case ArrowT(ptypes, rtype) =>
        ptypes.map(wfType(_, env))
        wfType(rtype, env)
    }
    def substitute(sctype: Type, argus: List[VarT], tars: List[Type]): Type = sctype match {
        case AppT(name, targs) =>
          AppT(name, targs.map(substitute(_, argus, tars)))
        case VarT(name) => 
          val index = argus.indexOf(sctype)
          if (index == -1) sctype else tars(index)
        case ArrowT(ptypes, rtype) =>
          ArrowT(ptypes.map(substitute(_, argus, tars)), substitute(rtypes, argus, tars))
        case v => v
    }




  }

  object U {
    import Untyped._

    type Sto = Map[Addr, Value]

    def interp(expr: Expr): Value = {
      val (value, sto) = ihelper(expr, Map(), Map())
      value
    }
    def ihelper(expr: Expr, env: Env, sto: Sto): (Value, Sto) = expr match {
      case Id(name) =>
        val a = lookupenv(name, env)
        lookupsto(a, sto) match {
          case ExprV(lexpr: Untyped.Expr, lenv: Env) =>
            val (v, s) = ihelper(lexpr, lenv, sto)
            (v, s + (a -> v))
          case v => (v, sto)
        }
      case IntE(n) => (IntV(n), sto)
      case BooleanE(b) => (BooleanV(b), sto)
      case UnitE => (UnitV, sto)
      case Add(l, r) =>
        val (lv, ls) = ihelper(l, env, sto)
        val (rv, rs) = ihelper(r, env, ls)
        (lv, rv) match {
          case (IntV(n), IntV(m)) => (IntV(n + m), rs)
          case v => error(s"not IntV: $lv, $rv")
      }
      case Mul(l, r) =>
        val (lv, ls) = ihelper(l, env, sto)
        val (rv, rs) = ihelper(r, env, ls)
        (lv, rv) match {
          case (IntV(n), IntV(m)) => (IntV(n * m), rs)
          case v => error(s"not IntV: $lv, $rv")
      }
      case Div(l, r) =>
        val (lv, ls) = ihelper(l, env, sto)
        val (rv, rs) = ihelper(r, env, ls)
        (lv, rv) match {
          case (IntV(n), IntV(m)) => if (m == 0) error("divided by 0") else (IntV(n / m), rs)
          case v => error(s"not IntV: $lv, $rv")
      }
      case Mod(l, r) =>
        val (lv, ls) = ihelper(l, env, sto)
        val (rv, rs) = ihelper(r, env, ls)
        (lv, rv) match {
          case (IntV(n), IntV(m)) => if (m == 0) error("divided by 0") else (IntV(n % m), rs)
          case v => error(s"not IntV: $lv, $rv")
      }
      case Eq(l, r) =>
        val (lv, ls) = ihelper(l, env, sto)
        val (rv, rs) = ihelper(r, env, ls)
        (lv, rv) match {
          case (IntV(n), IntV(m)) => if (n == m) (BooleanV(true), rs) else (BooleanV(false), rs)
          case v => error(s"not IntV: $lv, $rv")
      }
      case Lt(l, r) =>
        val (lv, ls) = ihelper(l, env, sto)
        val (rv, rs) = ihelper(r, env, ls)
        (lv, rv) match {
          case (IntV(n), IntV(m)) => if (n < m) (BooleanV(true), rs) else (BooleanV(false), rs)
          case v => error(s"not IntV: $lv, $rv")
      }
      case Sequence(l, r) =>
        val (lv, ls) = ihelper(l, env, sto)
        ihelper(r, env, ls)
      case If(cond, texpr, fexpr) =>
        val (condv, conds) = ihelper(cond, env, sto)
        condv match {
          case BooleanV(con) => if (con) ihelper(texpr, env, conds) else ihelper(fexpr, env, conds)
          case v => error(s"not BooleanV: $condv")
        }
      case Val(name, expr, body) =>
        val (v, s) = ihelper(expr, env, sto)
        val a = newadd(0, s)
        ihelper(body, env + (name -> a), s + (a -> v))
      case RecBinds(defs, body) =>
        val (envadd, nsto) = rec(defs, env, Map(), sto)
        ihelper(body, env ++ envadd, nsto)
      case Fun(params, body) => (CloV(params, body, env), sto)
      case Assign(name, expr) =>
        val a = lookupenv(name, env)
        val (v, s) = ihelper(expr, env, sto)
        (UnitV, s + (a -> v))
      case App(fun: Expr, args: List[Expr]) =>
        val (v, s) = ihelper(fun, env, sto)
        val (vlist, nsto) = appargs(args, Nil, env, s)
        v match {
          case CloV(params, body, fenv) =>
            if (params.length == args.length) {
              val (newenv, newsto) = arg_mapping(params, vlist, fenv, nsto)
              ihelper(body, newenv, newsto)
            }
            else error("app error: diff length")
          case ConstructorV(name) => (VariantV(name, vlist), nsto)
          case w => error("app error")
        }
      case Match(expr, cases) =>
        val (v, s) = ihelper(expr, env, sto)
        v match {
          case VariantV(name, values) =>
            val matchcase = findcase(name, cases)
            if (values.length == (matchcase.names).length) {
              val (nenv, nsto) = arg_mapping(matchcase.names, values, env, s)
              ihelper(matchcase.body, nenv, nsto)
            }
            else error("match: diff length")
          case w => error("match error")
        }
    }

    def lookupenv(id: String, env: Env): Addr = (env.getOrElse(id, error(s"free id(env) : $id")))
    def lookupsto(addr: Addr, sto: Sto): Value = (sto.getOrElse(addr, error(s"free id(sto) : $addr")))
    def newadd(n: Addr, sto: Sto): Addr = sto.get(n) match {
      case Some(v) => newadd(n + 1, sto)
      case None => n
    }
    def rec(defs: List[RecDef], env: Env, nenv: Env, sto: Sto): (Env, Sto) = defs match {
      case h :: t => h match {
          case Lazy(name, expr) =>
            val a = newadd(0, sto)
            rec(t, env, nenv + (name -> a), sto + (a -> ExprV(expr, env)))
          case RecFun(name, params, body) =>
            val a = newadd(0, sto)
            rec(t, env, nenv + (name -> a), sto + (a -> CloV(params, body, env)))
          case TypeDef(variants) =>
            val (newenv, newsto) = rec_type(variants, nenv, sto)
            rec(t, env, newenv, newsto)
        }
        case Nil => (nenv, sto)
    }
    def rec_type(variants: List[Variant], nenv: Env, nsto: Sto): (Env, Sto) = variants match {
      case Variant(name, empty) :: t =>
        val a = newadd(0, nsto)
        if (empty)
          rec_type(t, nenv + (name -> a), nsto + (a -> VariantV(name, Nil)))
        else
          rec_type(t, nenv + (name -> a), nsto + (a -> ConstructorV(name)))
      case Nil => (nenv, nsto)
    }
    def appargs(args: List[Expr], vlist: List[Value], env: Env, sto: Sto): (List[Value], Sto) = args match {
      case h :: t =>
        val (v, s) = ihelper(h, env, sto)
        appargs(t, v :: vlist, env, s)
      case Nil => (vlist, sto)
    }
    def arg_mapping(params: List[String], vlist: List[Value], env: Env, sto: Sto): (Env, Sto) = (params, vlist) match {
      case (hp :: tp, hv :: tv) =>
        val a = newadd(0, sto)
        arg_mapping(tp, tv, env + (hp -> a), sto + (a -> hv))
      case (Nil, Nil) => (env, sto)
    }
    def findcase(name: String, cases: List[Case]): Case = cases match {
      case Case(variant, names, body) :: t => if (variant == name) Case(variant, names, body) else findcase(name, t)
      case Nil => error("no case")
    }

    
  }
}
