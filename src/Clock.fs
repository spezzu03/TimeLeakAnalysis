module Clock

open AST

let rec clockExpressionH (e: expr) : int =
    match e with
    | Num(_)
    | Variable(_)
    | UMinusExpr(Num(_)) -> 0 // Possibly change the minus here
    | Array(_, e) -> clockExpressionH e
    | UMinusExpr(e) -> 1 + clockExpressionH e
    // binary operators
    | TimesExpr(e1, e2)
    | DivExpr(e1, e2)
    | PlusExpr(e1, e2)
    | MinusExpr(e1, e2)
    | PowExpr(e1, e2) -> 1 + clockExpressionH e1 + clockExpressionH e2

let clockExpression (e: expr) = Num(clockExpressionH e)

let rec clockBooleanH (b: boolean) : int =
    match b with
    | Value(_) -> 0 // true and false are constants
    | Not(b) -> 1 + clockBooleanH b
    // binary operators
    | BitOr(b1, b2)
    | BitAnd(b1, b2)
    | LogOr(b1, b2)
    | LogAnd(b1, b2) -> 1 + clockBooleanH b1 + clockBooleanH b2
    | Equal(e1, e2)
    | NotEqual(e1, e2)
    | Greater(e1, e2)
    | GreaterEqual(e1, e2)
    | Less(e1, e2)
    | LessEqual(e1, e2) -> 1 + clockExpressionH e1 + clockExpressionH e2

let clockBoolean (b: boolean) : expr = Num(clockBooleanH b)

let rec clock (ast: command) =
    match ast with
    | Skip -> Skip
    | Assignment(v, e) ->
        Sequence(
            Assignment(
                Variable "time",
                PlusExpr(PlusExpr(PlusExpr(Variable("time"), Num(1)), clockExpression v), clockExpression e)
            ),
            Assignment(v, e)

        )
    | Sequence(c1, c2) -> Sequence(clock c1, clock c2)
    | If gc ->
        let gc', _ = clockGuarded gc (Num(0))
        If(gc')
    | Do(gc, inv) ->
        let gc', tau_done = clockGuarded gc (Num(0))
        Sequence(Do(gc', inv), Assignment(Variable("time"), PlusExpr(Variable("time"), tau_done)))

and clockGuarded (gc: guarded) (tau: expr) : guarded * expr =
    match gc with
    | Arrow(b, c) ->
        let tau' = PlusExpr(clockBoolean b, tau)
        Arrow(b, Sequence(Assignment(Variable("time"), PlusExpr(Variable("time"), tau')), clock c)), tau'

    | Guard(gc1, gc2) ->
        let gc1', tau1 = clockGuarded gc1 tau
        let gc2', tau2 = clockGuarded gc2 tau1
        Guard(gc1', gc2'), tau2


let rec instrument (ast: command) =
    Sequence(Assignment(Variable("time"), Num(0)), clock ast)
