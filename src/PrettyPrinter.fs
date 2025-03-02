module PrettyPrinter

open AST

let rec prettifyExpression ast : string =
    match ast with
    | Num(n) -> (string) n
    | Variable(x) -> x
    | UMinusExpr(e1) -> "-" + prettifyExpression e1
    | Array(x, e1) -> x + "[" + prettifyExpression e1 + "]"
    | TimesExpr(e1, e2) -> "(" + prettifyExpression e1 + " * " + prettifyExpression e2 + ")"
    | DivExpr(e1, e2) -> "(" + prettifyExpression e1 + " / " + prettifyExpression e2 + ")"
    | PlusExpr(e1, e2) -> "(" + prettifyExpression e1 + " + " + prettifyExpression e2 + ")"
    | MinusExpr(e1, e2) -> "(" + prettifyExpression e1 + " - " + prettifyExpression e2 + ")"
    | PowExpr(e1, e2) -> "(" + prettifyExpression e1 + " ^ " + prettifyExpression e2 + ")"

let rec prettifyBoolean ast : string =
    match ast with
    | Value x -> ((string) x).ToLower()
    | BitOr(b1, b2) -> "(" + prettifyBoolean b1 + " | " + prettifyBoolean b2 + ")"
    | BitAnd(b1, b2) -> "(" + prettifyBoolean b1 + " & " + prettifyBoolean b2 + ")"
    | LogOr(b1, b2) -> "(" + prettifyBoolean b1 + " || " + prettifyBoolean b2 + ")"
    | LogAnd(b1, b2) -> "(" + prettifyBoolean b1 + " && " + prettifyBoolean b2 + ")"
    | Not(b1) -> "!" + prettifyBoolean b1
    | Equal(e1, e2) -> "(" + prettifyExpression e1 + " = " + prettifyExpression e2 + ")"
    | NotEqual(e1, e2) -> "(" + prettifyExpression e1 + " != " + prettifyExpression e2 + ")"
    | Greater(e1, e2) -> "(" + prettifyExpression e1 + " > " + prettifyExpression e2 + ")"
    | GreaterEqual(e1, e2) -> "(" + prettifyExpression e1 + " >= " + prettifyExpression e2 + ")"
    | Less(e1, e2) -> "(" + prettifyExpression e1 + " < " + prettifyExpression e2 + ")"
    | LessEqual(e1, e2) -> "(" + prettifyExpression e1 + " <= " + prettifyExpression e2 + ")"

let tabs n = String.replicate n "\t"

let rec prettifyH (ast: AST.command) (currentDepth: int) : string =
    match ast with
    | Skip -> "skip"
    | Sequence(c1, c2) ->
        prettifyH c1 currentDepth
        + ";\n"
        + tabs currentDepth
        + prettifyH c2 currentDepth
    | Assignment(e1, e2) -> prettifyExpression e1 + " := " + prettifyExpression e2
    | If gc -> "if " + prettifyGuarded gc (currentDepth) + "\n" + tabs currentDepth + "fi"
    | Do(gc, Some(inv)) ->
        "do"
        + "["
        + prettifyBoolean inv
        + "]"
        + "\n"
        + tabs (currentDepth)
        + prettifyGuarded gc (currentDepth)
        + "\n"
        + tabs currentDepth
        + "od"
    | Do(gc, _) ->
        "do"
        + "[false]"
        + "\n"
        + tabs (currentDepth)
        + prettifyGuarded gc (currentDepth)
        + "\n"
        + tabs currentDepth
        + "od"

and prettifyGuarded (gc: guarded) (currentDepth: int) : string =
    match gc with
    | Arrow(b, c) -> prettifyBoolean b + " -> " + prettifyH c (currentDepth + 1)
    | Guard(gc1, gc2) ->
        prettifyGuarded gc1 currentDepth
        + "\n"
        + tabs (currentDepth)
        + "[] "
        + prettifyGuarded gc2 currentDepth

let prettify c = prettifyH c 0 + "\n"
