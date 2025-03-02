// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module AST

// arithmetic expressions type definition; formed by expression and higher expression in Grammar.fsy
type expr =
    | TimesExpr of (expr * expr)
    | DivExpr of (expr * expr)
    | PlusExpr of (expr * expr)
    | MinusExpr of (expr * expr)
    | PowExpr of (expr * expr)
    | Num of int
    | UMinusExpr of (expr)
    | Variable of string
    | Array of (string * expr)

// boolean expressions type definition
type boolean =
    | Value of bool
    | BitOr of (boolean * boolean)
    | BitAnd of (boolean * boolean)
    | LogOr of (boolean * boolean)
    | LogAnd of (boolean * boolean)
    | Not of (boolean)
    | Equal of (expr * expr)
    | NotEqual of (expr * expr)
    | Greater of (expr * expr)
    | GreaterEqual of (expr * expr)
    | Less of (expr * expr)
    | LessEqual of (expr * expr)

// commands and guarded commands type definition
type command =
    | Skip
    | Sequence of (command * command)
    | Assignment of (expr * expr)
    | If of (guarded)
    | Do of (guarded * invariant)

and guarded =
    | Arrow of (boolean * command)
    | Guard of (guarded * guarded)

and invariant = Option<boolean>
