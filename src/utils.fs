module utils

open System
open System.IO
open FSharp.Text.Lexing
open Lexer
open Parser
open AST // Assuming you have an AST module for parsed results

// parsing
let parseInputProgram (input: string) =
    let lexbuf = LexBuffer<char>.FromString(input) // Convert input string into a lex buffer

    try
        let ast = Parser.start_command Lexer.tokenize lexbuf // Parse input
        Ok ast
    with ex ->
        Error("Parsing failed: " + ex.Message)

// file handling

let parseInputFile (filePath: string) : Set<string> * command =
    match Array.toList (File.ReadAllLines filePath) with
    | [] -> failwith "Empty program"
    | privateLine :: programLines ->
        let privateVars =
            privateLine
                .Replace("private:", "")
                .Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun v -> v.Trim())
            |> Set.ofArray

        match parseInputProgram (String.concat "\n" programLines) with
        | Ok ast -> (privateVars, ast)
        | Error e -> failwith ("Error " + e)

// Hoare contract
let generateHoareContract (publicVars: Set<string>) =
    "{ "
    + (publicVars
       |> Set.toList
       |> List.map (fun v -> v + " = _" + v)
       |> String.concat " & ")
    + " }"


// Write output file
let writeOutputFile (filename: string) (hoarePre: string) (hoarePost: string) (programStr: string) =
    let output = sprintf "%s\n%s\n%s\n" hoarePre programStr hoarePost
    File.WriteAllText(filename, output)


// extract variables
let rec extractAllVariablesExpr (e: expr) : Set<string> =
    match e with
    | Num(_) -> Set.empty
    | Variable x -> Set.singleton x
    | UMinusExpr e1 -> extractAllVariablesExpr e1
    | Array(x, e1) -> Set.add x (extractAllVariablesExpr e1)
    | TimesExpr(e1, e2)
    | DivExpr(e1, e2)
    | PlusExpr(e1, e2)
    | MinusExpr(e1, e2)
    | PowExpr(e1, e2) -> Set.union (extractAllVariablesExpr e1) (extractAllVariablesExpr e2)

let rec extractAllVariablesBool (b: boolean) : Set<string> =
    match b with
    | Value x -> Set.empty
    | BitOr(b1, b2)
    | BitAnd(b1, b2)
    | LogOr(b1, b2)
    | LogAnd(b1, b2) -> Set.union (extractAllVariablesBool b1) (extractAllVariablesBool b2)
    | Not(b1) -> extractAllVariablesBool b1
    | Equal(e1, e2)
    | NotEqual(e1, e2)
    | Greater(e1, e2)
    | GreaterEqual(e1, e2)
    | Less(e1, e2)
    | LessEqual(e1, e2) -> Set.union (extractAllVariablesExpr e1) (extractAllVariablesExpr e2)

let rec extractAllVariables (ast: command) : Set<string> =
    match ast with
    | Skip -> Set.empty
    | Assignment(e: expr, e': expr) -> Set.union (extractAllVariablesExpr e) (extractAllVariablesExpr e')
    | Sequence(c1, c2) -> Set.union (extractAllVariables c1) (extractAllVariables c2)
    | Do(gc, Some(inv)) -> Set.union (extractAllVariablesGuarded gc) (extractAllVariablesBool inv)
    | If gc
    | Do(gc, _) -> extractAllVariablesGuarded gc

and extractAllVariablesGuarded (gc: guarded) : Set<string> =
    match gc with
    | Arrow(b, c) -> Set.union (extractAllVariablesBool b) (extractAllVariables c)
    | Guard(gc1, gc2) -> Set.union (extractAllVariablesGuarded gc1) (extractAllVariablesGuarded gc2)

// extract private variables

let rec extractPublicVariables (c: command) (priv: Set<string>) =
    Set.difference (extractAllVariables c) priv

// duplicate code

let rec renameExpr (e: expr) : expr =
    match e with
    | Num(n) -> Num n
    | Variable x -> Variable("_" + x)
    | UMinusExpr e1 -> UMinusExpr(renameExpr e1)
    | Array(x, e1) -> Array("_" + x, renameExpr e1)
    | TimesExpr(e1, e2) -> TimesExpr(renameExpr e1, renameExpr e2)
    | DivExpr(e1, e2) -> DivExpr(renameExpr e1, renameExpr e2)
    | PlusExpr(e1, e2) -> PlusExpr(renameExpr e1, renameExpr e2)
    | MinusExpr(e1, e2) -> MinusExpr(renameExpr e1, renameExpr e2)
    | PowExpr(e1, e2) -> PowExpr(renameExpr e1, renameExpr e2)

let rec renameBool (b: boolean) : boolean =
    match b with
    | Value x -> Value x
    | BitOr(b1, b2) -> BitOr(renameBool b1, renameBool b2)
    | BitAnd(b1, b2) -> BitAnd(renameBool b1, renameBool b2)
    | LogOr(b1, b2) -> LogOr(renameBool b1, renameBool b2)
    | LogAnd(b1, b2) -> LogAnd(renameBool b1, renameBool b2)
    | Not b1 -> Not(renameBool b1)
    | Equal(e1, e2) -> Equal(renameExpr e1, renameExpr e2)
    | NotEqual(e1, e2) -> NotEqual(renameExpr e1, renameExpr e2)
    | Greater(e1, e2) -> Greater(renameExpr e1, renameExpr e2)
    | GreaterEqual(e1, e2) -> GreaterEqual(renameExpr e1, renameExpr e2)
    | Less(e1, e2) -> Less(renameExpr e1, renameExpr e2)
    | LessEqual(e1, e2) -> LessEqual(renameExpr e1, renameExpr e2)

let rec rename (ast: command) : command =
    match ast with
    | Skip -> Skip
    | Assignment(e: expr, e': expr) -> Assignment(renameExpr e, renameExpr e')
    | Sequence(c1, c2) -> Sequence(rename c1, rename c2)
    | If gc -> If(renameGuarded gc)
    | Do(gc, None) -> Do(renameGuarded gc, None)
    | Do(gc, Some(b)) -> Do(renameGuarded gc, Some(renameBool b))

and renameGuarded (gc: guarded) : guarded =
    match gc with
    | Arrow(b, c) -> Arrow(renameBool b, rename c)
    | Guard(gc1, gc2) -> Guard(renameGuarded gc1, renameGuarded gc2)


// Function to merge both ASTs
let mergePrograms (originalAST: command) (renamedAST: command) : command = Sequence(originalAST, renamedAST)

// determinism

let rec det (c: command) : command =
    match c with
    | If gc ->
        let gc', _ = detGC gc (Value false)
        If gc'
    | Do(gc, inv) ->
        let gc', _ = detGC gc (Value false)
        Do(gc', inv)
    | Sequence(c, c') -> Sequence(det c, det c')
    | x -> x

and detGC gc d =
    match gc with
    | Arrow(b, c) -> Arrow(BitAnd(b, Not(d)), det c), BitOr(b, d)
    | Guard(gc1, gc2) ->
        let gc1', d1 = detGC gc1 d
        let gc2', d2 = detGC gc2 d1
        Guard(gc1', gc2'), d2
