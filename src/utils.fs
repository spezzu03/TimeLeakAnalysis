module utils

open System
open System.IO
open FSharp.Text.Lexing
open Lexer
open Parser
open AST

// parsing
let parseInputProgram (input: string) = // parsing program
    let lexbuf = LexBuffer<char>.FromString(input) // Convert input string into a lex buffer

    try
        let ast = Parser.start_command Lexer.tokenize lexbuf // Parse input
        Ok ast
    with ex ->
        Error("Parsing failed: " + ex.Message)

let errMess = // parsing program arguments
    "Wrong use. Flags are: 
    --mode: what shall be output. Can be one of {timed, modular, self}
    --src: source file. Standard is \"Data/input.txt\"
    --dest: destination file. Standard is \"Data/output.txt\"
    --sens: whether to use output-sensitive or -insensitive definition of ct. Can be one of {sensitive, insensitive}"


let rec parseArgs =
    function
    | [] -> Map.empty
    | "--mode" :: m :: rest ->
        let restMap = parseArgs rest

        if List.contains m [ "timed"; "modular"; "self" ] then
            if not (Map.containsKey "mode" restMap) then
                Map.add "mode" m restMap
            else
                failwith errMess
        else
            failwith errMess
    | "--dest" :: f :: rest ->
        let restMap = parseArgs rest

        if not (Map.containsKey "dest" restMap) then
            Map.add "dest" f restMap
        else
            failwith errMess
    | "--src" :: f :: rest ->
        let restMap = parseArgs rest

        if not (Map.containsKey "src" restMap) then
            Map.add "src" f restMap
        else
            failwith errMess
    | "--sens" :: s :: rest ->
        let restMap = parseArgs rest

        if List.contains s [ "sensitive"; "insensitive" ] then
            if not (Map.containsKey "sens" restMap) then
                Map.add "sens" s restMap
            else
                failwith errMess
        else
            failwith errMess
    | _ -> failwith errMess

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

// Takes a set of variables and two indices i and j and returns the string {xi = xj & yi=yj & ...}
// where xi and yi are the variables in the set
// and xj and yj are the variables in the set with the suffix j
// e.g. {x1 = x2 & y1 = y2 & ...}
let generatePrecondition (vars: Set<string>) (i: string) (j: string) : string =
    let precondition =
        vars
        |> Set.map (fun v -> sprintf "%s = %s" (v + i) (v + j))
        |> Set.union (Set.singleton "true")
        |> String.concat " & "

    sprintf " %s " precondition



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

let rec renameExpr (e: expr) (suf: string) : expr =
    match e with
    | Num(n) -> Num n
    | Variable x -> Variable(x + suf)
    | UMinusExpr e1 -> UMinusExpr(renameExpr e1 suf)
    | Array(x, e1) -> Array("_" + x, renameExpr e1 suf)
    | TimesExpr(e1, e2) -> TimesExpr(renameExpr e1 suf, renameExpr e2 suf)
    | DivExpr(e1, e2) -> DivExpr(renameExpr e1 suf, renameExpr e2 suf)
    | PlusExpr(e1, e2) -> PlusExpr(renameExpr e1 suf, renameExpr e2 suf)
    | MinusExpr(e1, e2) -> MinusExpr(renameExpr e1 suf, renameExpr e2 suf)
    | PowExpr(e1, e2) -> PowExpr(renameExpr e1 suf, renameExpr e2 suf)

let rec renameBool (b: boolean) (suf: string) : boolean =
    match b with
    | Value x -> Value x
    | BitOr(b1, b2) -> BitOr(renameBool b1 suf, renameBool b2 suf)
    | BitAnd(b1, b2) -> BitAnd(renameBool b1 suf, renameBool b2 suf)
    | LogOr(b1, b2) -> LogOr(renameBool b1 suf, renameBool b2 suf)
    | LogAnd(b1, b2) -> LogAnd(renameBool b1 suf, renameBool b2 suf)
    | Not b1 -> Not(renameBool b1 suf)
    | Equal(e1, e2) -> Equal(renameExpr e1 suf, renameExpr e2 suf)
    | NotEqual(e1, e2) -> NotEqual(renameExpr e1 suf, renameExpr e2 suf)
    | Greater(e1, e2) -> Greater(renameExpr e1 suf, renameExpr e2 suf)
    | GreaterEqual(e1, e2) -> GreaterEqual(renameExpr e1 suf, renameExpr e2 suf)
    | Less(e1, e2) -> Less(renameExpr e1 suf, renameExpr e2 suf)
    | LessEqual(e1, e2) -> LessEqual(renameExpr e1 suf, renameExpr e2 suf)

let rec rename (ast: command) (suf: string) : command =
    match ast with
    | Skip -> Skip
    | Assignment(e: expr, e': expr) -> Assignment(renameExpr e suf, renameExpr e' suf)
    | Sequence(c1, c2) -> Sequence(rename c1 suf, rename c2 suf)
    | If gc -> If(renameGuarded gc suf)
    | Do(gc, None) -> Do(renameGuarded gc suf, None)
    | Do(gc, Some(b)) -> Do(renameGuarded gc suf, Some(renameBool b suf))

and renameGuarded (gc: guarded) (suf: string) : guarded =
    match gc with
    | Arrow(b, c) -> Arrow(renameBool b suf, rename c suf)
    | Guard(gc1, gc2) -> Guard(renameGuarded gc1 suf, renameGuarded gc2 suf)


// Function to merge both ASTs
let selfcompose (ast: command) : command =
    Sequence(rename ast "1", rename ast "2")

// determinism

let rec det (c: command) : command =
    match c with
    | If gc ->
        let gc', _ = detGCfirst gc
        If gc'
    | Do(gc, inv) ->
        let gc', _ = detGCfirst gc
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

and detGCfirst gc =
    match gc with
    | Guard(gc1, gc2) ->
        let gc1', d1 = detGCfirst gc1
        let gc2', d2 = detGC gc2 d1
        Guard(gc1, gc2'), d2
    | Arrow(b, c) -> Arrow(b, c), b

let rec detEvolv (c: command) : command =
    match c with
    | If gc ->
        let gc', d = detGCfirst gc
        If(Guard(gc', Arrow(Not(d), Skip)))
    | Do(gc, inv) ->
        let gc', d = detGCfirst gc
        Do(Guard(gc', Arrow(Not(d), Skip)), inv)
    | Sequence(c, c') -> Sequence(detEvolv c, detEvolv c')
    | x -> x
