module Verification

open AST
open utils

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

// Function to merge both ASTs
let selfcompose (ast: command) : command =
    Sequence(Misc.rename ast "1", Misc.rename ast "2")
