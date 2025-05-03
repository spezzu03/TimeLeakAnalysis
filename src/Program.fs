open System.IO
open utils
open Clock
open ModularComposition

[<EntryPoint>]
let main argv =
    try
        let args = Parsing.parseArgs (Array.toList argv)

        let source =
            if Map.containsKey "src" args then
                Map.find "src" args
            else
                "Data/input.txt"

        let dest =
            if Map.containsKey "dest" args then
                Map.find "dest" args
            else
                "Data/output.txt"

        let mode =
            if Map.containsKey "mode" args then
                Map.find "mode" args
            else
                "self"

        let sens =
            if Map.containsKey "sens" args then
                Map.find "sens" args
            else
                "insensitive"

        let privateVars, ast = Parsing.parseInputFile source
        let publicVars = Misc.extractPublicVariables ast privateVars

        let precondition =
            sprintf "{ %s }" (Verification.generatePrecondition publicVars "1" "2")

        let postcondition =
            match sens with
            | "insensitive" -> "{ time1 = time2 }"
            | "sensitive" ->
                "{ ("
                + Verification.generatePrecondition publicVars "1" "2"
                + ") ==> time1 = time2 }"
            | _ -> failwith Parsing.errMess

        let timedAST = instrument ast
        let detAST = Misc.det timedAST
        let selfcomposedAST = Verification.selfcompose detAST
        let modProductAST = modProdProg detAST (AST.Variable "q")

        File.WriteAllText(
            dest,
            match mode with
            | "timed" -> PrettyPrinter.prettify timedAST
            | "self" -> sprintf "%s\n%s%s\n" precondition (PrettyPrinter.prettify selfcomposedAST) postcondition
            | "modular" -> sprintf "%s\n%s%s\n" precondition (PrettyPrinter.prettify modProductAST) postcondition
            | _ -> failwith Parsing.errMess
        )
    with ex ->
        printfn "%s" ex.Message

    0 // Return exit code
