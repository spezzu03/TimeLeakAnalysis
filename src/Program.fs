open System.IO
open utils
open Clock
open ModularComposition

[<EntryPoint>]
let main argv =
    try
        let args = parseArgs (Array.toList argv)

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

        let privateVars, ast = parseInputFile source
        let publicVars = extractPublicVariables ast privateVars
        let precondition = sprintf "{ %s }" (generatePrecondition publicVars "1" "2")

        let postcondition =
            match sens with
            | "insensitive" -> "{ time1 = time2 }"
            | "sensitive" -> "{ (" + generatePrecondition publicVars "1" "2" + ") ==> time1 = time2 }"
            | _ -> failwith errMess

        let timedAST = instrument ast
        let detAST = det timedAST
        let selfcomposedAST = selfcompose detAST
        let modProductAST = modProdProg detAST (AST.Variable "q")

        match mode with
        | "timed" -> File.WriteAllText("Data/output.txt", PrettyPrinter.prettify timedAST)
        | "modular" ->
            writeOutputFile "Data/output.txt" precondition postcondition (PrettyPrinter.prettify modProductAST)
        | "self" ->
            writeOutputFile "Data/output.txt" precondition postcondition (PrettyPrinter.prettify selfcomposedAST)
        | _ -> failwith errMess
    with ex ->
        printfn "%s" ex.Message
        File.WriteAllText("Data/output.txt", ex.Message)

    0 // Return exit code
