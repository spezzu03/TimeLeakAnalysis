open System.IO
open utils
open Clock
open ModularComposition

let rec parseArgs =
    function
    | [] -> Map.empty
    | "--mode" :: m :: rest ->
        let restMap = parseArgs rest

        if List.contains m [ "timed"; "modular"; "self" ] then
            if not (Map.containsKey "mode" restMap) then
                Map.add "mode" m restMap
            else
                failwith "you can specify mode only once"
        else
            failwith "mode does not exist"
    | "--dest" :: f :: rest ->
        let restMap = parseArgs rest

        if not (Map.containsKey "dest" restMap) then
            Map.add "dest" f restMap
        else
            failwith "you can specify destination file only once"
    | "--src" :: f :: rest ->
        let restMap = parseArgs rest

        if not (Map.containsKey "src" restMap) then
            Map.add "src" f restMap
        else
            failwith "you can specify source file only once"
    | "--sens" :: s :: rest ->
        let restMap = parseArgs rest

        if List.contains s [ "sensitive"; "insensitive" ] then
            if not (Map.containsKey "sens" restMap) then
                Map.add "sens" s restMap
            else
                failwith "you can specify sensitivity only once"
        else
            failwith "sensitivity does not exist"
    | _ ->
        failwith
            "Wrong use. Flags are: 
            \n--mode: what shall be output. Can be one of {timed, modular, self}
            \n--src: source file. Standard is \"input.txt\"
            \n--dest: destination file. Standard is \"output.txt\"
            \n--sens: whether to use output-sensitive or -insensitive definition of ct. Can be one of {sensitive, insensitive}"


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
            | _ -> failwith "Sensitivity does not exist"

        let timedAST = instrument ast
        let detAST = det timedAST
        let astRenamed1 = rename detAST "1"
        let astRenamed2 = rename detAST "2"
        let merged = mergePrograms astRenamed1 astRenamed2
        let product = modProdProg detAST (AST.Variable "q")

        match mode with
        | "timed" -> File.WriteAllText("Data/output.txt", PrettyPrinter.prettify timedAST)
        | "modular" -> writeOutputFile "Data/output.txt" precondition postcondition (PrettyPrinter.prettify product)
        | "self" -> writeOutputFile "Data/output.txt" precondition postcondition (PrettyPrinter.prettify merged)

    //writeOutputFile "Data/output.txt" precondition postcondition (PrettyPrinter.prettify merged)
    with ex ->
        File.WriteAllText("Data/output.txt", ex.Message)

    0 // Return exit code
