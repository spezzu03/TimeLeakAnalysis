open System.IO
open utils
open Clock


[<EntryPoint>]
let main argv =
    try
        let privateVars, ast = parseInputFile "Data/input.txt"
        let publicVars = extractPublicVariables ast privateVars
        let hoareContract = generateHoareContract (Set.add "time" publicVars)
        let detAST = det ast
        let timedAST = instrument detAST
        let astRenamed = rename timedAST
        let merged = mergePrograms timedAST astRenamed
        writeOutputFile "Data/output.txt" hoareContract hoareContract (PrettyPrinter.prettify merged)
    with ex ->
        File.WriteAllText("Data/output.txt", ex.Message)

    0 // Return exit code
