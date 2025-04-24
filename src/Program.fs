open System.IO
open utils
open Clock
open ModularComposition


[<EntryPoint>]
let main argv =
    try
        let privateVars, ast = parseInputFile "Data/input.txt"
        let publicVars = extractPublicVariables ast privateVars
        let precondition = generatePrecondition (Set.add "time" publicVars) "1" "2"
        let postcondition = "{ time1 = time2 }"
        let timedAST = instrument ast
        let detAST = det timedAST
        // let astRenamed1 = rename detAST "1"
        // let astRenamed2 = rename detAST "2"
        // let product = mergePrograms astRenamed1 astRenamed2
        let product = modProdProg detAST (AST.Variable "q")
        writeOutputFile "Data/output.txt" precondition postcondition (PrettyPrinter.prettify product)
    // File.WriteAllText("Data/output.txt", PrettyPrinter.prettify timedAST)
    //writeOutputFile "Data/output.txt" hoareContract hoareContract (PrettyPrinter.prettify merged)
    with ex ->
        File.WriteAllText("Data/output.txt", ex.Message)

    0 // Return exit code
