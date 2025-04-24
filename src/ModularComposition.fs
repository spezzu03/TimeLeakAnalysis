module ModularComposition

open AST
open utils

let rec disjunction =
    function
    | Arrow(b, _) -> b
    | Guard(gc1, gc2) -> LogAnd(disjunction gc1, disjunction gc2)

let mutable counter = 0

let rec extractGuards gc =
    match gc with
    | Guard(gc1, gc2) -> extractGuards gc1 @ extractGuards gc2
    | x -> [ x ]

let getCounter () =
    counter <- counter + 1
    counter - 1

open AST

let mutable freshCounter = 0

let freshVar prefix =
    freshCounter <- freshCounter + 1
    Variable(prefix + string freshCounter)

let rec cross (ast: command) (p: expr) : command =
    match ast with
    | Skip -> Skip

    | Assignment(_, _) ->
        Sequence(
            If(
                Guard(
                    Arrow(Equal(renameExpr p "1", Num 1), rename ast "1"),
                    Arrow(NotEqual(renameExpr p "1", Num 1), Skip)
                )
            ),
            If(
                Guard(
                    Arrow(Equal(renameExpr p "2", Num 1), rename ast "2"),
                    Arrow(NotEqual(renameExpr p "2", Num 1), Skip)
                )
            )
        )

    | Sequence(c1, c2) -> Sequence(cross c1 p, cross c2 p)

    | If g ->
        // Decompose guarded command into flat list
        let rec flatten g =
            match g with
            | Arrow(b, c) -> [ (b, c) ]
            | Guard(g1, g2) -> flatten g1 @ flatten g2

        let branches = flatten g

        // Generate fresh fi_1, fi_2 variables for each branch
        let flags = branches |> List.map (fun _ -> freshVar "f")

        // Initialize all flags to 0
        let initFlags =
            flags
            |> List.collect (fun f -> [ Assignment(renameExpr f "1", Num 0); Assignment(renameExpr f "2", Num 0) ])
            |> function
                | s when List.length s < 2 -> failwith "How??"
                | s -> List.reduce (fun f1 f2 -> Sequence(f1, f2)) s

        // Build "if ... fi" conditionals for setting flags
        let condBlock =
            branches
            |> List.map (fun (b, _) -> b)
            |> List.zip flags
            |> List.map (fun (f, b) -> Arrow(b, Assignment(f, p)))
            |> function
                | [] -> failwith "How did we get here?"
                | [ ar ] -> If(ar)
                | ars -> List.reduce (fun (ar1: guarded) (ar2: guarded) -> Guard(ar1, ar2)) ars |> If

        let condBlock1 = rename condBlock "1"
        let condBlock2 = rename condBlock "2"

        let crossedBranches =
            branches
            |> List.map (fun (_, c) -> c)
            |> List.zip flags
            |> List.map (fun (f, c) -> cross c f)
            |> function
                | [] -> failwith "There is no way bruh"
                | [ c ] -> c
                | cs -> List.reduce (fun cr1 cr2 -> Sequence(cr1, cr2)) cs

        Sequence(initFlags, Sequence(condBlock1, Sequence(condBlock2, crossedBranches)))

    | Do(gc, inv) ->
        let disj = disjunction gc
        let execCondition = LogAnd(Equal(p, Num 1), disj)

        Do(
            Arrow(
                LogOr(renameBool execCondition "1", renameBool execCondition "2"),
                cross (If(Guard(gc, Arrow(Not(disj), Skip)))) p
            ),
            inv
        )

let modProdProg ast actVar =
    Sequence(
        Sequence(Assignment(renameExpr actVar "1", Num 1), Assignment(renameExpr actVar "2", Num 1)),
        cross ast actVar
    )
