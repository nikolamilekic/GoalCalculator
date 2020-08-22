// Learn more about F# at http://fsharp.org

open System
open System.IO
open Argu
open Milekic.YoLo

[<NoComparison; NoEquality>]
type Argument =
    | [<ExactlyOnce>] BudgetId of string
    | [<ExactlyOnce>] AuthenticationToken of string
    | [<NoAppSettings>] Version
    interface IArgParserTemplate with
        member _.Usage = " "

let configFilePath =
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
        ".GoalCalculator",
        "GoalCalculator.config")

let reportFilePath =
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
        ".GoalCalculator",
        "Report.txt")

Directory.CreateDirectory(Path.GetDirectoryName(configFilePath)) |> ignore

[<EntryPoint>]
let main argv =
    printfn
        "GoalCalculator. Version: %s"
        (Metadata.getCallingAssemblyInformationalVersion())

    try
        let arguments =
            ArgumentParser
                .Create(programName = "GoalCalculator")
                .Parse(
                    inputs = argv,
                    configurationReader =
                        if File.Exists configFilePath
                        then ConfigurationReader.FromAppSettingsFile configFilePath
                        else ConfigurationReader.NullReader
                )

        if arguments.TryGetResult Version |> Option.isSome then exit 0

        arguments.GetAllResults()
        |> arguments.Parser.PrintAppSettingsArguments
        |> curry File.WriteAllText configFilePath

        let headers = YnabApi.makeHeaders (arguments.GetResult AuthenticationToken)
        let budgetId = arguments.GetResult BudgetId

        let categories =
            YnabApi.getCategories headers budgetId
            |> GoalCalculator.parseCategories

        let transactions =
            YnabApi.getScheduledTransactions headers budgetId
            |> GoalCalculator.parseTransactions

        let result =
            GoalCalculator.calculateGoals categories transactions |> Seq.toList

        let categoryNameLength =
            result |> Seq.map (fun x -> x.Category.Name.Length) |> Seq.max

        printfn
            "%s Average Goal    Current Adjustment"
            ("".PadRight(categoryNameLength))

        result
        |> flip Seq.map <| fun g ->
            let currentGoal =
                g.Category.Goal
                |> Option.map (sprintf "%7.2f")
                |> Option.defaultValue "   /   "
            let paddedName = g.Category.Name.PadRight(categoryNameLength)
            sprintf
                "%s %7.2f %7.2f %s %7.2f"
                paddedName
                g.AverageAssignment
                g.PeakAssignment
                currentGoal
                g.Adjustment
        |> Seq.iter (printfn "%s")

        0
    with
        | :? ArguParseException as ex ->
            printfn "%s" ex.Message
            int ex.ErrorCode
        | x ->
            printfn "%A" x
            -1
