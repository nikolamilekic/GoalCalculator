// Learn more about F# at http://fsharp.org

open System
open System.Diagnostics
open System.IO
open Argu
open Milekic.YoLo
open FSharpPlus

[<NoComparison; NoEquality>]
type Argument =
    | [<ExactlyOnce>] BudgetId of string
    | AuthenticationToken of string
    | AuthenticationTokenCommand of string
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

let runCommand (command : string) =
    let words = command.Split (" ")
    let command = Array.head words
    let args = words |> Array.skip 1 |> String.concat " "
    let psi =
        ProcessStartInfo(
            command,
            args,
            RedirectStandardOutput=true,
            RedirectStandardError=true)
    let p = Process.Start(psi)
    p.WaitForExit()
    if p.ExitCode = 0
    then p.StandardOutput.ReadToEnd()
    else failwith (p.StandardError.ReadToEnd())

[<EntryPoint>]
let main argv =
    printfn
        "GoalCalculator. Version: %s (%s)"
        (Metadata.getCallingAssemblyInformationalVersion())
        (DateTimeOffset.Parse(ThisAssembly.Git.CommitDate).ToString("yyyy-MM-dd"))

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

        let authenticationToken =
            match arguments.TryGetResult AuthenticationToken with
            | Some x -> x
            | None -> arguments.PostProcessResult(AuthenticationTokenCommand, runCommand)

        let headers = YnabApi.makeHeaders authenticationToken
        let budgetId = arguments.GetResult BudgetId

        let categories =
            YnabApi.getCategories headers budgetId
            |> GoalCalculator.parseCategories

        let transactions =
            YnabApi.getScheduledTransactions headers budgetId
            |> GoalCalculator.parseTransactions categories

        let result = GoalCalculator.calculateGoals transactions |> toList
        let categoryNameLength =
            result |> map (fun x -> x.Category.Name.Length) |> maximum

        printfn
            "%s Average Goal    Current Adjustment"
            ("".PadRight(categoryNameLength))

        result
        |> flip map <| fun g ->
            let currentGoal = g.Category.Goal |> option (sprintf "%7.2f") "   /   "
            let paddedName = g.Category.Name.PadRight(categoryNameLength)
            sprintf
                "%s %8.2f %8.2f %s %8.2f"
                paddedName
                g.AverageAssignment
                g.PeakAssignment
                currentGoal
                g.Adjustment
        |> iter (printfn "%s")

        0
    with
        | :? ArguParseException as ex ->
            printfn "%s" ex.Message
            int ex.ErrorCode
        | x ->
            printfn "%A" x
            -1
