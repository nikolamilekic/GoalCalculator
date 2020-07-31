module GoalCalculator

open System
open Microsoft.FSharp.Core
open Milekic.YoLo
open YnabApi

[<Measure>] type money
[<Measure>] type month

type Category = { Id : Guid; Name : string; Balance : decimal<money> }

type CategoryGoal = {
    Category : Category
    AverageAssignment : decimal<money/month>
    PeakAssignment : decimal<money/month>
    MonthsUntilPeak : decimal<month>
    Available : decimal<money>

    /// Adjust Available by this amount to have PeakAssignment match AverageAssignment
    Adjustment : decimal<money>
}

type RepetitionRule =
    | Never
    | Daily
    | Weekly
    | EveryOtherWeek
    | TwiceAMonth
    | Every4Weeks
    | Monthly
    | EveryOtherMonth
    | Every3Months
    | Every4Months
    | TwiceAYear
    | Yearly
    | EveryOtherYear

type Transaction = {
    Date : DateTime
    Amount : decimal<money>
    Category : Guid
    RepetitionRule : RepetitionRule
}

let getStartOfMonth (x : DateTime) = DateTime(x.Year, x.Month, 1)
let expandTransaction transaction =
    let increment =
        match transaction.RepetitionRule with
        | Never -> None
        | Daily -> Some (fun (x : DateTime) -> x.AddDays 1.0)
        | Weekly -> Some (fun (x : DateTime) -> x.AddDays 7.0)
        | EveryOtherWeek -> Some (fun (x : DateTime) -> x.AddDays 14.0)
        | TwiceAMonth -> Some (fun (x : DateTime) -> x.AddDays 15.0)
        | Every4Weeks -> Some (fun (x : DateTime) -> x.AddDays 28.0)
        | Monthly -> Some (fun (x : DateTime) -> x.AddMonths 1)
        | EveryOtherMonth -> Some (fun (x : DateTime) -> x.AddMonths 2)
        | Every3Months -> Some (fun (x : DateTime) -> x.AddMonths 3)
        | Every4Months -> Some (fun (x : DateTime) -> x.AddMonths 4)
        | TwiceAYear -> Some (fun (x : DateTime) -> x.AddMonths 6)
        | Yearly -> Some (fun (x : DateTime) -> x.AddYears 1)
        | EveryOtherYear -> Some (fun (x : DateTime) -> x.AddYears 2)

    match increment with
    | None -> seq { transaction }
    | Some increment ->
        let rec inner current = seq {
            yield current
            yield! { current with Date = increment current.Date } |> inner
        }
        inner transaction

let expandTransactions lastRelevantDate =
    Seq.collect <| fun transaction ->
        expandTransaction transaction
        |> Seq.takeWhile (fun x -> x.Date < lastRelevantDate)

let calculateGoals categories transactions =
    let nextMonthStart = DateTime.Now.Date.AddMonths 1 |> getStartOfMonth
    let lastRelevantDate = nextMonthStart.AddYears 1

    expandTransactions lastRelevantDate transactions
    |> Seq.groupBy (fun x -> x.Category)
    |> flip Seq.map <|fun (cId, ts) ->
        categories |> Map.tryFind cId |> Option.map (fun c -> c, ts)
    |> Seq.onlySome
    |> flip Seq.map <| fun (category, transactions) ->
        let fundsToReserve =
            transactions
            |> Seq.where (fun x -> x.Date < nextMonthStart)
            |> Seq.sumBy (fun x -> x.Amount)
        let available = category.Balance - fundsToReserve

        let relevantTransactions =
            transactions
            |> Seq.where (fun x -> x.Date >= nextMonthStart)

        // if category.Name = "Operations" then
        //     relevantTransactions
        //     |> Seq.sortBy (fun t -> t.Date)
        //     |> Seq.iter (fun t->
        //         printfn "%A %7.2f" t.Date t.Amount)

        let averageAssignment =
            (relevantTransactions |> Seq.sumBy (fun x -> x.Amount))
            / 12m<month>
        let allocations =
            relevantTransactions
            |> Seq.groupBy (fun x -> x.Date |> getStartOfMonth)
            |> Seq.sortBy fst
            |> Seq.mapFold
                (fun (previousMonths) (budgetDate, transactions) ->
                    let monthsSinceStart : decimal<month> =
                        (budgetDate - nextMonthStart).TotalDays / 30.0
                        |> fun x -> Math.Round (decimal x) + 1m
                        |> LanguagePrimitives.DecimalWithMeasure
                    let totalFundsNeeded =
                        (transactions |> Seq.sumBy (fun x -> x.Amount))
                        + previousMonths
                    let perMonth =
                        (totalFundsNeeded - available)
                        / monthsSinceStart
                    (monthsSinceStart, perMonth), totalFundsNeeded)
                0m<money>
            |> fst

        // if category.Name = "Operations" then
        //     allocations
        //     |> Seq.iter (fun (month, allocation) ->
        //         printfn "%3.0f %7.2f" (month) allocation)

        let (monthsUntilPeak, peakAssignment) = allocations |> Seq.maxBy snd

        {
            Category = category
            AverageAssignment = averageAssignment
            PeakAssignment = peakAssignment
            MonthsUntilPeak = monthsUntilPeak
            Available = available
            Adjustment =
                (peakAssignment - averageAssignment) * monthsUntilPeak
        }

let parseAmount : _ -> decimal<money> =
    decimal >> (fun x -> x / 1000m) >> LanguagePrimitives.DecimalWithMeasure

let parseCategories categoryGroups =
    categoryGroups
    |> Seq.collect (fun (g : CategoryGroups.CategoryGroup) -> g.Categories)
    |> flip Seq.map <| fun c ->
        {
            Id = c.Id
            Name = c.Name
            Balance = parseAmount c.Balance
        }
    |> Seq.map (fun c -> c.Id, c)
    |> Map.ofSeq

let parseRepetitionRule = function
    | "never" -> Never
    | "daily" -> Daily
    | "weekly" -> Weekly
    | "everyOtherWeek" -> EveryOtherWeek
    | "twiceAMonth" -> TwiceAMonth
    | "every4Weeks" -> Every4Weeks
    | "monthly" -> Monthly
    | "everyOtherMonth" -> EveryOtherMonth
    | "every3Months" -> Every3Months
    | "every4Months" -> Every4Months
    | "twiceAYear" -> TwiceAYear
    | "yearly" -> Yearly
    | "everyOtherYear" -> EveryOtherYear
    | x -> failwithf "Unsupported repetition rule %s" x

let parseTransactions transactions =
    transactions
    |> flip Seq.collect <| fun (t : ScheduledTransactions.ScheduledTransaction) ->
        let baseTransaction = {
            Date = t.DateNext
            Amount = parseAmount t.Amount * -1m
            Category = t.CategoryId
            RepetitionRule = parseRepetitionRule t.Frequency
        }

        if t.CategoryName <> "Split (Multiple Categories)..." then
            seq { baseTransaction }
        else
            t.Subtransactions
            |> Seq.where (fun st -> st.Amount < 0)
            |> flip Seq.map <| fun st -> {
                baseTransaction with
                    Amount = parseAmount st.Amount * -1m
                    Category = st.CategoryId
            }
