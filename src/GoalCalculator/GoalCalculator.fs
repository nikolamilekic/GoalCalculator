module GoalCalculator

open System
open Microsoft.FSharp.Core
open Milekic.YoLo
open FSharpPlus
open FSharpPlus.Lens

open YnabApi

[<Measure>] type money
[<Measure>] type month

type Category = {
    Name : string
    Balance : decimal<money>
    Goal : decimal<money> option
    SortField : IComparable
}
module Category =
    let inline _sortField f s = s.SortField |> f <&> fun v -> { s with SortField = v }

type CategoryGoal = {
    Category : Category
    AverageAssignment : decimal<money/month>
    PeakAssignment : decimal<money/month>
    MonthsUntilPeak : decimal<month>
    Available : decimal<money>

    /// Adjust Available by this amount to have PeakAssignment match AverageAssignment
    Adjustment : decimal<money>
}

type Transaction = {
    Date : DateTime
    Amount : decimal<money>
    Category : Category
    RepetitionRule : (DateTime -> DateTime) option
}
module Transaction =
    let inline _date f s = s.Date |> f <&> fun v -> { s with Date = v }
    let inline _amount f s = s.Amount |> f <&> fun v -> { s with Amount = v }
    let inline _category f s = s.Category |> f <&> fun v -> { s with Category = v }
    let inline _total f = items << _amount <| f

let getStartOfMonth (x : DateTime) = DateTime(x.Year, x.Month, 1)
let expandTransactions lastRelevantDate =
    bind <| fun transaction ->
        transaction.RepetitionRule
        |>> fun rule ->
            let rec inner current = seq {
                yield current
                yield! inner (current |> Transaction._date %-> rule)
            }
            inner transaction
        |> Option.defaultValue (result transaction)
        |> takeWhile (view Transaction._date >> fun d -> d < lastRelevantDate)
    >> toList
let calculateGoals (transactions : _ seq) =
    let nextMonthStart = DateTime.Now.Date.AddMonths 1 |> getStartOfMonth
    let lastRelevantDate = nextMonthStart.AddYears 1

    expandTransactions lastRelevantDate transactions
    |> toSeq
    |> groupBy (view Transaction._category)
    |> sortBy (view (_1 << Category._sortField))
    |> flip map <| fun ((category : Category), (transactions : Transaction seq)) ->
        let fundsToReserve =
            transactions
            |> filter (view Transaction._date >> fun d -> d < nextMonthStart)
            |> view Transaction._total
        let available = category.Balance - fundsToReserve

        let relevantTransactions =
            transactions
            |> filter (view Transaction._date >> fun d -> d >= nextMonthStart)

        // if category.Name = "Operations" then
        //     relevantTransactions
        //     |> Seq.sortBy (fun t -> t.Date)
        //     |> Seq.iter (fun t->
        //         printfn "%A %7.2f" t.Date t.Amount)

        let averageAssignment =
            (relevantTransactions^.Transaction._total) / 12m<month>
        let allocations =
            relevantTransactions
            |> groupBy (view Transaction._date >> getStartOfMonth)
            |> sortBy fst
            |> fold
                (fun (allocations, previousMonths) (budgetDate, transactions) ->
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
                    (monthsSinceStart, perMonth)::allocations, totalFundsNeeded)
                ([], 0m<money>)
            |> fst

        // if category.Name = "Operations" then
        //     allocations
        //     |> Seq.iter (fun (month, allocation) ->
        //         printfn "%3.0f %7.2f" (month) allocation)

        let (monthsUntilPeak, peakAssignment) = allocations |> maxBy snd

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
    >>= (fun (g : CategoryGroups.CategoryGroup) -> g.Categories)
    |> flip mapi <| fun sortIndex c ->
        c.Id,
        {
            Name = c.Name
            Balance = parseAmount c.Balance
            Goal = c.GoalType
                   |> Option.map (fun _ -> c.GoalTarget |> parseAmount)
            SortField = sortIndex
        }
    |> Map.ofSeq

let parseRepetitionRule = function
    | "never" -> None
    | "daily" -> Some (fun (x : DateTime) -> x.AddDays 1.0)
    | "weekly" -> Some (fun (x : DateTime) -> x.AddDays 7.0)
    | "everyOtherWeek" -> Some (fun (x : DateTime) -> x.AddDays 14.0)
    | "twiceAMonth" -> Some (fun (x : DateTime) -> x.AddDays 15.0)
    | "every4Weeks" -> Some (fun (x : DateTime) -> x.AddDays 28.0)
    | "monthly" -> Some (fun (x : DateTime) -> x.AddMonths 1)
    | "everyOtherMonth" -> Some (fun (x : DateTime) -> x.AddMonths 2)
    | "every3Months" -> Some (fun (x : DateTime) -> x.AddMonths 3)
    | "every4Months" -> Some (fun (x : DateTime) -> x.AddMonths 4)
    | "twiceAYear" -> Some (fun (x : DateTime) -> x.AddMonths 6)
    | "yearly" -> Some (fun (x : DateTime) -> x.AddYears 1)
    | "everyOtherYear" -> Some (fun (x : DateTime) -> x.AddYears 2)
    | x -> failwithf "Unsupported repetition rule %s" x

let parseTransactions categories =
    bind <| fun (t : ScheduledTransactions.ScheduledTransaction) ->
        let makeTransaction categoryId = {
            Date = t.DateNext
            Amount = parseAmount t.Amount * -1m
            Category = Map.find categoryId categories
            RepetitionRule = parseRepetitionRule t.Frequency
        }

        if t.CategoryName <> "Split (Multiple Categories)..." then
            result (makeTransaction t.CategoryId)
        else
            t.Subtransactions
            |> filter (fun st -> st.Amount < 0)
            |> flip map <| fun st ->
                makeTransaction st.CategoryId
                |> Transaction._amount .-> (parseAmount st.Amount * -1m)
