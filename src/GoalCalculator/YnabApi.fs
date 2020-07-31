module YnabApi

open FSharp.Data

[<Literal>]
let ScheduledTransactionsSamplePath = __SOURCE_DIRECTORY__ + "/Samples/ScheduledTransactions.json"
type ScheduledTransactions = JsonProvider<ScheduledTransactionsSamplePath>

[<Literal>]
let CategoryGroupsSamplePath = __SOURCE_DIRECTORY__ + "/Samples/CategoryGroups.json"
type CategoryGroups = JsonProvider<CategoryGroupsSamplePath>

let makeHeaders key = [
    "Authorization", sprintf "Bearer %s" key
    "Accept", "application/json"
]

let getScheduledTransactions headers budgetId =
    let endpoint =
        sprintf
            "https://api.youneedabudget.com/v1/budgets/%O/scheduled_transactions"
            budgetId
    Http.RequestString (endpoint, headers=headers )
    |> ScheduledTransactions.Parse
    |> fun x -> x.Data.ScheduledTransactions

let getCategories headers budgetId =
    let endpoint =
        sprintf
            "https://api.youneedabudget.com/v1/budgets/%O/categories"
            budgetId
    Http.RequestString (endpoint, headers=headers )
    |> CategoryGroups.Parse
    |> fun x -> x.Data.CategoryGroups
