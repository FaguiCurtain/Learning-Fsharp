 let rawfile = CsvFile.Load("/Users/francois-guillaume.rideau/Documents/crypto/trading/BitFinex trades.csv",";")

 for row in rawfile.Rows do
    // printfn "%A" (System.DateTime.Parse (row.GetColumn("Date")))
    // printfn "%A" (ParsePair_Poloniex (row.GetColumn("Market")))
    // printfn "%A" (float ( buy_or_sell(row.GetColumn("Type")) * float (row.GetColumn("Amount"))) )
    // printfn "%A" (float (row.GetColumn("Price")))
    printfn "%A" (float (row.GetColumn("Fee")))


type Polo = 
   CsvProvider<"/Users/francois-guillaume.rideau/Documents/crypto/trading/Poloniex tradeHistory.csv",";",
                 Schema = "Fee=float">