// Learn more about F# at http://fsharp.net

namespace pavelm.lib.stockQuotesLib

open System
open pavelm.lib.stockQuotesLib

type IQuoteProvider = 
    abstract QuoteTicker : string -> StockQuote
    abstract HistoricalQuote : string -> DateTime -> DateTime -> seq<StockQuote>



