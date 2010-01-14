module html

open System
open System.IO
open System.Text.RegularExpressions
open common

// It assumes no table inside table ...
let tableExpr = "<table[^>]*>(.*?)</table>"
let headerExpr = "<th[^>]*>(.*?)</th>"
let rowExpr = "<tr[^>]*>(.*?)</tr>"
let colExpr = "<td[^>]*>(.*?)</td>"
let regexOptions = RegexOptions.Multiline ||| RegexOptions.Singleline 
                                          ||| RegexOptions.IgnoreCase

let scrapHtmlCells html =
    seq { for x in Regex.Matches(html, colExpr, regexOptions) -> x.Groups.Item(1).ToString()}

let scrapHtmlRows html = 
    seq { for x in Regex.Matches(html, rowExpr, regexOptions) -> scrapHtmlCells x.Value }


//http://ichart.finance.yahoo.com/table.csv?s=MSFT&a=02&b=13&c=2009&d=00&e=14&f=2010&g=d&ignore=.csv
let commonUrl ticker span = 
    let url = @"http://ichart.finance.yahoo.com/table.csv"
    String.Format("{0}?s={1}&a={2}&b={3}&c={4}&d={5}&e={6}&f={7}", url, ticker,
        (span.Start.Month - 1), span.Start.Day, span.Start.Year, (span.End.Month-1), span.End.Day, span.End.Year)

let priceUrl ticker span =  commonUrl ticker span + "&g=d&ignore=.csv"
let dividendUrl ticker span = commonUrl ticker span + "&g=v&ignore=.csv"

let parsePrice (line:string) = 
    let tokens = line.Split([|','|])
    { Date = DateTime.Parse(tokens.[0]);
      Event = Price ({ Open = money (Double.Parse(tokens.[1]));
                       High = money (Double.Parse(tokens.[2]));
                       Low = money (Double.Parse(tokens.[3]));
                       Close = money (Double.Parse(tokens.[4]));
                       Volume = volume (Double.Parse(tokens.[5]))})}

let parseDividend (line: string) =
    let tokens = line.Split([|','|])
    let date = DateTime.Parse(tokens.[0])
    let amount = money (Double.Parse(tokens.[1]))
    {Date = date; Event = Dividend amount}   

