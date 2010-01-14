//Based on http://blogs.msdn.com/lucabol/archive/2008/08/29/downloading-stock-prices-in-f-part-i-data-modeling.aspx
module common

open System
[<Measure>] type money
let money (f:float) = f * 1.<money>

[<Measure>] type shares
let shares (f:float) = f * 1.<shares>

[<Measure>] type volume 
let volume (f:float) = f * 1.<volume>

[<Measure>] type rate
let rate (f:float) = f * 1.<rate>

type Span = { Start: DateTime; End : DateTime;}
type Price = {  Open : float<money>; 
                High : float<money>;
                Low : float<money>;
                Close : float<money>;
                Volume : float<volume>; }

type StockEvent =  
    | Price of Price
    | Split of float
    | Dividend of float<money>

type Observation = { Date : DateTime; Event : StockEvent }

let date y m d = new DateTime(y,m,d)
let span sy sm sd ey em ed = {Start = date sy sm sd; End = date ey em ed}

let now () = DateTime.Now

let idem x = x
let someIdem x = Some(x)