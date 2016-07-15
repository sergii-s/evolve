module Rand

open System

let randomGen = Random(DateTime.Now.Ticks|>int)
let rm max = randomGen.Next(max)
let rmof a b = if randomGen.Next(2) = 0 then a else b
let rmd () = randomGen.NextDouble()
let gaus () =
    let u1 = randomGen.NextDouble()
    let u2 = randomGen.NextDouble()
    Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2)
let gausOffset offset range = offset + range*gaus()