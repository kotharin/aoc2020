namespace Day13

open System.IO
module Part1 = 

    let Solution file =

        let lines =
            File.ReadLines file
            |> Seq.toArray

        let timestamp = (int)lines.[0]
        let buses = 
            lines.[1].Split(",".ToCharArray())
            |> Array.fold (fun state b ->
                if (b = "x") then
                    state
                else
                    state@[((int)b)]
            ) List.empty

        let busData =
            buses
            |> List.map (fun b ->
                let rem = timestamp%b
                if (rem = 0) then
                    0,b
                else
                    let nextMul = int (floor (float timestamp/float b)) + 1
                    ((b*nextMul) - timestamp), b
            )|> List.minBy fst
        (fst busData) * (snd busData)