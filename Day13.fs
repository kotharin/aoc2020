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

module Part2 =

    let validate (busTimes:List<(int64*int64)>) totalBusTimes (num:int64) =
        let x = 
            busTimes
            |>  List.takeWhile (fun bt ->
                let a = num + snd bt 
                let b = fst bt
                (a)%(b) = int64(0))
        
        List.length x = totalBusTimes

    let Solution file =

        let lines =
            File.ReadLines file
            |> Seq.toArray
        
        let busData = 
            lines.[1].Split(",".ToCharArray())
            |> Array.fold (fun (i,state) b ->
                if (b = "x") then
                    (i+1,state)
                else
                    let bId = (int64)b
                    (i+1),state@[(bId,int64(i+1))]
            ) (0,List.empty)
            |> snd
        let maxBusId,maxBusIdPos =
            List.maxBy (fst) busData

        let busTimes =
            busData
            |> List.map (fun (busId, pos) ->
                busId, (pos - maxBusIdPos)
            )
        
        printfn "%A" busTimes

        let validateNum = validate busTimes (List.length busTimes)

        let seed = int64("100000000000000")
        let b = Seq.unfold (fun state -> 
            Some(state, state + maxBusId)) seed
        
        let c =
            b|> 
            Seq.where (fun x ->
                let v = validateNum x
                v
            )

        printfn "%A" ((Seq.head c) - maxBusIdPos + (int64 1))

        0