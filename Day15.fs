namespace Day15

open System.IO

module Part1 = 

    let rec nextValue maxTurns turn previousValue numbersMap  =
        if (turn = maxTurns + 1) then
            previousValue
        else
            // check if the previous number occured before
            let occuredAtTurn =
                Map.tryFind previousValue numbersMap |> Option.defaultValue (turn - 1)
            // update map
            let newNumbersMap = Map.add previousValue (turn - 1) numbersMap
            // if the last time this occurd was at turn - 1, hen this is the first time
            // we are seeing this number
            let nextNumber = (turn - 1) - occuredAtTurn
            nextValue maxTurns (turn + 1) nextNumber newNumbersMap

    let getTurnValue maxTurn numbersMap lastValue =
        nextValue maxTurn (Map.count numbersMap + 2) lastValue numbersMap

    let Solution file =
        let numbers =
            File.ReadLines file
            |> Seq.toArray
            |> Array.head
            
        let startingNumbers =
            numbers.Split [|','|]

        let lastNumber = (int)startingNumbers.[Array.length startingNumbers - 1]
        
        let numbersMap = 
            [|0..(Array.length startingNumbers - 2)|]
            |> Array.fold (fun map i ->
                Map.add ((int)startingNumbers.[i]) (i+1) map
            ) Map.empty
            
        getTurnValue 2020 numbersMap lastNumber
