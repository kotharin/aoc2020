namespace Day9

open System.IO

module Part1 =


    
    let findEncodingViolation (numbers:int array) batchSize =
        let mapNumbers =
            [|0..batchSize-1|]
            |> Array.fold (fun state i -> 
                // Add the appropriate number to the map
                Map.add numbers.[i] 1 state
            ) Map.empty

        let rec findViolation (numbers: int array) currentIndex mapNumbers =
            // get the number to check
            let number = numbers.[currentIndex]

            let complements =
                mapNumbers
                |> Map.fold (fun state num _ ->
                    // check if there is a pair
                    // of numbers that adds to the 
                    // given number
                    let complement = number - num
                    // check if complement exists
                    if ((List.isEmpty state) && (Map.containsKey complement mapNumbers)) then
                        number::state
                    else
                        state
                ) List.empty

            match complements with
            | [] ->
                [number]
            | head::tail ->
                // chekck the next batch
                // remove the number at the begining of the batch
                // add new number to bottom of the batch
                let removeNumber = numbers.[currentIndex - batchSize]
                let newMap =
                    Map.remove removeNumber mapNumbers
                    |> Map.add number 1
                findViolation numbers (currentIndex + 1) newMap

        findViolation numbers batchSize mapNumbers

    let Solution file =
        let numbers =
            File.ReadLines file
            |> Seq.map int
            |> Seq.toArray

        findEncodingViolation numbers 5
