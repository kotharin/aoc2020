namespace Day9

open System.IO

module Part1 =


    
    let findEncodingViolation (numbers:int64 array) batchSize =
        let mapNumbers =
            [|0..batchSize-1|]
            |> Array.fold (fun state i -> 
                // Add the appropriate number to the map
                Map.add numbers.[i] 1 state
            ) Map.empty

        let rec findComplement (numbers: int64 array) mapNums num currentIndex endIndex complement =
            if (currentIndex <= endIndex) then
                // check if the complement exists
                let testNum = numbers.[currentIndex]
                let comp = num - testNum
                if ((comp <> testNum) && (Map.containsKey comp mapNums)) then
                    Some comp
                else
                    findComplement numbers mapNums num (currentIndex+1) endIndex None
            else
                complement
        
        let rec findViolation2 (numbers: int64 array) currentIndex mapNumbers =
            // get the number to check
            let number = numbers.[currentIndex]

            
            let complement =
                findComplement numbers mapNumbers number (currentIndex - batchSize) (currentIndex - 1) None

            match complement with
            | None ->
                // No complement found
                [number]
            | Some _ ->
                // found the complement
                // chekck the next batch
                // remove the number at the begining of the batch
                // add new number to bottom of the batch
                let removeNumber = numbers.[currentIndex - batchSize]
                let newMap =
                    Map.remove removeNumber mapNumbers
                    |> Map.add number 1
                findViolation2 numbers (currentIndex + 1) newMap

        findViolation2 numbers batchSize mapNumbers


    let Solution file =
        let numbers =
            File.ReadLines file
            |> Seq.map int64
            |> Seq.toArray

        findEncodingViolation numbers 25