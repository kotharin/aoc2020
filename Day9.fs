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

module Part2 =

    (*  Fairly brute force. 
        Start a segment from the first element and keep adding
        a number if under the desired total. If over, remove the
        element from the head of the list and try again.
    *)
    let findContiguousNumbersSegment (numbers:int64 array) total =

        let rec findContiguousNumbers total (numbers:int64 array) startIndex endIndex contiguousNumbers tempTotal =
            if (tempTotal = total) then
                contiguousNumbers
            else
                if (tempTotal < total) then
                    // add the next number
                    let newEndIndex = endIndex + 1
                    let nextNum = numbers.[newEndIndex]
                    let newTempTotal = tempTotal + nextNum
                    let newContiguousNumbers = List.append contiguousNumbers [nextNum]
                    findContiguousNumbers total numbers startIndex newEndIndex newContiguousNumbers newTempTotal
                else
                    // remove the first number and reevaluate
                    let newStartIndex = startIndex + 1
                    let head::newContiguousNumbers = contiguousNumbers
                    let newTempTotal = tempTotal - head
                    findContiguousNumbers total numbers newStartIndex endIndex newContiguousNumbers newTempTotal

        let contiguousNumbers = [numbers.[0];numbers.[1]]

        let tempTotal = numbers.[0] + numbers.[1]

        findContiguousNumbers total numbers 0 1 contiguousNumbers tempTotal


    let Solution file =
        let numbers =
            File.ReadLines file
            |> Seq.map int64
            |> Seq.toArray

        let segment = findContiguousNumbersSegment numbers (int64 32321523) |> List.sort

        let min, max = List.head segment, List.last segment

        min + max