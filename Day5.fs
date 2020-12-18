namespace Day5

open System.IO

module Part1 =

    type SeatRange = {
        Start: int
        End: int
    } with
        static member DefaultRowRange =
            {
                SeatRange.Start = 0
                SeatRange.End = 127
            }
        static member DefaultColRange =
            {
                SeatRange.Start = 0
                SeatRange.End = 7
            }

    let nextRange operation (currentRange:SeatRange) =
        if currentRange.Start + 1 = currentRange.End then
            match operation with
            | 'F' | 'L' ->
                {
                    SeatRange.Start = currentRange.Start
                    SeatRange.End = currentRange.Start
                }
            | _ -> 
                {
                    SeatRange.Start = currentRange.End
                    SeatRange.End = currentRange.End
                }

        else
            // depending on the operation pick the range
            let diff = (currentRange.End - currentRange.Start) + 1
            match operation with
            | 'F' | 'L' -> // lower half
                {
                    SeatRange.Start = currentRange.Start
                    SeatRange.End = currentRange.Start + (diff/2) - 1
                }
            | 'B' | 'R' -> // upper half
                {
                    SeatRange.Start = currentRange.Start + (diff/2)
                    SeatRange.End = currentRange.End
                }
            | _ -> currentRange


        
    let getSeatId (seatData:string) =
        let rowData = seatData.Substring(0,7)
        let colData = seatData.Substring(7)

        // get the encoded row
        let row =
            rowData.ToCharArray()
            |> Array.fold (fun state op ->
                nextRange op state
            ) SeatRange.DefaultRowRange
        
        let col =
            colData.ToCharArray()
            |> Array.fold (fun state op ->
                nextRange op state
            ) SeatRange.DefaultColRange

        //printfn "data:%s, row:%i, col:%i" seatData row.Start col.Start

        (row.Start * 8) + col.Start

    let Solution file =
        File.ReadLines file
        |> Seq.map getSeatId
        |> Seq.max
        