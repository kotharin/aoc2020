namespace Day11

open System.IO

module Part1 =

    type PositionState =
        Floor | EmptySeat | OccupiedSeat
        with 
            static member parse c =
                match c with
                | '.' -> Floor
                | 'L' -> EmptySeat
                | _ -> OccupiedSeat

    let nextState seatMap (row,col) =
        // get the adjacent seat data
        let adjacentSeats =
            [
                Map.tryFind (row, col - 1) seatMap;
                Map.tryFind (row ,col + 1) seatMap;
                Map.tryFind (row - 1,col) seatMap;
                Map.tryFind (row + 1,col) seatMap;
                Map.tryFind (row - 1,col - 1) seatMap;
                Map.tryFind (row - 1,col + 1) seatMap;
                Map.tryFind (row + 1,col - 1) seatMap;
                Map.tryFind (row + 1,col + 1) seatMap
            ]|> List.choose id

        // check how many occupied seats
        let adjacentSeatCounts =
            List.countBy id adjacentSeats |> Map.ofList

        // Get current seat state
        let currentState = Map.find (row,col) seatMap

        let occupiedSeats = Map.tryFind OccupiedSeat adjacentSeatCounts |> Option.defaultValue 0
        let emptySeats = Map.tryFind EmptySeat adjacentSeatCounts |> Option.defaultValue 0
        
        if ( (currentState = EmptySeat) && (occupiedSeats = 0)) then   
            // no occupied seats, set seat state to Occupied
            (row,col), OccupiedSeat
        else if ((currentState = OccupiedSeat) && (occupiedSeats >= 4 )) then
            (row,col), EmptySeat
        else
            // return existing state
            (row,col), Map.find (row,col) seatMap

    let nextStateForAllSeats seatMap =
        seatMap
        |> Map.fold (fun state (row,col) _ ->
            let _,nextSeatState = nextState seatMap (row,col)
            Map.add (row,col) nextSeatState state
        ) Map.empty


    let print seatMap =
        [0..9]
        |> List.iter (fun i ->
            [0..9]
            |> List.iter (fun j -> 
                match Map.find (i, j) seatMap with
                | Floor -> 
                    printf "."
                | EmptySeat ->
                    printf "L"
                | OccupiedSeat ->
                    printf "#"
            )
            printfn ""
        )

    let rec getSteadyStateSeatMap seatMap nextSeatMap =
        if (seatMap = nextSeatMap) then
            seatMap
        else
            let newMap = nextStateForAllSeats seatMap
            //printfn "-----------"
            //print newMap
            getSteadyStateSeatMap newMap seatMap

    let Solution file =
        let _, seatMap =
            File.ReadLines file
            |> Seq.fold (fun (row,map) line ->
                let _, newMap =
                    line.ToCharArray()
                    |> Seq.fold (fun (col, state) c ->
                        let newColMap = Map.add (row,col) (PositionState.parse c) state
                        (col + 1) , newColMap
                    )(0,map)
                (row + 1), newMap
            ) (0,Map.empty)
        
        //print seatMap
        // iterate next state till they stop changing

        let steadyState = getSteadyStateSeatMap seatMap Map.empty

        //printfn "---------"
        //print steadyState

        steadyState
        |> Map.toList
        |> List.countBy snd

module Part2 =
    open Part1

    type Direction = Left|Right|Top|Bottom|TopRight|TopLeft|BottomRight|BottomLeft


    let getNextLocation currentRow currentCol direction =
        match direction with
            | TopRight ->
                // increment
                currentRow - 1,currentCol + 1
            | BottomRight ->
                // increment
                currentRow + 1,currentCol + 1
            | TopLeft ->
                // increment
                currentRow - 1,currentCol - 1
            | BottomLeft ->
                // increment
                currentRow + 1,currentCol - 1
            | _ -> currentRow, currentCol

    let getAdjacentSeatLocations maxRows maxCols row col =

        let rec getSeatLocations currentRow currentCol seatList direction =
            match direction with
            | Top ->
                [currentRow - 1..-1..0]
                |> List.map (fun r-> 
                    (r, currentCol)
                )
            | Bottom ->
                [currentRow + 1..maxRows - 1]
                |> List.map (fun r ->
                    (r, currentCol)
                )
            | Left ->
                [currentCol - 1..-1..0]
                |> List.map (fun c ->
                    (currentRow, c)
                )
            | Right ->
                [currentCol + 1..maxCols - 1]
                |> List.map (fun c ->
                    (currentRow, c)
                )
            | TopRight ->
                let newRow, newCol = getNextLocation currentRow currentCol direction
                // add to the list if its within the limits
                if ((newRow < maxRows) && (newCol < maxCols) && ((newRow > 0)) && (newCol > 0)) then
                    getSeatLocations newRow newCol (seatList@[(newRow, newCol)]) direction
                else    
                    seatList
            | BottomRight ->
                let newRow, newCol = getNextLocation currentRow currentCol direction
                // add to the list if its within the limits
                if ((newRow < maxRows) && (newCol < maxCols) && ((newRow >= 0)) && (newCol >= 0)) then
                    getSeatLocations newRow newCol (seatList@[(newRow, newCol)]) direction
                else    
                    seatList
            | TopLeft ->
                let newRow, newCol = getNextLocation currentRow currentCol direction
                // add to the list if its within the limits
                if ((newRow < maxRows) && (newCol < maxCols) && ((newRow >= 0)) && (newCol >= 0)) then
                    getSeatLocations newRow newCol (seatList@[(newRow, newCol)]) direction
                else    
                    seatList
            | BottomLeft ->
                let newRow, newCol = getNextLocation currentRow currentCol direction
                // add to the list if its within the limits
                if ((newRow < maxRows) && (newCol < maxCols) && ((newRow >= 0)) && (newCol >= 0)) then
                    getSeatLocations newRow newCol (seatList@[(newRow, newCol)]) direction
                else    
                    seatList

        [
            Left;
            Right;
            Top;
            Bottom;
            TopRight;
            TopLeft;
            BottomRight;
            BottomLeft
        ]
        |> List.map (fun d ->
                let sl = getSeatLocations row col List.empty d
                sl
        )



    let nextState maxRows maxCols seatMap (row,col) =
        // get the adjacent seat data
        let asl = getAdjacentSeatLocations maxRows maxCols row col

        let adjacentSeats = 
            (List.collect (fun sl ->
                let a1 = 
                    sl
                    |> List.map (fun s ->
                        Map.find s seatMap
                    )
                let a2 = 
                    a1 |> List.filter (fun s ->
                        ((s = EmptySeat) || (s = OccupiedSeat))
                    )
                    |> List.tryHead
                    |> Option.defaultValue Floor
                [a2]
                ) asl)
            

        // check how many occupied seats
        let adjacentSeatCounts =
            List.countBy id adjacentSeats |> Map.ofList

        // Get current seat state
        let currentState = Map.find (row,col) seatMap

        let occupiedSeats = Map.tryFind OccupiedSeat adjacentSeatCounts |> Option.defaultValue 0
        
        if ( (currentState = EmptySeat) && (occupiedSeats = 0)) then   
            // no occupied seats, set seat state to Occupied
            (row,col), OccupiedSeat
        else if ((currentState = OccupiedSeat) && (occupiedSeats >= 5 )) then
            (row,col), EmptySeat
        else
            // return existing state
            (row,col), Map.find (row,col) seatMap

    let nextStateForAllSeats maxRows maxCols seatMap =
        seatMap
        |> Map.fold (fun state (row,col) _ ->
            let _,nextSeatState = nextState maxRows maxCols seatMap (row,col)
            Map.add (row,col) nextSeatState state
        ) Map.empty

    let rec getSteadyStateSeatMap maxRows maxCols seatMap nextSeatMap =
        if (seatMap = nextSeatMap) then
            seatMap
        else
            let newMap = nextStateForAllSeats maxRows maxCols seatMap
            printfn "-----------"
            print newMap
            getSteadyStateSeatMap maxRows maxCols newMap seatMap

    let Solution file =

        let lines = 
            File.ReadLines file
            |> Seq.toList
        let maxRows, seatMap =
            lines
            |> List.fold (fun (row,map) line ->
                let _, newMap =
                    line.ToCharArray()
                    |> Seq.fold (fun (col, state) c ->
                        let newColMap = Map.add (row,col) (PositionState.parse c) state
                        (col + 1) , newColMap
                    )(0,map)
                (row + 1), newMap
            ) (0,Map.empty)

        let maxCols =
            (List.head lines).ToCharArray() |> Array.length

        getSteadyStateSeatMap maxRows maxCols seatMap Map.empty
        |> Map.toList
        |> List.countBy snd
