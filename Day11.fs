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