namespace Day12

open System.IO

module Part1 =


    type Direction = North = 0 | East = 1 | South = 2 | West = 3

    type Instruction =
        | North of int
        | East of int
        | South of int
        | West of int
        | Left of int
        | Right of int
        | Forward of int
    with 
        static member parse (ins:string) =
            let magnitude =
                int(ins.Substring(1))
            match ins.Substring(0,1) with
            | "N" -> North(magnitude)
            | "E" -> East(magnitude)
            | "S" -> South(magnitude)
            | "W" -> West(magnitude)
            | "L" -> Left(magnitude)
            | "R" -> Right(magnitude)
            | _ -> Forward(magnitude)

    type Location = {
        X:int
        Y:int
        Direction: Direction
    }

    let nextDirection currentDirection (instruction:Instruction) =
        // normalize the turn degrees
        let td =
            match instruction with
            | Left i ->
                360 - i
            | Right i ->
                i
            | _ 
                -> 0
        let turnIncrement = (td % 360) / 90

        (currentDirection + turnIncrement) % 4

    let move (currentLocation:Location) (instruction:Instruction) =
        let x = currentLocation.X
        let y = currentLocation.Y
        let direction = currentLocation.Direction

        match instruction with
        | North i  ->
            {currentLocation with Y = y + i}
        | East i ->
            {currentLocation with X = x + i}
        | South i ->
            {currentLocation with Y = y - i}
        | West i ->
            {currentLocation with X = x - i}
        | Left i | Right i ->
            let nextDirection = (nextDirection (int direction) instruction )
            {
                currentLocation with Direction = enum nextDirection
            }
        | Forward i when direction = Direction.North ->
            {currentLocation with Y = y + i}
        | Forward i when direction = Direction.East ->
            {currentLocation with X = x + i}
        | Forward i when direction = Direction.South ->
            {currentLocation with Y = y - i}
        | Forward i when direction = Direction.West ->
            {currentLocation with X = x - i}
        | _ ->
            printfn "should never get here" 
            currentLocation
        

    let Solution file =

        let instructions =
            File.ReadLines file
            |> Seq.map Instruction.parse
            |> Seq.toList

        instructions
        |> List.fold (move
        ) {Location.Direction = Direction.East
           X = 0
           Y = 0}
        
module Part2 =

    type Location = {
        X: int
        Y: int
    }

    type BoatTracker = {
        BoatLocation: Location
        WaypointLocation: Location
    }

    let rotateWaypoint (currentWaypoint:Location) (instruction:Part1.Instruction) =
        match instruction with
        | Part1.Instruction.Right i when (i%360) = 0 ->
            currentWaypoint
        | Part1.Instruction.Right i when (i%270) = 0 ->
            {X= (-1)*currentWaypoint.Y; Y = currentWaypoint.X }
        | Part1.Instruction.Right i when (i%180) = 0 ->
            {X= (-1)*currentWaypoint.X; Y = (-1)*currentWaypoint.Y }
        | Part1.Instruction.Right i when (i%90) = 0 ->
            {X= currentWaypoint.Y; Y = (-1)*currentWaypoint.X }
        | Part1.Instruction.Left i when (i%360) = 0 ->
            currentWaypoint
        | Part1.Instruction.Left i when (i%270) = 0 ->
            {X= currentWaypoint.Y; Y = (-1)*currentWaypoint.X }
        | Part1.Instruction.Left i when (i%180) = 0 ->
            {X= (-1)*currentWaypoint.X; Y = (-1)*currentWaypoint.Y }
        | Part1.Instruction.Left i when (i%90) = 0 ->
            {X= (-1)*currentWaypoint.Y; Y = currentWaypoint.X }
        | _ ->
            printfn "should never reach here"
            currentWaypoint

    let move (current:BoatTracker) (instruction:Part1.Instruction) =
        let boatX = current.BoatLocation.X
        let boatY = current.BoatLocation.Y
        let wpX = current.WaypointLocation.X
        let wpY = current.WaypointLocation.Y

        match instruction with
        | Part1.Instruction.North i  ->
            {current with WaypointLocation = {X = wpX; Y = wpY + i}}
        | Part1.Instruction.East i ->
            {current with WaypointLocation = {X = wpX + i; Y = wpY}}
        | Part1.Instruction.South i ->
            {current with WaypointLocation = {X = wpX; Y = wpY - i}}
        | Part1.Instruction.West i ->
            {current with WaypointLocation = {X = wpX - i; Y = wpY}}
        | Part1.Instruction.Left i | Part1.Instruction.Right i ->
            let nextWP = (rotateWaypoint current.WaypointLocation instruction )
            {
                current with WaypointLocation = nextWP
            }
        | Part1.Instruction.Forward i ->
            {current with BoatLocation = {X = boatX + (wpX * i); Y = boatY + (wpY * i)}}


    let Solution file =
        let instructions =
            File.ReadLines file
            |> Seq.map Part1.Instruction.parse
            |> Seq.toList

        let start = {
            BoatTracker.BoatLocation = {X = 0; Y=0}
            BoatTracker.WaypointLocation = {X=10; Y=1}
        }

        instructions
        |> List.fold (move
        ) start

        