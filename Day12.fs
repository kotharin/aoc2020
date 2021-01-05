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

        let newDirection = (currentDirection + turnIncrement) % 4

        newDirection

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
        

    let traverseInstructions instructions =
        ()

    let Solution file =
        let directionMap =
            [
                (1,"N");
                (2,"E");
                (3,"S");
                (4,"W")
            ]|> Map.ofList


        let instructions =
            File.ReadLines file
            |> Seq.map Instruction.parse
            |> Seq.toList

        instructions
        |> List.fold (move
        ) {Location.Direction = Direction.East
           X = 0
           Y = 0}
        