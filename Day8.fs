namespace Day8

open System.IO

module Part1 =

    type Instruction =
        | Accumulator of int
        | Jump of int
        | NoOp
        static member parse (line:string) =
            // split the line by the space between
            // the instruction and the argument
            let insParts = line.Split [|' '|]
            match insParts.[0] with
            | "acc" ->
                Instruction.Accumulator (int insParts.[1])
            | "jmp" ->
                Instruction.Jump (int insParts.[1])
            | _ -> Instruction.NoOp

    let traverseInstructions ins =

        let rec traverse instructions accum executedInstructions currentIns =
            // If the current instruction has already been executed,
            // return the accumulator value
            match Map.containsKey currentIns executedInstructions with
            | true -> accum
            | false ->
                let currentInstruction = Array.get instructions currentIns
                // add the instrcution as executed
                let newExecuted = Map.add currentIns 1 executedInstructions
                match currentInstruction with
                | Accumulator arg ->
                    traverse instructions (accum + arg) newExecuted (currentIns + 1)
                | Jump arg ->
                    traverse instructions accum newExecuted (currentIns + arg)
                | NoOp ->
                    traverse instructions accum newExecuted (currentIns + 1)

        traverse ins 0 Map.empty 0

    let Solution file =
        let instructions = 
            File.ReadLines file
            |> Seq.map Instruction.parse
            |> Seq.toArray
    

        traverseInstructions instructions