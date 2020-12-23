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


module Part2 =

    type InstructionType =
        | Jump
        | Accumulator
        | NoOp

    type Instruction = {
        Type: InstructionType
        Value: int
    } with
        static member parse (line:string) =
            // split the line by the space between
            // the instruction and the argument
            let insParts = line.Split [|' '|]
            match insParts.[0] with
            | "acc" ->
                {Instruction.Type = Accumulator 
                 Instruction.Value = (int insParts.[1])
                }
            | "jmp" ->
                {Instruction.Type = Jump 
                 Instruction.Value = (int insParts.[1])
                }
            | _ ->
                {Instruction.Type = NoOp 
                 Instruction.Value = 0
                }

    let traverseInstructions ins repIns repInd =

        let instructionCount = Array.length ins

        let rec traverse instructions accum executedInstructions replaceInstruction replaceIndex currentIns =

            // if you reached past the last instruction, then the program terminates correctly
            if (currentIns = instructionCount) then
                accum,true
            else
                // If the current instruction has already been executed,
                // return the accumulator value
                match Map.containsKey currentIns executedInstructions with
                | true -> accum,false
                | false ->
                    // check if the current instruction needs to be repolaced
                    // Use the original instruction value
                    let originalInstruction = Array.get instructions currentIns
                    let currentInstruction =
                        if (currentIns = replaceIndex) then
                            
                            {originalInstruction with Type = replaceInstruction}
                        else
                            originalInstruction
                    // add the instrcution as executed
                    let newExecuted = Map.add currentIns 1 executedInstructions
                    match currentInstruction.Type with
                    | Accumulator ->
                        traverse instructions (accum + currentInstruction.Value) newExecuted replaceInstruction replaceIndex (currentIns + 1)
                    | Jump ->
                        traverse instructions accum newExecuted replaceInstruction replaceIndex (currentIns + currentInstruction.Value)
                    | NoOp ->
                        traverse instructions accum newExecuted replaceInstruction replaceIndex (currentIns + 1)

        traverse ins 0 Map.empty repIns repInd 0

    let findTerminatingFix currentInstruction replaceInstruction instructions =
        // find the indexes which have the instruction that needs to be replaced

        let replaceIndexes =
            instructions
            |> Array.mapi (fun index instruction -> 
                if (instruction.Type = currentInstruction) then
                    Some index
                else
                    None
            ) |> Array.choose id

        let mi = Array.length replaceIndexes
        let rec findFix index maxIndex =
            // check if this replacement causes the program to terminate
            let replaceIndex = replaceIndexes.[index]
            let a,term = traverseInstructions instructions replaceInstruction replaceIndex

            if term then
                a,term
            else
                if (index = maxIndex - 1) then
                    0,false
                else 
                    findFix (index+1) mi

        findFix 0 mi
        
    let Solution file =
        let instructions = 
            File.ReadLines file
            |> Seq.map Instruction.parse
            |> Seq.toArray

        // replace noop with jump
        let a1,term1 = findTerminatingFix NoOp Jump instructions
        if term1 then
            a1
        else
            findTerminatingFix Jump NoOp instructions |> fst
        