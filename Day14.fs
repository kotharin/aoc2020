namespace Day14

open System
open System.IO

module Part1 =

    type MemoryInstruction = {
        Location: int64
        Value: int64
    }

    type Instruction = Memory of MemoryInstruction | Mask of string
    with
        static member parse (s:string) =
            let parts = s.Split [|'='|]
            if (s.Substring(0,4) = "mask") then
                // Mask
                Mask (parts.[1].Trim())
            else
                // Memory
                let value = (int64)(parts.[1].Trim())
                let location = (int64)(s.Substring(4, (s.IndexOf("]") - 4)))
                let m = {MemoryInstruction.Location = location; Value = value}
                Instruction.Memory (m)


    let toBinary (i:int64) =
        let s = Convert.ToString (i,2)
        let pre = 
            Array.init (36 - s.Length) (fun _ -> '0')
            |> System.String
        pre + s

    let toMap (s:string) =
        s.ToCharArray()
        |> Array.fold (fun (ind, map) c ->
            ind + 1, Map.add ind c map
        ) (0,Map.empty)
        |> snd

    let toBinaryMap = toBinary >> toMap

    let applyMaskToMemory (mask:string) location (value) =
        let maskMap = mask |> toMap
        let valueMap = value |> toBinaryMap

        let memValue = 
            [0..35]
            |> List.fold (fun state i ->
                // resulting binary string
                if (maskMap.[i] = 'X') then
                    (state + valueMap.[i].ToString())
                else
                    (state + maskMap.[i].ToString())
            ) String.Empty
        location, Convert.ToInt64(memValue.Trim(), 2)
        
    let Solution file =
        let instructions =
            File.ReadLines file
            |> Seq.map Instruction.parse

        instructions
        |> Seq.fold (fun (mask,mems) i ->
            match i with
            | Mask m ->
                m, mems
            | Memory(mi) ->
                let loc, mv = applyMaskToMemory mask mi.Location mi.Value
                mask, (Map.add loc mv mems)
        ) (String.Empty, Map.empty)
        |> snd
        |> Map.toList
        |> List.sumBy snd

module Part2 =
    open Part1

    let toMap (s:string) =
        s.ToCharArray()
        |> Array.fold (fun (ind, map) c ->
            ind + 1, Map.add ind c map
        ) (0,Map.empty)
        |> snd

    // Take the memory address and get all
    // the combinations possible 2^n, where
    // n = number of X's in the address
    let rec permute s =
        match s with
        | [] ->
            [""]
        | head::tail ->
            if (head = 'X') then
                List.collect (fun i ->
                    permute tail
                    |> List.map (fun cc ->
                        i.ToString() + cc
                    )) [0..1]
            else
                permute tail
                |> List.map(fun cc -> 
                    head.ToString() + cc
                )

    // Apply the mask to the memory address
    let applyMaskToMemoryAddress mask address =
        let maskMap = toMap mask
        let addressMap = toMap address

        let memAddress = 
            [0..35]
            |> List.fold (fun state i ->
                // resulting binary string
                if (maskMap.[i] = '0') then
                    (state + addressMap.[i].ToString())
                else
                    (state + maskMap.[i].ToString())
            ) String.Empty
        memAddress

    let Solution file =
        let instructions =
            File.ReadLines file
            |> Seq.map Instruction.parse

        instructions|> Seq.fold (fun (mask, mems) i ->
            match i with
            | Mask m ->
                m, mems
            | Memory mi ->
                let um = applyMaskToMemoryAddress mask (toBinary mi.Location)
                // Get the permutations
                let memory = 
                    permute (um.ToCharArray() |> List.ofArray)
                    |> List.map (fun ma -> Convert.ToInt64(ma.Trim(), 2))
                    |> List.fold (fun state loc ->
                        Map.add loc mi.Value state
                    ) mems
                mask, memory
        ) (String.Empty,Map.empty)
        |> snd
        |> Map.toList
        |> List.sumBy snd