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