namespace Day14

open System
open System.IO

module Part1 =

    type MemoryInstruction = {
        Location: int
        Value: int
    }

    type Instruction = Memory of MemoryInstruction | Mask of string
    with
        static member parse (s:string) =
            let parts = s.Split [|'='|]
            if (s.Substring(0,4) = "mask") then
                // Mask
                Mask (parts.[1])
            else
                // Memory
                let value = (int)parts.[1]
                let location = (int)(s.Substring(4, (s.IndexOf("]") - 4)))
                let m = {MemoryInstruction.Location = location; Value = value}
                Instruction.Memory (m)


    let toBinary (i:int) =
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

    let applyMaskToMemory (mask:string) location value =
        ()
        
    let Solution file =
        let instructions =
            File.ReadLines file
            |> Seq.map Instruction.parse

        ()