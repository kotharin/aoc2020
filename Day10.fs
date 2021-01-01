namespace Day10

open System.IO

module Part1 =

    let Solution file =
        let adps =
            0::
            (File.ReadLines file
            |> Seq.map int
            |> Seq.toList)
            |> List.sort
        
        let adapters = List.append adps [(List.last adps + 3)]

        let diffs =
            List.pairwise adapters
            |> List.map (fun (a,b) -> b - a)
            |> List.countBy id
            |> Map.ofList

        
        (Map.find 3 diffs) * (Map.find 1 diffs)

module Part2 =
        
    let Solution file =
        let adps =
            0::
            (File.ReadLines file
            |> Seq.map int
            |> Seq.toList)
            |> List.sort
        
        let adapters = List.append adps [(List.last adps + 3)]

        let mapCount =
            adapters
            |> List.fold (fun state a -> 
                Map.add a 0L state
            ) Map.empty
            |> Map.add 0 1L


        (*
for adapter in sorted(adapters):
    for diff in range(1, 4):
        next_adapter = adapter + diff
        if next_adapter in adapters:
            paths[next_adapter] += paths[adapter]
print(paths[max_voltage])        
        *)

        let count adapter mapCount =
            [1..3]
            |> List.fold (fun state i -> 
                match (Map.tryFind (adapter + i) state) with
                | None ->
                    state
                | Some c ->
                    // get the count of the current adapter
                    let cac = Map.find adapter state
                    Map.add (adapter + i) (c+cac) state
               
            ) mapCount

        let count = 
            adapters
            |> List.fold (fun state a ->
                count a state
            ) mapCount

        Map.find (List.last adapters) count

