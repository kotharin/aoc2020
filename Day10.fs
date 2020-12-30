namespace Day10

open System.IO

module Part1 =

    type DiffBucket = Diff1 | Diff2 | Diff3

    let findNextAdapter adapters currentAdapter =
        // check if there is an adapter with +1 rating.
        // if not check for +2 and +3
        match Map.tryFindKey (fun k _ -> k = (currentAdapter + 1)) adapters with
        | Some v ->
            Some (v, Diff1)
        | None ->
            match Map.tryFindKey (fun k _ -> k = (currentAdapter + 2)) adapters with
            | Some v ->
                Some (v, Diff2)
            | None ->
                match Map.tryFindKey (fun k _ -> k = (currentAdapter + 3)) adapters with
                | Some v ->
                    Some (v, Diff3)
                | _ -> None

    let getAdapterChain adapters =

        // add all adapters to a map
        let mapAdapters =
            adapters
            |> List.map (fun a -> a,1)
            |> Map.ofList

        // Seed the diff counts
        // The 3 diff is seeded with 1
        // because the last adapter, 
        // which is not on the list has
        // a diff of 3 with the last one
        // that IS on the the list.
        let mapDiffs =
            [(Diff1,0);(Diff2,0);(Diff3,1)]
            |> Map.ofList

        let rec getChain map currentAdapter chain diffCount=
            match findNextAdapter map currentAdapter with
            | None ->
                chain,diffCount
            | Some (nextAdapter, diff) ->
                // Add to the chain
                let newChain = List.append chain [nextAdapter]
                // Add to diff bucket
                let dc = Map.find diff diffCount
                let newDiffCount = Map.add diff (dc + 1) diffCount
                // find the next adapter
                getChain map nextAdapter newChain newDiffCount

        getChain mapAdapters 0 [0] mapDiffs 


    let Solution file =
        let adapters =
            File.ReadLines file
            |> Seq.map int
            |> Seq.toList
            |> List.sort

        let _,diffs = getAdapterChain adapters

        (Map.find Diff1 diffs) * (Map.find Diff3 diffs)