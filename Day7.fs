namespace Day7

open System.IO

module Part1 =


    type BagInfo = {
        Name:string
        Quantity: int
    } with
        static member parse (bi:string) =
            (* 
                split on space on get the details

                bright white bags contain 1 shiny gold bag.
                muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
            *)
            let bagInfo = bi.Trim()
            let qi = bagInfo.IndexOf ' '
            let quantity = int (bagInfo.Substring(0, qi))
            let name = bagInfo.Substring(qi + 1).Replace("bags", "bag").Replace(".","")
            {
                BagInfo.Name = name
                BagInfo.Quantity = quantity
            }

    let parseLine (line:string) =
        // Separate on contain
        let bagData = line.Replace("contain", ";").Split([|';'|])
        let containerBag = bagData.[0].Replace("bags", "bag")

        let insideBags =
            match bagData.[1].Trim() with
            | "no other bags." ->
                [||]
            | ib ->
                // Split on comma to get the bags
                let ibs =
                    ib.Split([|','|])
                    |> Array.map(BagInfo.parse)
                ibs

        // return the container bag and the inside bags
        containerBag, insideBags


    let Solution file =
        let bagData =
            File.ReadLines file
            |> Array.ofSeq
            |> Array.map parseLine
        
         // create a reverse map
        let bagMap =
            bagData|> Array.fold (fun state (cb, ibs) ->
                // for each inside bag, add it as key that
                // points to the container bag
                let d =
                    ibs
                    |> Array.fold (fun s ib ->
                        let addedContainerBags = 
                            Map.tryFind ib.Name state 
                            |> Option.defaultValue List.empty
                        
                        Map.add ib.Name (cb::addedContainerBags) s
                    ) state

                d
            ) Map.empty

        printfn "%A" bagMap
        bagMap.Count