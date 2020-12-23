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
        let containerBag = bagData.[0].Replace("bags", "bag").Trim()

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

    (*
        Traverse the map till you hit a node with no parent
    *)
    let traverseBagMap bagToFind (bagMap:Map<string, string list>) =

        let rec traverseRec currentNode (bm:Map<string, string list>) traversed count =
            // check if the current node has any node that point to it
            match Map.tryFind currentNode bm with
            | None | Some ([]) ->
                count,traversed
            | Some (nodes) ->
                nodes
                |> List.fold (fun (stepCount, tn) node ->
                    // if not traversed, traverse it else skip
                    if (Map.containsKey node traversed) then
                        count,traversed
                    else
                        let updatedTraversed = (Map.add node 1 tn)
                        traverseRec node bm updatedTraversed (stepCount + 1)
                ) (count, traversed)
                // for each of them traverse the nodes till the roots
                // traverseRec head bm (count + 1)
            
        let traversedAlready = Map.empty
        traverseRec bagToFind bagMap traversedAlready 0 


    let traverseContainerBagMap bagToFind (cbMap:Map<string, Map<string, int>>) =

        let rec traverse (bagsToFind:List<string>) bagMap accumulatedContainers =
            match bagsToFind with
            | head::tail ->
                // find all containers that can contain this bag
                let newBagsToFind =
                    bagMap
                    |> Map.fold (fun state cb ib ->
                        if (Map.containsKey head ib) then
                            // add this to the list of bags to find
                            cb::state                            
                        else
                            state
                    ) tail
                // add these to the accumulated containers
                // IF it doesn't already exist
                let newAccumulatedContainers =
                    newBagsToFind
                    |> List.fold (fun state c ->
                        if (not (Map.containsKey c state)) then
                            Map.add c 1 state
                        else
                            state
                    ) accumulatedContainers
                traverse newBagsToFind bagMap newAccumulatedContainers
            | [] ->
                accumulatedContainers

        traverse [bagToFind] cbMap Map.empty

    let Solution file =
        let bagData =
            File.ReadLines file
            |> Array.ofSeq
            |> Array.map parseLine
        
        // create a map container bag -> list of inside bags
        let containerBagMap =
            bagData
            |> Array.fold( fun state (cb,ibs) -> 
                // map of all containers with their numbers
                let cm =
                    ibs
                    |> Array.fold (fun state ib ->
                        Map. add ib.Name ib.Quantity state
                    ) Map.empty
                Map.add cb cm state
            ) Map.empty

        
        // create a reverse map
        (*
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
        traverseBagMap "shiny gold bag" bagMap
        *)
        traverseContainerBagMap "shiny gold bag" containerBagMap |> Map.count

module Part2 =

    (*
        Brute force solution. Worst runtime.
    *)
    let countInsideBags bagToCount (bagMap:Map<string, Map<string, int>>) =
        
        let rec countBags bag (bags:Map<string, Map<string, int>>) =

            // find the inside bags for this container bag
            let ibc = Map.find bag bags
            if (Map.count ibc = 0) then
                0
            else
                // get the count for all these bags
                let innerCount =
                    ibc|> Map.fold (fun state b c ->
                        let cnt = countBags b bags
                        state + (c * (cnt) + c)
                    ) 0
                innerCount

        countBags bagToCount bagMap

    let Solution file =
        let bagData =
            File.ReadLines file
            |> Array.ofSeq
            |> Array.map Part1.parseLine

        // create a map container bag -> list of inside bags
        let containerBagMap =
            bagData
            |> Array.fold( fun state (cb,ibs) -> 
                // map of all containers with their numbers
                let cm =
                    ibs
                    |> Array.fold (fun map ib ->
                        Map. add ib.Name ib.Quantity map
                    ) Map.empty
                Map.add cb cm state
            ) Map.empty

        countInsideBags "shiny gold bag" containerBagMap
