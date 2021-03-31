namespace Day16

open System.IO

module Part1 =

    type FieldInfo = {
        Name: string
        Ranges: (int*int) array
    } 
                    

    type Ticket = {
        Values: List<int>
    } with
        static member empty =
            {Values = List.empty}

    type Data = 
        | FieldInfo of FieldInfo
        | Ticket of Ticket
        | MyTicketHeader
        | NearbyTicketHeader
        | NotUsed


    let parseFieldInfo (s:string) =
            let parts = s.Split [|':'|]
            if ((Array.length parts > 1) && (parts.[1].Trim().Length > 0)) then
                let name = parts.[0].Trim()
                let rngInfo = 
                    parts.[1].Trim().Replace(" or ", ",").Split [|','|]
                    |> Array.map (fun ri ->
                        let rngParts = ri.Split [|'-'|]
                        (int)(rngParts.[0].Trim()), (int)(rngParts.[1].Trim())
                    )
                (FieldInfo({FieldInfo.Name = name; FieldInfo.Ranges = rngInfo})) |> Some
            else
                None

    let parseTicket (s:string) =
            let parts = s.Split [|','|]
            if (Array.length parts > 1) then
                let vals =
                    parts
                    |> Array.map (fun p ->
                        (int)p
                    )|> List.ofArray
                Some (Ticket({Ticket.Values = vals}))
            else
                None
    let parseTicketHeader (s:string) =
        if (s = "your ticket:") then
            Some MyTicketHeader
        elif (s = "nearby tickets:") then
            Some NearbyTicketHeader
        else None

    let parseNotUsed _ =
        Some NotUsed

    type Data with
        static member parse (s:string) =
            [
                parseFieldInfo
                parseTicket
                parseTicketHeader
                parseNotUsed
            ]
            |> List.pick (fun fn -> fn s)

    let isInRange start stop value =
        ((start <= value) && (value <= stop))

    let isInAnyRange ranges value =
        ranges
        |> Array.fold (fun state (start,stop) -> 
            state || isInRange start stop value
        ) false
    
    // Partition the numbers by whether they are part
    // of any of the ranges provided. 
    let partitionNumbers ranges numbers =
        numbers
        |> List.partition (fun n ->
            isInAnyRange ranges n
        )

    let Solution file =
        let _,myTicket,fieldInformation, nearbyTickets = 
            File.ReadLines file
            |> Seq.fold (fun (prevWasMyTkHdr,myTkt, fieldInfos, ticketInfos) line ->
                let d = Data.parse line
                match d with
                | FieldInfo fi ->
                    // add to field info list
                    false,myTkt, fieldInfos@[fi], ticketInfos
                | MyTicketHeader ->
                    true,myTkt, fieldInfos, ticketInfos
                | NearbyTicketHeader ->
                    false,myTkt, fieldInfos, ticketInfos
                | Ticket tkt ->
                    if (prevWasMyTkHdr) then
                        false,tkt, fieldInfos, ticketInfos
                    else
                        false,myTkt, fieldInfos, ticketInfos@[tkt]
                | NotUsed ->
                    false,myTkt, fieldInfos, ticketInfos
            ) (false, Ticket.empty, List.empty, List.empty)
        
        // flatten out all the field value range info into 1 list
        let fieldRanges =
            fieldInformation
            |> List.map (fun fi -> fi.Ranges)
            |> Array.concat

        let ticketValues =
            List.collect (fun nt -> nt.Values) nearbyTickets

        let _, notInRange = partitionNumbers fieldRanges ticketValues

        List.sum notInRange

module Part2 =

    open Part1
    
    let isTicketInvalid fieldRanges ticket =
        let isInvalid = isInAnyRange fieldRanges >> not
        ticket.Values
        |> List.exists (isInvalid)

    let isOutOfRange value range1 range2 =
        let min1,max1 = range1
        let min2,max2 = range2
        let x = ((value < min1) || (value > max1)) && ((value < min2) || (value > max2))
        x

    let isColumnInRanges colValues range1 range2 =
        // check if min and max are both in either range
        // if so, thenthe whole column is in range
        let sortedValues = Array.sort colValues
        let rec recIsColumnInRanges (sv:int array) range1 range2 =
            let min, max = sv.[0], Array.last sv
            let min1,max1 = range1
            let min2,max2 = range2
            if (((min1 <= min) && (max <= max1)) || ((min2 <= min) && (max <= max2))) then
                true
            else
                if ((isOutOfRange min range1 range2) || (isOutOfRange max range1 range2)) then
                    false
                else
                    // if there are only 2 value in the array and you reach here, that means
                    // both values are in some range. we are done.
                    if (Array.length sv = 2) then
                        true
                    else
                        // need to check if there are any values that fall outside the 2 ranges
                        // get the mid point. if it falls outside the ranges, we are done as
                        // it falls outside the ranges. if it falls inside one of the ranges, then
                        // we need to check the other half to see if it falls outside the range.
                        let midPoint = int (floor (((float (Array.length sortedValues)) - (float 0))/ float 2))
                        let midValue = sortedValues.[midPoint]
                        if (isOutOfRange midValue range1 range2) then
                            false
                        else
                            // check which half is in range
                            if ((min1 <= midValue) && (midValue <=max1)) then
                                // check second half of the array
                                let toCheck = snd (Array.splitAt (midPoint + 1) sv)
                                recIsColumnInRanges toCheck range1 range2
                            else
                                // check first half of range
                                let toCheck = fst (Array.splitAt (midPoint + 1) sv)
                                recIsColumnInRanges toCheck range1 range2

        recIsColumnInRanges sortedValues range1 range2
        
    let Solution file =

        let _,myTicket,fieldInformation, nearbyTickets = 
            File.ReadLines file
            |> Seq.fold (fun (prevWasMyTkHdr,myTkt, fieldInfos, ticketInfos) line ->
                let d = Data.parse line
                match d with
                | FieldInfo fi ->
                    // add to field info list
                    false,myTkt, fieldInfos@[fi], ticketInfos
                | MyTicketHeader ->
                    true,myTkt, fieldInfos, ticketInfos
                | NearbyTicketHeader ->
                    false,myTkt, fieldInfos, ticketInfos
                | Ticket tkt ->
                    if (prevWasMyTkHdr) then
                        false,tkt, fieldInfos, ticketInfos
                    else
                        false,myTkt, fieldInfos, ticketInfos@[tkt]
                | NotUsed ->
                    false,myTkt, fieldInfos, ticketInfos
            ) (false, Ticket.empty, List.empty, List.empty)

        // flatten out all the field value range info into 1 list
        let fieldRanges =
            fieldInformation
            |> List.map (fun fi -> fi.Name,fi.Ranges)
            |> Map.ofList
            
        let allFieldRanges =
            Array.collect snd (Map.toArray fieldRanges)

        // get only valid tickets
        let validTickets =
            nearbyTickets
            |> List.filter (isTicketInvalid allFieldRanges >> not)

        // bucket all values for the same column together
        // Initialize the buckets
        let emptyBuckets =
            [0..(List.length myTicket.Values - 1)]
            |> List.fold (fun map col ->
                Map.add col List.empty map
            ) Map.empty
        // Add values into each bucket
        let columnBuckets =
            validTickets
            |> List.fold (fun bucket tkt ->
                tkt.Values
                |> List.fold (fun (col, map) v ->
                    let list = Map.find col map
                    col + 1, Map.add col (list@[v]) map
                ) (0,bucket)
                |> snd
            ) emptyBuckets
            |> Map.map (fun col bucket -> List.toArray bucket)

        let columnFieldMapping =
            columnBuckets
            |> Map.map (fun col bucket ->
                // check if the column fits into any of the field ramges
                let x = 
                    fieldInformation
                    |> List.fold (fun state fi ->
                        if (Option.isNone state) then
                            if (isColumnInRanges bucket fi.Ranges.[0] fi.Ranges.[1]) then
                                Some(fi)
                            else None
                        else
                            state
                    ) None
                x
            )
        
        printfn "%A" columnFieldMapping
        0