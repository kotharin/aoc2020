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