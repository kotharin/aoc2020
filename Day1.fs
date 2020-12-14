namespace Day1

module Part1 = 
    let Solution nums s =

        let findNumbersThatAddToSum numbers sum =
            let rec findNumbers sum remainingNumbers compliments =
                match remainingNumbers with
                | head::tail ->
                    // check if next number has an existing compliment,
                    let compliment = sum - head
                    if Map.containsKey head compliments then
                        // return the 2 numbers
                        Some(head,compliment)
                    else
                        // add the compliment to the map
                        let newCompliments = Map.add compliment compliment compliments
                        // check next number
                        findNumbers sum tail newCompliments
                | _ ->
                    None
            
            findNumbers sum numbers Map.empty

        //let input = [12;13]

        findNumbersThatAddToSum nums s

module Part2 =
    let Solution input s =
        let rec findThreeNumbersThatAddToTotal sum numbers =
            match numbers with
            | head::tail ->
                let rem = sum - head
                match Part1.Solution tail rem with
                | None ->
                    findThreeNumbersThatAddToTotal sum tail
                | Some(n1,n2) ->
                    Some(head, n1, n2)
            | _ ->
                None

        findThreeNumbersThatAddToTotal s input

