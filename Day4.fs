namespace Day4

open System
open System.IO

module Part1 =

    (*
        byr (Birth Year)
        iyr (Issue Year)
        eyr (Expiration Year)
        hgt (Height)
        hcl (Hair Color)
        ecl (Eye Color)
        pid (Passport ID)
        cid (Country ID)        
    *)

    type Document = {
        Attributes: Map<string, string>
        RawData: string
    } with
        static member atributesFromString (data:string) =
            // split by space
            let kvs = data.Split [|' '|]
            let attributes =
                kvs
                |> Array.fold (fun state kv -> 
                    let keyValue = kv.Split [|':'|]
                    List.Cons ((keyValue.[0],keyValue.[1]) ,state)
                ) List.empty

            attributes
        static member isValid (doc:Document) =
            ((doc.Attributes.Count > 7) || (doc.Attributes.Count = 7 && (not (Map.containsKey "cid" doc.Attributes))))

    let splitIntoDocumentChunks (docLines:seq<string>) =
        let rec chunk (lines:List<string>) (currentDocumentAttributes:List<string*string>) (currentDocumentRawData:string) (accumulatedDocuments:List<Document>) =
            match lines with
            | head::tail ->
                if (head.Length > 0) then
                    let attrbs = Document.atributesFromString head
                    let rawData = currentDocumentRawData + "\n" + head
                    chunk tail (List.append attrbs currentDocumentAttributes) rawData accumulatedDocuments
                else
                    // create a new document from the attributes and append to the accumulated documents
                    let newDoc = 
                        {
                            Document.Attributes = Map.ofList currentDocumentAttributes
                            Document.RawData = currentDocumentRawData
                        }
                    chunk tail List.empty "" (newDoc::accumulatedDocuments)
            | [] ->
                    let newDoc = 
                        {
                            Document.Attributes = Map.ofList currentDocumentAttributes
                            Document.RawData = currentDocumentRawData
                        }
                    newDoc::accumulatedDocuments

        chunk (List.ofSeq docLines) List.empty "" List.empty

                


    let Solution file =
        File.ReadLines file 
        |> splitIntoDocumentChunks
        |> List.filter Document.isValid
        |>List.length

module Part2 =

    (*
        byr (Birth Year) - four digits; at least 1920 and at most 2002.
        iyr (Issue Year) - four digits; at least 2010 and at most 2020.
        eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
        hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
        hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
        ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
        pid (Passport ID) - a nine-digit number, including leading zeroes.
        cid (Country ID) - ignored, missing or not.
    *)
    let allowedEyeColors = Set.ofArray [|"amb"; "blu" ;"brn"; "gry"; "grn"; "hzl"; "oth"|]

    type Part1.Document with
        static member isValidWithFieldRestrictions (doc:Part1.Document) =
            
            if (not (Part1.Document.isValid doc)) then
                false
            else
                // Check each field restirction
                doc.Attributes
                |> Map.fold (fun state key value ->
                    let vorn =
                        match key with
                        | "byr" ->
                            let v = int value
                            ((v <= 2002) && (v >= 1920))
                        | "iyr" ->
                            let v = int value
                            ((v <= 2020) && (v >= 2010))
                        | "eyr" ->
                            let v = int value
                            ((v <= 2030) && (v >= 2020))
                        | "hgt" ->
                            let h = value.Replace("cm","").Replace("in","")
                            let units = value.Replace(h,"")
                            // check if the numeric part is first
                            // check if the numeric ranges are OK
                            let hv = int h
                            ((units.Length = 2) && (h+units = value) && ((units = "cm" && (hv >= 150) && (hv <= 193) ) || (units = "in" && (hv >= 59) && (hv <= 76))))
                        | "hcl" ->
                            let fc = value.[0]

                            let rest = value.Substring(1)

                            let invalidCharacters =
                                rest
                                |> Seq.where (fun c -> not ((c >= '0' && c<='9') || (c >= 'a' && c <='f')))
                                |> Seq.length

                            (
                                (value.Length = 7) &&
                                (fc = '#') && // Starts with a #
                                (rest.Length = 6) && // 6 characters after #
                                (invalidCharacters = 0) // character 0-9 or a-f only allowed
                            )
                        | "ecl" ->
                            (Set.contains value allowedEyeColors)

                        | "pid" ->
                            (
                                (value.Length = 9) &&
                                not (value
                                |> Seq.exists (fun c -> (c < '0' || c > '9'))))

                        | _ -> true
                    //printfn "key:%s, value:%s, valid:%b" key value vorn
                    state && vorn

                ) true
                
    let Solution file =
        File.ReadLines file 
        |> Part1.splitIntoDocumentChunks
        |> List.filter Part1.Document.isValidWithFieldRestrictions
        |>List.length