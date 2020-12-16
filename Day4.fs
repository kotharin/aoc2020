namespace Day4

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