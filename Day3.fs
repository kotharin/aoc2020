namespace Day3

open System.IO

module Part1 =

    type LocationDetail = Tree | Empty

    type RowData = {
        Columns: LocationDetail[]
    } with
        static member fromString (line:string) =
            let cols = 
                line.ToCharArray()
                |> Array.map (fun c -> if (c = '#') then LocationDetail.Tree else LocationDetail.Empty)
            {
                RowData.Columns = cols
            }

        static member getLocationDetail (row:RowData) (col:int) =
            let tc = Array.length row.Columns
            let similarColumn = col % tc
            row.Columns.[similarColumn]
                

    let navigateMap rightMoves downMoves (rows:RowData[]) =

        let rec moveAndCount (rows:RowData[]) currentRow currentCol rightMoves downMoves totalTrees =
            if (currentRow = rows.Length - 1) then
                // past last row, return total rows
                totalTrees
            else
                // move appropriate number to right and down
                let newCol = currentCol + rightMoves
                let newRow = currentRow + downMoves
                // read the position detail
                let newTotal =
                    if (RowData.getLocationDetail rows.[newRow] newCol = LocationDetail.Tree) then
                        totalTrees + 1
                    else
                        totalTrees
                
                moveAndCount rows newRow newCol rightMoves downMoves newTotal

        moveAndCount rows 0 0 rightMoves downMoves 0

    let Solution file =
        File.ReadLines file
        |> Seq.map RowData.fromString
        |> Array.ofSeq
        |> navigateMap 3 1