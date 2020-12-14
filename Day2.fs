namespace Day2

open System.IO

module Part1 =

    type PasswordRule1 = {
        Min:  int
        Max: int
        Character: char
        Password: string
    } with
        static member fromLine (line:string) =
            // 1-3 a: abcde
            let parts = line.Split [|' '|]
            // first part has min and max separated by -
            let minmax = parts.[0].Split [|'-'|]
            // second part has repeat character
            let repeatChar = parts.[1].Remove 1
            // last part is password
            {
                Min = int minmax.[0]
                Max = int minmax.[1]
                Character = char repeatChar
                Password = parts.[2]
            }

        static member isValid (rule:PasswordRule1) =
            let charCount =
                rule.Password.ToCharArray()
                |> Array.sumBy (fun c -> if (c = rule.Character) then 1 else 0)
            (rule.Min <= charCount) && (charCount <= rule.Max)

    let Solution file =

        let validPaswords =
            File.ReadLines file
            |> Seq.map PasswordRule1.fromLine 
            |> Seq.fold (fun state rule -> 
                    if (PasswordRule1.isValid rule) then
                        state + 1
                    else
                        state
                ) 0
        validPaswords


module Part2 =

    type PasswordRule2 = {
        PresentPos:  int
        AbsentPos: int
        Character: char
        Password: string
    } with
        static member fromLine (line:string) =
            // 1-3 a: abcde
            let parts = line.Split [|' '|]
            // first part has min and max separated by -
            let pos = parts.[0].Split [|'-'|]
            // second part has repeat character
            let repeatChar = parts.[1].Remove 1
            // last part is password
            {
                PresentPos = int pos.[0]
                AbsentPos = int pos.[1]
                Character = char repeatChar
                Password = parts.[2]
            }
        
        static member isValid (rule:PasswordRule2) =
            let pwd = rule.Password.ToCharArray()

            ( (pwd.[rule.PresentPos - 1] = rule.Character) && (pwd.[rule.AbsentPos - 1] <> rule.Character) ||
              (pwd.[rule.AbsentPos - 1] = rule.Character) && (pwd.[rule.PresentPos - 1] <> rule.Character)
            )

    let Solution file =

        let validPaswords =
            File.ReadLines file
            |> Seq.map PasswordRule2.fromLine 
            |> Seq.fold (fun state rule -> 
                    if (PasswordRule2.isValid rule) then
                        state + 1
                    else
                        state
                ) 0
        validPaswords
    