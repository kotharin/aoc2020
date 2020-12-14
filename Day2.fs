namespace Day2

open System.IO

module Part1 =

    type PasswordRule = {
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

        static member isValid (rule:PasswordRule) =
            let charCount =
                rule.Password.ToCharArray()
                |> Array.sumBy (fun c -> if (c = rule.Character) then 1 else 0)
            (rule.Min <= charCount) && (charCount <= rule.Max)

    let Solution file =
        let lines =  File.ReadLines file

        let rules = 
            lines
            |> Seq.map PasswordRule.fromLine
        let validPaswords =
            rules 
            |> Seq.fold (fun state rule -> 
                    if (PasswordRule.isValid rule) then
                        state + 1
                    else
                        state
                ) 0
        validPaswords