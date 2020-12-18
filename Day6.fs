namespace Day6

open System.IO

module Part1 =

    type Group = {
        Questions:Set<char>
        RawData:string
    }

    let splitIntoGroups (docLines:List<string>) =

        let rec group (lines:List<string>) rawData setOfQuestions accumulatedGroups=
            match lines with
            | head::tail ->
                if (head.Length > 0) then
                    // add the questions to an existing group
                    let newSet =
                        head.ToCharArray()
                        |> Array.fold (fun state c -> 
                            Set.add c state
                        ) setOfQuestions
                    let newData = rawData + "\n" + head
                    group tail newData newSet accumulatedGroups
                else
                    // group is done.
                    let newGroup = 
                        {
                            Group.Questions = setOfQuestions
                            Group.RawData = rawData
                        }
                    group tail "" Set.empty (newGroup::accumulatedGroups)
            | [] ->
                let newGroup =
                    {
                        Group.Questions = setOfQuestions
                        Group.RawData = rawData
                    }
                (newGroup::accumulatedGroups)
        
        group docLines "" Set.empty List.empty

    let Solution file =
        File.ReadLines file
        |> List.ofSeq
        |> splitIntoGroups
        |> List.fold (fun state grp ->
            state + grp.Questions.Count
        ) 0
        