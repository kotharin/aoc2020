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
        
module Part2 =
    type Group = {
        Questions:Map<char,int>
        RawData:string
        MemberCount: int
    }

    let filterGroupMap map memberCount =
        Map.filter (fun _ v -> 
            v = memberCount
        ) map

    let splitIntoGroups (docLines:List<string>) =

        let rec group (lines:List<string>) rawData mapOfQuestions count accumulatedGroups =
            match lines with
            | head::tail ->
                if (head.Length > 0) then
                    // add the questions to an existing group
                    let newMap =
                        head.ToCharArray()
                        |> Array.fold (fun state c ->
                            let cc = Map.tryFind c state |> Option.defaultValue 0
                            Map.add c  (cc + 1) state
                        ) mapOfQuestions
                    let newData = rawData + "\n" + head
                    group tail newData newMap (count + 1) accumulatedGroups
                else
                    // group is done.
                    // only keep items in Map
                    // that has a count = member count
                    let filteredMap = filterGroupMap mapOfQuestions count

                    let newGroup = 
                        {
                            Group.Questions = filteredMap
                            Group.RawData = rawData
                            Group.MemberCount = count
                        }
                    group tail "" Map.empty 0 (newGroup::accumulatedGroups)
            | [] ->
                // only keep items in Map
                // that has a count = member count
                let filteredMap = filterGroupMap mapOfQuestions count
                let newGroup =
                    {
                        Group.Questions = filteredMap
                        Group.RawData = rawData
                        Group.MemberCount = count
                    }
                (newGroup::accumulatedGroups)
        
        group docLines "" Map.empty 0 List.empty

    let Solution file =
        File.ReadLines file
        |> List.ofSeq
        |> splitIntoGroups
        |> List.sumBy (fun g -> g.Questions.Count)
        