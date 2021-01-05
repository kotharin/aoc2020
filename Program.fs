// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System


[<EntryPoint>]
let main argv =

    // Day 1
    // Part 1
    let input = [1313;1968;
                        1334;
                        1566;
                        820;
                        1435;
                        1369;
                        1230;
                        1383;
                        1816;
                        1396;
                        1974;
                        1911;
                        1989;
                        1824;
                        1430;
                        1709;
                        1204;
                        1792;
                        1800;
                        1703;
                        2009;
                        1467;
                        1400;
                        1315;
                        1985;
                        1598;
                        1215;
                        1574;
                        1770;
                        1870;
                        1352;
                        1544;
                        1339;
                        188;
                        1347;
                        1986;
                        2003;
                        1538;
                        1839;
                        1688;
                        1350;
                        1191;
                        1961;
                        1578;
                        1946;
                        1548;
                        1975;
                        1745;
                        1631;
                        1390;
                        1811;
                        1586;
                        1409;
                        247;
                        1600;
                        1565;
                        1929;
                        1854;
                        1602;
                        1773;
                        1815;
                        1887;
                        1689;
                        1266;
                        1573;
                        1534;
                        1939;
                        1909;
                        1273;
                        1386;
                        1713;
                        1268;
                        1611;
                        1348;
                        1478;
                        1857;
                        1916;
                        1113;
                        936;
                        1603;
                        1716;
                        1875;
                        1855;
                        1834;
                        1701;
                        1279;
                        1346;
                        1503;
                        1797;
                        1287;
                        1447;
                        1475;
                        1950;
                        1614;
                        1261;
                        1442;
                        1299;
                        1465;
                        896;
                        1481;
                        1804;
                        1931;
                        1849;
                        1675;
                        1726;
                        355;
                        1485;
                        1343;
                        1697;
                        1735;
                        1858;
                        1205;
                        1345;
                        1281;
                        253;
                        1808;
                        1557;
                        1964;
                        1771;
                        1891;
                        1583;
                        1896;
                        1398;
                        1930;
                        1258;
                        1338;
                        1208;
                        1328;
                        1493;
                        1963;
                        1374;
                        1212;
                        1223;
                        1501;
                        2004;
                        1591;
                        1954;
                        115;
                        1972;
                        1814;
                        1643;
                        1270;
                        1349;
                        1297;
                        1399;
                        1969;
                        1237;
                        1228;
                        1379;
                        1779;
                        1765;
                        1427;
                        1464;
                        1247;
                        1967;
                        1577;
                        1719;
                        1559;
                        1274;
                        1879;
                        1504;
                        1732;
                        1277;
                        1758;
                        1721;
                        1936;
                        1605;
                        1358;
                        1505;
                        1411;
                        1823;
                        1576;
                        1682;
                        1439;
                        1901;
                        1940;
                        1760;
                        1414;
                        1193;
                        1900;
                        1990;
                        1781;
                        1801;
                        1239;
                        1729;
                        1360;
                        1780;
                        1848;
                        1468;
                        1484;
                        1280;
                        1278;
                        1851;
                        1903;
                        1894;
                        1731;
                        1451;
                        549;
                        1570]

    match Day1.Part1.Solution input 2020 with
    | Some (n1, n2) -> printfn "Day 1/Part1 - %i" (n1*n2)
    | None -> printfn "Day1/Part1 - No Solution"

    // Part 2
    match Day1.Part2.Solution input 2020 with
    | Some (n1, n2, n3) -> printfn "Day 1/Part2 - %i" (n1*n2*n3)
    | None -> printfn "Day1/Part2 - No Solution"

    // Day 2
    // Part 1
    printfn "Day 2/Part 1 Valid Passwords:%i" (Day2.Part1.Solution "Day2Part1.txt")

    // Part 2
    printfn "Day 2/Part 2 Valid Passwords:%i" (Day2.Part2.Solution "Day2Part2.txt")


    // Day 3
    // Part 1
    printfn "Day 3/Part 1 Tree Count:%i" (Day3.Part1.Solution "Day3Part1.txt")

    // Part 2
    printfn "Day 3/Part 2 Tree Count:%A" (Day3.Part2.Solution "Day3Part1.txt")


    // Day 4
    // Part 1
    printfn "Day 4/Part 1 %i" (Day4.Part1.Solution "Day4Part1.txt")

    // Part 2
    printfn "Day 4/Part 2 %i" (Day4.Part2.Solution "Day4Part1.txt")

    // Day 5
    // Part 1
    printfn "Day5/Part 1 %i" (Day5.Part1.Solution "Day5Part1.txt")

    // Part 2
    printfn "Day5/Part 2 %i" (Day5.Part2.Solution "Day5Part1.txt")

    // Day 6
    // Part 1
    printfn "Day6/Part 1 %i" (Day6.Part1.Solution "Day6Part1.txt")

    // Part 2
    printfn "Day6/Part 2 %i" (Day6.Part2.Solution "Day6Part1.txt")

    // Day 7
    // Part 1
    printfn "Day7/Part 1 %i " (Day7.Part1.Solution "Day7Part1.txt")

    // Part 2
    printfn "Day7/Part 2 %i " (Day7.Part2.Solution "Day7Part1.txt")

    // Day 8
    // Part 1
    printfn "Day8/Part 1 %i " (Day8.Part1.Solution "Day8Part1.txt")

    // Part 2
    printfn "Day8/Part 2 %i " (Day8.Part2.Solution "Day8Part1.txt")

    // Day 9
    // Part 1
    printfn "Day9/Part 1 %i " ((Day9.Part1.Solution "Day9Part1.txt") |> List.head)

    // Part 2
    printfn "Day9/Part 2 %A " ((Day9.Part2.Solution "Day9Part1.txt"))

    // Day 10
    // Part 1
    printfn "Day10/Part 1 %A " (Day10.Part1.Solution "Day10Part1.txt")

    // Part 2
    printfn "Day10/Part 2 %A " (Day10.Part2.Solution "Day10Part1.txt")

    // Day 11
    // Part 1
    //printfn "Day11/Part 1 %A " (Day11.Part1.Solution "Day11Part1.txt")

    // Part 2
    //printfn "Day11/Part 2 %A " (Day11.Part2.Solution "Day11Part1.txt")

    // Day 12
    // Part 1
    printfn "Day12/Part 1 %A " (Day12.Part1.Solution "Day12Part1.txt")

    // Part 2
    printfn "Day12/Part 2 %A " (Day12.Part2.Solution "Day12Part1.txt")
    0 // return an integer exit code