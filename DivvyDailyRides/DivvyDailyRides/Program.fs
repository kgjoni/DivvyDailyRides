//
// F# program to analyze Divvy daily ride data.
//
// KRISTI GJONI
// U. of Illinois, Chicago
// CS 341, Spring 2018
// Project #04
//

#light

let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides
              
// Count recursively the number of male riders
let rec countMale ridedata =
  match ridedata with
  | []      -> 0
  | e::rest -> if (List.item 2 e = 1) then 
                 1 + countMale rest
               else 
                 0 + countMale rest

// Count recursively the number of female riders
let rec countFemale ridedata =
  match ridedata with
  | []      -> 0
  | e::rest -> if (List.item 2 e = 2) then 
                 1 + countFemale rest
               else 
                 0 + countFemale rest
               
// Count recursively the total amount of trip duration
let rec countDuration ridedata = 
  match ridedata with
  |[]       -> 0
  |e::rest  -> (List.item 1 e) + countDuration rest

// Count recursively the total age of male riders
let rec countAgeMale ridedata =
  match ridedata with
  |[]       -> 0
  |e::rest  -> if (List.item 2 e = 1) then
                 (2018 - (List.item 3 e)) + countAgeMale rest
               else 
                 countAgeMale rest

// Count recursively the total age of felmale riders
let rec countAgeFemale ridedata =
  match ridedata with
  |[]       -> 0
  |e::rest  -> if (List.item 2 e = 2) then
                 (2018 - (List.item 3 e)) + countAgeFemale rest
               else 
                 countAgeFemale rest

[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists --- [ [1308;321;2;1991]; ... ]
  //
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents
  //printfn "%A" ridedata

  let N = List.length ridedata
  printfn "# of rides: %A" N

  // Get the total number of male riders
  // Compute the average and print the precent average
  let M = countMale ridedata
  let avgMale = ((double M) / (double N)) * 100.00
  printfn "%% of male riders: %A" avgMale


  // Get the total number of female riders
  // Compute the average and print the precent average
  let F = countFemale ridedata
  let avgFemale = ((double F) / (double N)) * 100.00
  printfn "%% of female riders: %A" avgFemale


  // Get the total trip duration
  // Compute the average and print the precent average
  let D = countDuration ridedata
  let avgDuration = ((double D) / (double N * 60.00))
  printfn "Avg duration: %A mins" avgDuration


  // Get the sum of ages for male riders
  // Compute the average and print the precent average
  let AGEMale = countAgeMale ridedata
  let avgMale = ((double AGEMale) / (double M))
  printfn "Avg age of male riders: %A" avgMale


  // Get the sum of ages for female riders
  // Compute the average and print the precent average
  let AGEFemale = countAgeFemale ridedata
  let avgFemale = ((double AGEFemale) / (double F))
  printfn "Avg age of female riders: %A" avgFemale
   
  0 
