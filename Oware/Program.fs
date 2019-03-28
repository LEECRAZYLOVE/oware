module Oware

type StartingPosition =
    | South
    | North

type House =
   {
        number : int
        seeds : int
   }

type GameBoard = {
    Board : (House*House*House*House*House*House)*
                (House*House*House*House*House*House)*
                (House*House)
    Turn : StartingPosition }
//Each player has six houses and a housebag
let createHouse num = 
    {number = num; seeds= 4}
//A house record with a house number and number of seeds in each house initially
let createHouseBags num = 
    {number = num; seeds= 0}

let createPlayerHouses =
    (createHouse 1,createHouse 2,createHouse 3,createHouse 4,createHouse 5,createHouse 6),
    (createHouse 7,createHouse 8,createHouse 9,createHouse 10,createHouse 11,createHouse 12),
    (createHouseBags 13,createHouseBags 14)

let InitializeBoard = 
    {Board = createPlayerHouses; Turn = StartingPosition.South}
   

let getSeeds n board = 
   let ({Board=((h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14)); Turn = z}) = board
   match n with
    | 1 -> h1.seeds
    | 2 -> h2.seeds
    | 3 -> h3.seeds
    | 4 -> h4.seeds
    | 5 -> h5.seeds
    | 6 -> h6.seeds
    | 7 -> h7.seeds
    | 8 -> h8.seeds
    | 9 -> h9.seeds
    | 10 -> h10.seeds
    | 11 -> h11.seeds
    | 12 -> h12.seeds
    | _ -> -1
   
///let addSeed House =
   // {House with seeds = House.seeds+1}

let addSeed n board =
    let ({Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}) = board
    match n with
        |1 -> 
            let H1={h1 with seeds = h1.seeds+1} 
            {Board=(H1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}
        |2 ->
            let H2={h2 with seeds = h2.seeds+1} 
            {Board=(h1,H2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}
        |3 -> 
            let H3={h3 with seeds = h3.seeds+1} 
            {Board=(h1,h2,H3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}
        |4 ->
            let H4={h4 with seeds = h4.seeds+1} 
            {Board=(h1,h2,h3,H4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}
        |5 -> 
            let H5={h5 with seeds = h5.seeds+1} 
            {Board=(h1,h2,h3,h4,H5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}
        |6 ->
            let H6={h6 with seeds = h6.seeds+1} 
            {Board=(h1,h2,h3,h4,h5,H6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}
        |7 -> 
            let H7={h7 with seeds = h7.seeds+1} 
            {Board=(h1,h2,h3,h4,h5,h6),(H7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}
        |8 ->
            let H8={h8 with seeds = h8.seeds+1} 
            {Board=(h1,h2,h3,h4,h5,h6),(h7,H8,h9,h10,h11,h12),(h13,h14); Turn = z}
        |9 -> 
            let H9={h9 with seeds = h9.seeds+1} 
            {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,H9,h10,h11,h12),(h13,h14); Turn = z}
        |10 ->
            let H10={h10 with seeds = h10.seeds+1} 
            {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,H10,h11,h12),(h13,h14); Turn = z}
        |11 -> 
            let H11={h11 with seeds = h11.seeds+1} 
            {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,H11,h12),(h13,h14); Turn = z}
        |12 ->
            let H12={h12 with seeds = h12.seeds+1} 
            {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,H12),(h13,h14); Turn = z}
        |_ -> {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}
   
let afterCapture n board currentScore =
    let ({Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}) = board
    let captured_house = n
    match captured_house with
    |1 -> {Board=({h1 with seeds = 0},h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),currentScore; Turn = North}
    |2 -> {Board = (h1,{h2 with seeds = 0},h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),currentScore; Turn = North}
    |3 -> {Board=(h1,h2,{h3 with seeds = 0},h4,h5,h6),(h7,h8,h9,h10,h11,h12),currentScore; Turn = North}
    |4 -> {Board=(h1,h2,h3,{h4 with seeds = 0},h5,h6),(h7,h8,h9,h10,h11,h12),currentScore; Turn = North}
    |5 -> {Board=(h1,h2,h3,h4,{h5 with seeds = 0},h6),(h7,h8,h9,h10,h11,h12),currentScore; Turn = North}
    |6 -> {Board=(h1,h2,h3,h4,h5,{h6 with seeds = 0}),(h7,h8,h9,h10,h11,h12),currentScore; Turn = North}
    |7 -> {Board=(h1,h2,h3,h4,h5,h6),({h7 with seeds = 0},h8,h9,h10,h11,h12),currentScore; Turn = South}
    |8 -> {Board=(h1,h2,h3,h4,h5,h6),(h7,{h8 with seeds = 0},h9,h10,h11,h12),currentScore; Turn = South}
    |9 -> {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,{h9 with seeds = 0},h10,h11,h12),currentScore; Turn = South}
    |10 -> {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,{h10 with seeds = 0},h11,h12),currentScore; Turn = South}
    |11 -> {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,{h11 with seeds = 0},h12),currentScore; Turn = South}
    |12 -> {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,{h12 with seeds = 0}),currentScore; Turn = South}
    |_ -> board

let score_cap n board = 
    let ({Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}) = board
    let iniSeeds = getSeeds n board //initial seeds inside house n
    //let numSeeds = getSeeds ((n + iniSeeds)%12) board //number of seeds in the house where the last seed will be dropped 
    let rec MoreToSow n numSeeds board =
        let ({Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}) = board
        match numSeeds = 2 || numSeeds = 3 with
        |true -> 
                match h13.seeds,h14.seeds,numSeeds,z with
                |a,b,2,South -> 
                        let sScore = {h13 with seeds = h13.seeds+numSeeds}
                        let ({Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}) = board                      
                        MoreToSow (n - 1) (getSeeds (n-1) board) (afterCapture n board (sScore,{h14 with seeds = b}))
                |a,b,2,North -> 
                        let nScore = {h14 with seeds = h14.seeds+numSeeds}
                        let ({Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}) = board
                        MoreToSow (n - 1) (getSeeds (n-1) board) (afterCapture n board ({h13 with seeds = a},nScore))
                |a,b,3,South -> 
                       let sScore = {h13 with seeds = h13.seeds+numSeeds}
                       let ({Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}) = board
                       MoreToSow (n - 1) (getSeeds (n-1) board) (afterCapture n board (sScore,{h14 with seeds = b}))
                |a,b,3,North -> 
                        let nScore = {h14 with seeds = h14.seeds+numSeeds}
                        let ({Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}) = board
                        MoreToSow (n - 1) (getSeeds (n-1) board) (afterCapture n board ({h13 with seeds = a},nScore))
                |_ -> board 
        |false -> board 
    MoreToSow (((n + iniSeeds)%12)) (getSeeds ((n + iniSeeds)%12) board) board


let useHouse n board =
    let ({Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}) = board
    let numseeds= getSeeds n board
    match numseeds=0 with
    |true->{Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}
    |false->
            match n, z=South with 
    | 1,true -> 
        let rec honey numSeeds house current =  
            match numSeeds = 0 with
                //|true -> {Board=({h1 with seeds = 0},h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = South}
                |true -> 
                    let {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z} = score_cap 1 current
                    {Board=({h1 with seeds = 0},h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = North}
                |false ->
                    match house with
                    |1->  honey (numSeeds-1) (house+1) (addSeed 2 current) 
                    |2-> honey (numSeeds-1) (house+1) (addSeed 3 current)
                    |3-> honey (numSeeds-1) (house+1) (addSeed 4 current)
                    |4-> honey (numSeeds-1) (house+1) (addSeed 5 current)
                    |5-> honey (numSeeds-1) (house+1) (addSeed 6 current)
                    |6-> honey (numSeeds-1) (house+1) (addSeed 7 current)
                    |7-> honey (numSeeds-1) (house+1) (addSeed 8 current)
                    |8-> honey (numSeeds-1) (house+1) (addSeed 9 current)
                    |9-> honey (numSeeds-1) (house+1) (addSeed 10 current)
                    |10-> honey (numSeeds-1) (house+1) (addSeed 11 current)
                    |11-> honey (numSeeds-1) (house+1) (addSeed 12 current)
                    |12-> honey (numSeeds-1) (1) (addSeed 2 current)      
                    |_ -> board
        honey h1.seeds 1 board
    | 2,true ->
        let rec honey numSeeds house current =  
            match numSeeds = 0 with
                |true -> 
                    let {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z} = score_cap 2 current
                    {Board=(h1,{h2 with seeds = 0},h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = North}
                |false ->
                    match house with
                    |1->  honey (numSeeds-1) (house+1) (addSeed 3 current) 
                    |2-> honey (numSeeds-1) (house+1) (addSeed 3 current)
                    |3-> honey (numSeeds-1) (house+1) (addSeed 4 current)
                    |4-> honey (numSeeds-1) (house+1) (addSeed 5 current)
                    |5-> honey (numSeeds-1) (house+1) (addSeed 6 current)
                    |6-> honey (numSeeds-1) (house+1) (addSeed 7 current)
                    |7-> honey (numSeeds-1) (house+1) (addSeed 8 current)
                    |8-> honey (numSeeds-1) (house+1) (addSeed 9 current)
                    |9-> honey (numSeeds-1) (house+1) (addSeed 10 current)
                    |10-> honey (numSeeds-1) (house+1) (addSeed 11 current)
                    |11-> honey (numSeeds-1) (house+1) (addSeed 12 current)
                    |12-> honey (numSeeds-1) (1) (addSeed 1 current)   
                    |_ -> board
        honey h2.seeds 2 board
    | 3,true ->  
        let rec honey numSeeds house current =  
            match numSeeds = 0 with
                |true -> 
                    let {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z} = score_cap 3 current
                    {Board=(h1,h2,{h3 with seeds = 0},h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = North}
                |false ->
                    match house with
                    |1->  honey (numSeeds-1) (house+1) (addSeed 2 current) 
                    |2-> honey (numSeeds-1) (house+1) (addSeed 4 current)
                    |3-> honey (numSeeds-1) (house+1) (addSeed 4 current)
                    |4-> honey (numSeeds-1) (house+1) (addSeed 5 current)
                    |5-> honey (numSeeds-1) (house+1) (addSeed 6 current)
                    |6-> honey (numSeeds-1) (house+1) (addSeed 7 current)
                    |7-> honey (numSeeds-1) (house+1) (addSeed 8 current)
                    |8-> honey (numSeeds-1) (house+1) (addSeed 9 current)
                    |9-> honey (numSeeds-1) (house+1) (addSeed 10 current)
                    |10-> honey (numSeeds-1) (house+1) (addSeed 11 current)
                    |11-> honey (numSeeds-1) (house+1) (addSeed 12 current)
                    |12-> honey (numSeeds-1) (1) (addSeed 1 current)  
                    |_ -> board
        honey h3.seeds 3 board
    | 4,true -> 
        let rec honey numSeeds house current =  
            match numSeeds = 0 with
                |true -> 
                    let {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z} = score_cap 4 current
                    {Board=(h1,h2,h3,{h4 with seeds = 0},h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = North}
                |false ->
                    match house with
                    |1->  honey (numSeeds-1) (house+1) (addSeed 2 current) 
                    |2-> honey (numSeeds-1) (house+1) (addSeed 3 current)
                    |3-> honey (numSeeds-1) (house+1) (addSeed 5 current)
                    |4-> honey (numSeeds-1) (house+1) (addSeed 5 current)
                    |5-> honey (numSeeds-1) (house+1) (addSeed 6 current)
                    |6-> honey (numSeeds-1) (house+1) (addSeed 7 current)
                    |7-> honey (numSeeds-1) (house+1) (addSeed 8 current)
                    |8-> honey (numSeeds-1) (house+1) (addSeed 9 current)
                    |9-> honey (numSeeds-1) (house+1) (addSeed 10 current)
                    |10-> honey (numSeeds-1) (house+1) (addSeed 11 current)
                    |11-> honey (numSeeds-1) (house+1) (addSeed 12 current)
                    |12-> honey (numSeeds-1) (1) (addSeed 1 current)
                    |_ -> board
        honey h4.seeds 4 board
    | 5,true -> 
        let rec honey numSeeds house current =  
            match numSeeds = 0 with
                |true -> 
                    let {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z} = score_cap 5 current
                    {Board=(h1,h2,h3,h4,{h5 with seeds = 0},h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = North}
                |false ->
                    match house with
                    |1->  honey (numSeeds-1) (house+1) (addSeed 2 current) 
                    |2-> honey (numSeeds-1) (house+1) (addSeed 3 current)
                    |3-> honey (numSeeds-1) (house+1) (addSeed 4 current)
                    |4-> honey (numSeeds-1) (house+1) (addSeed 6 current)
                    |5-> honey (numSeeds-1) (house+1) (addSeed 6 current)
                    |6-> honey (numSeeds-1) (house+1) (addSeed 7 current)
                    |7-> honey (numSeeds-1) (house+1) (addSeed 8 current)
                    |8-> honey (numSeeds-1) (house+1) (addSeed 9 current)
                    |9-> honey (numSeeds-1) (house+1) (addSeed 10 current)
                    |10-> honey (numSeeds-1) (house+1) (addSeed 11 current)
                    |11-> honey (numSeeds-1) (house+1) (addSeed 12 current)
                    |12-> honey (numSeeds-1) (1) (addSeed 1 current)  
                    |_ -> board
        honey h5.seeds 5 board
    | 6,true ->
        let rec honey numSeeds house current =  
            match numSeeds = 0 with
                |true -> 
                    let {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z} = score_cap 6 current
                    {Board=(h1,h2,h3,h4,h5,{h6 with seeds = 0}),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = North}
                |false ->
                    match house with
                    |1->  honey (numSeeds-1) (house+1) (addSeed 2 current) 
                    |2-> honey (numSeeds-1) (house+1) (addSeed 3 current)
                    |3-> honey (numSeeds-1) (house+1) (addSeed 4 current)
                    |4-> honey (numSeeds-1) (house+1) (addSeed 5 current)
                    |5-> honey (numSeeds-1) (house+1) (addSeed 7 current)
                    |6-> honey (numSeeds-1) (house+1) (addSeed 7 current)
                    |7-> honey (numSeeds-1) (house+1) (addSeed 8 current)
                    |8-> honey (numSeeds-1) (house+1) (addSeed 9 current)
                    |9-> honey (numSeeds-1) (house+1) (addSeed 10 current)
                    |10-> honey (numSeeds-1) (house+1) (addSeed 11 current)
                    |11-> honey (numSeeds-1) (house+1) (addSeed 12 current)
                    |12-> honey (numSeeds-1) (1) (addSeed 1 current)    
                    |_ -> board
        honey h6.seeds 6 board
    | 7,false ->
        let rec honey numSeeds house current =  
            match numSeeds = 0 with
                |true -> 
                    let {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z} = score_cap 7 current
                    {Board=(h1,h2,h3,h4,h5,h6),({h7 with seeds=0},h8,h9,h10,h11,h12),(h13,h14); Turn = South}                 
                |false ->
                    match house with
                    |1->  honey (numSeeds-1) (house+1) (addSeed 2 current) 
                    |2-> honey (numSeeds-1) (house+1) (addSeed 3 current)
                    |3-> honey (numSeeds-1) (house+1) (addSeed 4 current)
                    |4-> honey (numSeeds-1) (house+1) (addSeed 5 current)
                    |5-> honey (numSeeds-1) (house+1) (addSeed 6 current)
                    |6-> honey (numSeeds-1) (house+1) (addSeed 8 current)
                    |7-> honey (numSeeds-1) (house+1) (addSeed 8 current)
                    |8-> honey (numSeeds-1) (house+1) (addSeed 9 current)
                    |9-> honey (numSeeds-1) (house+1) (addSeed 10 current)
                    |10-> honey (numSeeds-1) (house+1) (addSeed 11 current)
                    |11-> honey (numSeeds-1) (house+1) (addSeed 12 current)
                    |12-> honey (numSeeds-1) (1) (addSeed 1 current)    
                    |_ -> board
        honey h7.seeds 7 board
    | 8,false -> 
        let rec honey numSeeds house current =  
            match numSeeds = 0 with
                |true -> 
                    let {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z} = score_cap 8 current
                    {Board=(h1,h2,h3,h4,h5,h6),(h7,{h8 with seeds=0},h9,h10,h11,h12),(h13,h14); Turn = South} 
                |false ->
                    match house with
                    |1->  honey (numSeeds-1) (house+1) (addSeed 2 current) 
                    |2-> honey (numSeeds-1) (house+1) (addSeed 3 current)
                    |3-> honey (numSeeds-1) (house+1) (addSeed 4 current)
                    |4-> honey (numSeeds-1) (house+1) (addSeed 5 current)
                    |5-> honey (numSeeds-1) (house+1) (addSeed 6 current)
                    |6-> honey (numSeeds-1) (house+1) (addSeed 7 current)
                    |7-> honey (numSeeds-1) (house+1) (addSeed 9 current)
                    |8-> honey (numSeeds-1) (house+1) (addSeed 9 current)
                    |9-> honey (numSeeds-1) (house+1) (addSeed 10 current)
                    |10-> honey (numSeeds-1) (house+1) (addSeed 11 current)
                    |11-> honey (numSeeds-1) (house+1) (addSeed 12 current)
                    |12-> honey (numSeeds-1) (1) (addSeed 1 current)  
                    |_ -> board
        honey h8.seeds 8 board
    | 9,false ->
        let rec honey numSeeds house current =  
            match numSeeds = 0 with
                |true -> 
                    let {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z} = score_cap 9 current
                    {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,{h9 with seeds=0},h10,h11,h12),(h13,h14); Turn = South}
                |false ->
                    match house with
                    |1->  honey (numSeeds-1) (house+1) (addSeed 2 current) 
                    |2-> honey (numSeeds-1) (house+1) (addSeed 3 current)
                    |3-> honey (numSeeds-1) (house+1) (addSeed 4 current)
                    |4-> honey (numSeeds-1) (house+1) (addSeed 5 current)
                    |5-> honey (numSeeds-1) (house+1) (addSeed 6 current)
                    |6-> honey (numSeeds-1) (house+1) (addSeed 7 current)
                    |7-> honey (numSeeds-1) (house+1) (addSeed 8 current)
                    |8-> honey (numSeeds-1) (house+1) (addSeed 10 current)
                    |9-> honey (numSeeds-1) (house+1) (addSeed 10 current)
                    |10-> honey (numSeeds-1) (house+1) (addSeed 11 current)
                    |11-> honey (numSeeds-1) (house+1) (addSeed 12 current)
                    |12-> honey (numSeeds-1) (1) (addSeed 1 current)  
                    |_ -> board
        honey h9.seeds 9 board
    | 10,false -> 
        let rec honey numSeeds house current =  
            match numSeeds = 0 with
                |true -> 
                    let {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z} = score_cap 10 current
                    {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,{h10 with seeds=0},h11,h12),(h13,h14); Turn = South}
                |false ->
                    match house with
                    |1->  honey (numSeeds-1) (house+1) (addSeed 2 current) 
                    |2-> honey (numSeeds-1) (house+1) (addSeed 3 current)
                    |3-> honey (numSeeds-1) (house+1) (addSeed 4 current)
                    |4-> honey (numSeeds-1) (house+1) (addSeed 5 current)
                    |5-> honey (numSeeds-1) (house+1) (addSeed 6 current)
                    |6-> honey (numSeeds-1) (house+1) (addSeed 7 current)
                    |7-> honey (numSeeds-1) (house+1) (addSeed 8 current)
                    |8-> honey (numSeeds-1) (house+1) (addSeed 9 current)
                    |9-> honey (numSeeds-1) (house+1) (addSeed 11 current)
                    |10-> honey (numSeeds-1) (house+1) (addSeed 11 current)
                    |11-> honey (numSeeds-1) (house+1) (addSeed 12 current)
                    |12-> honey (numSeeds-1) (1) (addSeed 1 current) 
                    |_ -> board                    
        honey h10.seeds 10 board
    | 11,false ->
        let rec honey numSeeds house current =  
            match numSeeds = 0 with
                |true -> 
                    let {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z} = score_cap 11 current
                    {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,{h11 with seeds=0},h12),(h13,h14); Turn = South}
                |false ->
                    match house with
                    |1->  honey (numSeeds-1) (house+1) (addSeed 2 current) 
                    |2-> honey (numSeeds-1) (house+1) (addSeed 3 current)
                    |3-> honey (numSeeds-1) (house+1) (addSeed 4 current)
                    |4-> honey (numSeeds-1) (house+1) (addSeed 5 current)
                    |5-> honey (numSeeds-1) (house+1) (addSeed 6 current)
                    |6-> honey (numSeeds-1) (house+1) (addSeed 7 current)
                    |7-> honey (numSeeds-1) (house+1) (addSeed 8 current)
                    |8-> honey (numSeeds-1) (house+1) (addSeed 9 current)
                    |9-> honey (numSeeds-1) (house+1) (addSeed 10 current)
                    |10-> honey (numSeeds-1) (house+1) (addSeed 12 current)
                    |11-> honey (numSeeds-1) (house+1) (addSeed 12 current)
                    |12-> honey (numSeeds-1) (1) (addSeed 1 current)         
                    |_ -> board
        honey h11.seeds 11 board
    | 12,false ->
        let rec honey numSeeds house current =  
            match numSeeds = 0 with
                |true -> 
                    let {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z} = score_cap 12 current
                    {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,{h12 with seeds=0}),(h13,h14); Turn = South}
                |false ->
                    match house with
                    |1->  honey (numSeeds-1) (house+1) (addSeed 2 current) 
                    |2-> honey (numSeeds-1) (house+1) (addSeed 3 current)
                    |3-> honey (numSeeds-1) (house+1) (addSeed 4 current)
                    |4-> honey (numSeeds-1) (house+1) (addSeed 5 current)
                    |5-> honey (numSeeds-1) (house+1) (addSeed 6 current)
                    |6-> honey (numSeeds-1) (house+1) (addSeed 7 current)
                    |7-> honey (numSeeds-1) (house+1) (addSeed 8 current)
                    |8-> honey (numSeeds-1) (house+1) (addSeed 9 current)
                    |9-> honey (numSeeds-1) (house+1) (addSeed 10 current)
                    |10-> honey (numSeeds-1) (house+1) (addSeed 11 current)
                    |11-> honey (numSeeds-1) (house+1) (addSeed 1 current)
                    |12-> honey (numSeeds-1) (1) (addSeed 1 current)        
                    |_ -> board
        honey h12.seeds 12 board
    | _ -> 
        {Board=(h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14); Turn = z}


let start position =
 match position with 
 |South -> {InitializeBoard with Turn = South}
 |North -> {InitializeBoard with Turn = North}


let score board =
     let ({Board=((h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14)); Turn = z}) = board
     (h13.seeds,h14.seeds)

let gameState board = 
                    let ({Board=((h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14)); Turn = z}) = board
                    match board.Turn,(h13.seeds>24,h14.seeds>24,h13.seeds=24,h14.seeds=24) with
                    |_,(true,false,_,_) -> "South won"
                    |_,(false,true,_,_) -> "North won"
                    |_,(_,_,true,true) -> "Game ended in a draw"
                    |North,_ -> "North's turn"
                    |South,_ -> "South's turn"                   
                   
            

[<EntryPoint>]
let main _ =
    let n = getSeeds 1 InitializeBoard; //check if getseeds works
    
    printfn "House 1 has %A" n

    0 // return an integer exit code
