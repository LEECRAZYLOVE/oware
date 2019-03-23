module Oware

type StartingPosition =
    | South
    | North

type House =
   {
        number : int
        seeds : int
   }

type GameBoard = 
    |  Board of (House*House*House*House*House*House)*
                (House*House*House*House*House*House)*
                (House*House)
    |Turn of StartingPosition
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
    Board createPlayerHouses
   

let getSeeds n board = 
   let (Board((h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14))) = board
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
   
let useHouse n board =
    let (Board((h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14))) = board
    match n with 
    | 1 -> 
        let a={h1 with seeds=0}
        let b={h2 with seeds=5}
        let c={h3 with seeds=5}
        let d={h4 with seeds=5}
        let e={h5 with seeds=5}
        Board((a,b,c,d,e,h6),(h7,h8,h9,h10,h11,h12),(h13,h14))
    | 2 ->
        let a={h2 with seeds=0}
        let b={h3 with seeds=5}
        let c={h4 with seeds=5}
        let d={h5 with seeds=5}
        let e={h6 with seeds=5}
        Board((h1,a,b,c,d,e),(h7,h8,h9,h10,h11,h12),(h13,h14))
    | 3 ->  
        let a={h3 with seeds=0}
        let b={h4 with seeds=5}
        let c={h5 with seeds=5}
        let d={h6 with seeds=5}
        let e={h7 with seeds=5}
        Board((h1,h2,a,b,c,d),(e,h8,h9,h10,h11,h12),(h13,h14))
    | 4 -> 
        let a={h4 with seeds=0}
        let b={h5 with seeds=5}
        let c={h6 with seeds=5}
        let d={h7 with seeds=5}
        let e={h8 with seeds=5}
        Board((h1,h2,h3,a,b,c),(d,e,h9,h10,h11,h12),(h13,h14))
    | 5 -> 
        let a={h5 with seeds=0}
        let b={h6 with seeds=5}
        let c={h7 with seeds=5}
        let d={h8 with seeds=5}
        let e={h9 with seeds=5}
        Board((h1,h2,h3,h4,a,b),(c,d,e,h10,h11,h12),(h13,h14))
    | 6 ->
        let a={h6 with seeds=0}
        let b={h7 with seeds=5}
        let c={h8 with seeds=5}
        let d={h9 with seeds=5}
        let e={h10 with seeds=5}
        Board((h1,h2,h3,h4,h5,a),(b,c,d,e,h11,h12),(h13,h14))
    | 7 ->
        let a={h7 with seeds=0}
        let b={h8 with seeds=5}
        let c={h9 with seeds=5}
        let d={h10 with seeds=5}
        let e={h11 with seeds=5}
        Board((h1,h2,h3,h4,h5,h6),(a,b,c,d,e,h12),(h13,h14))
    | 8 -> 
        let a={h8 with seeds=0}
        let b={h9 with seeds=5}
        let c={h10 with seeds=5}
        let d={h11 with seeds=5}
        let e={h12 with seeds=5}
        Board((h1,h2,h3,h4,h5,h6),(h7,a,b,c,d,e),(h13,h14))
    | 9 ->
        let a={h9 with seeds=0}
        let b={h10 with seeds=5}
        let c={h11 with seeds=5}
        let d={h12 with seeds=5}
        let e={h1 with seeds=5}
        Board((e,h2,h3,h4,h5,h6),(h7,h8,a,b,c,d),(h13,h14))
    | 10 -> 
        let a={h10 with seeds=0}
        let b={h11 with seeds=5}
        let c={h12 with seeds=5}
        let d={h1 with seeds=5}
        let e={h2 with seeds=5}
        Board((d,e,h3,h4,h5,h6),(h7,h8,h9,a,b,c),(h13,h14))
    | 11 ->
        let a={h11 with seeds=0}
        let b={h12 with seeds=5}
        let c={h1 with seeds=5}
        let d={h2 with seeds=5}
        let e={h3 with seeds=5}
        Board((c,d,e,h4,h5,h6),(h7,h8,h9,h10,a,b),(h13,h14))
    | 12 ->
        let a={h12 with seeds=0}
        let b={h1 with seeds=5}
        let c={h2 with seeds=5}
        let d={h3 with seeds=5}
        let e={h4 with seeds=5}
        Board((b,c,d,e,h5,h6),(h7,h8,h9,h10,h11,a),(h13,h14))
    | _ -> 
        let a={h12 with seeds= -1}
        Board((a,a,a,a,a,a),(a,a,a,a,a,a),(a,a))


let start position =
 match position with 
 |South -> InitializeBoard
 |North -> InitializeBoard


let score board =
     let (Board((h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12),(h13,h14))) = board
     (h13,h14)

let gameState board = failwith "Not implemented"

[<EntryPoint>]
let main _ =
    let n = getSeeds 1 InitializeBoard; //check if getseeds works
    
    printfn "House 1 has %A" n

    0 // return an integer exit code
