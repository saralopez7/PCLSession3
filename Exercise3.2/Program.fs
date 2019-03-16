
let rec pmLength ls =
    match ls with
    | [] -> 0
    | [ _ ] -> 1
    | [ _; _ ] -> 2
    | [ _; _; _ ] -> 3
    | hd :: tail -> 1 + pmLength tail
  
pmLength [1 ; 2; 3]

//#region Region Name
let pclSum (ls: int list) = List.sum ls

pclSum [2 ; 3; 5; 8]
 //#endregion
let rec SumPatternMatching (ls: int list) = 
    match ls with
    | [] -> 0
    | (hd :: restOfTheList) -> hd + SumPatternMatching restOfTheList

let rec SumPatternMatching2 (ls: int list) = 
    match ls with
    | [] -> 0
    | _ -> ls.Head + SumPatternMatching ls.Tail
