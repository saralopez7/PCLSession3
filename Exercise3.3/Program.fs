let takeSome n (ls : 'T list) = [ for i = 0 to n - 1 do yield ls.[i] ]

let reducedList = takeSome 2 ['a'; 'b'; 'c'; 'd']
let reducedIntegerList = takeSome 3 [1; 45; 89; 23]

let stringToList (stringToConvert: string) = [for c in stringToConvert do yield c]

let rec takeSomePatternMatching n (ls: 'T list) =
    match ls with
    | [] -> []
    | (hd :: tail) -> match n with 
                      | 0 -> []
                      | _ -> hd :: takeSomePatternMatching (n - 1) tail

takeSomePatternMatching 3 ['a'; 'b'; 'c'; 'd'];;