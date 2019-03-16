
let mutable vowels = [|'a'; 'e'; 'i'; 'o'; 'u'|]

let vowelToUpper x = 
    match x with
    | 'a' -> 'A'
    | 'e' -> 'E'
    | 'i' -> 'I'
    | 'o' -> 'O'
    | 'u' -> 'U'
    | _ -> x
    
for i = 0 to (vowels.Length - 1) do
    vowels.[i] <- vowelToUpper vowels.[i]
    printfn "%c" vowels.[i]

let rec vowelToUpperRecursive recursiveString =
    match String.length recursiveString with 
    | 1 -> string (vowelToUpper (recursiveString.Chars 0))
    | _ -> string (vowelToUpperRecursive (recursiveString.Substring(0, recursiveString.Length - 1))) + 
           string (vowelToUpper (recursiveString.Chars (recursiveString.Length - 1))) 

vowelToUpperRecursive "aBeIoU"

