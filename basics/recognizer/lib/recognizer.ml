type word = char list

let rec lang1 l = 
  match l with 
  [] -> false 
  |['1'] -> true
  |['0'] -> true
  |'0' :: tl -> lang1 tl
  |'1' :: tl -> lang1 tl
  |_ -> false;;



let rec lang2 l =
  match l with 
  [] -> true 
  |['0'] -> true 
  |['1'] -> true
  |'0' :: tl -> lang2 tl 
  |'1' :: tl -> lang2 tl

let rec lang3 l = 
  match l with 
  [] -> true 
  |['0'] -> true 
  |['1'] -> false
  |'0' :: '0' :: '1' :: tl -> lang3 tl 
  |'1' :: '0' :: '1' :: tl -> lang3 tl
  |_ -> false;;

let lang4 _ = failwith ""

let lang5 _ = failwith ""
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
