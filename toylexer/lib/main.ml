open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(*   let rec aux x l = 
    match l with 
    []-> 0
    |hd::tl-> 
      if hd = x then 1 + aux x tl 
      else aux x tl;;

(* (frequency : int -> 'a list -> ('a * int) list *) 
let rec frequency n l =
  match l with 
  []->[]
  |hd::tl-> (hd, aux hd l) :: frequency n tl  *)

  let frequency n l =
    (* Accumulate counts in an associative list *)
    let counts = List.fold_left (fun acc x ->
        match List.assoc_opt x acc with
        | Some count -> (x, count + 1) :: List.remove_assoc x acc
        | None -> (x, 1) :: acc
      ) [] l
    in
    (* Sort by frequency in descending order *)
    let sorted_counts = List.sort (fun (_, a) (_, b) -> compare b a) counts in
    (* Take the top n elements *)
    List.filteri (fun i _ -> i < n) sorted_counts
  ;;