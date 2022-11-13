open Sets

type symbol =
  | T of string  (* terminal symbol *)
  | N of string  (* nonterminal symbol *)
  | Epsilon      (* empty string *)
  | End          (* end marker *)

type production = (symbol * symbol list) list
type cfg = symbol list * symbol list * symbol * production

(* let rec elem x a =
  match a with
  | h::t -> (h = x) || (elem x t)
  | [] -> false

let rec insert x a =
if not (elem x a) then x::a else a
  
let rec union a b =
match a with
| h::t -> insert h (union t b)
| [] ->
  (match b with
    | h::t -> insert h (union [] t)
    | [] -> []) *)

let cfg1 = (
    [N "E"; N "E'"; N "T"; N "T'"; N "F"],
    [T "+"; T "*"; T "("; T ")"; T "id";Epsilon],
    N "E",
    [
    (N "E", [N "T"; N "E'"]);
    (N "E'", [T "+"; N "T"; N "E'"]);
    (N "E'", [Epsilon]);
    (N "T", [N "F"; N "T'"]);
    (N "T'", [T "*"; N "F"; N "T'"]);
    (N "T'", [Epsilon]);
    (N "F", [T "("; N "E"; T ")"]);
    (N "F", [T"id"])
    ])
    let rec first_helper (productions : production) (nonterminal : symbol) : symbol list =
      match productions with
      | head :: tail ->
          let nont_in_prod, symbol_list = head in
          if nont_in_prod = nonterminal then
            match symbol_list with
            | [] -> []
            | h :: _ -> (
                match h with
                | T _ -> h :: first_helper tail nonterminal
                | N _ -> first_helper tail h
                | Epsilon -> Epsilon :: first_helper tail nonterminal
                | _ -> [])
          else first_helper tail nonterminal
      | [] -> []
    let first (cfg : cfg) (nont : symbol) : symbol list =
      let _, _, _, prods = cfg in
      first_helper prods nont


      let rec exit_epsilon (cfg : cfg)(nont : symbol) : bool = 
        let first_list = first cfg nont in
        List.mem (Epsilon)(first_list)
       
       let rec follow (cfg : cfg)(nont : symbol) : symbol list =
         let _, _, _, prods = cfg in 
         let rec follow_helper (productions : production)(nonterminal :symbol)(cfg : cfg) : symbol list =
           let rec lookahead (symbol_list : symbol list) (nonterminal : symbol)(tail : production)(cfg : cfg)(prod : production)(header : symbol) : symbol list =
             if List.mem (nonterminal)(symbol_list) then
               match symbol_list with
               | h :: t ->
                 if nonterminal = h then
                   match t with
                   | next :: _ -> (
                     match next with
                     | T _ -> next :: lookahead t nonterminal tail cfg prod header
                     | N _ -> if exit_epsilon (cfg)(next) then union (follow cfg header) (first cfg next) else first cfg next
                     | _ -> []
                   )
                   | [] -> follow cfg header
                 else
                   lookahead t nonterminal tail cfg prod header
               | [] -> []
             else
               follow_helper tail nonterminal cfg
           in
           match productions with
           | head :: tail ->
             let  nont_in_prod , symbol_list = head in
             if nont_in_prod = nonterminal then
               End :: lookahead symbol_list nonterminal tail cfg productions nont_in_prod
             else lookahead symbol_list nonterminal tail cfg productions nont_in_prod
           | [] -> []
           in
         follow_helper prods nont cfg

let nepsilon x =
   x != Epsilon  

let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t


let find_select (cfg : cfg) (prod : symbol * symbol list) =
    let first_alpha =
      match snd prod with 
      | []->[]
      | h :: t -> (
        match h with 
        | T _ -> h :: []
        | N _ -> first cfg h
        | Epsilon -> h :: [] 
        | _ -> []
      )in
let rec exit_epsilon (first_list : symbol list) : bool = 
  match first_list with 
  | [] -> false
  | h :: t -> (
    match h with 
    | Epsilon -> true
    | T _ -> exit_epsilon t
    | _ -> false;
  )  in
if exit_epsilon first_alpha = false then first_alpha
else let first_filter = filter nepsilon first_alpha in 
  let follow_alpha = follow cfg (fst prod) in 
  union first_filter  (filter nepsilon follow_alpha)
  

let select (cfg : cfg) (prod : symbol * symbol list) : symbol list =
  find_select cfg prod


