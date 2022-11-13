open Sets

type symbol =
  | T of string (* terminal symbol *)
  | N of string (* nonterminal symbol *)
  | Epsilon (* empty string *)
  | End (* end marker *)

type production = (symbol * symbol list) list
type cfg = symbol list * symbol list * symbol * production

let cfg1 =
  ( [ N "E"; N "E'"; N "T"; N "T'"; N "F" ],
    [ T "+"; T "*"; T "("; T ")"; T "id"; Epsilon ],
    N "E",
    [
      (N "E", [ N "T"; N "E'" ]);
      (N "E'", [ T "+"; N "T"; N "E'" ]);
      (N "E'", [ Epsilon ]);
      (N "T", [ N "F"; N "T'" ]);
      (N "T'", [ T "*"; N "F"; N "T'" ]);
      (N "T'", [ Epsilon ]);
      (N "F", [ T "("; N "E"; T ")" ]);
      (N "F", [ T "id" ]);
    ] )
let cfg2 = (
  [N "X"; N "Y"; N "Z"],
  [T "a"; T "c"; T "d"],
  N "X",
  [
  (N "X", [N "X" ; T "c" ; T "+"]);
  (N "X", [T "a"]);
  (N "X", [T "c"]);
  (N "Y", [T "c"]);
  (N "Y", [Epsilon]);
  (N "Z", [N "Y" ;]);
  (N "Z", [T "d"; N "Y"]);
  (N "Z", [T "X"; N "Y"; T "Z"])
  ])

let rec first_helper (productions : production) (nonterminal : symbol) :
    symbol list =
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

let rec exit_epsilon (cfg : cfg) (nont : symbol) : bool =
  let first_list = first cfg nont in
  List.mem Epsilon first_list

let rec follow (cfg : cfg) (nont : symbol) : symbol list =
  let _, _, _, prods = cfg in
  let rec follow_helper (productions : production) (nonterminal : symbol)
      (cfg : cfg) : symbol list =
    let rec lookahead (symbol_list : symbol list) (nonterminal : symbol)
        (tail : production) (cfg : cfg) (prod : production) (header : symbol) :
        symbol list =
      if List.mem nonterminal symbol_list then
        match symbol_list with
        | h :: t ->
            if nonterminal = h then
              match t with
              | next :: _ -> (
                  match next with
                  | T _ -> next :: lookahead t nonterminal tail cfg prod header
                  | N _ ->
                      if exit_epsilon cfg next then
                        union (follow cfg header) (first cfg next)
                      else first cfg next
                  | _ -> [])
              | [] -> follow cfg header
            else lookahead t nonterminal tail cfg prod header
        | [] -> []
      else follow_helper tail nonterminal cfg
    in
    match productions with
    | head :: tail ->
        let nont_in_prod, symbol_list = head in
        if nont_in_prod = nonterminal then
          End
          :: lookahead symbol_list nonterminal tail cfg productions nont_in_prod
        else lookahead symbol_list nonterminal tail cfg productions nont_in_prod
    | [] -> []
  in
  follow_helper prods nont cfg

let nepsilon x = x != Epsilon

let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

let find_select (cfg : cfg) (prod : symbol * symbol list) =
  let first_alpha =
    match snd prod with
    | [] -> []
    | h :: t -> (
        match h with
        | T _ -> h :: []
        | N _ -> first cfg h
        | Epsilon -> h :: []
        | _ -> [])
  in
  let rec exit_epsilon (first_list : symbol list) : bool =
    match first_list with
    | [] -> false
    | h :: t -> (
        match h with Epsilon -> true | T _ -> exit_epsilon t | _ -> false)
  in
  if exit_epsilon first_alpha = false then first_alpha
  else
    let first_filter = filter nepsilon first_alpha in
    let follow_alpha = follow cfg (fst prod) in
    union first_filter (filter nepsilon follow_alpha)

let select (cfg : cfg) (prod : symbol * symbol list) : symbol list =
  find_select cfg prod

let rec factor_helper (productions : production) (nonterminal : symbol) :
    symbol list =
  match productions with
  | head :: tail ->
      let nont_head, symbol_list = head in
      if nont_head = nonterminal then
        match symbol_list with
        | h :: _ -> ( match h with _ -> h :: factor_helper tail nonterminal)
        | [] -> []
      else factor_helper tail nonterminal
  | [] -> []

(* let rec elem_find a =
    match a with
    | h :: t -> (List.mem h t) || elem_find t
    | [] -> false *)

let left_factor (cfg : cfg) (nonterminal : symbol) : bool =
  let _, _, _, prods = cfg in
  let symbol_list = factor_helper prods nonterminal in
  elem_find symbol_list

let rec same_element (symbol : symbol) (select : symbol list) : bool =
  match select with 
  | [] -> false
  | h :: t -> (
    if symbol = h then true
    else same_element symbol t
  )
let rec exit_intersection (select1 : symbol list) (select2 : symbol list) : bool =
  match select1 with 
  | [] -> false
  | h1 :: t1 ->(
    match select2 with 
    | [] -> false
    | h2 :: _ -> if same_element h1 select2 then true
    else exit_intersection t1 select2
  )


let is_LL1 (cfg : cfg) : bool =
  let _, _, _, prods = cfg in 
  let rec find_same_element (h: symbol * symbol list) (t : (symbol*symbol list)list) : symbol*symbol list =
    match t with 
    | [] -> (N "",[])
    | head :: tail -> (
      if fst h = fst head then head
      else find_same_element h tail
    ) in
  let rec is_LL1_helper (prod : production) : bool = 
  match prod with 
  | [] -> true
  | h :: t -> (
    let left_same = find_same_element h t in 
    if (exit_intersection (select cfg h) (select cfg left_same)) then false
    else is_LL1_helper  t
  ) in
  is_LL1_helper prods

(* let _, _, _, prods = cfg  *)

let rec factor_check (prods : production)(nonterminal : symbol) : symbol list list = 
  match prods with
  | head :: tail ->
    let cont_head , symbol_list = head in 
    if cont_head = nonterminal then
      symbol_list :: factor_check tail nonterminal
    else
      factor_check tail nonterminal
  | [] -> []

let rec find_factor (list : symbol list list) : symbol list =
  match list with
  | head :: tail ->(
    match head with
    | h :: t -> 
       h :: find_factor tail
    | [] -> [])
  | [] -> []

let dirct_left_recursion (cfg : cfg)(nonterminal : symbol) : bool =
  let  _, _, _, prods = cfg in 
  let lst = factor_check prods nonterminal in 
  List.mem nonterminal (find_factor lst)

let rec get_rhs (lst : symbol list list)(nonterminal : symbol) : symbol list list =
  match lst with
  | head :: tail -> (
    if List.mem nonterminal head then
      match head with
      | h :: t -> 
       t :: get_rhs tail nonterminal
      | [] -> []
    else
      get_rhs tail nonterminal
  )
  | [] -> []

let rec get_lhs (lst : symbol list list)(nonterminal : symbol) : symbol list list =
  match lst with
  | head :: tail -> (
    if List.mem nonterminal head then
      get_lhs tail nonterminal
    else
      head :: get_lhs tail nonterminal
  )
  | [] -> []

let rec symbol_to_string (symbol :symbol) : string =
  match symbol with
  | T str -> str
  | N str -> str
  | Epsilon -> "e"
  | End -> "#"

let rec drop (symbol : symbol)(productions : production)(new_prods : production) : production =
  match productions with
  | head :: tail -> 
    let nont_head , symbol_list = head in 
    if nont_head = symbol then
     let new_prods = remove head productions in 
      drop symbol new_prods new_prods
    else
     drop symbol tail new_prods
  | [] -> new_prods

let get_lhs_rhs(cfg : cfg)(nonterminal : symbol) : (symbol * symbol list) list =
  let  _, _, _, prods = cfg  in
  let str = symbol_to_string nonterminal in 
  let new_nont = (N (str ^ "'")) in  
  let lst = factor_check prods nonterminal in 
  let lst_rhs = get_rhs lst nonterminal in
  let lst_lhs = get_lhs lst nonterminal in
  let rhs_with_new = cat ([new_nont]) (lst_rhs)  in 
  let lhs_with_new =  cat ([new_nont]) (lst_lhs) in 
  let new_prods = drop nonterminal prods prods in 
  union new_prods (union (illness_cat (new_nont) (rhs_with_new) ) (illness_cat (nonterminal) (lhs_with_new)))