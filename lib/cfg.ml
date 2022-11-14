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
  (N "X", [N "Y"]);
  (N "X", [T "a"]);
  (N "Y", [T "c"]);
  (N "Y", []);
  (N "Z", [T "d"]);
  (N "Z", [T "d"]);
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
let rec find_same_left (h: symbol * symbol list) (t : (symbol*symbol list)list) : symbol*symbol list =
  let compare (s1 : symbol ) (s2 : symbol) : bool=
    if s1 = s2 then true
    else false in
  match t with 
  | [] -> (N "",[])
  | head :: tail -> (
    if fst h = fst head && not(List.equal (compare) (snd h) (snd head)) then head
    else find_same_left h tail
  ) 

let is_LL1 (cfg : cfg) : bool =
  let _, _, _, prods = cfg in 
  let rec is_LL1_helper (prod : production) : bool = 
  match prod with 
  | [] -> true
  | h :: t -> (
    let left_same = find_same_left h t in 
    if (exit_intersection (select cfg h) (select cfg left_same)) then false
    else is_LL1_helper  t
  ) in
  is_LL1_helper prods



let rec find_next_nonterminal (prod : symbol *symbol list) : symbol list = 
  match snd prod with 
  | [] -> []
  | h :: t -> (
    match h with 
    | T _ -> find_next_nonterminal ((fst prod) , t)
    | N _ -> h :: find_next_nonterminal ((fst prod) , t)
    | _ -> find_next_nonterminal ((fst prod) , t)
  )

let rec get_next_prod (prods : production) (symbol_nonterminal :symbol) : symbol *symbol list=
  match prods with 
  | [] -> (N "",[])
  | h :: t -> (
    if fst h = symbol_nonterminal then h
    else get_next_prod t symbol_nonterminal
  )
let cut = function
| [] -> [] 
| _::t -> t
let get_head (lst : symbol * symbol list) : symbol = 
  match snd lst with
  | [] ->T ""
  | h :: t ->h

  let rec sub_parse (cfg : cfg )(symbol_list : symbol list) (prods : production) (prod : symbol *symbol list) (symbol_nonterminal :symbol) : bool = 
  let _, nonts, _, _ = cfg in
  let rec next_parse (cfg : cfg )(symbol_list : symbol list) (prods : production) (prod : symbol *symbol list)(nonterminal : symbol list) = 
    match nonterminal with 
    | [] -> false
    | h :: t -> ( 
      let next_prod = get_next_prod prods h in
      sub_parse cfg symbol_list prods next_prod h || next_parse cfg symbol_list prods prod t 
    ) in 
  match symbol_list with
  | [] -> false
  | head :: tail -> (
        let same_left = find_same_left prod prods in 
        if snd same_left = [] then (
          let select1 = select cfg prod in 
          if elem head select1 && head != End then (
            let head_symbol = get_head prod in
            if elem head_symbol nonts then (
              let next_noterminal =  find_next_nonterminal prod in 
             next_parse cfg tail prods prod next_noterminal
            )
            else(
            let next_noterminal = cut (find_next_nonterminal prod) in 
             next_parse cfg tail prods prod next_noterminal
            )
          )
          else if elem head select1 && head = End then true
          else false
        )
        else(
          let select1 = select cfg prod in
          let select2 = select cfg same_left in
          if elem head select1 && head != End then (
            let head_symbol = get_head prod in
            if elem head_symbol nonts then (
              let next_noterminal =  find_next_nonterminal prod in 
             next_parse cfg tail prods prod next_noterminal
            )
            else(
            let next_noterminal = cut (find_next_nonterminal prod) in 
             next_parse cfg tail prods prod next_noterminal
            )
          )
          else if elem head select2 && head != End then (
            let head_symbol = get_head prod in
            if elem head_symbol nonts then (
              let next_noterminal =  find_next_nonterminal prod in 
             next_parse cfg tail prods same_left next_noterminal
            )
            else(
            let next_noterminal = cut (find_next_nonterminal prod) in 
             next_parse cfg tail prods same_left next_noterminal
            )
          )
          else if elem head select1 && head = End then true
          else if elem head select2 && head = End then true
          else false
        )
      ) 

  


let parse (lst : symbol list) (cfg : cfg) : bool = 
  let _, _, start_nonterminal, prods = cfg in 
  match prods with
  | h :: t -> sub_parse cfg lst prods h start_nonterminal
  | [] -> false
  