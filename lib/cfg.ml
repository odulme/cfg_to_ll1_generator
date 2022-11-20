open Sets
open Utils

type symbol =
  | T of string (* terminal symbol *)
  | N of string (* nonterminal symbol *)
  | Epsilon (* empty string *)
  | End (* end marker *)

type production = (symbol * symbol list) list
type cfg = symbol list * symbol list * symbol * production
type occur = symbol * int

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

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
      (N "X", [N "X" ; T "c"]);
      (N "X", [N "Y"]);
      (N "Y", [T "c"]);
      (N "Y", [Epsilon]);
      (N "Z", [N "Y" ;]);
      (N "Z", [T "d"; N "Y"]);
      (N "Z", [T "X"; N "Y"; T "Z"])
      ])
let cfg3 = (
  [N "A"; N "B" ; N "C"],
  [T "x"; T "y" ; T "z"],
  N "X",
  [
    (N "A" , [N "B" ; T "y" ; T"z"]);
    (N "A" , [T "x" ; T "y"]);
    (N "A" , [T "z" ; N "C"]);
    (N "B" , [T "x"]);
    (N "C" , [T "y"]);
  ]
)

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
            | T _ ->union [h] (first_helper tail nonterminal)
            | N _ -> first_helper tail h
            | Epsilon ->union [Epsilon] ( first_helper tail nonterminal)
            | _ -> [])
      else first_helper tail nonterminal
  | [] -> []

let first (cfg : cfg) (nont : symbol) : symbol list =
  let _, _, _, prods = cfg in
  first_helper prods nont

let rec exist_epsilon (cfg : cfg) (nont : symbol) : bool =
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
                  | T _ ->union [next] (lookahead t nonterminal tail cfg prod header)
                  | N _ ->
                      if exist_epsilon cfg next then
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
         union [End] (lookahead symbol_list nonterminal tail cfg productions nont_in_prod)
        else lookahead symbol_list nonterminal tail cfg productions nont_in_prod
    | [] -> []
  in
  follow_helper prods nont cfg

let nepsilon x = x != Epsilon
let nemptylst x = x != []

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
  let rec exist_epsilon (first_list : symbol list) : bool =
    match first_list with
    | [] -> false
    | h :: t -> (
        match h with Epsilon -> true | T _ -> exist_epsilon t | _ -> false)
  in
  if exist_epsilon first_alpha = false then first_alpha
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

let rec is_left_factor (cfg : cfg) (nonterminal : symbol) : bool =
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
let rec exist_intersection (select1 : symbol list) (select2 : symbol list) : bool =
  match select1 with 
  | [] -> false
  | h1 :: t1 ->(
    match select2 with 
    | [] -> false
    | h2 :: _ -> if same_element h1 select2 then true
    else exist_intersection t1 select2
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
    if (exist_intersection (select cfg h) (select cfg left_same)) then false
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
     let new_prods = remove head new_prods in 
      drop symbol new_prods new_prods
    else
     drop symbol tail new_prods
  | [] -> new_prods

let get_lhs_rhs(cfg : cfg)(nonterminal : symbol) : (symbol * symbol list) list =
  let  _, _, _, prods = cfg  in
  let str = symbol_to_string nonterminal in 
  let new_nont = (N (str ^ "_")) in  
  let lst = factor_check prods nonterminal in 
  let lst_rhs = get_rhs lst nonterminal in
  let lst_lhs = get_lhs lst nonterminal in
  let rhs_with_new = super_cat ([new_nont]) (lst_rhs)  in 
  let lhs_with_new =  super_cat ([new_nont]) (lst_lhs) in 
  let new_prods = drop nonterminal prods prods in 
  union (new_prods) (union (illness_cat (new_nont) (rhs_with_new) ) (illness_cat (nonterminal) (lhs_with_new)))

let rec is_left_dirct_recursion_helper (cfg : cfg)(n_symbol : symbol list) : bool =  
  match n_symbol with
  | head :: tail ->
   (dirct_left_recursion cfg head) || is_left_dirct_recursion_helper cfg tail
  | [] -> false

let is_left_dirct_recursion (cfg : cfg) : bool =
  let n_symbol , _, _, prods = cfg in 
  is_left_dirct_recursion_helper cfg n_symbol

let rec eliminate_helper (cfg : cfg)(n_symbol : symbol list)(prods : production) : (symbol * symbol list) list=
 match n_symbol with
 | head :: tail -> (
  if dirct_left_recursion cfg head then
    let prods = get_lhs_rhs cfg head in
    let nonterminal , terminal , start  , productions = cfg in 
    let cfg = nonterminal , terminal , start , prods in 
    eliminate_helper cfg tail prods
  else
    eliminate_helper cfg tail prods
 )
 | [] -> prods

let rec eliminate_n_symbol (cfg : cfg)(n_symbol : symbol list)(new_n_symbol : symbol list) : symbol list =
  match n_symbol with
  | head :: tail ->(
    if dirct_left_recursion cfg head then
      let str = symbol_to_string head in 
      let new_str = (N (str ^ "_")) in
      let new_n_symbol = new_str :: n_symbol in 
      eliminate_n_symbol cfg tail new_n_symbol
    else 
      eliminate_n_symbol cfg tail new_n_symbol
  )
  | [] -> new_n_symbol

let eliminate_direct_left_recursion(cfg : cfg) : cfg =
 let n_symbol , t_symbol, symbol, prods = cfg in  
 let new_prods = eliminate_helper cfg n_symbol prods in 
 let new_n_symbol = eliminate_n_symbol cfg n_symbol n_symbol in 
 new_n_symbol , t_symbol , symbol ,new_prods

let symbol_map (symbol : symbol) (prods : symbol * symbol list) =
  let lhs, rhs = prods in
  if lhs = symbol then rhs else []

let rec concat_prods (symbol : symbol) (prod_list : production) =
  let tmp_list = 
  match prod_list with
  | [] -> []
  | h :: t -> symbol_map symbol h :: concat_prods symbol t
  in List.filter nemptylst tmp_list

let get_all_head_occ lst : occur list =
  let tmp = List.map List.hd lst in
  assoc_list tmp

let get_left_factor (lst : occur list) =
  let tmp_list =
    let check_factor (occ_map : occur) =
      let symbol, occ = occ_map in
      if occ > 1 then [ symbol ] else []
    in
    List.map check_factor lst
  in
  List.fold_left ( @ ) [] tmp_list

let get_remainder (symbol:symbol) (prods: symbol list list) = 
  let get_remiander_helper lst = 
    match lst with
    | [] -> []
    | h::t -> 
      if h = symbol && t != [] then t
      else if h = symbol && t = [] then [Epsilon]
      else if  h = symbol && t = [Epsilon] then []
      else []
  in
   (symbol,List.filter nemptylst (List.map get_remiander_helper prods))

let rec get_left_factor_list_check (cfg : cfg)(n_symbol : symbol list)(prods : production)(concat : symbol list list) : symbol list list =
 match n_symbol with
 | head :: tail -> 
  if is_left_factor cfg head then
    let concat = concat_prods head prods in 
    get_left_factor_list_check cfg tail prods concat
  else
    get_left_factor_list_check cfg tail prods concat
 | [] -> concat

let rec drop_first (symbol : symbol)(nonterminal : symbol)(productions : production)(new_prods : production) : production =
  match productions with
  | head :: tail  ->(
  let  nont_head , symbol_list = head in
  match symbol_list with
   | h :: _ ->
    if h = symbol && nont_head = nonterminal then
      let  new_prods = remove head new_prods in 
      drop_first nonterminal symbol new_prods new_prods
    else 
      drop_first nonterminal symbol tail new_prods
   | [] -> new_prods
  )
  |[] -> new_prods

let rec get_note (cfg : cfg)(n_symbol : symbol list) : symbol list =
  match n_symbol with
  | head :: tail -> (
    if is_left_factor cfg head then
      head :: get_note cfg tail
    else 
      get_note cfg tail
   )
  | [] -> []

let rec left_factor_concat (cfg : cfg) :  cfg =
  let n_symbol , t_symbol, symbol, prods = cfg in
  let concat = get_left_factor_list_check cfg n_symbol prods [[]] in
  let nonte =  get_note cfg n_symbol in 
  let first_list = get_all_head_occ concat in 
  let first_factor = get_left_factor first_list in 
  let remainder =get_remainder (List.hd(first_factor)) concat in 
  let new_productions = drop_first (List.hd(first_factor)) (List.hd(nonte)) prods prods in 
  let final_productions = drop_first(List.hd(first_factor)) (List.hd(nonte)) new_productions new_productions in 
  let str_symbol , symbol_list_list = remainder in
  let new_str_symbol =  (N (symbol_to_string(List.hd(nonte)) ^ "'" )) in 
  let new_nont_prods =List.hd(nonte) ,first_factor @ [new_str_symbol] in  
  let add_prods = new_nont_prods :: ( cat new_str_symbol symbol_list_list) in 
  let new_n_symbol = n_symbol @ [new_str_symbol] in  
  let final_prods = union final_productions add_prods in 
  new_n_symbol , t_symbol , symbol , final_prods

let rec left_factor_judge (cfg : cfg)(n_symbol : symbol list) : cfg =
  match n_symbol with
  | head :: tail ->
    if is_left_factor cfg head then
      let cfg = left_factor_concat cfg in 
      let new_symbol ,_ ,_ ,_ = cfg in 
      left_factor_judge cfg new_symbol
    else
      left_factor_judge cfg tail
  | [] -> cfg

let rec is_factor (cfg : cfg) : bool = 
  let n_symbol,t_symbol,start,prods  = cfg in
  match n_symbol with
  | head :: tail -> 
    let cfg = tail , t_symbol , start , prods in   
    (is_left_factor cfg head) || (is_factor cfg)
  | [] -> false


let rec match_common_list (nonterminal : symbol)(symbol_list : symbol list)(head : symbol * symbol list) : production =
  match symbol_list with
  | h :: t -> (
    match h with
    | N _ ->(
      if h = nonterminal then
        []
      else
        [head]
    )
    |_ -> []
  )
  |[] -> []


let rec find_common_factor_helper(prods : production)(nonterminal : symbol): production =
  match prods with
  | head :: tail ->( 
    let head_nonte , symbol_list = head in 
    if head_nonte = nonterminal then
    union (match_common_list nonterminal symbol_list head) (find_common_factor_helper tail nonterminal)
    else find_common_factor_helper tail nonterminal
  )
  | [] -> []

let rec find_common_factor (cfg : cfg) : production = 
  let n_symbol , t_symbol , start ,prods = cfg in
  match  n_symbol with
  | head :: tail ->
    let cfg = tail, t_symbol , start , prods in 
    union (find_common_factor_helper prods head)(find_common_factor cfg)
  | [] -> []

let rec match_nonte (cfg : cfg)(nonterminal : symbol) : symbol list = 
  let n_symbol , t_symbol , start , prods = cfg in 
  match prods with 
  | head :: tail ->(
    let head_nonte , symbol_list = head in 
    let cfg = n_symbol , t_symbol , start , tail in 
    if head_nonte = nonterminal then
      union symbol_list (match_nonte cfg nonterminal)
    else 
      match_nonte cfg nonterminal
  )
  | [] -> []

let rec replace_nonterminal_helper(symbol_list : symbol list)(prods : production) : production = 
  match prods with
  | head :: tail ->(
    let head_nonte , new_symbol_list = head in 
    match new_symbol_list with
    | h :: t ->
      let new_symbol_list = union symbol_list t in 
       [head_nonte , new_symbol_list]
    | [] -> []
  )
  | [] -> []


let rec replace_nonterminal (prods : production) (cfg : cfg) : cfg =
  let n_symbol , t_symbol , start , productions = cfg in 
  match prods with
  | head :: tail -> (
    let head_nonte , symbol_list = head in 
    match symbol_list with
    | h :: _ -> (
      let match_symbol_list = match_nonte cfg h in 
      let new_prods = replace_nonterminal_helper (match_symbol_list)(prods) in
      let new_productions = remove head productions in 
      let final_productions = union new_prods new_productions in 
      n_symbol,t_symbol,start,final_productions
    )
    | [] -> cfg
  )
  | [] -> cfg

let rec replace_cfg (cfg : cfg) : cfg = 
  let prods = find_common_factor cfg in 
  replace_nonterminal prods cfg

let rec is_common_factor (cfg : cfg) : bool =
  let n_symbol , t_symbol , start , prods = cfg in 
  match prods with
  |head :: tail ->(
    let head_nonte , symbol_list = head in
    let cfg = n_symbol , t_symbol , start , tail  in 
    match symbol_list with
    | h :: _ ->(
      match h with
      | N _ ->  (
        if h = head_nonte then 
          is_common_factor cfg
        else
          true || is_common_factor cfg
      )
      | _ -> is_common_factor cfg
    )
    | [] -> is_common_factor cfg
  )
  | [] -> false

let rec eliminate_left_factor (cfg : cfg) : cfg = 
    let n_symbol , t_symbol, symbol, prods = cfg in
    if is_common_factor cfg then
      let cfg = replace_cfg cfg in 
      eliminate_left_factor cfg
    else 
    left_factor_judge cfg n_symbol
  