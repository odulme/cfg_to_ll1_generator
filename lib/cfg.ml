open Sets;;

type symbol =
  | T of string (* terminal symbol *)
  | N of string (* nonterminal symbol *)
  | Epsilon (* empty string *)
  | End (* end marker *)

type production = (symbol * symbol list) list
type cfg = symbol list * symbol list * symbol * production

let first (cfg : cfg) (nont : symbol) : symbol list =
  let _, _, _, prods = cfg in
  let rec find (productions : production) (nonterminal : symbol) : symbol list =
    match productions with
    | head :: tail ->
        let nont_in_prod, symbol_list = head in
        if nont_in_prod = nonterminal then
          match symbol_list with
          | [] -> []
          | h :: _ -> (
              match h with
              | T _ -> h :: find tail nonterminal
              | N _ -> find tail h
              | _ -> [])
        else find tail nonterminal
    | [] -> []
  in
  find prods nont

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

let cfg1 = (
  [N "E"; N "E'"; N "T"; N "T'"; N "F"],
  [T "+"; T "*"; T "("; T ")"; T "id"],
  N "E",
  [
  (N "E", [N "T"; N "E'"]);
  (N "E'", [T "+"; N "T'"; N "E"]);
  (N "E", []);
  (N "T", [N "F"; N "T'"]);
  (N "T'", [T "*"; N "F"; N "T'"]);
  (N "T", []);
  (N "F", [T "("; N "E"; T ")"]);
  (N "F", [T"id"])
  ])

