type symbol = T of string | N of string | Epsilon | End
type production = (symbol * symbol list) list
type cfg = symbol list * symbol list * symbol * production
type occur = symbol * int
val fresh : unit -> int
val cfg1 : symbol list * symbol list * symbol * (symbol * symbol list) list
val cfg2 : symbol list * symbol list * symbol * (symbol * symbol list) list
val cfg3 : symbol list * symbol list * symbol * (symbol * symbol list) list
val first_helper : production -> symbol -> symbol list
val first : cfg -> symbol -> symbol list
val exist_epsilon : cfg -> symbol -> bool
val follow : cfg -> symbol -> symbol list
val nepsilon : symbol -> bool
val nemptylst : 'a list -> bool
val filter : ('a -> bool) -> 'a list -> 'a list
val find_select : cfg -> symbol * symbol list -> symbol list
val select : cfg -> symbol * symbol list -> symbol list
val factor_helper : production -> symbol -> symbol list
val is_left_factor : cfg -> symbol -> bool
val same_element : symbol -> symbol list -> bool
val exist_intersection : symbol list -> symbol list -> bool
val find_same_left :
  symbol * symbol list -> (symbol * symbol list) list -> symbol * symbol list
val is_LL1 : cfg -> bool
val find_next_nonterminal : symbol * symbol list -> symbol list
val get_next_prod : production -> symbol -> symbol * symbol list
val cut : 'a list -> 'a list
val get_head : symbol * symbol list -> symbol
val sub_parse :
  cfg -> symbol list -> production -> symbol * symbol list -> symbol -> bool
val parse : symbol list -> cfg -> bool
val factor_check : production -> symbol -> symbol list list
val find_factor : symbol list list -> symbol list
val dirct_left_recursion : cfg -> symbol -> bool
val get_rhs : symbol list list -> symbol -> symbol list list
val get_lhs : symbol list list -> symbol -> symbol list list
val symbol_to_string : symbol -> string
val drop : symbol -> production -> production -> production
val get_lhs_rhs : cfg -> symbol -> (symbol * symbol list) list
val is_left_dirct_recursion_helper : cfg -> symbol list -> bool
val is_left_dirct_recursion : cfg -> bool
val eliminate_helper :
  cfg -> symbol list -> production -> (symbol * symbol list) list
val eliminate_n_symbol : cfg -> symbol list -> symbol list -> symbol list
val eliminate_direct_left_recursion : cfg -> cfg
val symbol_map : symbol -> symbol * symbol list -> symbol list
val concat_prods : symbol -> production -> symbol list list
val get_all_head_occ : symbol list list -> occur list
val get_left_factor : occur list -> symbol list
val get_remainder : symbol -> symbol list list -> symbol * symbol list list
val get_left_factor_list_check :
  cfg -> symbol list -> production -> symbol list list -> symbol list list
val drop_first : symbol -> symbol -> production -> production -> production
val get_note : cfg -> symbol list -> symbol list
val left_factor_concat : cfg -> cfg
val left_factor_judge : cfg -> symbol list -> cfg
val is_factor : cfg -> bool
val match_common_list :
  symbol -> symbol list -> symbol * symbol list -> production
val find_common_factor_helper : production -> symbol -> production
val find_common_factor : cfg -> production
val match_nonte : cfg -> symbol -> symbol list
val replace_nonterminal_helper : symbol list -> production -> production
val replace_nonterminal : production -> cfg -> cfg
val replace_cfg : cfg -> cfg
val is_common_factor : cfg -> bool
val eliminate_left_factor : cfg -> cfg
