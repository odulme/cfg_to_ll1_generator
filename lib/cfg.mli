type symbol = T of string | N of string | Epsilon | End
type production = (symbol * symbol list) list
type cfg = symbol list * symbol list * symbol * production
val cfg1 : symbol list * symbol list * symbol * (symbol * symbol list) list
val cfg2 : symbol list * symbol list * symbol * (symbol * symbol list) list
val first_helper : production -> symbol -> symbol list
val first : cfg -> symbol -> symbol list
val exit_epsilon : cfg -> symbol -> bool
val follow : cfg -> symbol -> symbol list
val nepsilon : symbol -> bool
val filter : ('a -> bool) -> 'a list -> 'a list
val find_select : cfg -> symbol * symbol list -> symbol list
val select : cfg -> symbol * symbol list -> symbol list
val factor_helper : production -> symbol -> symbol list
val left_factor : cfg -> symbol -> bool
val same_element : symbol -> symbol list -> bool
val exit_intersection : symbol list -> symbol list -> bool
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
