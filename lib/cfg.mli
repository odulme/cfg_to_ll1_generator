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
val is_LL1 : cfg -> bool
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
