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
