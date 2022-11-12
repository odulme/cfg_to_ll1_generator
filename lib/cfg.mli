type symbol = T of string | N of string | Epsilon | End
type production = (symbol * symbol list) list
type cfg = symbol list * symbol list * symbol * production
val first : cfg -> symbol -> symbol list
val exit_epsilon : cfg -> symbol -> bool
val follow : cfg -> symbol -> symbol list
val nepsilon : symbol -> bool
val filter : ('a -> bool) -> 'a list -> 'a list
val find_select : cfg -> symbol * symbol list -> symbol list
val select : cfg -> symbol * symbol list -> symbol list
val cfg1 : symbol list * symbol list * symbol * (symbol * symbol list) list
