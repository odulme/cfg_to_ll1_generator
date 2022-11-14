let contains_elem lst e =
  let lst' =
    let equal = ( = ) e in
    List.map equal lst
  in
  List.fold_left ( || ) false lst'

let is_present lst x =
  let present y = if x = y then 1 else 0 in
  List.map present lst

let count_occ lst target =
  let lst' = is_present lst target in
  List.fold_left ( + ) 0 lst'

let uniq lst =
  let concat lst' x = if contains_elem lst' x then lst' else x :: lst' in
  List.fold_left concat [] lst

let assoc_list lst =
  let lst' = uniq lst in
  let makepair x = (x, count_occ lst x) in
  List.map makepair lst'

let ap fns args =
  let lst =
    let maps fn = List.map fn args in
    List.map maps fns
  in
  List.fold_left ( @ ) [] lst