let create func n = ref (func,Array.make n []);;

(*type ('a,'b) t

val create: ('a->int) -> int -> ('a,'b) t
val add: ('a,'b) t -> 'a -> 'b -> 'b option
val mem: ('a,'b) t -> 'a -> bool
val find: ('a,'b) t -> 'a -> 'b option
val delete: ('a,'b) t -> 'a -> 'b option

val iter: ('a,'b) t -> (('a*'b) -> unit) -> unit
val fold: ('a,'b) t -> (('a*'b) -> 'c -> 'c) -> 'c -> 'c*)

let help1 hashtable=
match (!hashtable) with
(a,b)-> a;;

let help2 hashtable=
match (!hashtable) with
(a,b)-> b;;


let hash hashtable str =
(((help1 hashtable) str) mod (Array.length (help2 hashtable)));;

let add hashtable str a=
let rec sup_f l=
match l with
(s,n)::b->if s = str then (Some n,((s,a)::b)) else (let (a1,a2) = sup_f b in (a1,(s,n)::a2)) 
|[]->(None,[(str,a)]) in
let (a1,a2) = sup_f ((help2 hashtable).(hash hashtable str)) in
(help2 hashtable).(hash hashtable str) <- a2;
a1;;

let find hashtable str =

let rec sup_f l=
match l with
(s,n)::b -> if s = str then (Some n) else (sup_f b)
|[]-> None in

sup_f ((help2 hashtable).(hash hashtable str));;

let rec mem hashtable str =
let rec sup_f l=
match l with
(s,n)::b -> if s = str then true else (sup_f b)
|[]-> false in
sup_f ((help2 hashtable).(hash hashtable str));;

let delete hashtable str =
let rec sup_f l=
match l with
(s,n)::b -> if s = str then (Some n,b) else (let (a1,a2) = (sup_f b) in (a1,(s,n)::a2))
|[]-> (None,[]) in
let (a1,a2) = (sup_f ((help2 hashtable).(hash hashtable str))) in
(help2 hashtable).(hash hashtable str) <- a2;
a1;;

let iter hashtable func =
Array.iter (fun l -> (List.iter func l)) (help2 hashtable);;

let fold hashtable func start=
let rec sup_f l c=
match l with
a::b->(sup_f b (func a c))
|[]->c in (sup_f (List.concat (Array.to_list (help2 hashtable))) start);;

let replace hashtable str num=
let rec sup_f l=
match l with
(s,n)::b -> if s = str then ((s,num)::b) else ((s,n)::(sup_f b))
|[]-> failwith"" in
(help2 hashtable).(hash hashtable str) <- (sup_f ((help2 hashtable).(hash hashtable str)));;










