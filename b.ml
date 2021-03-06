type 'a for_hashtbl_content = Empty | One of string * 'a| Deleted;;

let create func n = ref (func,Array.make n Empty);;

let help1 hashtable=
match (!hashtable) with
(a,b)-> a;;

let help2 hashtable=
match (!hashtable) with
(a,b)-> b;;

(*type ('a,'b) t

val create: ('a->int) -> int -> ('a,'b) t
val add: ('a,'b) t -> 'a -> 'b -> 'b option
val mem: ('a,'b) t -> 'a -> bool
val find: ('a,'b) t -> 'a -> 'b option
val delete: ('a,'b) t -> 'a -> 'b option

val iter: ('a,'b) t -> (('a*'b) -> unit) -> unit
val fold: ('a,'b) t -> (('a*'b) -> 'c -> 'c) -> 'c -> 'c*)


let hash hashtable str =
(((help1 hashtable) str) mod (Array.length (help2 hashtable)));;

let rec add hashtable str a=
let rec sup_f x i=
if i = 20 then (increse hashtable; add hashtable str a) 
          else (match ((help2 hashtable).(x)) with
                    One (s,n) -> (if s = str then ((help2 hashtable).(x) <- (One (str,a)); Some n) else (sup_f ((x+i*i) mod (Array.length (help2 hashtable))) (i+1)))
                   |_->((help2 hashtable).(x) <- (One (str,a));None)) in
(sup_f (hash hashtable str) 1)
and increse hashtable=
let new_hashtable = (create (help1 hashtable) ((Array.length (help2 hashtable))*2)) in
Array.iter (fun x-> match x with
Empty -> ()
|One (str,a) -> (let _ = add new_hashtable str a in ())
|Deleted -> ()) (help2 hashtable);
hashtable := (help1 hashtable,(help2 new_hashtable));;

let find hashtable str=             
let rec sup_f x i=
if i = 20 then (None) 
          else (match ((help2 hashtable).(x)) with
                    One (s,n) -> if s = str then (Some n) else (sup_f ((x+i*i) mod (Array.length (help2 hashtable))) (i+1))
                   |Deleted->(sup_f ((x+i*i) mod (Array.length (help2 hashtable))) (i+1)) 
                   |Empty -> failwith"") in
(sup_f (hash hashtable str) 1);;

let mem hashtable str=
let rec sup_f x i=
if i = 20 then (false) 
          else (match ((help2 hashtable).(x)) with
                    One (s,n) -> if s = str then true else (sup_f ((x+i*i) mod (Array.length (help2 hashtable))) (i+1))
                   |Deleted -> (sup_f ((x+i*i) mod (Array.length (help2 hashtable))) (i+1))
                   |_->false) in
(sup_f (hash hashtable str) 1);;

let delete hashtable str =
let rec sup_f x i=
if i = 20 then (None) 
          else (match ((help2 hashtable).(x)) with
                    One (s,n) -> if s = str then ((help2 hashtable).(x) <- Deleted; Some n) else (sup_f ((x+i*i) mod (Array.length (help2 hashtable))) (i+1))
                   |Deleted-> (sup_f ((x+i*i) mod (Array.length (help2 hashtable))) (i+1))
                   |_ -> failwith"") in
(sup_f (hash hashtable str) 1);;

let iter hashtable func =
Array.iter (fun p -> match p with 
                         One (s,n) -> func (s,n)
                        |_->()) (help2 hashtable);;

let fold hashtable func start=
let rec sup_f i c=
if i = Array.length (help2 hashtable) then c else (match ((help2 hashtable).(i)) with
                                                  One (s,n)-> sup_f (i+1) (func (s,n) c)
                                                 |_-> sup_f (i+1) c) in
sup_f 0 start;;

let replace hashtable str num=
let rec sup_f x i=
if i = 20 then (failwith"") 
          else (match ((help2 hashtable).(x)) with
                    One (s,n) -> if s = str then ((help2 hashtable).(x) <- One(s,num)) else (sup_f ((x+i*i) mod (Array.length (help2 hashtable))) (i+1))
                   |Deleted-> (sup_f ((x+i*i) mod (Array.length (help2 hashtable))) (i+1))
                   |_->failwith"") in
(sup_f (hash hashtable str) 1);;


