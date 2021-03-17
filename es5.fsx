(*
Definizione delle funzioni map, filter, exists, forall analoghe a quello viste usando la ricorsione;
usando FsCheck, verificare l'equivalenza con le omonime funzioni di List. 
*) 

#r "FsCheck"
open FsCheck

// Funziona List.map

let rec map ls f =
  match ls with
  | [] -> []
  | x::xs -> (f x)::map xs f

let prop_map ls f =
  List.map(fun x -> f x) ls = map ls f 

do Check.Quick prop_map

// Funzione List.filter

let rec filter ls f = 
  match ls with
  | [] -> []
  | x::xs -> if (f x) then x::filter xs f else filter xs f 

let prop_filter ls f =
  List.filter(fun x -> f x) ls = filter ls f 

do Check.Quick prop_filter

// Funzione List.exists

let rec exists ls f =
  match ls with
  | [] -> false
  | x::xs -> if (f x) then true else exists xs f
   
let prop_exists ls f =
  List.exists(fun x -> f x) ls = exists ls f 

do Check.Quick prop_exists

// Funzione List.forall

let rec forall ls f = 
  match ls with
  | [] -> true
  | x::xs -> if (f x) then forall xs f else false

let prop_forall ls f =
  List.forall(fun x -> f x) ls = forall ls f 

do Check.Quick prop_forall

// Esempi di utilizzo funzioni High-Order

// List.map :  ('a -> 'b) -> 'a list -> 'b list

// Costruire la lista sq10 dei quadrati dei numeri da 1 a 10:

let sq10 = List.map(fun x -> x * x) [1 .. 10];;

(* Definire la funzione
   sqList : int -> int list
   che, dato n >=1, costruisce la lista dei quadrati da 1 a n. *)

let sqList n = if n >= 1 then List.map(fun x -> x * x) [1 .. n] else []

let sq4 = sqList 4  // [1; 4; 9; 16]

// List.filter : ('a -> bool) -> 'a list -> 'a list

(* Definire la funzione
   pari : int -> int list
che, dato n >=0, costruisce la lista dei numeri pari compresi fra 0 e n. *)

let pari n  = if n >= 0 then List.filter(fun x -> x%2=0) [0 .. n] else []

let p1 = pari 10  // [0; 2; 4; 6; 8; 10]
let p2 = pari 15  // [0; 2; 4; 6; 8; 10; 12; 14]

(* Definire la funzione
 mult : int -> int -> int list
che, dati due interi k e m, tali che 0 < k <= m,
costruisce la lista dei multipli di k compresi tra k e m. *)

let mult k m = if m > k then List.filter(fun x -> x%k=0) [k .. m] else []

let m1 = mult 3 15 // [3; 6; 9; 12; 15]
let m2 = mult 5 27 // [5; 10; 15; 20; 25]

// List.exists : ('a -> bool) -> 'a list -> bool
// List.forall : ('a -> bool) -> 'a list -> bool

(* Usando List.exists, definire la funzione
    contains : x:'a -> xs:'a list -> bool when 'a : equality
che controlla se un elemento x appartiene a una lista xs.
Validare  con FsCheck l'equivalenza fra  contains e List.contains. *)

let contains n ls = List.exists(fun x -> x = n) ls

let prop_contains x xs = 
  contains x xs = List.contains x xs

do Check.Quick prop_contains

(* Usando List.exists, definire la funzione
  isSq : int -> bool
che, dato un intero n >= 0, determina se n e' un quadrato perfetto.
Verificare che
  List.filter isSq [0 .. 200]
produce la lista
[0; 1; 4; 9; 16; 25; 36; 49; 64; 81; 100; 121; 144; 169; 196] *)
 
let isSq n = if n >= 1 then List.exists (fun k -> n = k * k) [0 .. (n/2 + 1)] else false

let square200 = List.filter isSq [0 .. 200]

(* Validare con FsCheck la seguente proprieta' prop_sq,
che utilizza la  funzione  sqList definita sopra:
   per ogni n, la lista 'sqList n' contiene solo quadrati perfetti. *)   

let prop_sq n = 
  sqList n |> List.forall(fun x -> isSq x)

do Check.Quick prop_sq
