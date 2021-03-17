// ESERCIZI 
   
(* Definire la funzione

  Or : bool ->  bool -> bool

che calcola or di due valori booleani. *)

let Or x y = 
    match (x, y) with
    | (false, false) -> false
    | _ -> true

(* Definire la funzione

  isPariString : int -> string

che, applicata a un intero n, restituisce la stringa  "pari" se n e' pari,
"dispari"  se n e' dispari. *)

let isPariString n =
    match n%2 with
    | 1 -> "dispari"
    | _ -> "pari"

(* Definire quindi la funzione

 s : int -> string

che, applicata a un intero n, restituisce una stringa che descrive se n e' pari o dispari *)

let s n = string n + " è un numero " + isPariString n
    
// Esempio:
s 4 // "4 e' un numero pari"
s 5  // "5 e' un numero dispari"

(* Definire la funzione

 isPositive  : int -> bool

tale che isPositive  x =  true se x >= 0, false altrimenti *) 

let isPositive n =
  match n with
    | n when n < 0 -> false
    | _ -> true
