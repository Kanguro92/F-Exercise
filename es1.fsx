(****  ESERCIZI  *****)
   
(*

1) Definire la funzione

  Or : bool ->  bool -> bool

che calcola or di due valori booleani.  


*)

let Or x y = 
    match (x, y) with
    | (false, false) -> false
    | _ -> true

(*
2) Definire la funzione

  isPariString : int -> string

che, applicata a un intero n, restituisce la stringa  "pari" se n e' pari,
"dispari"  se n e' dispari.

Definire quindi la funzione

 s : int -> string

che, applicata a un intero n, restituisce una stringa che descrive se n e' pari o dispari

Esempio:

s 4 // "4 e' un numero pari"
s 5  // "5 e' un numero dispari"

Notare che:

o E' possibile concatenare stringhe usando l'operatore + (come in Java)

   Esempio:

   "Il" + " " + "cane" + " abbaia" + "!!!" ;;
   val it : string = "Il cane abbaia!!!"

  Notare che l'operatore binario + e' *overloaded*
  (il suo significato e' determinato in  base ai tipi degli argomenti)

o Per trasformare un intero nella stringa corrispondente, usare la funzione string 

  Esempio:

  string 5 ;;
  val it : string = "5"

  let c = 100 ;;
  val c : int = 100
  string c ;;
  val it : string = "100"

*)

let isPariString n =
    match n%2 with
    | 1 -> string n + " è un numero dispari"
    | _ -> string n + " è un numero pari"

(*

Definire la funzione

 isPositive  : int -> bool

tale che

  isPositive  x =  true se x >= 0, false altrimenti

*) 

let isPositive n =
  match n with
    | n when n < 0 -> false
    | _ -> true