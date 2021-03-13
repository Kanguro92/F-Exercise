(*
Esercizio 1: esponenziale
^^^^^^^^^^^^^^^^^^^^^^^

Definire una funzione ricorsiva

   exp : float -> int -> float

che calcola l'esponenziale:

  exp b n = b ^n   

Si *assume* n>= 0. 
Usiamo la seguente definizione ricorsiva (induzione su n)

  b^n   =  1                se n = 0    [CASO BASE}

  b^n  =   b * b^(n-1)      se n > 0    [PASSO INDUTTIVO] 

*)

// exp : b:float -> n:int -> float
// si *assume*  n>= 0
let rec exp b n =
    match n with
    | 0 -> 1.
    | _ -> b * exp b (n-1)

(*

Esercizio 2
^^^^^^^^^^^

i) Definire una funzione ricorsiva

make_str : int -> string

che, dato un intero n>=0, costruisce la stringa "0 1 2 ... n"

Esempio:

make_str 20 

deve costruire la stinga

 "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20"

Per concatenare stringhe, usare operatore +.
Per convertire da intero a stringa, usare la funzione

   string : int ->  string

*)

// i)
// make_str : n:int -> string
// si *assume*  n>= 0
let rec make_str n =
    match n with
    | 0 -> string 0
    | _ -> make_str (n - 1) + " " + string n;;

(*

ii) Definire la funzione ricorsiva

  make_sum_str  int -> int * string

tale che 

  make_sum_str n

calcola la coppia (sum,str) dove:

- sum e' l'intero corrispondente alla somma  0 + 1 + 2 + ... + n
- str e' stringa "0 1 2 ... n"  (come nel punto precedente)

Va fatta una *unica* chiamata ricorsiva  

Esempio:

mk_sum_str 5 
 val it : int * string = (15, "0 1 2 3 4 5")

*)

// ii)
// mk_sum_str : n:int -> int * string
// si *assume*  n>= 0
let rec mk_sum_str n = 
    match n with
    | 0 -> (0, string 0)
    | _ -> 
        let (sum, str) = mk_sum_str (n-1)
        (sum + n, str + " " + string n)

mk_sum_str 5 
// val it : int * string = (15, "0 1 2 3 4 5")

(*

iii) Definire  una funzione

  somma_n : int -> string

tale che

   somma_n n

stampa una stringa della forma

  "0 + 1 + ... + n = k"

con k il valore della somma 0+1+ ... +k

Suggerimento
^^^^^^^^^^^^

Definire una funzione ausiliaria

 make_sum_str1  int -> int * string

analoga alla funzione make_sum_str del punto ii) in cui pero' la stringa ha 
il formato

 "0 + 1 + ... + n"

La funzione ausiliaria puo' anche essere definita internamente alla funzione somma_n

*)

let somma_n n =
    let rec make_sum_str1 n =
        match n with
        | 0 -> (0, string 0)
        | _ -> 
            let (sum , str) = make_sum_str1 (n-1)
            (sum + n, str + " + " + string n)
    let (sum2, str2) = make_sum_str1 n
    str2 + " = " + string sum2;;

//forma alternativa

let rec make_sum_str1 n =    
    match n with
    | 0 -> (0, string 0)
    | _ -> 
        let (sum , str) = make_sum_str1 (n-1)
        (sum + n, str + " + " + string n)

let somma_n n = 
    let (sum, str) = make_sum_str1 n
    str + " = " + string sum
    
let str1 = somma_n 5
// "0 + 1 + 2 + 3 + 4 + 5 = 15"

let str2 = somma_n 10
// "0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 = 55"


