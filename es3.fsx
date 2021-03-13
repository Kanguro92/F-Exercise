// ESERCIZI SU TAG VALUE

type figura = 
   | Rettangolo of  float * float      // (base, altezza)
   | Quadrato   of  float              // lato
   | Triangolo  of  float * float      // (base, altezza)

let area fig =
   match fig with
   | Rettangolo(b,h) ->   b * h     
   | Quadrato l      ->   l * l  
   | Triangolo(b,h)  ->  ( b * h )  / 2. 
// val  area : figura -> float
// Una figura e' *ben definita* se nessuna delle sue dimensioni e' negativa.

(* Definire la funzione areaOpt che calcola l'area di una figura fig, se fig e' ben definita. 
La funzione areaOpt ha tipo:

  areaOpt : figura -> float option

e restituisce:
-   None       se fig non e' ben definita 
-   Some a     se fig e' ben definita e l'area di fig e' a (un float).
*)

let areaOpt fig =
    match fig with
    | Rettangolo(b,h) when (b>0.&&h>0.) -> Some (area fig)
    | Quadrato(l) when l>0. -> Some (area fig)
    | Triangolo(b,h) when(b>0.&&h>0.) -> Some (area fig)
    | _ -> None

// Esempi:
let a1 = areaOpt ( Rettangolo(2.0,3.0) ) 
// val a1 : float option = Some 6.0
let a2 = areaOpt ( Rettangolo(2.0, -3.0) ) 
// val a2 : float option = None
let a3 =  areaOpt ( Triangolo(2.5, 3.6) )
// val a3 : float option = Some 4.5
let a4 =  areaOpt ( Triangolo(-2.5, 3.6) )
// val a4 : float option = None

(* Definire la funzione

    printArea : figura -> string

che  calcola l'area di una figura  e restituisce una stringa col risultato -- usare la funzione string per la conversione;
se l'area non e' definita, va restituita un opportuno messaggio come da esempi sotto
*)

let printArea fig =
    match areaOpt fig with
    | Some x -> "area: " + string x
    | None -> "la figura non è ben definita"

// Esempi:
let as1 = printArea ( Quadrato 10. ) 
//val as1 : string = "area: 100"
let as2 = printArea ( Quadrato -10. ) 
//val as2 : string = "la figura non e' ben definita"

(* Definire la funzione

   sommaArea :  figura * figura -> float option

che, date due figure fig1 e fig2, restituisce la somma delle areee delle due figure,
se definita (ossia, se entambe le figure fig1 e fig2 sono ben definite).
Il risultato deve essere un option type.
Per calcolare l'area, usare la funzione areaOpt definita sopra.
*)

let sommaArea (fig1,fig2) =
    match (areaOpt fig1, areaOpt fig2) with
    | (Some x, Some y) -> Some (x+y)
    | _ -> None

// Esempi: 
let sum1 = sommaArea ( Rettangolo(2.,5.) , (Quadrato 10.)) ;;
//  val sum1 : float option = Some 110..0
let sum2 = sommaArea ( Rettangolo(2.,-5.),  (Quadrato 10.)) ;;
// val sum2 : float option = None
let sum3 = sommaArea ( Rettangolo(2., 5.), (Quadrato -10.)) ;;
// val sum3 : float option = None
let sum4 =  sommaArea ( Triangolo(10.,5.), Triangolo(3.5,5.)) ;;
// val sum4  : float option = Some 33.75

(* Definire la funzione

    sommaAreaList : figura list -> float

che, data una lista figs di figure, calcola la somma delle aree delle
figure ben definite contenute in figs.
Se la lista figs non contiene figure ben definite, la somma vale zero.
*)

let rec sommaAreaList ls =
    match ls with
    | [] -> 0.
    | x::xs -> 
        match areaOpt x with
        | Some y -> y + sommaAreaList xs
        | None -> 0. + sommaAreaList xs 

// Esempi:
let ret = Rettangolo (5.0, 6.0)
let quad =  Quadrato 2.0
let tr = Triangolo (2.0, -2.0)

let figs1 = [tr]   // la lista non contiene figure ben definite
let s1 = sommaAreaList figs1
// val s1 : float = 0.0
let figs2 = [ ret ; tr ; quad ; tr ; ret ; quad] 
let s2 = sommaAreaList figs2
// s2 : float = 68.0

// ESERCIZI SULLE LISTE

(* Definire la funzione ricorsiva

    rmEven : int list -> int list

che cancella da una lista di interi tutti i numeri pari. *)

let rec rmEven ls =
    match ls with
    | [] -> []
    | x::xs when x%2<>0 -> x::rmEven xs
    | x::xs -> rmEven xs 

// Esempi:
let rm1 = rmEven [-10 .. 10 ] 
// val rm1 : int list = [-9; -7; -5; -3; -1; 1; 3; 5; 7; 9]
let rm2 = rmEven [2; 5; 5; 6; 6; 87; 6; 100; 2]  
// val rm2 : int list =  [5; 5; 87]

(* Definire la funzione ricorsiva

    rmOddPos :  a' list -> 'a list

che cancella tutti gli elementi di una lista in posizione dispari;
il primo elemento della lista ha posizione 0. *)

let rec rmOddPos ls =
    match ls with
    | [] -> []
    | [x] -> [x]
    | x::y::xs -> x::rmOddPos xs

// Esempi:
let rmp1 = rmOddPos ['a'  .. 'z'] 
// val rmp1 : char list = ['a'; 'c'; 'e'; 'g'; 'i'; 'k'; 'm'; 'o'; 'q'; 's'; 'u'; 'w'; 'y']
let rmp2 = rmOddPos ["zero" ;  "uno" ; "due" ; "tre" ; "quattro"]  
// val rmp2 : string list = ["zero"; "due" ; "quattro"]

(*
Suggerimento: Distinguere con un opportuno pattern matching i casi in cui la lista passata come argomento
abbia zero, uno,  almeno due  elementi.
*)

(* Definire la funzione 

   split : 'a list -> 'a list * 'a list

che, data una lista, costruisce la coppia di liste 
degli elementi in posizione pari e in posizione dispari. *)

let rec split ls =
  match ls with 
  | [] -> ([],[])
  | [x] -> ([x],[])
  | x::y::xs ->
    let (a,b) = split xs
    (x::a,y::b)

// Esempi:
let s1 = split [0 .. 9]
// val s1 : int list * int list = ([0; 2; 4; 6; 8], [1; 3; 5; 7; 9])
let s2 = split ["ciao"] 
// val s2 : string list * string list = (["ciao"], [])
let s3 = split ["ciao" ; "ciao!!!" ]  
//val s3 : string list * string list = (["ciao"], ["ciao!!!"])
let s4 = split [ 'a' .. 'k'] 
// val s4 : char list * char list = (['a'; 'c'; 'e'; 'g'; 'i'; 'k'], ['b'; 'd'; 'f'; 'h'; 'j'])

// Suggerimento: Usare uno schema di pattern matching simile a quello dell'esercizio precedente.

(* Definire la funzione ricorsiva 

  cmpLength : cmpLength : 'a list -> 'b list -> int

che, date due liste ls0 e ls1, confronta le lunghezza
(length) delle liste e restituisce:

   -1    se  length(ls0) < length(ls1)
    0    se  length(ls0) = length(ls1)
    1    se  length(ls0) > length(ls1)

*Non* va usata la funzione length definita sulle liste. *)

let rec cmpLength ls0 ls1 =
  match (ls0, ls1) with
  | ([], []) -> 0 
  | (xs, []) -> 1
  | ([], ys) -> -1 
  | (x::xs, y::ys) -> cmpLength xs ys

// Esempi:
let c1 = cmpLength  [1 .. 10]  ['a' .. 'z']   // -1
let c2 = cmpLength  [1 .. 26]  ['a' .. 'z']   // 0
let c3 = cmpLength  ['a'; 'b';'c']  ["e" ; "f"]  // 1 

// Suggerimento: Definire un opportuno pattern matching sulla coppia (ls0,ls1)

(* Definire la funzione ricorsiva

  remove :  'a -> 'a list -> 'a list when 'a : equality

che dato un elemento x e una lista ls, restituisce la 
lista ottenuta da ls eliminando tutte le occorrenze di x. *)

let rec remove n ls = 
  match ls with
  | [] -> []
  | x::xs -> if (x = n) then remove n xs else x::remove n xs

// Esempi:
let ls1 = remove  2  [0 ..10] 
//val ls1 : int list = [0; 1; 3; 4; 5; 6; 7; 8; 9; 10]
let ls2 = remove   "uva"  [ "mele" ; "uva" ; "pere" ; "uva" ; "banane" ; "uva" ] 
// val ls2 : string list = ["mele"; "pere"; "banane"]
let ls3 = remove  11  [0 .. 10] 
// val ls3 : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

(* Usando remove, definire la funzione ricorsiva

removeDup : 'a list -> 'a list when 'a : equality 

che rimuove tutti i duplicati in una lista.
Piu' precisamente, se un elemento x compare piu' volte,
viene mantenuta solo la prima occorrenza. *)

let rec removeDup ls =
  match ls with
  | [] -> []
  | x::xs -> x::removeDup(remove x xs)

// Esempi:
let ls4 = removeDup [1; 2; 1; 2; 3] 
// val ls4 : int list = [1; 2; 3]
let ls5 = removeDup  [ "mele" ; "uva" ; "mele" ; "pere" ; "uva" ; "banane" ; "uva" ; "pere" ; "pere" ; "banane"] 
// val ls5 : string list = ["mele"; "uva"; "pere"; "banane"]

(* Definire le funzioni  ricorsive

  downto0 : int -> int list      
    upto : int -> int list

tali che
 
 downto0 n  = lista degli interi da n a 0
    upto n  = lista degli interi da 0 a n

dove si assume n >= 0 
(attenzione a non usare  `downto', che e' una keyword di F#) *)

let rec downto0 n = 
  match n with
  | 0 -> [0]
  | x -> x::(downto0 (n-1))

let downto0 n = [n .. -1 .. 0]

let upto n = [0 .. n]

// Esempi:
let downto10  =  downto0 10 ;;
// val downto10 : int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0]
let upto10  =  upto 10 ;;
// val upto10 : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
