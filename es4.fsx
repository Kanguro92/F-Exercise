// Exercises on PBT

(* Remember your functions:

1.1 remove even numbers from int list
    rmEven : int list -> int list
    
1.2 remove all elements in odd **position** from a list 
    considering the first element an even position.
    rmOdd : rmOdd : 'a list -> 'a list
    
1.3 split a list into two pair of lists consosting of the even and odd positions
    split : 'a list -> 'a list * 'a list
*)    

// Validate them with FsCheck writing the following properties

//  + if I remove even numbers from a list, what's left are odds

let rec rmEven ls =
    match ls with
    | [] -> []
    | x::xs when x%2<>0 -> x::rmEven xs
    | x::xs -> rmEven xs

let prop_oddsList ls =
    List.forall(fun x -> x%2 <> 0) (rmEven ls)

//  + if I remove the odd positions, the length of the resulting list is more or less halved

let rec rmOddPos ls =
    match ls with
    | [] -> []
    | [x] -> [x]
    | x::y::xs -> x::rmOddPos xs

let prop_lenList (ls:int list) = 
    ls <> [] ==>
    lazy (rmOddPos ls |> List.length <= ((List.length ls)/2)+1)

(* + in cases 1.2 and 1.3, the functions do not add "new" elements,
  that is the underlying resulting set is a subset of the starting one
    - Hint for 1.3: define the inverse function of split, say merge and
    show merge (split xs) = xs) *)

let rec split ls =
  match ls with 
  | [] -> ([],[])
  | [x] -> ([x],[])
  | x::y::xs ->
    let (a,b) = split xs
    (x::a,y::b)

let rec merge (ls0,ls1) =
    match (ls0,ls1) with
    | ([],[]) -> []
    | (xs, []) -> xs
    | ([], ys) -> ys
    | (x::xs, y::ys) -> x :: y :: merge (xs,ys)

let prop_splitMerge xs = 
    merge (split xs) = xs

// + check that your def of downto0 corresponds to [n .. -1 .. 0]: (Hint: exclude the case for n negative)

let rec downto0 n = 
  match n with
  | 0 -> [0]
  | x -> x::(downto0 (n-1))

let prop_downto0 n =
    n > 0 ==> downto0 n = [n .. -1 .. 0]




