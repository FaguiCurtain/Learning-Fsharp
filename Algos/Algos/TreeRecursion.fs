// recursion with trees
//based on http://blog.moertel.com/posts/2013-06-03-recursion-to-iteration-3.html
open System
// open MyModule1.Stack

type Tree<'T> =
  | Tree of 'T * Tree<'T> * Tree<'T>
  | Tip of 'T

let smallTree = Tree ("1", Tree ("2", Tip "a", Tip "b"), Tip "c");;

let tree0 = Tip "None"
let tree1 = Tree("5",Tip "None",Tip "None")
let tree2 = Tree("7",tree1,Tip "None")
let tree3 = Tree("7",tree1,Tree("9",Tip "None",Tip "None"))
let tree4 = Tree("2",Tip "None",tree3)
let tree5 = Tree("2",Tree("1",Tip "None", Tip "None"),tree3)

// https://en.wikipedia.org/wiki/Tree_traversal#In-order
let cons h t = h::t

let rec tree_left_fold (tree:Tree<'T>) (f:'T->'b->'b) (acc:'b) =
    match tree with
      | Tip s -> acc
      | Tree (n,l,r) -> tree_left_fold r f (tree_left_fold l f (f n acc))
   

let rec tree_mid_fold (tree:Tree<'T>) (f:'T->'b->'b) (acc:'b) :'b = //souvent 'b est un nombre ou une liste
    match tree with
      | Tip s -> acc
      | Tree (n,l,r) -> // tree_mid_fold r f (f n (tree_mid_fold l f acc))
                     //                     'T  (            'T'b 'b )
                     //                         (            'b      )
                      let x = tree_mid_fold l f acc
                      let y = f n x
                      let z = tree_mid_fold r f y 
                      z

let zl = tree_left_fold tree5 cons []
let zm = tree_mid_fold tree5 cons []
printfn "zl %A" zl
printfn "zm %A" zm

 // pour transformer une fonction ('A1->'A2->'A3->...'An->'B)
 // ca devient ('A1->'A2->'A3->...'An->('B->'r)->'r) 
let rec tree_mid_fold_c (tree:Tree<'T>) (f_c:'T->'b->('b->'r)->'r) (acc:'b) (cont:'b->'r):'r=
     match tree with
       | Tip s -> cont acc
       | Tree (n,l,r) ->   // (tree_mid_fold_c l f_c acc cont1) //type 'r
                           //  tree_mid_fold_c l f_c acc est une évaluation partielle de type 'b
                           //  la continuation prend ce résultat et lui applique une fonction
                           //  c'est à dire ce qu'on fait de ce résultat
                           // (fun (acc:'b) -> (tree_mid_fold_c l f_c acc cont)) //type ('b->'r) 
                               
                               // on regarde d'abord la définition de z qu'on calcule en dernier
                               let cont1 = (fun res -> tree_mid_fold_c r f_c res cont) // c'est ce qu'on calcule en dernier donc on met cont ici
                               let cont2 = (fun res -> f_c n res cont1) // puis on remonte à la définition de y
                               tree_mid_fold_c l f_c acc cont2 // et on a la bonne continuation à appliquer à x

let myf_c (nodeV:'T) (list:list<'T>) (cont:list<'T>->'r) :'r = cont (nodeV::list)
                                      
let z= tree_mid_fold_c (tree5) (myf_c) [] id    
printfn "tree_mid_fold CPS %A" z |> ignore                                                         
//// 

///////////////////// trampoline /////////////////////
type 'a response = 
   | Done of 'a 
   | Continue of (unit -> 'a response) // doit-on définir 'b ici ?

let rec trampoline cont =
    match cont with 
      | Done x -> x
      | Continue more -> trampoline (more ())

// pour faire la trampoline,
// remplacer les ('r) et (unit -> 'r) par 'r continuation
//                    ou ('a   -> 'r)
// remplacer les continuations explicites par Continue (fun (...))
// avantage par rapport au CPS : ça ne bouffe pas de stack, ça marche sans proper tail call par le compileur
// désavantage : les tails calls coutent 2 à 10 fois plus cher                   

let go (cont:'b->'r response)(acc:'b):'r response = Continue (fun()-> cont acc) // cette fonction est fondamentale

let rec tree_mid_fold_t (tree:Tree<'T>) (f_t:'T->'b->('b->'r response)->'r response)(acc:'b) (cont:'b->'r response):'r response=
     
     match tree with
       | Tip s -> go cont acc
       | Tree (n,l,r) ->   // (tree_mid_fold_c l f_c acc cont1) //type 'r
                           //  tree_mid_fold_c l f_c acc est une évaluation partielle de type 'b
                           //  la continuation prend ce résultat et lui applique une fonction
                           //  c'est à dire ce qu'on fait de ce résultat
                           // (fun (acc:'b) -> (tree_mid_fold_c l f_c acc cont)) //type ('b->'r) 
                               
                               // on regarde d'abord la définition de z qu'on calcule en dernier
                               // let cont1 = Continue (fun () -> tree_mid_fold_t r f_t res cont)
                               //           (fun res -> tree_mid_fold_c r f_c res cont) // 'b->'r
                                let cont1 = go (fun res -> tree_mid_fold_t r f_t res cont) //  évaluation partielle !!!!
                               // c'est ce qu'on calcule en dernier donc on met cont ici
                               // puis on remonte à la définition de y
                               // let cont2 = (fun res -> f_c n res cont1)
                                let cont2 = go (fun res -> f_t n res cont1)
                               // tree_mid_fold_c l f_c acc cont2
                                let z= go (fun res-> tree_mid_fold_t l f_t res cont2) acc // et on a la bonne continuation à appliquer à x
                                z

let myf_t (nodeV:'T) (list:list<'T>) (cont:list<'T>->'r) :'r = cont (nodeV::list)
                                      
let z3 = trampoline (Continue (fun() -> tree_mid_fold_t (tree5) (myf_t) [] (fun acc-> Done acc)   ))
printfn "tree_mid_fold trampo %A" z3 |> ignore           

///////////////////// trampoline iter /////////////////////

// ci-dessous, on matche plusieurs fois, c'est moche !!!

let isDone computation =
   match computation with
     | Done _     -> true
     | Continue _ -> false

let trampoline_iter computation =
    let state = ref computation in
    while not(isDone !state) do
       match !state with
         | Done _ -> ()
         | Continue more -> state := more()
    match !state with
      | Done x -> x
      | _ -> failwith "foo"

let z4 = trampoline_iter (Continue (fun() -> tree_mid_fold_t (tree5) (myf_t) [] (fun acc-> Done acc)   ))
printfn "tree_mid_fold trampo %A" z4 |> ignore 

////////////////////////////////////////////////

let FoldTree nodeF leafV tree =   
    let rec Loop t cont =   
        match t with   
        | Tree (x,left,right) -> Loop left  (fun lacc ->    
                                Loop right (fun racc ->   
                                cont (nodeF x lacc racc)))   
        | Tip s -> cont leafV   
    Loop tree (fun x -> x)     

// acc est une liste de continuation (fonctions à appliquer à un arbre)
// l et r sont des fonctions sur ces listes
// x est une continuation à appliquer sur un noeud)
 
let InOrder t = (FoldTree (fun x l r acc -> l (x :: (r acc))) (fun acc -> acc) t) [] 
printfn "InOrder %A" (InOrder tree5)

// this is O(n^2) very bad !!
let rec flatten (tree:Tree<'T>) =  // in-order traversal
    match tree with 
      | Tip s -> []
      | Tree (s,ltree,rtree) -> List.append (flatten ltree)(s::flatten rtree)

let rec sizeOfTree tree =
   match tree with
     | Tree(_, l, r) -> 1 + sizeOfTree l + sizeOfTree r
     | Tip _ -> 1

let IsTip (tree:Tree<'T>)=
    match tree with 
      | Tip s -> true
      | Tree (node,l,r) -> false

let rec step (parents: list<Tree<'T>>, leftres:'T list) =
    let (bst:Tree<'T>) = List.head parents
    match bst with 
    | Tree (node,l,r) ->  match leftres with
                          | [] ->  let left =  List.append (flatten1 l)(node::(flatten1 r))                 
                                   (List.tail parents, left)
                          | _  ->  let left = List.append (leftres) (node::(flatten1 r))
                                   (List.tail parents, left)
    | _ -> failwith "it should not happen. bad algo"


and flatten1(bst:Tree<'T>) :'T list  = 
    let mutable left = []
    let mutable parents = []
    let mutable z = (parents,left)
    let mutable b = bst
    // create stack of parents
    let rec create_parents (bst:Tree<'T>)(acc:list<Tree<'T>>)=
        match bst with
                      |Tree (node,l,r) -> create_parents l (bst::acc)
                      |Tip s           -> acc

    let mutable parents = create_parents bst []
    // iterate to compute results
    while not(parents=[]) do
           (z <- step (parents,left)) |>ignore
           (parents <- fst z) |> ignore
           (left  <- snd z) |> ignore
    left

let z2 = flatten1 tree5 
printfn "%A" z2
Console.ReadKey() |> ignore

//let rec FoldBack (combine:'a->'b->'b) (l:'a list) (acc:'b) :'b =
//  match l with
//    | h :: t -> combine h (FoldBack combine t acc)
//    | [] -> acc

let FoldBack combine l acc =
// Loop : list<'a> -> ('b -> 'c) -> 'c
   let rec Foldback_c l (cont:'b->'r) :'r =
     match l with
       | h :: t -> Foldback_c t (fun racc -> cont (combine h racc)) //en fait c'est combine_c. combine est soumis à la taille de la pile
       | [] -> cont acc
   Foldback_c l (fun x -> x)

 // pour transformer une fonction ('A1->'A2->'A3->...'An->'B)
 // ca devient ('A1->'A2->'A3->...'An->('B->'r)->'r)

let g = FoldBack (fun x s-> s+1) [1;2;3;4;5] 0
printfn "Foldback %A" g |> ignore