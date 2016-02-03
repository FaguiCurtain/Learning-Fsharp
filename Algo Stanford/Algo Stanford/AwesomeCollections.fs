(* AwesomeCollections.fs *)
namespace AwesomeCollections

type 'a stack =
    | EmptyStack
    | StackNode of 'a * 'a stack
        
module Stack =        
    let hd = function
        | EmptyStack -> failwith "Empty stack"
        | StackNode(hd, tl) -> hd
        
    let tl = function
        | EmptyStack -> failwith "Emtpy stack"
        | StackNode(hd, tl) -> tl
        
    let cons hd tl = StackNode(hd, tl)
    
    let empty = EmptyStack
        
    let rec rev s =
        let rec loop acc = function
            | EmptyStack -> acc
            | StackNode(hd, tl) -> loop (StackNode(hd, acc)) tl
        loop EmptyStack s

type Queue<'a>(f : stack<'a>, r : stack<'a>) =
    let check = function
        | EmptyStack, r -> Queue(Stack.rev r, EmptyStack)
        | f, r -> Queue(f, r)

    member this.hd =
        match f with
        | EmptyStack -> failwith "empty"
        | StackNode(hd, tl) -> hd
        
    member this.tl =
        match f, r with
        | EmptyStack, _ -> failwith "empty"
        | StackNode(x, f), r -> check(f, r)
        
    member this.enqueue(x) = check(f, StackNode(x, r))
    
    static member empty = Queue<'a>(Stack.empty, Stack.empty)

type color = R | B    
type 'a tree =
    | E
    | T of color * 'a tree * 'a * 'a tree
    
module Tree =
    let hd = function
        | E -> failwith "empty"
        | T(c, l, x, r) -> x
        
    let left = function
        | E -> failwith "empty"
        | T(c, l, x, r) -> l
    
    let right = function
        | E -> failwith "empty"
        | T(c, l, x, r) -> r
        
    let rec exists item = function
        | E -> false
        | T(c, l, x, r) ->
            if item = x then true
            elif item < x then exists item l
            else exists item r
            
    let balance = function                              (* Red nodes in relation to black root *)
        | B, T(R, T(R, a, x, b), y, c), z, d            (* Left, left *)
        | B, T(R, a, x, T(R, b, y, c)), z, d            (* Left, right *)
        | B, a, x, T(R, T(R, b, y, c), z, d)            (* Right, left *)
        | B, a, x, T(R, b, y, T(R, c, z, d))            (* Right, right *)
            -> T(R, T(B, a, x, b), y, T(B, c, z, d))
        | c, l, x, r -> T(c, l, x, r)
    
    let insert item tree =
        let rec ins = function
            | E -> T(R, E, item, E)
            | T(c, a, y, b) as node ->
                if item = y then node
                elif item < y then balance(c, ins a, y, b)
                else balance(c, a, y, ins b)

        (* Forcing root node to be black *)                
        match ins tree with
            | E -> failwith "Should never return empty from an insert"
            | T(_, l, x, r) -> T(B, l, x, r)
            
    let rec print (spaces : int) = function
        | E -> ()
        | T(c, l, x, r) ->
            print (spaces + 4) r
            printfn "%s %A%A" (new System.String(' ', spaces)) c x
            print (spaces + 4) l
    
type BinaryTree<'a when 'a : comparison> (inner : 'a tree) =
    member this.hd = Tree.hd inner
    member this.left = BinaryTree(Tree.left inner)
    member this.right = BinaryTree(Tree.right inner)
    member this.exists item = Tree.exists item inner
    member this.insert item = BinaryTree(Tree.insert item inner)
    member this.print() = Tree.print 0 inner
    static member empty = BinaryTree<'a>(E)

type 'a heap =
    | EmptyHeap
    | HeapNode of int * 'a * 'a heap * 'a heap
 
module Heap =
    let height = function
        | EmptyHeap -> 0
        | HeapNode(h, _, _, _) -> h
 
    (* Helper function to restore the leftist property *)        
    let makeT (x, a, b) =
        if height a >= height b then HeapNode(height b + 1, x, a, b)
        else HeapNode(height a + 1, x, b, a)
 
    let rec merge comparer = function
        | x, EmptyHeap -> x
        | EmptyHeap, x -> x
        | (HeapNode(_, x, l1, r1) as h1), (HeapNode(_, y, l2, r2) as h2) ->
            if comparer x y <= 0 then makeT(x, l1, merge comparer (r1, h2))
            else makeT (y, l2, merge comparer (h1, r2))
 
    let hd = function
        | EmptyHeap -> failwith "empty"
        | HeapNode(h, x, l, r) -> x
 
    let tl comparer = function
        | EmptyHeap -> failwith "empty"
        | HeapNode(h, x, l, r) -> merge comparer (l, r)
        
    let rec to_seq comparer = function
        | EmptyHeap -> Seq.empty
        | HeapNode(h, x, l, r) as node -> seq { yield x; yield! to_seq comparer (tl comparer node) }
 
type 'a BinaryHeap(comparer : 'a -> 'a -> int, inner : 'a heap) =
    (* private *)
    member this.inner = inner
 
    (* public *)
    member this.hd = Heap.hd inner
    member this.tl = BinaryHeap(comparer, Heap.tl comparer inner)
    member this.merge (other : BinaryHeap<_>) = BinaryHeap(comparer, Heap.merge comparer (inner, other.inner))
    member this.insert x = BinaryHeap(comparer, Heap.merge comparer (inner,(HeapNode(1, x, EmptyHeap, EmptyHeap))))
    
    interface System.Collections.Generic.IEnumerable<'a> with
        member this.GetEnumerator() = (Heap.to_seq comparer inner).GetEnumerator()
            
    interface System.Collections.IEnumerable with
        member this.GetEnumerator() = (Heap.to_seq comparer inner :> System.Collections.IEnumerable).GetEnumerator()
 
    static member make(comparer) = BinaryHeap<_>(comparer, EmptyHeap)

type 'a lazyStack =
    | Node of Lazy<'a * 'a lazyStack>
    | EmptyStack
 
module LazyStack =
    let (|Cons|Nil|) = function
        | Node(item) ->
            let hd, tl = item.Force()
            Cons(hd, tl)
        | EmptyStack -> Nil
 
    let hd = function
        | Cons(hd, tl) -> hd
        | Nil -> failwith "empty"
 
    let tl = function
        | Cons(hd, tl) -> tl
        | Nil -> failwith "empty"
 
    let cons(hd, tl) = Node(lazy(hd, tl))
    
    let empty = EmptyStack
 
    let rec append x y =
        match x with
        | Cons(hd, tl) -> Node(lazy(printfn "appending... got %A" hd; hd, append tl y))
        | Nil -> y
 
    let rec iter f = function
        | Cons(hd, tl) -> f(hd); iter f tl
        | Nil -> ()