(* AwesomeCollections.fsi *)
namespace AwesomeCollections

type 'a stack =
  | EmptyStack
  | StackNode of 'a * 'a stack
  
module Stack = begin
  val hd : 'a stack -> 'a
  val tl : 'a stack -> 'a stack
  val cons : 'a -> 'a stack -> 'a stack
  val empty : 'a stack
  val rev : 'a stack -> 'a stack
end

[<Class>]
type 'a Queue =
    member hd : 'a
    member tl : 'a Queue
    member enqueue : 'a -> 'a Queue
    static member empty : 'a Queue

[<Class>]
type BinaryTree<'a when 'a : comparison> =
    member hd : 'a
    member left : 'a BinaryTree
    member right : 'a BinaryTree
    member exists : 'a -> bool
    member insert : 'a -> 'a BinaryTree
    member print : unit -> unit
    static member empty : 'a BinaryTree

//[<Class>]
//type 'a AvlTree =
//    member Height : int
//    member Left : 'a AvlTree
//    member Right : 'a AvlTree
//    member Value : 'a
//    member Insert : 'a -> 'a AvlTree
//    member Contains : 'a -> bool
//
//module AvlTree =
//    [<GeneralizableValue>]
//    val empty<'a> : AvlTree<'a>

[<Class>]
type 'a BinaryHeap =
    member hd : 'a
    member tl : 'a BinaryHeap
    member insert : 'a -> 'a BinaryHeap
    member merge : 'a BinaryHeap -> 'a BinaryHeap
    interface System.Collections.IEnumerable
    interface System.Collections.Generic.IEnumerable<'a>
    static member make : ('b -> 'b -> int) -> 'b BinaryHeap