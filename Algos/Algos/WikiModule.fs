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