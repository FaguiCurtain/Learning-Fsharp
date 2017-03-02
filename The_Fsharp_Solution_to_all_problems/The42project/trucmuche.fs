module WrappedString = 

    /// An interface that all wrapped strings support
    type IWrappedString = 
        abstract Value : string

    /// Create a wrapped value option
    /// 1) canonicalize the input first
    /// 2) If the validation succeeds, return Some of the given constructor
    /// 3) If the validation fails, return None
    /// Null values are never valid.
    let create canonicalize isValid ctor (s:string) = 
        if s = null 
        then None
        else
            let s' = canonicalize s
            if isValid s'
            then Some (ctor s') 
            else None

    /// Apply the given function to the wrapped value
    let apply f (s:IWrappedString) = 
        s.Value |> f 

    /// Get the wrapped value
    let value s = apply id s

    /// Equality test
    let equals left right = 
        (value left) = (value right)
        
    /// Comparison
    let compareTo left right = 
        (value left).CompareTo (value right)


    // ... code from above ...

    /// Canonicalizes a string before construction
    /// * converts all whitespace to a space char
    /// * trims both ends
    let singleLineTrimmed s =
        System.Text.RegularExpressions.Regex.Replace(s,"\s"," ").Trim()

    /// A validation function based on length
    let lengthValidator len (s:string) =
        s.Length <= len 

    /// A string of length 100
    type String100 = String100 of string with
        interface IWrappedString with
            member this.Value = let (String100 s) = this in s

    /// A constructor for strings of length 100
    let string100 = create singleLineTrimmed (lengthValidator 100) String100 

    /// Converts a wrapped string to a string of length 100
    let convertTo100 s = apply string100 s

    /// A string of length 50
    type String50 = String50 of string with
        interface IWrappedString with
            member this.Value = let (String50 s) = this in s

    /// A constructor for strings of length 50
    let string50 = create singleLineTrimmed (lengthValidator 50)  String50

    /// Converts a wrapped string to a string of length 50
    let convertTo50 s = apply string50 s

    /// map helpers
    let mapAdd k v map = 
        Map.add (value k) v map    

    let mapContainsKey k map =  
        Map.containsKey (value k) map    

    let mapTryFind k map =  
        Map.tryFind (value k) map



let tryStrToInt str = 
    try Some (int str)
    with | :? System.FormatException -> None
      

type LoggingBuilder() =
    let log p = printfn "expression is %A" p

    member this.Bind(x, f) = 
        log x
        f x

    member this.Return(x) = 
        x


type MaybeBuilder() =
    member this.Bind(m, f) = printfn "expression is %A" m; Option.bind f m
    member this.Return(x) = Some x

         
let logger = new MaybeBuilder()


let stringAddWorkflow x y z = 
    logger 
        {
        let! a = tryStrToInt x
        let! b = tryStrToInt y
        let! c = tryStrToInt z
        return a + b + c
        }    

// test
let good = stringAddWorkflow "12" "3" "2"
let bad = stringAddWorkflow "12" "xyz" "2"

// part 2

let tryStrAdd str i = 
    match (tryStrToInt str) with
      | Some k -> (Some (k+i))
      | _ -> None

let (>>=) m f = printfn "expression is %A" m; Option.bind f m

let good2 = tryStrToInt "1" >>= tryStrAdd "2" >>= tryStrAdd "3"
let bad2  = tryStrToInt "1" >>=  tryStrAdd "xyz" >>= tryStrAdd "3"
   

//

type DbResult<'a> = 
    | Success of 'a
    | Error of string

type CustomerId =  CustomerId of string
type OrderId =  OrderId of int
type ProductId =  ProductId of string

let getCustomerId name =
    if (name = "") 
    then Error "getCustomerId failed"
    else Success (CustomerId "Cust42")

let getLastOrderForCustomer (CustomerId custId) =
    if (custId = "") 
    then Error "getLastOrderForCustomer failed"
    else Success (OrderId 123)

let getLastProductForOrder (OrderId orderId) =
    if (orderId  = 0) 
    then Error "getLastProductForOrder failed"
    else Success (ProductId "Product456")

type DbResultBuilder() =

    member this.Bind(m, f) = 
        match m with
        | Error e -> Error e
        | Success a -> 
            printfn "\tSuccessful: %A" a
            f a

    member this.Return(x) = 
        Success x

let dbresult = new DbResultBuilder()

let product' = 
    dbresult {
        let! custId = getCustomerId "Alice"
        let! orderId = getLastOrderForCustomer custId
        let! productId = getLastProductForOrder orderId 
        printfn "Product is %A" productId
        return productId
        }
printfn "%A" product'

type MaybeBuilder1() =
    member this.Bind(m, f) = Option.bind f m
    member this.Return(x) = 
        printfn "Wrapping a raw value into an option"
        Some x
    member this.ReturnFrom(m) = 
        printfn "Returning an option directly"
        m

let maybe = new MaybeBuilder1()

let divideBy bottom top =
    if bottom = 0
    then None
    else Some(top/bottom)

let a = maybe 
          {
           let! x = 12 |> divideBy 3
           return! x |> divideBy 2  // return an Option
           }    



type StringIntBuilder() =

    member this.Bind(m, f) = 
        let b,i = System.Int32.TryParse(m)
        match b,i with
        | false,_ -> "error"
        | true,i -> f i

    member this.Return(x) = 
        sprintf "%i" x

let stringint = new StringIntBuilder()

let good = 
    stringint {
        let! i = "42"
        let! j = "43"
        return i+j
        }
printfn "good=%s" good

let bad = 
    stringint {
        let! i = "42"
        let! j = "xxx"
        return i+j
        }
printfn "bad=%s" bad

let b1 = "xxx"
let b2 = stringint {
            let! i = b1
            return i
            }
printfn "b1=%s b2=%s" b1 b2

let bind(list,f) =
    list 
    |> List.map f 
    |> List.concat

let added = 
    bind( [1;2;3], fun elem1 -> 
    bind( [10;11;12], fun elem2 -> 
//       elem1 + elem2    // error. 
        [elem1 + elem2]   // correctly returns a list.
    ))

type ListWorkflowBuilder() =

    member this.Bind(list, f) = 
        list |> List.collect f 
    
    member this.Return(x) = 
        [x]

let listWorkflow = new ListWorkflowBuilder()

let added = 
    listWorkflow {
        let! i = [1;2;3]
        let! j = [10;11;12]
        return i+j
        }
printfn "added=%A" added

let multiplied = 
    listWorkflow {
        let! i = [1;2;3]
        let! j = [10;11;12]
        return i*j
        }
printfn "multiplied=%A" multiplied 


let prices =
  [ 26.24,25.80,26.22,25.95; 26.40,26.18,26.26,26.20
    26.37,26.04,26.11,26.08; 26.78,26.15,26.60,26.16
    26.86,26.51,26.69,26.58; 26.95,26.50,26.91,26.55
    27.06,26.50,26.64,26.77; 26.86,26.43,26.53,26.59
    27.10,26.52,26.78,26.59; 27.21,26.99,27.13,27.06
    27.37,26.91,26.97,27.21; 27.07,26.60,27.05,27.02
    27.33,26.95,27.04,26.96; 27.27,26.95,27.21,27.23
    27.81,27.07,27.76,27.25; 27.94,27.29,27.93,27.50
    28.26,27.91,28.19,27.97; 28.34,28.05,28.10,28.28
    28.34,27.79,27.80,28.20; 27.84,27.51,27.70,27.77 ]
