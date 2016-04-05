namespace FSharp.Numerics
 
type IntegerZ5 = 
  | Z5 of int
  member z.ToInt32() =   
    let (Z5 n) = z in n
  override z.ToString() = 
    sprintf "%d (mod 5)" (z.ToInt32())

  static member Create(n) = 
    let z5 = n % 5
    Z5(max ((z5 + 5) % 5) z5)

// let Z = IntegerZ5.Create(423)
// let z = Z.ToInt32();;