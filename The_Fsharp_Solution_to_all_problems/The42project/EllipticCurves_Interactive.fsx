// For F# interactive

let BigZero = BigInteger (0)
let BigOne  = BigInteger(1)
let BigFour = BigInteger(4)
let Big5 = BigInteger(5)
let Big25 = BigInteger(25)

let E  = new EllipticCurve_ZnZ (Big25,BigZero, BigOne)
let E1 = new EllipticCurve_ZnZ (BigFour,BigZero, BigOne)

let P1 = new EllipticCurve_ZnZ_Point (E,BigZero,BigOne, BigOne)
let P2 = new EllipticCurve_ZnZ_Point (E,BigOne ,BigOne,BigOne)
let P3 = new EllipticCurve_ZnZ_Point (E1,BigOne ,BigOne,BigOne)
let P4 = new EllipticCurve_ZnZ_Point (E,Big5,BigOne,BigOne)

// pseudo addition example

let BigZero = BigInteger (0)
let BigOne  = BigInteger (1)
let E = new EllipticCurve_ZnZ (BigInteger(77),BigOne,BigOne)

let P      = new EllipticCurve_ZnZ_Point (E,BigZero,BigOne, BigOne)
let TwoP   = new EllipticCurve_ZnZ_Point (E,BigInteger(58),BigInteger(47), BigOne)
let ThreeP = new EllipticCurve_ZnZ_Point (E,BigInteger(72),BigInteger(72), BigOne)
let SixP   = new EllipticCurve_ZnZ_Point (E,BigInteger(0 ),BigInteger(43), BigOne)
let FourP  = new EllipticCurve_ZnZ_Point (E,BigInteger(28),BigInteger(27), BigOne)
let EightP = new EllipticCurve_ZnZ_Point (E,BigInteger(44),BigInteger(23), BigOne)

PseudoAddition(P,TwoP)
PseudoAddition(TwoP,P)


// Proving that a number is Prime.
// GoldWasser and Killian algorithm example

let N = BigInteger.Parse("10000000000000000000000013")
let q = BigInteger.Parse("16423310748511")
let m = N+ BigOne
let r = (m/q)
let q_divides_m = (r*q = m)

let x = BigInteger(1000000)
let y = BigInteger.Parse("4518958593766208406366106")

let E  = new EllipticCurve_ZnZ (N,BigZero, BigOne)
let P = new EllipticCurve_ZnZ_Point(E,x,y,BigOne)

PseudoMultiple(P,m) // O point at infinity
PseudoMultiple(P,r) // (6338443046606608613398821, 7712287413141680467591102) is different from O

// Factorization of large numbers with Elliptic Curves (ECM method)
// Example 7.22 (1)

let N= BigInteger.Parse("100000000000000000000000000000000000000000000000003")
let n1 = N/BigInteger(19*97*283)
let n1_divides_N = ((N % n1).IsZero)

let B = BigInteger(1000)
let E = new EllipticCurve_ZnZ(n1,BigInteger(360),BigOne) 
let P = new EllipticCurve_ZnZ_Point(E,BigZero,BigOne,BigOne)
let Bfact = Factorial B

let res = PseudoFactorialMultiple(P,BigInteger(1000))

//val it : PseudoAdditionOutput option =
//   Some (Divisor 994327748569 {IsEven = false;
//                               IsOne = false;
//                               IsPowerOfTwo = false;
//                               IsZero = false;
//                               Sign = 1;})

let p = BigInteger.Parse("994327748569") // p is prime (trust the teacher)
let n2 = n1 / p
let p_divides_n1 = ( (n1 % p).IsZero)

let E' = new EllipticCurve_ZnZ(n2,BigInteger(2016),BigOne) 
let P' = new EllipticCurve_ZnZ_Point(E',BigZero,BigOne,BigOne)
let res' = PseudoFactorialMultiple(P',BigInteger(1000))

// val res' : PseudoAdditionOutput option = Some (Divisor 61236769827829)

let q = BigInteger.Parse("61236769827829")
let q_divides_n2 = ( (n2%q).IsZero)
let r = (n2/q) 
// 3148809563627188687
