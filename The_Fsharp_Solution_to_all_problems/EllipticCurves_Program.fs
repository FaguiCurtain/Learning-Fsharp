// pseudo addition example

open System.Numerics
open EllipticCurves.EllipticCurve

[<EntryPoint>]
let main args =
    printfn "Arguments passed to function : %A" args


    let BigZero = BigInteger (0)
    let BigOne  = BigInteger (1)
    let E = new EllipticCurve_ZnZ (BigInteger(77),BigOne,BigOne)

    let P      = new EllipticCurve_ZnZ_Point (E,BigZero,BigOne, BigOne)
    let TwoP   = new EllipticCurve_ZnZ_Point (E,BigInteger(58),BigInteger(47), BigOne)
    let ThreeP = new EllipticCurve_ZnZ_Point (E,BigInteger(72),BigInteger(72), BigOne)
    let SixP   = new EllipticCurve_ZnZ_Point (E,BigInteger(0 ),BigInteger(43), BigOne)
    let FourP  = new EllipticCurve_ZnZ_Point (E,BigInteger(28),BigInteger(27), BigOne)
    let EightP = new EllipticCurve_ZnZ_Point (E,BigInteger(44),BigInteger(23), BigOne)

    let print_result (B:EllipticCurve_ZnZ_Point)=
        printf "result=%A %A %A" B.X B.Y B.Z
    
    print_result EightP

    0
    // Return 0. This indicates success.
