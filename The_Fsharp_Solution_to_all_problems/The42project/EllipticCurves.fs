namespace EllipticCurves

open System.Numerics

// Elliptic Curve over Z/nZ of the form y2z = x3 + axz2 + bz3
// reference Cours de Cryptographie MM067 2012-13 (Alain Kraus) chapitre VII Courbes Elliptiques p41- 
// http://www.usthb.dz/fmath/IMG/pdf/Chapitre_7.pdf

module ModularArithmetic =

    let BigZero = BigInteger(0)
    let BigOne  = BigInteger(1)
    let BigTwo  = BigInteger(2)

    // Euclid Algorithm defined in a recursive fashion
    // finds a Bezout relationship a*u + b*v = r 

    let rec eucl (r:int,u:int,v:int,r':int,u':int,v':int) = 

        match r' with 
          | 0 -> (r,u,v)
          | _ ->  let t = (r/r')
                  eucl(r',u',v',r - t*r',u - t*u', v-t*v')

    let euclid(a, b) = eucl(a, 1, 0, b, 0, 1) // outputs (r,u,v)

    let rec eucl_BigInt (r:BigInteger,u:BigInteger,v:BigInteger,r':BigInteger,u':BigInteger,v':BigInteger) =

        if r'.IsZero then (r,u,v)
                     else 
                          let t = (r/r')
                          eucl_BigInt(r',u',v',r - t*r',u - t*u', v-t*v')
    
    let euclid_BigInt(a, b) = eucl_BigInt(a, BigOne, BigZero, b, BigZero, BigOne)

    // modulo in Z/NZ

    let modulo (X,N) = 
        if X>=0 then X % N
                else (X % N) + N
    
    let modulo_BigInt (X,N) = 
        if X >= BigZero then X % N
                        else let tmp = (X % N)
                             if (tmp = BigZero) then BigZero else tmp+N

    // computes the inverse of X modulo N when it exists, i.e. when GCD(X,N) = 1

    let inverse (X,N) =
        let XX = modulo(X,N)
        let (gcd,u,v) = euclid (N,XX)
        match gcd with 
          | 1 -> Some (modulo(v,N))
          | _ -> None

    let inverse_BigInt (X,N) =
        let XX= modulo_BigInt(X,N)
        let (gcd,u,v) = euclid_BigInt (N,XX)
        match gcd.IsOne with 
          | true  -> Some (modulo_BigInt(v,N))
          | false -> None

    let rec BinaryExpansion_BigInt (X:BigInteger) =
        if X = BigInteger(0) then [BigZero]
           else if X = BigInteger(1) then [BigOne]
                   else (X % BigTwo)::BinaryExpansion_BigInt ((X-X%BigTwo)/BigTwo)

    let rec BinaryExpansion (x:int) =
        match x with 
          | 0  -> [0]
          | 1  -> [1]
          | _  -> (x % 2)::BinaryExpansion((x-x%2)/2)

    let rec Factorial (B:BigInteger)=
        if B.IsOne then BigOne
                   else B * (Factorial (B-BigOne))


//////// end of module ///////

module EllipticCurve = 

  open ModularArithmetic

  let BigZero = BigInteger(0)
  let BigOne = BigInteger(1)

  type ProjectivePlane (N:BigInteger) = 
       
       member private p.noninversibleZnZ = 
         let isRelativePrimeWithN k =
             (BigInteger.GreatestCommonDivisor (k,N)).IsOne
              
         seq {for k in BigZero..(N-BigOne) do if isRelativePrimeWithN k = false then yield k}

       member p.points = 
         let seq1 = seq {for x in BigZero..(N-BigOne) do for y in BigZero..(N-BigOne) do yield (x,y,BigOne)}
         let seq2 = seq {for z in p.noninversibleZnZ do for y in BigZero..(N-BigOne) do yield (BigOne,y,z)}
         let seq3 = seq {for x in p.noninversibleZnZ do for z in p.noninversibleZnZ do yield (x,BigOne,z)}
         Seq.concat [seq1;seq2;seq3]
     
       member p.cardinality = Seq.length p.points
       member p.showpoints = for P in p.points do (printfn "%A" P)


  type EllipticCurve_ZnZ (N:BigInteger, a:BigInteger, b:BigInteger) =
       let PP = new ProjectivePlane (N)

       member p.N = N
       member p.A = a
       member p.B = b

       member p.discriminant = BigInteger(4)*a*a*a + BigInteger(27)*b*b

     // makes a list of points naively ! careful, doesn't work for large curves !!!



     

  exception Error of string



  type EllipticCurve_ZnZ_Point (E:EllipticCurve_ZnZ, x:BigInteger,y:BigInteger,z:BigInteger) =
       member p.IsOnCurve = 
         ( modulo_BigInt(y*y*z-x*x*x-E.A * x*z*z - E.B*z*z*z, E.N) ).IsZero
       // member p.IsOnCurve1 = ( (y*y*z-x*x*x-E.A * x*z*z - E.B*z*z*z, E.N) ) //for debugging
       member p.E = E
       member p.X = x
       member p.Y = y
       member p.Z = z


       static member (+) (P1: EllipticCurve_ZnZ_Point,P2:EllipticCurve_ZnZ_Point) =
           try
              if (P1.E.N,P1.E.A,P1.E.B) <> (P2.E.N,P2.E.A,P2.E.B) then raise(Error("Incompatible curves"))
                  else Some (P1.X + P2.X , P1.Y+P2.Y)
           with
               | Error(str) -> printfn "Error : %s" str ; None


  let list_EllipticCurve_ZnZ_points (E:EllipticCurve_ZnZ) = 
      let PP = new ProjectivePlane (E.N)
      PP.points |> Seq.filter (fun (x,y,z) -> let P= new EllipticCurve_ZnZ_Point (E,x,y,z)
                                              P.IsOnCurve)
  
 
  let show_list_EllipticCurve_ZnZ_points (E:EllipticCurve_ZnZ) = 
      let l = list_EllipticCurve_ZnZ_points(E)
      for P in l do (printfn "%A" P)                                        

  let IsInPlane (N,x,y,z) = 
      Seq.exists (fun s -> s = (x,y,z)) (ProjectivePlane(N).points)

  type PseudoAdditionOutput =
      | Divisor of BigInteger
      | PseudoSum of EllipticCurve_ZnZ_Point

  let PseudoAddition (P:EllipticCurve_ZnZ_Point,Q:EllipticCurve_ZnZ_Point):Option<PseudoAdditionOutput> =
    let NN = P.E.N
    let E = P.E
    let a = P.E.A
    let b = P.E.B

    // we should first check that P and Q share the same curve else raise an exception

  // case 1
    if (P.X,P.Y,P.Z)=(BigZero,BigOne,BigZero) then Some (PseudoSum Q)
        else if (Q.X,Q.Y,Q.Z) = (BigZero,BigOne,BigZero) then Some (PseudoSum P)

             else 
                 let fst3 (a,b,c) = a
                 let d = (fst3 (euclid_BigInt (modulo_BigInt(Q.X - P.X,NN),NN)) )
               // case 2
                 if ( (d<>BigOne) && (d<>NN) ) then Some (Divisor d)
                   
                    else
                        // case 3
                        if (d=BigOne) then 
                                          try
                                             // printfn "debug1 %A %A %A" P.X Q.X (modulo_BigInt(P.X-Q.X,NN))
                                             let tmp = inverse_BigInt(P.X-Q.X,NN)
                                             // printfn "tmp=%A" tmp //debug
                                             match tmp with
                                               | None   -> raise(Error("this should not happen 3"))
                                               | Some t -> let lambda = modulo_BigInt((P.Y - Q.Y) * t,NN)
                                                           let nu = modulo_BigInt( (P.X*Q.Y - P.Y*Q.X)*t,NN )
                                                           let tmp1 = modulo_BigInt(lambda*lambda-P.X-Q.X,NN)
                                                           let R = new EllipticCurve_ZnZ_Point(E,tmp1,modulo_BigInt(-lambda*tmp1-nu,NN),BigOne)
                                                           Some (PseudoSum R)
                                          with 
                                               | Error(str) -> printfn "Error : %s" str ; None
                                      else 
                                      // case 4 d=n , i.e. P.X = Q.X
                                          let d' = (fst3 (euclid_BigInt (P.Y + Q.Y,NN)) )
                                          // case 4.1
                                          if ( (d'<>BigOne) && (d'<>NN)) then Some (Divisor d')
                                             else
                                                 // case 4.2 d'=n , i.e. P.Y = -Q.Y
                                                 if (d'=NN) then let R = new EllipticCurve_ZnZ_Point(E,BigZero,BigOne,BigZero)
                                                                 Some (PseudoSum R)
                                                            else
                                                                // case 4.3 d'=1 P = Q

                                                                try 
                                                                   let tmp2 = inverse_BigInt(P.Y+P.Y,NN)
                                                                   match tmp2 with 
                                                                     | None -> raise(Error("this should not happen 4"))
                                                                     | Some tmp2 -> let lambda = modulo_BigInt( (BigInteger(3)*P.X*P.X+a)*tmp2, NN )                                                                
                                                                                    let nu = modulo_BigInt( (-P.X*P.X*P.X+a*P.X+BigInteger(2)*b)*tmp2,NN)
                                                                                    let tmp3 = modulo_BigInt(lambda*lambda-P.X-P.X,NN)
                                                                                    let R = new EllipticCurve_ZnZ_Point(E,tmp3,modulo_BigInt(-lambda*tmp3-nu,NN),BigOne)
                                                                                    Some (PseudoSum R)
                                                                with 
                                                                     | Error(str) -> printfn "Error : %s" str ; None

  
  let PseudoMultiple_helper (P:EllipticCurve_ZnZ_Point,k:int) =
      let E = P.E
      let O = new EllipticCurve_ZnZ_Point(E,BigZero,BigOne,BigZero)
      let ans = [|for i in 0..k do yield (Some (PseudoSum O))|]
      ans.[0] <- Some (PseudoSum P)
      let mutable ok = true
      let mutable i = 1  
      let mutable divisor = BigZero

      while ((i<=k) && ok) do
          let res = ans.[i-1]
          match res with
            | Some (Divisor d)   -> (ok <- false) ; divisor <- d
            | Some (PseudoSum Q) -> (ans.[i] <- PseudoAddition(Q,Q) ) ; i<-i+1
            | None -> raise(Error("this should not happen 5"))
      (ans,divisor)
   
  let PseudoMultiple (P:EllipticCurve_ZnZ_Point,m:BigInteger) =
      let BinaryExpansion_m = BinaryExpansion_BigInt(m)
      let k = BinaryExpansion_m |> List.length
      let E = P.E
      let O = new EllipticCurve_ZnZ_Point(E,BigZero,BigOne,BigZero)

      let (ans,d) = PseudoMultiple_helper(P,k)

      if (d <> BigZero) then Some (Divisor d)
         else
             let mutable i = 0
             let mutable acc = Some (PseudoSum O)
             let mutable ok = true
             let mutable divisor = BigZero
             let res = ans |> Array.map ( fun s -> match s with 
                                                    | Some (PseudoSum P) -> P
                                                    | Some (Divisor d) -> raise(Error("this should not happen 7a"))
                                                    | None -> raise(Error("this should not happen 7b"))
                                                    )
             while ((i<k) && ok) do
                 if (BinaryExpansion_m.[i]=BigOne) then  
                                                        match acc with
                                                          | Some (Divisor d) -> (ok <- false )
                                                          | Some (PseudoSum Q) -> (acc<- PseudoAddition(Q,res.[i]);(i<-i+1))
                                                          | None -> raise(Error("this should not happen 6"))
                                                   else (i<- i+1)
             acc

  let rec PseudoFactorialMultiple (P:EllipticCurve_ZnZ_Point,B:BigInteger) =
      if (B.IsOne) then Some (PseudoSum P)
                   else let tmp = PseudoFactorialMultiple (P,B-BigOne)
                        match tmp with
                          | Some (Divisor d) -> tmp
                          | Some (PseudoSum Q) -> PseudoMultiple(Q,B)
                          | None -> raise(Error("this should not happen 8"))

    


                                             

    

                      

