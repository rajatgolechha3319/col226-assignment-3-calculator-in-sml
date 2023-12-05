signature BIGINT =
sig
    type bigint   
    val pandas : char list * char * bool -> bool
    val red_panda : char list * char list -> string * string
    val convert_back   : int list -> string
    val dec_back   : int list * int list * int list -> int list * int list
    val str_to_deci   : string -> bool * int list * int list * int list
    val convert_to_dec  : int list * int list -> int list * int list * int list
    val make_bg : bool * int list -> bigint
    val break_bg : bigint -> bool * int list
    val add : bigint * bigint -> bigint
    val sub : bigint * bigint -> bigint
    val mul : bigint * bigint -> bigint
    val divb : bigint * bigint -> bigint
    val modb : bigint * bigint -> bigint
    val quot : bigint * bigint -> bigint
    val rem : bigint * bigint -> bigint
    val eq : bigint * bigint -> bool
    val lt  : bigint * bigint -> bool
    val le : bigint * bigint -> bool
    val gt  : bigint * bigint -> bool
    val ge : bigint * bigint -> bool
    val neg : bigint -> bigint
    val jly : bigint -> bigint
    val abs : bigint -> bigint
    val min : bigint * bigint -> bigint
    val max : bigint * bigint -> bigint
    val sign : bigint -> int
    val sameSign : bigint * bigint -> bool
    val toString : bigint -> string
    val fromString : string -> bigint option
    val reduce : bigint * bigint -> bigint * bigint
end
(*False = Negative and True = Positive *)
structure BigInt : BIGINT=
struct
    type bigint = bool * int list
    fun length nil = 0 | length (x::xs) = 1 + (length xs); (*Function to calculate
  the length of a given list*)
local

    fun reverse nil = nil | reverse (x::xs) = (reverse xs) @ [x];(*Function to
      reverse a given list*)

    fun add_zero ( l1 , x) =
        if x <= 0 then l1
          else add_zero ([0]@l1,x-1);(*Function to add zeros to a given list*)

    fun ninji ( l1 , x) =
        if x <= 0 then l1
          else ninji ([9]@l1,x-1);(*Function to add nines to a given list*)

    fun ninja(x) = ninji([],x);(*Function to add nines to a given list*)

    fun ldngzrorem( v1 : int list) = (*Function to remove leading zeros from a given int list*)
      if length(v1) <= 1 then v1
      else if hd(v1)=0 then ldngzrorem(tl(v1))
      else v1;

    fun subtr ( v1:int list, v2:int list ,v3:int list , n: int, carry: int)
      =(*Function to subtract two given int lists*)
            if n=0 then v3
            else
              if hd(v1)-hd(v2)-carry < 0 then
                subtr(tl(v1),tl(v2),[10+hd(v1)-hd(v2)-carry]@v3,n-1,1)
              else
                subtr(tl(v1),tl(v2),[hd(v1)-hd(v2)-carry]@v3,n-1,0);

    fun sub_list( v1 , v2 ) =(*Function to subtract two given int lists*)
      ldngzrorem(subtr(reverse(v1),reverse(add_zero( v2,length(v1)-length(v2))),[],length(v1),0));

    fun addr (v1: int list, v2: int list, v3: int list, n:int, carry: int)
      =(*Function to add two given int lists*)
            if n=0 then v3
            else
              if hd(v1)+hd(v2)+carry > 9 then
                addr(tl(v1),tl(v2),[hd(v1)+hd(v2)+carry-10]@v3,n-1,1)
              else
                addr(tl(v1),tl(v2),[hd(v1)+hd(v2)+carry]@v3,n-1,0);

    fun addlist (v1: int list, v2: int list) =(*Function to add two given int lists*)
      if length(v1) > length(v2) then
            ldngzrorem(addr(reverse(add_zero((v1),1)),reverse(add_zero((v2),length(v1)-length(v2)+1)),[],length(v1)+1,0))
      else
             ldngzrorem(addr(reverse(add_zero((v2),1)),reverse(add_zero((v1),length(v2)-length(v1)+1)),[],length(v2)+1,0));

    fun multplr( v1: int list, v2: int list, x: int, n:int, carry: int) =(*Function to multiply a given int list with an integer*)
      if x=0 then [carry]@v2
      else
       multplr (tl(v1),[(hd(v1)*n+carry) mod 10]@v2,x-1,n,(hd(v1)*n+carry) div 10);


    fun multiplier( v1: int list, n: int) =(*Function to multiply a given int list with an integer*)

      ldngzrorem(multplr(reverse(v1),[],length(v1),n,0));

    fun gte (v1: int list, v2:int list)=(*Function to compare two given int lists*)
      if length(v1) > length(v2) then true
      else if length(v1) < length(v2) then false
      else if length(v1)=1 andalso hd(v1)>= hd(v2) then true
      else if length(v1)=1 andalso hd(v1) < hd(v2) then false
      else if hd(v1) > hd(v2) then true
      else if hd(v1) < hd(v2) then false
      else gte(tl(v1),tl(v2));
    fun convert (xi: char list, n: int, y:int list)=(*Function to convert a given string to an int list*)
            if n=0 then reverse(y)
            else
            let val x = hd(xi)
            in
             convert(tl(xi),n-1,[ord x - ord #"0"] @ y)
            end;
    fun converter(x : string)=convert(explode(x),length(explode(x)),[]);(*Function to convert a given string to an int list*)
    fun converter_back(x: int list,y: int,z : char list)=(*Function to convert a given int list to a string*)
      if y=0 then implode(reverse(z))
      else
        let val xi = hd(x)
        in
          converter_back(tl(x),y-1,[chr (48+xi)]@z)
        end;
    fun convert_back( x: int list)=converter_back(x,length(x),[]);(*Function to convert a given int list to a string*)
    fun g_e (v1: int list,v2:int list)=gte(ldngzrorem(v1),ldngzrorem(v2));(*Function to compare two given int lists*)
    fun mult2int( x1: int list, x2: int list, y1: int list, z:int )=(*This function multiplies two integers*)
        if length(x2)=0 then ldngzrorem(y1)
        else mult2int(x1,tl(x2),addlist(y1,reverse(add_zero(reverse(multiplier(x1,hd(x2))),z))),z+1);

    fun multint(x1: int list, x2: int list)=mult2int(x1,reverse(x2),[],0);(*This function multiplies two integers*)
    fun div2int( dividend: int list, divisor: int list, quotient: int list, remainder:int list )=
        if length(dividend)=0 then (ldngzrorem(quotient),ldngzrorem(remainder))
        else
            let
                val rem = remainder@[hd(dividend)]
            in
                if g_e(rem,multiplier(divisor,9)) then div2int(tl(dividend),divisor,quotient@[9],sub_list(rem,multiplier(divisor,9)))
                else
                    if g_e(rem,multiplier(divisor,8)) then div2int(tl(dividend),divisor,quotient@[8],sub_list(rem,multiplier(divisor,8)))
                    else
                        if g_e(rem,multiplier(divisor,7)) then div2int(tl(dividend),divisor,quotient@[7],sub_list(rem,multiplier(divisor,7)))
                        else
                            if g_e(rem,multiplier(divisor,6)) then div2int(tl(dividend),divisor,quotient@[6],sub_list(rem,multiplier(divisor,6)))
                            else
                                if g_e(rem,multiplier(divisor,5)) then div2int(tl(dividend),divisor,quotient@[5],sub_list(rem,multiplier(divisor,5)))
                                else
                                    if g_e(rem,multiplier(divisor,4)) then div2int(tl(dividend),divisor,quotient@[4],sub_list(rem,multiplier(divisor,4)))
                                    else
                                        if g_e(rem,multiplier(divisor,3)) then div2int(tl(dividend),divisor,quotient@[3],sub_list(rem,multiplier(divisor,3)))
                                        else
                                            if g_e(rem,multiplier(divisor,2)) then div2int(tl(dividend),divisor,quotient@[2],sub_list(rem,multiplier(divisor,2)))
                                            else
                                                if g_e(rem,multiplier(divisor,1)) then div2int(tl(dividend),divisor,quotient@[1],sub_list(rem,multiplier(divisor,1)))
                                                else div2int(tl(dividend),divisor,quotient@[0],rem)
            end;
    fun divint( dividend: int list, divisor: int list)=div2int(dividend,divisor,[],[]);(*This function divides two integers*)

    fun gcd(a : int list, b: int list) = 
        if g_e(b,[0]) andalso g_e([0],b) then a
        else gcd(b, #2(divint(a,b)));

    fun hcf(a : int list, b: int list) = 
        if g_e(a,b) then gcd(a,b)
        else gcd(b,a);

    fun split(x : int list, y: int , z: int list)=
        if y=0 then (z,x)
        else split(tl(x),y-1,z@[hd(x)]);

    fun loop_iterator(v1: int list , v2 : int list list, v3 : int)=
        if length(v2)=0 then ~1
        else 
            if g_e(v1,hd(v2)) andalso g_e(hd(v2),v1) then v3
            else loop_iterator(v1,tl(v2),v3+1);    

    fun loop_converter(v1:int list, v2: int list, v3: int list, v4: int list list)=
        if g_e(v1,[0]) andalso g_e([0],v1) then (v3,[])
        else
            let
                val new_v1 = #2(divint(v1@[0],v2))
                val next_dig = #1(divint(v1@[0],v2))
                val new_res = v3@(#1(divint(v1@[0],v2)))
            in 
                if loop_iterator(new_v1,v4,0) = ~1 then
                    loop_converter(new_v1,v2,new_res,v4@[new_v1])
                else
                    split(new_res,loop_iterator(new_v1,v4,0),[])
            end;

    fun string_break(x : char list, v1 : bool, v2: int list, v3 : int list , v4: int list ,pos : int)=
        if length(x)=0 then
        if length(v4)=0 then (v1,v2,v3,[0])
        else (v1,v2,v3,v4)
        else
            if pos=0 then 
                if hd(x) = #"~" then string_break(tl(x),false,v2,v3,v4,pos+1)
                else string_break(x,true,v2,v3,v4,pos+1)
            else if pos = 1 then 
                if hd(x) = #"." then string_break(tl(x),v1,v2,v3,v4,pos+1)
                else string_break(tl(x),v1,v2@[ord(hd(x))-48],v3,v4,pos)
            else if pos = 2 then
                if hd(x) = #"(" then string_break(tl(x),v1,v2,v3,v4,pos+1)
                else string_break(tl(x),v1,v2,v3@[ord(hd(x))-48],v4,pos)
            else if pos = 3 then
                if hd(x) = #")" then (v1,v2,v3,v4)
                else string_break(tl(x),v1,v2,v3,v4@[ord(hd(x))-48],pos)
            else if length(v4)=0 then (v1,v2,v3,[0])
        else (v1,v2,v3,v4)

in

    fun pandas(v1: char list, v2: char, flag: bool)=
        if length(v1)=0 then flag
        else
            if hd(v1) = v2 then true
            else pandas(tl(v1),v2,flag)

    fun red_panda (s : char list, t: char list)=
        if hd(s) = #"/" then (implode(t),implode(tl(s)))
        else red_panda(tl(s),t@[hd(s)])

    fun convert_back( x: int list)=converter_back(x,length(x),[]);(*Function to convert a given int list to a string*)

    fun dec_back(v1 : int list, v2: int list,v3 : int list)= (*If given the integer part, non recurring part and recurring part of a decimal
    then convert it back to p/q format ! *)
        let
            val new_v1 = sub_list(v1@v2@v3,v1@v2)
            val zee2 = sub_list(addlist(ninja(length(v2)+length(v3)),[1]),addlist(ninja(length(v2)),[1]));
        in 
            if hcf(new_v1,zee2) = [1] then (new_v1,zee2)
            else (#1(divint(new_v1,hcf(new_v1,zee2))),#1(divint(zee2,hcf(new_v1,zee2))))
        end;
    
    fun str_to_deci(x  :string) = string_break(explode(x),true,[],[],[],0);

    fun convert_to_dec(v1 : int list, v2: int list)=
        let
            val quot = #1(divint(v1,v2))
            val rem = #2(divint(v1,v2))
        in
            if g_e(rem,[0]) andalso g_e([0],rem) then (quot,[],[0])
            else
                let
                    val new_res = loop_converter(rem,v2,[],[rem])
                in
                    (quot,#1(new_res),#2(new_res))
                end
        end;
    
    fun make_bg(x: bool, y: int list) : bigint = (x,y);
    
    fun break_bg(x: bigint) : bool * int list =
        let
            val (sign, num) = x
        in
            (sign, num)
        end;

    fun add(v1 : bigint, v2 : bigint) : bigint =
        let
            val (sign1, num1) = v1
            val (sign2, num2) = v2
        in
            if sign1 = sign2 then (sign1, addlist(num1, num2))
            else
                if g_e(num1, num2) then (sign1, sub_list(num1, num2))
                else (sign2, sub_list(num2, num1))
        end
    
    fun sub(v1 : bigint, v2 : bigint) : bigint =
        let
            val (sign1, num1) = v1
            val (sign2, num2) = v2
        in
            if sign1 = sign2 then
                if g_e(num1, num2) then (sign1, sub_list(num1, num2))
                else (not sign1, sub_list(num2, num1))
            else (sign1, addlist(num1, num2))
        end
    
    fun mul(v1 : bigint, v2 : bigint) : bigint =
        let
            val (sign1, num1) = v1
            val (sign2, num2) = v2
        in
            (sign1 = sign2, multint(num1, num2))
        end
    
    fun divb(v1 : bigint, v2 : bigint) : bigint =
        let
            val (sign1, num1) = v1
            val (sign2, num2) = v2
        in
            if (sign1 = sign2) then
                (sign1 = sign2, #1(divint(num1, num2)))
            else
                (false, addlist(#1(divint(num1, num2)), [1]))
        end

    fun modb(v1 : bigint, v2 : bigint) : bigint =
        let
            val (sign1, num1) = v1
            val (sign2, num2) = v2
        in
            if (sign1=sign2) then
                (sign1 , #2(divint(num1, num2)))
            else
                (sign2, sub_list(num2,#2(divint(num1, num2))))
        end
    
    fun quot(v1 : bigint, v2 : bigint) : bigint =
        let
            val (sign1, num1) = v1
            val (sign2, num2) = v2
        in
            if (sign1 = sign2) then
                (sign1 = sign2, #1(divint(num1, num2)))
            else
                (false, addlist(#1(divint(num1, num2)), [1]))
        end

    fun rem(v1 : bigint, v2 : bigint) : bigint =
        let
            val (sign1, num1) = v1
            val (sign2, num2) = v2
        in
            if (sign1=sign2) then
                (sign1 , #2(divint(num1, num2)))
            else
                (sign2, sub_list(num2,#2(divint(num1, num2))))
        end

    fun eq(v1 : bigint, v2 : bigint) : bool =
        let
            val (sign1, num1) = v1
            val (sign2, num2) = v2
        in
            if sign1 = sign2 then
                if sign1 then
                    if g_e(num1, num2) andalso g_e(num2,num1) then true
                    else false
                else
                    if g_e(num1, num2) andalso g_e(num2,num1) then true
                    else false
            else false
        end

    fun lt(v1 : bigint, v2 : bigint) : bool =
        let
            val (sign1, num1) = v1
            val (sign2, num2) = v2
        in
            if sign1 = sign2 then
                if sign1 then
                    if g_e(num1, num2) then false
                    else true
                else
                    if g_e(num1, num2) then true
                    else false
            else
                if sign1 then true
                else false
        end
    
    fun le(v1 : bigint, v2 : bigint) : bool =
        if lt(v1, v2) orelse eq(v1, v2) then true
        else false

    fun gt(v1 : bigint, v2 : bigint) : bool =
        if lt(v1, v2) orelse eq(v1, v2) then false
        else true
    
    fun ge(v1 : bigint, v2 : bigint) : bool =
        if lt(v1, v2) then false
        else true
    
    fun neg(v1 : bigint) : bigint =
        let
            val (sign1, num1) = v1
        in
            (not sign1, num1)
        end
    fun jly(v1 : bigint) : bigint =
        let
            val (sign1, num1) = v1
        in
            (sign1, num1)
        end

    fun abs(v1 : bigint) : bigint =
        let
            val (sign1, num1) = v1
        in
            (true, num1)
        end
    
    fun min(v1 : bigint, v2 : bigint) : bigint =
        if lt(v1, v2) then v1
        else v2
    
    fun max(v1 : bigint, v2 : bigint) : bigint =
        if lt(v1, v2) then v2
        else v1
    
    fun sign(v1 : bigint) : int =
        let
            val (sign1, num1) = v1
        in
            if sign1 then 1
            else if num1 = [] then 0
            else ~1
        end
    
    fun sameSign(v1 : bigint, v2 : bigint) : bool =
        let
            val (sign1, num1) = v1
            val (sign2, num2) = v2
        in
            if sign1 = sign2 then true
            else false
        end
    
    fun toString(v1 : bigint) : string =
        let
            val (sign1, num1) = v1
        in
            if sign1 then
                if num1 = [] then "0"
                else
                    convert_back(num1)
            else
                if num1 = [] then "0"
                else
                    "~" ^ convert_back(num1)
        end    

    fun fromString(s : string) : bigint option =
        let
            val num = explode(s)
        in
            if num = [] then NONE
            else
                if hd(num) = #"~" then
                    if tl(num) = [] then NONE
                    else SOME (false, converter(implode(tl(num))))
                else SOME (true, converter(implode(num)))
        end
    
    fun reduce(v1 : bigint, v2: bigint) = 
        if ldngzrorem(hcf( #2(v1) , #2(v2) )) = [1] then (v1,v2)
        else
            let
                val (sign1, num1) = v1
                val (sign2, num2) = v2
                val hcf1 = hcf(num1, num2)
            in
                ((sign1, #1(divint(num1, hcf1))), (sign2, #1(divint(num2, hcf1))))
            end 
    end
end

signature RATIONAL = 
sig
    type rational
    type bigint
    exception rat_error 
    val make_rat: bigint * bigint -> rational option
    val rat: bigint -> rational option
    val reci: bigint -> rational option
    val neg: rational -> rational
    val inverse : rational -> rational option
    val equal : rational * rational -> bool (* equality *)
    val less : rational * rational -> bool (* less than *)
    val add : rational * rational -> rational (* addition *)
    val subtract : rational * rational -> rational (* subtraction *)
    val multiply  : rational * rational -> rational (* multiplication *)
    val divide : rational * rational -> rational option (* division *)
    val showRat : rational -> string
    val showDecimal : rational -> string
    val fromDecimal : string -> rational
    val toDecimal : rational -> string
end

functor Rational (BigInt : BIGINT) : RATIONAL =
struct
    type bigint = BigInt.bigint
    type rational = bigint * bigint
    exception rat_error
    
    fun make_rat(v1) : rational option =
        let
            val (num1, den1) = BigInt.reduce(v1)
        in
            if BigInt.eq(BigInt.jly(den1),BigInt.make_bg(false,[0])) orelse BigInt.eq(den1,BigInt.make_bg(true,[0])) then NONE
            else
                if BigInt.sign(num1) = BigInt.sign(den1) then
                    SOME (BigInt.abs(num1), BigInt.abs(den1))
                else
                    SOME (BigInt.neg(BigInt.abs(num1)), BigInt.abs(den1))
        end
    
    
    fun rat(v1 : bigint) : rational option =
        make_rat(v1, BigInt.make_bg(true, [1]))
    
    fun reci(v1 : bigint) : rational option =
        make_rat(BigInt.make_bg(true, [1]), v1)
    
    fun neg(v1 : rational) : rational =
        let
            val (num1, den1) = v1
        in
            (BigInt.neg(num1), den1)
        end
    
    fun inverse(v1 : rational) : rational option =
        let
            val (num1, den1) = v1
        in
            make_rat(den1, num1) 
        end

    fun equal(v1 : rational * rational) : bool =
        let
            val (num1, den1) = #1(v1)
            val (num2, den2) = #2(v1)
        in
            if BigInt.eq(BigInt.mul(num1, den2), BigInt.mul(num2, den1)) then true
            else false
        end
    
    fun less(v1 : rational * rational) : bool =
        let
            val (num1, den1) = #1(v1)
            val (num2, den2) = #2(v1)
        in
            if BigInt.lt(BigInt.mul(num1, den2), BigInt.mul(num2, den1)) then true
            else false
        end
    
    fun add (v1 : rational * rational) : rational =
        let
            val (num1, den1) = #1(v1)
            val (num2, den2) = #2(v1)
        in
            valOf(make_rat(BigInt.add(BigInt.mul(num1, den2), BigInt.mul(num2, den1)), BigInt.mul(den1, den2)))
        end
    
    fun subtract(v1 : rational * rational) : rational =
        let
            val (num1, den1) = #1(v1)
            val (num2, den2) = #2(v1)
        in
            valOf(make_rat(BigInt.sub(BigInt.mul(num1, den2), BigInt.mul(num2, den1)), BigInt.mul(den1, den2)))
        end
    
    fun multiply(v1 : rational * rational) : rational =
        let
            val (num1, den1) = #1(v1)
            val (num2, den2) = #2(v1)
        in
            valOf(make_rat(BigInt.mul(num1, num2), BigInt.mul(den1, den2)))
        end
    
    fun divide(v1 : rational * rational) : rational option =
        let
            val (num1, den1) = #1(v1)
            val (num2, den2) = #2(v1)
        in
            (make_rat(BigInt.mul(num1, den2), BigInt.mul(den1, num2)))
        end
    
    fun showRat(v1 : rational) : string =
        let
            val (num1, den1) = v1
        in
            BigInt.toString(num1) ^ "/" ^ BigInt.toString(den1)
        end
    
    fun showDecimal(v1 : rational) : string =
        let
            val (n1, d1) = v1
        in
            let
                val (sign1, num1) = BigInt.break_bg(n1)
                val (sign2, den1) = BigInt.break_bg(d1)
                val (l1,l2,l3) = BigInt.convert_to_dec(#2(BigInt.break_bg(n1)), #2(BigInt.break_bg(d1)))
            in
                if sign1 then 
                    if l3 = [] then "+" ^ BigInt.convert_back(l1) ^ "." ^ BigInt.convert_back(l2) ^ "(" ^ "0" ^ ")"
                    else
                    "+" ^ BigInt.convert_back(l1) ^ "." ^ BigInt.convert_back(l2) ^ "(" ^ BigInt.convert_back(l3) ^ ")"
                else
                    if l3 = [] then "~" ^ BigInt.convert_back(l1) ^ "." ^ BigInt.convert_back(l2) ^ "(" ^ "0" ^ ")"
                    else "~" ^ BigInt.convert_back(l1) ^ "." ^ BigInt.convert_back(l2) ^ "(" ^ BigInt.convert_back(l3) ^ ")"
            end
        end        

    fun fromDecimal(s : string) : rational =

        if BigInt.pandas(explode(s),#"/",false) then 
            let
                val mother_panda = #2(BigInt.red_panda(explode(s),[]))
                val baby_panda = #1(BigInt.red_panda(explode(s),[]))
            in
                valOf(make_rat(valOf(BigInt.fromString(mother_panda)),valOf(BigInt.fromString(baby_panda))))
            end
            
        else
            let
                val sign = #1(BigInt.str_to_deci(s))
                val (num1, den1) = BigInt.dec_back(#2(BigInt.str_to_deci(s)),#3(BigInt.str_to_deci(s)),#4(BigInt.str_to_deci(s)))
            in
                    if sign then valOf(make_rat(BigInt.make_bg(true,num1), BigInt.make_bg(true,den1)))
                    else valOf(make_rat(BigInt.neg(BigInt.make_bg(true,num1)), BigInt.make_bg(true,den1)))
            end
    
    fun toDecimal(v1 : rational) : string =
        let
            val (n1, d1) = v1
        in
            let
                val (sign1, num1) = BigInt.break_bg(n1)
                val (sign2, den1) = BigInt.break_bg(d1)
                val (l1,l2,l3) = BigInt.convert_to_dec(#2(BigInt.break_bg(n1)), #2(BigInt.break_bg(d1)))
            in
                if sign1 then 
                    if l3 = [] then "+" ^ BigInt.convert_back(l1) ^ "." ^ BigInt.convert_back(l2) ^ "(" ^ "0" ^ ")"
                    else
                    "+" ^ BigInt.convert_back(l1) ^ "." ^ BigInt.convert_back(l2) ^ "(" ^ BigInt.convert_back(l3) ^ ")"
                else
                    if l3 = [] then "~" ^ BigInt.convert_back(l1) ^ "." ^ BigInt.convert_back(l2) ^ "(" ^ "0" ^ ")"
                    else "~" ^ BigInt.convert_back(l1) ^ "." ^ BigInt.convert_back(l2) ^ "(" ^ BigInt.convert_back(l3) ^ ")"
            end
        end

end

structure Rational = Rational(BigInt)
