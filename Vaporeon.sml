(* Vaporeon.sml *)

(* This file provides glue code for building the calculator using the
 * parser and lexer specified in Vaporeon.lex and Vaporeon.grm.
*)

structure Vaporeon : sig
	           val parse : unit -> unit
                 end = 
struct
 
(* 
 * We apply the functors generated from Vaporeon.lex and Vaporeon.grm to produce
 * the VaporeonParser structure.
 *)

  structure VaporeonLrVals =
    VaporeonLrValsFun(structure Token = LrParser.Token)

  structure VaporeonLex =
    VaporeonLexFun(structure Tokens = VaporeonLrVals.Tokens)

  structure VaporeonParser =
    Join(structure LrParser = LrParser
	 structure ParserData = VaporeonLrVals.ParserData
	 structure Lex = VaporeonLex)

(* 
 * We need a function which given a lexer invokes the parser. The
 * function invoke does this.
 *)

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
	      TextIO.output(TextIO.stdOut,
			    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
       in VaporeonParser.parse(0,lexstream,print_error,())
      end

(* 
 * Finally, we need a driver function that reads one or more expressions
 * from the standard input. The function parse, shown below, does
 * this. It runs the calculator on the standard input and terminates when
 * an end-of-file is encountered.
 *)

  fun parse () = 
      let val lexer = VaporeonParser.makeLexer (fn _ =>
                                               (case TextIO.inputLine TextIO.stdIn
                                                of SOME s => s
                                                 | _ => ""))
	  val dummyEOF = VaporeonLrVals.Tokens.EOF(0,0)
	  val dummySEMI = VaporeonLrVals.Tokens.SEMI(0,0)
	  fun loop lexer =
	      let val (result,lexer) = invoke lexer
		  val (nextToken,lexer) = VaporeonParser.Stream.get lexer
		  val _ = case result
			    of SOME r =>
				TextIO.output(TextIO.stdOut,
				       "result = " ^ (Rational.showRat r) ^ "\n")
			     | NONE => ()
	       in if VaporeonParser.sameToken(nextToken,dummyEOF) then ()
		  else loop lexer
	      end
       in loop lexer
      end

end (* structure Vaporeon *)
