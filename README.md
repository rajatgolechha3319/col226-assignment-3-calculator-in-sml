# Grammar for Rationals : 
```
G = <N,T,P,S> where
N = {<Rationals>, <Decimals>, <Fractions>, <Integer>, <Sign>, <Number>, <Digit>}
T = {+, -, /, ., 0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
P = {
<Rationals>  =  <Decimals> | <Fractions>
<Integer> = <Sign> . <Number> | <Number>
<Sign> = "+" | "-" 
<Number> = <Digit> | <Digit> . <Number>
<Digit> = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<Fractions> = <Integer> / <Integer> 
<Decimals> = <Integer> . ".". <Digit>* . ( <Number> )
}
S = <Rationals>
```
# Grammar for Expressions :
```
G = <N,T,P,S> where
N = {<Expression>, <Term>, <Factor>, <Rational>, <Variable>, <Type>, <Identifier>, <Letter>, <Alphanumeric>, <Integer>, <Sign>, <Number>, <Digit>, <Fractions>, <Decimals>}
T = {+, -, *, /, :, (, ), rational, int, A, B, C, ..., Z, a, b, c, ..., z, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, .}
P = {
<Expression> ::= <Term> | <Expression> ( "+" | "-" ) <Term>
<Term> ::= <Factor> | <Term> ( "*" | "/" ) <Factor>
<Factor> ::= <Rational> | <Variable> | "(" <Expression> ")"
<Variable> ::= <Identifier> ":" <Type>
<Type> ::= "rational" | "int"
<Identifier> ::= <Letter> <Alphanumeric>*
<Letter> ::= "A" | "B" | "C" | ... | "Z" | "a" | "b" | "c" | ... | "z"
<Alphanumeric> ::= <Letter> | <Digit>
<Rationals>  =  <Decimals> | <Fractions>
<Integer> = <Sign> . <Number> | <Number>
<Sign> = "+" | "-" 
<Number> = <Digit> | <Digit> . <Number>
<Digit> = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<Fractions> = <Integer> / <Integer> 
<Decimals> = <Integer> . ".". <Digit>* . ( <Number> )
}
S = {<Expression>}
```

# Design Decisions : 
```
1. All the helper functions used are defined in local scope. This is done to avoid name clashes with other functions in the program.
2. The file Vaporeon.cm is used to compile the program. This is the makefile for the program.
3. The output are in p/q format only.
4. While the input can be in any format (p/q , [S]I.N(R) ).
```

# Acknowledgements :
```
1. The code for the parser is based on the code provided in the documentation for ML-Lex and ML-Yacc. 
2. The code for the lexer is based on the code provided in the documentation for ML-Lex and ML-Yacc.
```

## Citation Links : 
```
1. http://www.mlton.org/Documentation.attachments/mllex.pdf
2. http://www.mlton.org/Documentation.attachments/mlyacc.pdf
3. https://stackoverflow.com/questions/36529512/convert-fraction-to-string-with-repeating-decimal-places-in-brackets
```
## How to Run : 
1. Download the files.
2. Run sudo sml in the the current folder.
3. Then CM.make " Vaporeon.cm ";
4. Now run Vaporeon.parse(); in command line.
5. Enter the input in the format specified above.

Example : 

In : 12.23(4) + 23.46(5) ;
Out : result = 357/10
In : 0.(9) + 0.(9) ; 
Out : result = 2/1
In : 1.(1) + 2.(2) ;
Out : result = 10/3
In : 12 + 2/~1 + 10 ;   
Out : result = 20/1
In : ( ( 1 + 2 ) - ( 11 * 1) ) ;
Out : result = ~8/1

