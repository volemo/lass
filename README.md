Lass is a system-agnostic assembles written lisp.

Labels and constants are local to a macro block.
Lass uses Verilog-style numbers.
By default every line is 16 bit aligned and characters are wastefull (i.e. each takes up 16 bits).

# Syntax
```
                            ; this is a comment
> nao-16.lass               ; Import a library of Nao-16 primitives.
start:                      ; Variable label.
add 1 2                     ; Instruction from the library.
n = 'hC                     ; Constant.
macro1 = (r1 r2) {          ; Macro declaration with arguments r1 and r2.
    8'd1 r1 0               ; A 4 bit decimal 1 followed by the value of r1
}                           ; and a 0.
macro1 n 4'hF               ; Macro call
macro2 = (16'arg1) {        ; Macros may have fixed length arguments.
    arg1
}
'hFF00:                     ; Fixed label.
'b1111_0000                 ; Binary literal
"Hello world\n\0"           ; String literal.
```


## BNF
```
code = (statement)*
statement = expression (";" comment)? 
expression = import | label | definition | instruction
import = ">" (ws)? filename "\n"
label = (name | literal) ":" ("\n")?
instruction = (name | literal | string)+ "\n"
definition = name (ws)? "=" (ws)? (name | literal | macro) "\n"
macro = "(" (argument-list)? ")" block
argument-list = argument (ws)? ("," (ws)? argument-list)
argument = (width "'")? name
block = "{" (ws)? code (ws)? "}"
literal = (sign)? ((width)? "'" base)? number 
name = /[a-zA-Z/!~.@$%^&*_+]+[a-zA-Z0-9/!~-.@$%^&*_+]*/
number = /[0-9a-fA-F]+/
width = /[0-9]+/
string = /"(?:[^"\\]|\\.)*"/
comment = /[^\n]*\n/
ws = /\s+/
filename = /^[^<>:;,?"*|/]+$/
```
