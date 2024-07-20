Lass is a system-agnostic assembles written lisp.

Labels and constants are local to a macro block.
Blocks separated by labels may be rearranged to fit better.

# Syntax
```
; this is a comment
> nao-16.lass               ; Import a library of Nao-16 primitives.
start:                      ; Variable label.
add 1 2                     ; Instruction from the library.
n = #xC                     ; Constant.
macro1 = (r1 r2) {          ; Macro declaration with arguments r1 and r2.
    8#d1 r1 0               ; A 4 bit decimal 1 followed by the value of r1
}                           ; and a 0.
macro1 n 4#xF               ; Macro call
macro2 = (16#arg1) {        ; Macros may have fixed length arguments.
    arg1
}
#xFF00:                     ; Fixed label.
#b1111_0000                 ; binary literal
"Hello world\n\0"           ; String literal.
```
