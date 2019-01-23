#lang brag
p1-program : /NEWLINE* network DATA /NEWLINE+ instance (/NEWLINE /NEWLINE instance)* /NEWLINE*
@network : "p1c" | "p2c"
/instance : a /NEWLINE b /NEWLINE c
/a : /"a" (DATA /SEP)* DATA?
/b : /"b" (DATA /SEP)* DATA?
/c : /"c" (DATA /SEP)* DATA?
