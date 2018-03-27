# MiniC
Project 2: An LLVM Code Generator for MiniC
ECE 466/566 Fall 2017
You are encouraged to comment directly on this document rather than posting questions on Piazza about the specs, as comments make it easier to understand context. 

Objectives
•	Implement an LLVM bitcode generator for a subset of the C programming language.
•	Interpret a language specification and a grammar.
•	Explore the semantics of  the C language.
•	Apply theoretical concepts of the control-flow graph to code generation.
•	Build control flow with the LLVM API.

Description
This project is based on a subset of the C programming language.  For convenience, we’ll call it MiniC.  Many features of C have been eliminated to make this project easier.  Also, the supported subset differs between 466 and 566 so be sure to understand the differences well.

Features for 566:
1.	Types that must be supported:  int (i32), float (f32), int* (i32*), float* (f32*), void*.
2.	Global and local variables, including parameters, are allowed.
a.	Each variable is declared on a line by itself and may optionally be initialized.
3.	No post-fix and pre-fix increment/decrement operators (++/--).
4.	No operators on structs, unions, or arrays need be supported.
5.	Dereference (*) and address-of (&) must be supported.
6.	You must support short-circuiting on && and || operations.  
7.	Statements include if-then-else, while, for, expression statements, break and continue.  Also, if must always have a matching else.  You may support switch statement for bonus points.
8.	Statements may be arbitrarily nested.
9.	There will be only one function per source file, and it need not be named main.






