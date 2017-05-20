# atom-finder [![Build Status](https://travis-ci.org/dgopstein/atom-finder.svg?branch=master)](https://travis-ci.org/dgopstein/atom-finder)

A Clojure library for finding [Atoms of Confusion](https://atomsofconfusion.com) in C projects.

Facilities for:

 * Parsing C/C++ with Eclipse CDT library
 * Finding specific patterns in an AST
 * Traversing version histories through git
 * Parsing commit logs for bug/patch IDs

## Working with Clojure

This project is primariy written in Clojure(JVM), and uses many Java libraries.
In order to run this project you should install:

 * [Leiningen](https://leiningen.org/) - The Clojure build manager.
 This tool will automatically download the right version of Clojure, resolve all the necessary libraries, run tests, and execute the program.
 * One of [Emacs](https://www.gnu.org/software/emacs/)/[CIDER](https://cider.readthedocs.io/en/latest/), [Sublime](https://www.sublimetext.com/)/[SublimeREPL](https://packagecontrol.io/packages/SublimeREPL), [Nightcode](https://sekao.net/nightcode/), or anything that offers you a clojure-centric workflow.
 The way one writes Clojure (and lisp in general) is a bit more interactive than traditional development.
 It's important to be able to evaluate code as you write it.
 
 After you've installed these tools, first run `lein test` to make sure everything is up and running.
 Then you should be able to develop in your editor, executing snippets of code as you go.
 
 ## Using the framework
 
 The first thing you might want to do, is parse some C code. There are two main functions for doing this, `parse-source` and `parse-frag`. Both functions take a `String` as an argument, and return an [`IASTNode`](https://dgopstein.github.io/content/cdt/org/eclipse/cdt/core/dom/ast/IASTNode.html). The difference is that `parse-source` requires a whole program, and `parse-frag` can take any partial program. For example:
 
 ```clojure
(parse-source "int main() { 1 + 1; }") => CPPASTTranslationUnit
(parse-frag "1 + 1") => CPPASTBinaryExpression
```
 
 If you want to read the program out of a file, try putting the file in `src/test/resources/` and then doing this:
 
 ```clojure
 (->> "wdate-time.c"
       parse-resource
       (get-in-tree [2])
       print-tree)
```
                               
Which should output:
           
```
 -CPPASTSimpleDeclaration  (offset: 238, 39) -> const char
   -CPPASTSimpleDeclSpecifier  (offset: 238, 10) -> const char
   -CPPASTArrayDeclarator  (offset: 249, 27) -> timestamp[
     -CPPASTName  (offset: 249, 9) -> timestamp
     -CPPASTArrayModifier  (offset: 258, 2) -> []
     -CPPASTEqualsInitializer  (offset: 261, 15) -> = __TIMEST
       -CPPASTName  (offset: 263, 13) -> __TIMESTAM
```
                                                       
Some other useful functions are:

    write-ast -> Takes an AST and prints out the code (basically the opposite of parsing)
    print-tree -> Prints the whole tree structure of an AST
    get-in-tree -> Digs down into an AST to get at nested children
    default-finder -> Take a function that returns true/false for a single AST node, and run it over an entire AST
