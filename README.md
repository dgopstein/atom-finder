# atom-finder [![Build Status](https://travis-ci.org/dgopstein/atom-finder.svg?branch=master)](https://travis-ci.org/dgopstein/atom-finder)

A Clojure library for finding [Atoms of Confusion](https://atomsofconfusion.com) in C projects.

Facilities for:

 * Parsing C/C++ with Eclipse CDT library
 * Finding specific patterns in an AST
 * Traversing version histories through git
 * Parsing commit logs for bug/patch IDs

## Project Structure Overview

The majority of interesting files in this project are located in the top-level
`src` directory. Secondarily, test files are in located `test` and jars in
`resources`. Under `src` there are several important directories:

* `atom_finder` - Clojure files for parsing C/C++ files and searching for atoms
* `analysis` - R files for statistically analyzing the results of clojure
* `conf` - Configuration variables to customize each runtime environment

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
 
The first thing you might want to do, is parse some C code. There are three
main functions for doing this, `parse-file`, `parse-source` and
`parse-frag`. Both functions take a `String` as an argument, and return
an
[`IASTNode`](https://dgopstein.github.io/content/cdt/org/eclipse/cdt/core/dom/ast/IASTNode.html).
`parse-file` and `parse-source` both require whole programs, the former
accepting a filename as its argument and the latter a string containing the
full code. `parse-frag` on the other hand can take any (read "many") partial
program. For example:

```clj
(parse-file "gcc/testsuite/c-c++-common/wdate-time.c") => CPPASTTranslationUnit
(parse-source "int main() { 1 + 1; }") => CPPASTTranslationUnit
(parse-frag "1 + 1") => CPPASTBinaryExpression
```
 
After you've parsed some code, you might reasonable want to see what it looks like:
 
```clj
(->> "gcc/testsuite/c-c++-common/wdate-time.c"
      parse-file
      (get-in-tree [2])
      print-tree)
```
                               
Which should output:
           
```
[]  <SimpleDeclaration>                                      {:line 6, :off 238, :len 39}
[0]  <SimpleDeclSpecifier>                                   {:line 6, :off 238, :len 10}
[1]  <ArrayDeclarator>                                       {:line 6, :off 249, :len 27}
[1 0]  <Name>                                                {:line 6, :off 249, :len 9}
[1 1]  <ArrayModifier>                                       {:line 6, :off 258, :len 2}
[1 2]  <EqualsInitializer>                                   {:line 6, :off 261, :len 15}
[1 2 0]  <IdExpression>                                      {:line 6, :off 263, :len 13}
[1 2 0 0]  <Name>                                            {:line 6, :off 263, :len 13}
```
                                                       
Some other useful functions are:

    print-tree     -> Prints a debug view of the tree structure of an AST plus metadata
    write-tree     -> Takes an AST and returns the code that generated it (inverse parsing)
    get-in-tree    -> Digs down into an AST to get at nested children
    default-finder -> Take a function that returns true/false for a single AST node, and run it over an entire AST
