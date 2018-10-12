# atom-finder [![Build Status](https://travis-ci.org/dgopstein/atom-finder.svg?branch=master)](https://travis-ci.org/dgopstein/atom-finder)

A Clojure library for finding [Atoms of Confusion](https://atomsofconfusion.com)
in C projects.

Contains facilities for:

 * Parsing C/C++ with Eclipse CDT library
 * Finding specific patterns in an AST
 * Traversing version histories through git
 * Parsing commit logs for bug/patch IDs

Output from this work formed the basis of our paper at the Mining Software
Repositories 2018 conference: [Prevalence of Confusing Code in Software Projects - Atoms of
Confusion in the Wild](https://atomsofconfusion.com/papers/atom-finder-msr-2018.pdf).

## Running from the command-line

If you would like to use this project as-is to find all occurrences of the 15
atoms of confusion described in our 2018 MSR paper you can run our code from the
command-line as:

```sh
lein find-atoms-in-dirs dir1 dir2 > atoms.edn
```

This command will loop over each of the directories provided (in the example
above: `dir1 dir2`) and print an [edn](https://github.com/edn-format/edn) record
to the file `atoms.edn` for each file in the following shape:

```clj
{:file "src/test/resources/atom-comments.c",
 :atoms {:post-increment (4 10 13 25 28 30),
         :preprocessor-in-statement (),
         :operator-precedence (),
         ...}}
```

The output for each line indicates, for each atom of confusion, on which lines
was it found.

It is best to redirect this output to a file for further post-processing.

## Project Structure Overview

The majority of interesting files in this project are located in the top-level
`src` directory. Secondarily, test files are in located `test` and jars in
`resources`. Under `src` there are several important directories:

* [`atom_finder`](src/atom_finder) - Clojure files for parsing C/C++ files and searching for atoms
* [`analysis`](src/analysis) - R files for statistically analyzing the results of the code mining
* [`conf`](src/conf) - Configuration variables to customize each runtime environment

The most important, and complicated directory is `src/atom_finder` which
contains all the code analyze source code and repositories. Within the top-level
of `atom-finder` is code which is both specific to this project, but also
reusable between different analyses. Below the top-level are several other
useful directories:

* [`classifier`](src/atom_finder/classifier) - Every file in this directory is
  used to determine whether an individual AST node is a particular atom of
  confusion
* [`questions`](src/atom_finder/questions) - Every file in this directory
  corresponds to one of our [published, or potential) research hypotheses. These
  files implicitly use the classifier infrastructure to observe patterns.
* [`tree_diff`](src/atom_finder/tree_diff) - Tree diffing was a difficult enough problem that took several
  iterations to get working. Each evolution is it's own sub-namespace in this
  directory. Ultimately only [`difflib`](src/atom_finder/tree_diff/difflib.clj) ended up being used.
* [`util`](src/atom_finder/util) - The most reusable and general functions. Most
  of these files are potentially useful in other projects outside this one.

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
 
## Using the framework to parse code
 
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
(parse-file "gcc/testsuite/c-c++-common/wdate-time.c")  ;; => CPPASTTranslationUnit
(parse-source "int main() { 1 + 1; }")                  ;; => CPPASTTranslationUnit
(parse-frag "1 + 1")                                    ;; => CPPASTBinaryExpression
```
 
After you've parsed some code, you might reasonably want to see what it looks like:
 
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

## Using the framework to find atoms of confusion

You may also be interested in finding where in software projects atoms of
confusion live.

In the [`classifier`](src/atom_finder/classifier/) namespace there
are several functions for finding atoms. First, every type of atom has a
classifier which can be applied to an AST node to determine whether it
represents an atom of confusion.

```clj
(->> "x++" parse-expr post-*crement-atom?)      ;; => false
(->> "y = x++" parse-expr post-*crement-atom?)  ;; => true
```

Further, by applying the `default-finder` function, each classifier can be
adapted to find each example of an atom in a piece of code.

```clj
(->> "x = (1, 2) && y = (3, 4)"
     parse-expr
     ((default-finder comma-operator-atom?))
     (map write-tree))
;; => ("1, 2" "3, 4")
```

If you would like to find every atom in a piece of code you can use the helper
function `find-all-atoms` in
[`classifier.clj`](src/atom_finder/classifier.clj).

```clj
(->> "11 && 12 & 013"
     parse-expr
     find-all-atoms
     (map-values (partial map write-tree))
     (remove (comp empty? last))
     (into {}))
;; => {:operator-precedence ("11 && 12 & 013"), :literal-encoding ("12 & 013")}
```

## Using the framework to answer questions

Beyond simply finding atoms of confusion, there's a fair amount of code to
answer specific questions about how atoms of confusion relate to a
codebase. Much of this code lives in
the [`questions`](src/atom_finder/questions/) directory, and is very poorly
documented. Sorry in advance.
