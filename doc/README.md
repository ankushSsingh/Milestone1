## Compilers Milestone 1

### Group Members
- Ankush Singh (150107)
- Mayank Sharma (160392)
- Harsh Thakur (160279)

### Description

We're using PLY [Python-Lex-Yacc](https://www.dabeaz.com/ply/ply.html) toolchain to implement our lexer and parser. To generate the ASTs for the test cases, run the following commands --

1.

    ```
     $ cd src/
    ```

2.
    The below command will run all the test cases present in the "tests/" folder and will create "\*.dot" files inside "src/AST/" folder

    ```
     $ ls ../tests/*.java | xargs -n1 -I { python3 new_parser.py {
    ```

3.
    To generate the corresponding PNG files using the graphviz tool, run the following command (This will take some time because the ASTs generated can be huge):

    ```
    $ ls AST/*.dot | xargs -n1 -I { dot -Tpng { -o {.png
    ```

### Features

In addition to the basic features mentioned in the "features.pdf" file, we've also implemented support for the following:

* Support for Class Inheritance
* Support for Interfaces
* TypeArguments (Using the [Lexer Hack](https://en.wikipedia.org/wiki/Lexer_hack))
* Annotations
* Static Polymorphism via method overloading
* Type Casting
* Throw, Catch, Finally
