# awesome-low-level-programming-languages

A curated list of low level programming languages primarily aimed and OS and game
programming.

Excluded are languages relying on managed run-times and garbage collection.


## ATS

* main: http://www.ats-lang.org/
* repo: https://github.com/ats-lang
* documentation:
  - http://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/book1.html
  - http://www.ats-lang.org/Documents.html
* implementation-language: ATS
* meta-prgramming: N/A
* backends: C
* major projects using the language: N/A
* syntax: functional style
* highlights:
  - proofs, dependent types


```
#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

implement
main0() = println! ("Hello, world!")
```

```
fun fibc (n: int) : int = let
  fun loop(n: int, f0: int, f1: int): int =
    if n > 0 then loop(n-1, f1, f0+f1) else f0
  end of [loop]
in
  loop(n, 0, 1)
end // end of [fibc]
```

## Ada

* main: N/A
* repo: N/A
* documentation:
  - awesome-ada https://github.com/ohenley/awesome-ada
  - https://learn.adacore.com/
  - http://groups.umd.umich.edu/cis/course.des/cis400/ada/ada.html
* meta-prgramming: generics
* backends: gcc (gnat), several commerical implementations
* major projects using the language: numerous
* syntax: begin/end, type to the right of identifier
* highlights:
  - design by contract

```
with Ada.Text_IO;

procedure Hello_World is
begin
   Ada.Text_IO.Put_Line ("Hello World");
end Hello_World;
```

```
function fibonacci(n : in integer) return integer is
    f1, f2, fib : integer;
 begin
    f1 := 1;
    f2 := 1;
    for i in 3..n loop
       fib := f1 + f2;
       f1 := f2;
       f2 := fib;
    end loop;
    return fib;
 end fibonacci;
```
## C

* main: N/A
* repo: N/A
* documentation:
  - https://github.com/inputsh/awesome-c
  - https://github.com/uhub/awesome-c
* meta-prgramming: pre-processor
* backends: llvm, gcc, numerous others
* major projects using the language: numerous
* syntax: curly braces, type to the left of identifier
* highlights:
  - ubiquitous
  - often used as a backend
  - no namespaces
  - array to pointer auto conversion
  - no defer (or RAII) mechanism
  - lots of undefined / implementation defined behavior

```
#include <stdio.h>

int main(void) 
{
  printf("Hello World!");
}
```

```
int fib(int n) {
    int a = 0;
    int b = 1;
    for (int i = 0; i <= n; i++) {
       int c = a + b;
       a = b;
       b = c;
    }
    return a;
}
```


## C++
* repo:
* documentation:
  - standard https://isocpp.org/std/the-standard
  - reference https://en.cppreference.com/w/
  - awesome-cpp https://github.com/fffaraz/awesome-cpp
  - AwesomePerfCpp https://github.com/fenbf/AwesomePerfCpp
* meta-prgramming:
* backends:  llvm, gcc, numerous others
* major projects using the language: numerous
* syntax: curly braces, type to the left of identifier
* highlights:
  - curly braces, type to the left of identifier
  - large language

```
#include <iostream>

int main() 
{
  std::cout << "Hello World!" << std::endl;
}

```

```
int fib(int n) {
    int a = 0;
    int b = 1;
    for (int i = 0; i <= n; i++) {
       int c = a + b;
       a = b;
       b = c;
    }
    return a;
}
```
