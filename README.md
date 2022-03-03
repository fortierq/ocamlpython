# ocamlpython
Minimal Python implementation with OCaml + Lex + Menhir

## Prerequisites
- OCaml
- Dune
- OCamllex
- menhir

## Build

| make all | Build and run on test.py |
|:---:|:---:|
| make tests | Build and run tests |
| build | Build an executable interpreter |
| clean | Clean temporary dune files |

## Examples

- Lists: 
```python
L = [2, 3, 5, 7]

for e in L:
    print(e)
```

- Functions and recursion:
```python
def fibaux(a, b, k):
    if k == 0:
        return a
    else:
        return fibaux(b, a+b, k-1)

def fib(n):
    return fibaux(0, 1, n)

print(fib(10))
```

- Pipe operator:
```
def sum(x, y):
    return x + y

def double(x):
    return 2*x

print([1, 2] | sum | double | double)
```