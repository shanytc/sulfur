# Surf Programming Language
> [!Warning]
> Work in Progress

<p align="center">
  <img alt="Sulfur" src="https://github.com/user-attachments/assets/4e7cfd9d-b0b7-4d3e-b99e-743672514e03" />
</p>

# Code Examples

### Hello World
```c
fn main() {
    print("Hello, World!\n");
}
```

```bash
Hello, World!
```

### Fib sequence
```c
main() {
    let a, b, c;
    b = 1;
    while (a < 1000000) {
        print("%d\n", a);
        c = a + b;
        a = b;
        b = c;
    }
}
```

```bash
0
1
1
2
3
5
8
13
21
34
55
89
144
233
377
610
987
1597
2584
4181
6765
10946
17711
28657
46368
75025
121393
196418
317811
514229
832040
```
### FizzBuzz
```c
fn fizz_buzz(n) {
    while (n > 0) {
        if (n % 3 == 0 && n % 5 == 0) {
            print("FizzBuzz\n");
        } else if (n % 3 == 0) {
            print("Fizz\n");
        } else if (n % 5 == 0) {
            print("Buzz\n");
        } else {
            print("%d\n", n);
        }
        n = n - 1;
    }
}

fn main() {
   fizz_buzz(10);
}
```

```bash
Buzz
Fizz
8
7
Fizz
Buzz
4
Fizz
2
1
```

### Pointers (and arrays)
```c
fn main() {
    let a, idx = 0;
    a = malloc(10 * 4); // Allocate memory for 10 integers
    while (idx < 10) {
        *(a + idx) = idx;
        idx += 1; // Increment the index
    }

    idx = 0;
    while (idx < 10) {
        print(*(a + idx)); // Print the value at the current index
        idx += 1; // Increment the index
    }

    free(a); // Free the allocated memory
}
```

```bash
0
1
2
3
4
5
6
7
8
9
```

### Dereferencing pointers
```c
fn main() {
    let a;
    let b = 10;
    a = &b; // a is a reference to b
    print("Value of a: %d\n", *a);
}
```

```bash
Value of a: 10
```

### Pointer Indexing
```c
fn main() {
    let a, b;
    a = malloc(4 * 4);
    a[0] = 10;
    a[1] = 20;
    a[2] = 30;
    a[3] = 40;
    let i = 0;
    while (i < 4) {
        print("{}", a[i]);
        i+=1;
    }
}
```

```bash
10
20
30
40
```
