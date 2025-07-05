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

### Comments
```c
fn main() {
    // This is a single-line comment
    print("Comment added above!\n");
}
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
## Pointers
### 1. Basic “pointer out, dereference in 
```c
fn main() {
    let x, p;
    x = 7;
    p = &x;          // p holds the address of x
    *p = 99;         // writes through the pointer
    print("x = {}", x);      // → x = 99
}
```

```bash
x = 99
```

### Passing a raw pointer into a function
```c
fn inc_first(*arr) {          // parameter syntax sugar
    arr[0] = arr[0] + 1;      // bracket → *(arr + 0)
}

fn main() {
    let a;
    a = malloc(4);            // room for one i32
    a[0] = 10;
    inc_first(a);             // by pointer
    print("value = {}", a[0]);    // → value = 11
}
```

```bash
value = 11
```

### Function that returns a pointer
```c
fn make_array(n) -> int {     // returns a raw int (pointer value)
    let p;
    p = malloc(n * 4);        // n × i32
    return p;                 // implicit & not needed
}

fn main() {
    let nums;
    nums = make_array(3);     // pointer comes back in EAX
    nums[0] = 5;
    nums[1] = 8;
    nums[2] = 13;
    print("Value is: {}", nums[1]);     // → 8
}
```

```bash
Value is: 8
```

### Pointer-to-pointer (**)

```c
fn flip_first(**pp) {         // double dereference
    (*pp)[0] = -(*pp)[0];
}

fn main() {
    let arr, pp;
    arr = malloc(4);          // one int
    arr[0] = 42;
    pp = &arr;                // address of the *variable* arr
    flip_first(pp);           // changes arr[0] through **pp
    print("Value is: {}", arr[0]);      // → -42
}
```

```bash
Value is: -42
```

### Manual pointer arithmetic (no [] sugar)
```c
fn sum_three(*base) -> int {
    let total;
    total = *(base + 0) + *(base + 1) + *(base + 2);
    return total;
}

fn main() {
    let a;
    a = malloc(3 * 4);
    a[0] = 3;  a[1] = 4;  a[2] = 5;
    print("sum = {}", sum_three(a));   // → sum = 12
}
```

```bash
sum = 12
```

### Mixing &, *, and [] in a single expression
```c
fn main() {
    let buf, p, q;
    buf = malloc(2 * 4);     // buf : *int
    p   = &buf;              //  p : **int
    (*p)[1] = 17;            // deref once, index into the array
    q = &(*p)[1];            // q : *int  → address of that element
    print("{}", *q);         // → 17
}
```
```bash
error
```

### Iterating over an array with a pointer variable
```c
fn main() {
    let data, i, ptr;
    data = malloc(4 * 4);
    data[0] = 10; data[1] = 20; data[2] = 30; data[3] = 40;

    i = 0;
    ptr = data;              // start pointer

    while (i < 4) {
        print("{}", *ptr);   // dereference
        ptr = ptr + 1;       // advance by 1 (scaled later by backend)
        i = i + 1;
    }
}
/* output:
10
20
30
40
*/
```
```bash
error
```
