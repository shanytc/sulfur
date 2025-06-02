# Sulfur Programming Language
> [!Warning]
> Work in Progress


 <p align="center">
  <img src="https://github.com/user-attachments/assets/e692ddd1-7d1e-4f44-8a8e-e3fe3aa0dc94" />
</p>

# Code Examples

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
