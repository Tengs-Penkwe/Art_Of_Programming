The mathematical description of GCD
```math
\begin{align*}
f((m,n)) &= (m,n,0,1);\\
f((n)) &= (n);\\
f((m,n,r,1)) &= (m,n, \texttt{ m mod n }, 2);\\
f((m,n,r,2)) &= \texttt{if } (n),  \texttt{ otherwise } (m, n, r, 3);\\
f((m,n,p,3)) &= (n,p,p,1).\\
\end{align*}
```

1. Finiteness
2. Definiteness
3. Input
4. Output

? Effectiveness
