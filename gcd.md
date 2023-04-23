The mathematical description of GCD
$$ \begin{align*} f((m,n)) = (m,n,0,1);\\ f((n)) = (n);\\ f((m,n,r,1)) = (m,n, `m mod n`, 2);\\ f((m,n,r,2)) = (n) if r = 0, (m, n, r, 3)  otherwise;\\ f((m,n,p,3)) = (n,p,p,1).\\ \end{align*} $$
