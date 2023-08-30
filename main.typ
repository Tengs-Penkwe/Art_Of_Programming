#import "template.typ": *
#import "@preview/algo:0.3.1": algo, i, d, comment, code

// Take a look at the file `template.typ` in the file panel
// to customize this template and discover how it works.
#show: project.with(
  title: "The Art of Computer Programming",
  authors: (
    "Tengs",
  ),
)

= 1.2 Mathematical Preliminaries

== 1.2.7 Harmonic Numbers
$ H_n = 1 + 1/2 + 1/3 + ... + 1/n = sum_(k=1)^n 1/k, n>=0 $

$ H_oo^((r)) = 1/2 |B_r| (2pi)^r/(r!), space.quad "integer" r\/2 >= 1 $

3. [M21] Generalize the argument used in the previous exercise to show that, for $r > 1$, the sum $H^((r))_n$ remains bounded for all $n$. Find an upper bound.

  #answer()[$
  H_(2^m-1)^((r)) & = 1^(-r) + (2^(-r) + 3^(-r)) + (2^(-2r) + 5^(-r) + 6^(-r) + 7^(-r)) ... \
  & <= 1^(-r) + (2^(-r) + 2^(-r)) + 4 times 4^(-r) + ... \
  & = sum_(k=0)^(m-1) 2^k/2^(k r) \ 
  & = (2^((m-1)(1-r))-1)/(2^(1-r)-1)
  $]

4. [M21] Let $T(m, n) = H_m+ H_n− H_(m n)$. (a) Show that when $m$ or $n$ increases, $T(m, n)$ never increases (assuming that $m$ and $n$ are positive). (b) Compute the minimum and maximum values of $T(m, n)$ for $m, n > 0$.

  #answer()[assume $m >= n$, $
  T(m, n) &= sum_(k=0)^n 1/k - sum_(k=m)^(m n)1/k \
  T(m+1, n) - T(m,n) & = sum_(k=m n)^(m n+n) 1/k - 1/m \
  & < n times 1/(m n) - 1/m = 0 \
  T(m, n+1) - T(m, n) &= 1/(n+1) - sum_(k=m n)^(m n+m)1/k \
  & < 1/(n+1) - m times 1/(m(n+1)) = 0
  $
  Thus, I show that $T(m,n)$ decreases monotonically. 

  When $m=n=1$, $T(m,n)_(max) = 1$
  ]

  #wrong()[What's the lower bound of $T(m,n)$?]

== 1.2.9 Generating Functions

1. [M12] What is the generating function for the sequence $2, 5, 13, 35, . . . = ⟨2^n+3^n⟩$?
  
  #answer()[$
  & (1+2x + 4x^2 + ...+2^n x^n) + (1+3x + 9x+... + 3^n x^n) \
  &= 1/(1-2x) + 1/(1-3x)
  $]

3. [HM21] Differentiate the generating function (18) for $⟨H_n⟩$, and compare this with the generating function for $⟨sum^n_(k=0)H_k⟩$. What relation can you deduce?

  #answer()[$
  G'(z) & =  (ln 1/(1-z))/(1-z)^2 + 1/(1-z)^2 = 1 + 3z + 11/2 z^2 + ...  \
  G(z) = 
  $]

= 2.1 Introduction

1. In the situation depicted in (3), what's the value of (a) *SUIT(NEXT(TOP))*; (b)*NEXT(NEXT(NEXT(TOP)))*

  #answer()[
  (a) SUIT(NEXT(TOP)) = SUIT(NEXT(242)) = SUIT(386) = 242 \
  (b) NEXT(NEXT(NEXT(TOP))) = #sym.Lambda 
  ]

2. The text points out that in many cases *CONTENTS(LOC(V)) = V*. Under what conditions do we have *LOC(CONTENTS(V)) = V*?

3. [11] Give an algorithm that essentially undoes the effect of Algorithm A: It removes the top card of the pile (if the pile is not empty) and sets NEWCARD to the address of this card.  

Set *NEWCARD #sym.arrow.l TOP* and if

#align(center)[= 2.2 Linear Lists]

== 2.2.1 Stack, Queue, Deque

1. [06] //An input-restricted deque is a linear list in which items may be inserted at one end but removed from either end; clearly an input-restricted deque can operate either as a stack or as a queue, if we consistently remove all items from one of the two ends. Can an output-restricted deque also be operated either as a stack or as a queue?

2. [15] If there are six railroad cars numbered 123456, can they be permuted into the order 325641? Can they be permuted into the order 154623? (In case it is possible, show how to do it.)

  #answer()[
  1 in, 2 in, 3 in out, 2 out, 4 in, 5 in out, 6 in out, 4 out, 1 out.\
  Impossible, because we can't move things back to stack, the input and output are one-directional.
  ]

3. [25] Let us call a sequence of S’s and X’s admissible if it contains n S’s and n X’s, and if it specifies no operations that cannot be performed. Formulate a rule by which it is easy to distinguish between admissible and inadmissible sequences; show furthermore that no two different admissible sequences give the same output permutation.

  #answer()[
  Count the number of S and X from the beginning, if the number of X exceeds S's, it is _inadmissible_.
  ]
  
4. [M34] Find a simple formula for $a_n$, the number of permutations on $n$ elements that can be obtained with a stack like that in exercise 2

  #answer()[
  We can see there are $binom(2n,n)$ possible combinations, in all these combination, 
  ]
  
  #wrong()[
  How about the _inadmissible_ ? Inverse the X and S to find the _inadmissible_ one (REFLECTION PRINCIPLE)
  ]

5. [M28] Show that it is possible to obtain a permutation $p_1 p_2 #sym.dots p_n$ from $1 2 #sym.dots n$ using a stack if and only if there are no indices $i < j < k$ such that $p_j < p_k< p_i$

  #answer()[
  *Proof 1*: Show $p_j < p_k < p_i$ is impossible after permuataion\
  If $p_i < p_j$, I need let $p_i$ being poped before $p_j$ is pushed, if $p_j < p_i$, I need let $p_i$ being poped after $p_j$ is pushed.\
  Since $i < j < k$, I know they come into stack in this order. \
  Since $p_j < p_k< p_i$ \
  $p_i$ out before $p_j$ out, 
  $p_j$ out before $p_i$ in,\
  And this is impossible.
  
  *Proof 2*: Show we can find a permutation for any squence following that pattern\
  ]

  #wrong()[This is the 321-avoiding problem for stackable permutation, I'm not satisfied with knuth's answer]

7. [25] Consider the problem of exercise 2, with a deque substituted for a stack.\
   (a) Find a permutation of 1234 that can be obtained with an input-restricted deque, but it cannot be obtained with an output-restricted deque. \ (b) Find a permutation of 1234 that can be obtained with an output-restricted deque but not with an input-restricted deque.\ (c) Find a permutation of 1234 that cannot be obtained with either an input-restricted or an output-restricted deque. 
  
  #answer()[
  (a) 4 1 3 2\ (b) 4 2 1 3\ (c) 4 2 3 1 
  ]
 
8. [22] Are there any permutations of $12...n$ that cannot be obtained with the use of a deque that is neither input- nor output-restricted?

  #answer()[]
  #wrong()[May be I can develop a ladder named depth, right depth and left depth, the we can know, what output can a given depth permutate]

9. [M20] Let $b_n$ be the number of permutations on n elements obtainable by the use of an input-restricted deque. (Note that $b_4$ = 22, as shown in exercise 7.) Show that bn is also the number of permutations on n elements with an output-restricted deque.

  #answer()[Operate backward]
 
10. [M25] Find a way to define the concept of an admissible sequence of the symbols S, Q, and X, so that the following property holds: Every permutation of n elements that is attainable with an output-restricted deque corresponds to precisely one admissible sequence.

    #answer()[
    (1) There should be $n$ X's and $n$ Q's and S's \
    (2) Read from left, number of Q and S shouldn't exceed X \
]
    #wrong()[
    (3) When $n_X$ equals $n_Q + n_S$, the next one should be Q \
    (4) Two XQ must never be adjecent \
]

11. [M40]
12. [HM34]
13. [M48]

15. Suppose you are allowed to use only stacks as data structures. How can you implement a queue efficiently with two stacks?

    #answer()[
Let there be two queue R and S, they have same element but in reverse direction
0. When R is empty, push all S to R
1. Insert, push S
2. Delete, pop R
Each element is poped and pushed at most twice.
]

== 2.2.2 Sequential Allocation

1. [15] In the queue operations given by (6a) and (7a), how many items can be in the queue at one time without *OVERFLOW* occurring?

  #answer()[M-1, becuase if we allow M, then it's hard to examine if the queue is full]

2. [22] Generalize the method of (6a) and (7a) so that it will apply to any deque with fewer than M elements. In other words, give specifications for the other two operations, “delete from rear” and “insert at front.”

  #answer()[
  $ X #sym.arrow.l.double Y (:"Insert into front") cases(
"if" mono(F = 1) "then" mono(F #sym.arrow.l M) "otherwise" mono(F #sym.arrow.l F - 1 ),
"if" mono(F = R) "then" mono("OVERFLOW"),
mono(X[F] #sym.arrow.l Y)
) $
$ Y #sym.arrow.l.double X ("delete from rear") cases(
"if" mono(R = M) "then" mono(R #sym.arrow.l 1) "otherwise"
mono(R <- 1),
"if" mono(R = F)\, "then" mono("UNDERFLOW"),
mono(Y <- X[R])
) $]

3. 
4.
5.
6. [10] Starting with the memory configuration shown in Fig. 4, determine which of the following sequences of operations causes overflow or underflow:\ (a) $I_1$; (b) $I_2$; (c) $I_3$; (d) $I_4I_4I_4I_4I_4$; (e) $D_2D_2I_2I_2I_2$.

  #answer()[(e) will cause $mono("UNDERFLOW")$]
  
7. [12] Step G4 of Algorithm G indicates a division by the quantity $mono("INC")$. Can $mono("INC")$ ever be zero at that point in the algorithm?

== 2.2.3 Linked Allocation

== 2.2.6 Arrays and Orthogonal Lists


#align(center)[= 5.1 Sorting]


1. [M20] Prove, from the laws of trichotomy and transitivity, that the permutation $p(1) p(2)...p(N)$ is _uniquely_ determined when the sorting is assumed to be stable.

  #answer()[If it's not uniquely determined, assume for $R_1, R_2, ..., R_N$ have $p(1)p(2)...p(N)$ and $q(1)q(2)...q(N)$. which has $i != j$ that $p(i) = q(j)$ and $p(j) = q(i)$. Assume $i < j$. \
  We know $K_(p(i)) = K(p(j))$, or it's not sorted. \
]
  #wrong()[assume we have $p(1)p(2)...p(N)$ and $q(1)q(2)...q(N)$. let $i$ be the minimum for $p(i) != q(i)$, Then $p(i) = q(j)$ for some $j > i$, $q(i) = p(k)$ for some $k > i$.
  $ K_(p(i)) <= K(p(k)) = K_()$
]

2. [21] Alice took this file and sorted it first on the major keys, obtaining n groups of records with equal major keys in each group, $ K_p(1)= · · · = K_p(i_1)< K_p(i_1+1)= · · · = K_p(i_2)< · · · < K_p(i_(n−1)+1)= · · · = K_p(i_n) $, where $i_n = N$. Then she sorted each of the n groups $R_p(i_(j−1)+1), . . . , R_p(i_j)$ on their minor keys. \ Bill took the same original file and sorted it first on the minor keys; then he took the resulting file, and sorted it on the major keys.\ Chris took the same original file and did a single sorting operation on it, using lexicographic order on the major and minor keys $(K_j, k_j)$.\
   Did everyone obtain the same result?

  #answer()[Yes, if all the operations are stable]

3. [M25] Let $<$ be a relation on $K_1, . . . , K_N$ that satisfies the law of trichotomy but not the transitive law. Prove that even without the transitive law it is possible to sort the records in a stable manner, meeting conditions (1) and (2); in fact, there are at least three arrangements that satisfy the conditions!

  #answer()[I'll prove there must be one method to construct sequence $p(1)..p(i)$ where for any two adjecent elements $k$ and $k+1$, we have $K_(p(k)) <= K_(p(k+1))$.
  Select any two element in $K_1, ..., K_N$, we can construct such sequence with $i = 2$, now, select another element $K_j$, we first compare it with $p(i)$, if $K_j >= K_(p(i))$, we place it to the last, if not, we compare it with $i-1$, if $K_j >= K_(p(i-1))$, we continue this process, if for $1..i$, $K_j$ is always smaller, then we place it to the first place, we can do this to all element, and reach a correct sorting.
]

4. [21] Lexicographers don’t actually use strict lexicographic order in dictionaries, because uppercase and lowercase letters must be interfiled. Thus they want an ordering such as this:\ a < A < aa < AA < AAA < Aachen < aah < · · · < zzz < ZZZ. \ Explain how to implement dictionary order.

  #answer()[sort on first letter with lowercase smaller than uppercase, then sort on the next letter, if this letter doesn't exist, it's the smallest]

5. [M28] Design a binary code for all nonnegative integers so that if $n$ is encoded as the string $ρ(n)$ we have $m < n$ if and only if $ρ(m)$ is lexicographically less than $ρ(n)$. \ Moreover, $ρ(m)$ should not be a prefix of $ρ(n)$ for any $m != n$. If possible, the length of $ρ(n)$ should be at most $lg n + O(log log n)$ for all large $n$. (Such a code is useful if we want to sort texts that mix words and numbers, or if we want to map arbitrarily large alphabets into binary strings.)

  #answer()[ ]
  #wrong()[it's $ rho ((1 alpha)_2) = 1#sym.rho (| alpha |) alpha $]

9. [M27] After $N$ independent, uniformly distributed random variables between 0 and 1 have been sorted into nondecreasing order, what is the probability that the $r$th smallest of these numbers is $≤ x$?

  #answer()[it means we need to get at least $r$ variables from $N$ and each one is less than $x$, since each one is uniformly distributed, so for a single variable, the probability is $x$, $ sum_(i=r)^N binom(N, i) (1-x)^(N-i) x^r $]

Each of the following exercises states a problem that a computer programmer might have had to solve in the old days when computers didn’t have much random-access memory. Suggest a “good” way to solve the problem, _assuming that only a few thousand words of internal memory are available_, supplemented by about half a dozen tape units (enough tape units for sorting). Algorithms that work well under such limitations also prove to be efficient on modern machines.

10. [15] You are given a tape containing one million words of data. How do you determine how many distinct words are present on the tape?

  #wrong()[Sort it first, then count]

11. [18] You are the U.S. Internal Revenue Service; you receive millions of “information” forms from organizations telling how much income they have paid to people, and millions of “tax” forms from people telling how much income they have been paid. How do you catch people who don’t report all of their income?

12. [M25] (_Transposing a matrix._) You are given a magnetic tape containing one million words, representing the elements of a 1000×1000 matrix stored in order by rows: $a_(1,1) a_(1,2). . . a_(1,1000) a_(2,1). . . a_(2,1000). . . a_(1000,1000).$ How do you create a tape in which the elements are stored by columns $a_(1,1) a_(2,1). . . a_(1000,1) a(1,2). . . a_(1000,2). . . a_(1000,1000)$ instead? (Try to make less than a dozen passes over the data.)

19. [24] Given a file containing a million or so distinct 30-bit binary words $x_1, . . . , x_N$, what is a good way to find all complementary pairs ${x_i, x_j}$ that are present? (Two words are complementary when one has 0 wherever the other has 1, and conversely; thus they are complementary if and only if their sum is $(11 . . . 1)_2$, when they are treated as binary numbers.)

  #answer()[First, we sort it and then for a given word $x_i$, we can know its complementary pair $x_j$ and find it using bisect]

  #wrong()[Sort it and search from two ends, $i <- 1$, $j <- N$]

20. [25] Given a file containing 1000 30-bit words $x_1, . . . , x_1000$, how would you prepare a list of all pairs $(x_i, x_j)$ such that $x_i = x_j$ except in at most two bit positions?

  #answer()[Use Hamming Code to encode these words]
  #wrong()[You can not make sure you can encode them]

21. [M28] Given the specifications of a fairly large number of directed graphs, what approach will be useful for grouping the isomorphic ones together? (Directed graphs are isomorphic if there is a one-to-one correspondence between their vertices and a one-toone correspondence between their arcs, where the correspondences preserve incidence between vertices and arcs.)

  #answer()[]

== 5.1.1 Inversions

2. [M20] In the classical problem of Josephus (exercise 1.3.2–22), $n$ men are initially arranged in a circle; the $m$th man is executed, the circle closes, and every $m$th man is repeatedly eliminated until all are dead. The resulting execution order is a permutation of ${1, 2, . . . , n}$. For example, when $n = 8$ and $m = 4$ the order is 5 4 6 1 3 8 7 2 (man 1 is 5th out, etc.); the inversion table corresponding to this permutation is 3 6 3 1 0 0 1 0.\ Give a simple recurrence relation for the elements $b_1b_2. . . b_n$ of the inversion table in the general Josephus problem for $n$ men, when every mth man is executed.

  #wrong()[Everyone before $m$th is removed after, so $ b_1 = (m-1) mod n $ every time we are finding the one after $m$ and remove the $b_j$, so $ b_(j+1) = (b_j + m -1) mod(n-j) $]

3. [18] If the permutation $a_1a_2. . . a_n$ corresponds to the inversion table $b_1b_2. . . b_n$, what is the permutation $macron(a)_1 macron(a)_2. . . macron(a)_n)$ that corresponds to the inversion table$ (n − 1 − b_1)(n − 2 − b_2) . . . (0 − b_n) ? $

  #answer()[the reversed permutation $ macron(a)_j = a_(n+1-j) $]

4. [20] Design an algorithm suitable for computer implementation that constructs the permutation $a_1a_2. . . a_n$ corresponding to a given inversion table $b_1b_2. . . b_n$ satisfying (3). [Hint: Consider a linked-memory technique.] 

  #answer()[Construct a linked list, first put $n$ into it, then for $i<n$ put $i$ to the $(b_i + 1)$th place
  #algo( 
    title: "InverToPermut",
    parameters: ("Inversion",)
  )[  
    let Permut $<-$ LinkedList()\
    
    for i from 1 to Inversion.len()#i\
    
    let Order $<-$ Inversion[i]\
    Permut[Order] $<-$ i#d\
    
    return Permut
   ]
   The Initialization takes $O(1)$ time, since we used a linked list, each insertion takes $Theta(k)$ time, $k$ is the place to insert, there are $n$ insertions,\ the best scenarion is the inversion is all 0, takes $n$ time,\ the worst case scenarion is in reverse order: (n-1) (n-2) ... 0 , it takes $n(n-1)/2$ time,\ the amortized insertion time is $Theta(n^2)$
]

5. [35] The algorithm of exercise 4 requires an execution time roughly proportional to $n + b_1+ · · · + b_n$ on typical computers, and this is $Θ(n^2)$ on the average. Is there an algorithm whose worst-case running time is substantially better than order $n^2$?

  #answer()[Use a balanced tree, so the insertion time can be $Theta(log n)$]

  #wrong()[The key here is the insertion time]

6. [26] Design an algorithm that computes the inversion table $b_1b_2. . . b_n$ corresponding to a given permutation $a_1a_2. . . a_n$ of ${1, 2, . . . , n}$, where the running time is essentially proportional to $n log n$ on typical computers.

  #answer()[ ]

7. Several other kinds of inversion tables can be defined, corresponding to a given permutation $a_1a_2. . . a_n$ of ${1, 2, . . . , n}$, besides the particular table $b_1b_2. . . b_n$ defined in the text; in this exercise we will consider three other types of inversion tables that arise in applications.\ Let $c_j$ be the number of inversions whose first component is $j$, that is, the number of elements to the right of $j$ that are less than $j$. [Corresponding to (1) we have the table 0 0 0 1 4 2 1 5 7; clearly $0 ≤ c_j < j$.] Let $B_j = b_(a_j)$ and $C_j = c_(a_j)$.\ Show that $0 ≤ B_j < j$ and $0 ≤ C_j ≤ n − j$, for $1 ≤ j ≤ n$; furthermore show that the permutation $a_1a_2. . . a_n$ can be determined uniquely when either $c_1c_2. . . c_n$ or $B_1B_2. . . B_n$ or $C_1C_2. . . C_n$ is given.

  #answer()[
1. $B_j = b_(a_j)$, $b_(a_j)$ is the number of elements bigger than $a_j$ to the left, since there are only $j-1$ elements in the left $0 <= B_j <j$.
2. $C_j = c_(a_j)$, $c_(a_j)$ is the number of elements smaller than $a_j$ to the right, since there are only $n-j$ elements in the right $0 <= C_j < n-j$.
3. We know $C_j = c_(a_j)$, the projection between $j$ and $a_j$ is injective and surjective,so $C_j$ and $c_(a_j)$ can unqiuely determine each other. so did $B_j$ and $b_j$, since $a_j$ has unique $b_j$,
4. We can show the algorithm generate $a_j$ from $C_1C_2...C_n$ is injective.
]

9. [M21] Prove that, in the notation of exercise 7, the permutation $a_1a_2... a_n$ is an involution (that is, its own inverse) if and only if $b_j = C_j$ for $1 ≤ j ≤ n$.

  #answer()[*Forward:* Since $a_(a_i) = i$, ]

11. [M25] If $π = a_1a_2. . . a_n$ is a permutation of ${1, 2, . . . , n}$, let $ E(π) = {(a_i, a_j) | i < j, a_i > a_j} $ be the set of its inversions, and let $ macron(E)(π) = {(a_i, a_j) | i > j, a_i > a_j} $ be the non-inversions.\
    a) Prove that $E(π)$ and $macron(E)(π)$ are transitive. (A set $S$ of ordered pairs is called _transitive_ if $(a, c)$ is in S whenever both $(a, b)$ and $(b, c)$ are in $S$.)\ b) Conversely, let $E$ be any transitive subset of $T = {(x, y) | 1 ≤ y < x ≤ n}$ whose complement $macron(E) = T \\ E$ is also transitive. Prove that there exists a permutation π such that $E(π) = E$. 

  #answer()[a) if $(a_i, a_j)$ and $(a_j, a_k)$ are inside $E(pi)$, then $i < j < k$ and $a_i > a_j > a_k$, then $(a_i, a_k)$ is also inside. the same argument applies to $macron(E)(pi)$.\ 
  b) For each number $x$, create a node; for each pair inside $macron(E)(pi)$ and $E(pi)$ like $(x, y)$, creat an edge $x -> y$. This is a directed graph, with all nodes connected each other.\
  For such graph, if we can do topological sorting and get a unqiue result, because each node will have $n-1$ edge pointed to and pointed out, count the number $i$ it be pointed to, the order of this number is $i+1$.\ Here, I need to prove it's possible to sort it topologically, that is: it doesn't contain a loop.\
  If a loop does exist, we arrange the nodes as $a_(i_1) -> a_(i_2) -> .. -> a_(i_j) -> a_(j_1) -> a_(j_2) -> ..  -> a_(i_1)$, here $i_j$ is increasive, $j_i$ is decreasive or vice versa, since $macron(E)$ and $E$ is transitive, we can eliminate all the internal nodes and get $a_(i_1) -> a_(i_j) -> a_(i_1)$ where $i_j > i_1$, this is impossible, beacuse there is only one edge between two nodes.
]

12. [M28] Continuing the notation of the previous exercise, prove that if $π_1$ and $π_2$ are permutations and if $E$ is the smallest transitive set containing $E(π_1) ∪ E(π_2)$, then $macron(E)$ is transitive. [Hence, if we say $π_1$ is “above” $π_2$ whenever $E(π_1) ⊆ E(π_2)$, a lattice of permutations is defined; there is a unique “lowest” permutation “above” two given permutations. Figure 1 is the lattice diagram when $n = 4$.]

  #answer()[Suppose we have $(a, b) in macron(E), (b,c) in macron(E) "but" (a, c) in E$, there are 2 possible situations:
  1. $(a, c) in E(pi_1) union E(pi_2)$, but this contradicts that $(a,b), (b,c) in macron(E)$
  2. $(a, c) in.not E(pi_1) union E(pi_2)$, then suppose we have $(a, x) in E(pi_1)$ and $(x, c) in E(pi_2)$, then $ a > x > c$;because $(a, b) in macron(E)$ and $(b, c) in macron(E)$, then $a > b >c$\
    2.1. if $b > x$ then $(b, x) in E(pi_1)$, then ${(b, x), (x, c)} subset E(pi_1) union E(pi_2)$ so $(b, c) in E$, contradicts.
    2.2. if $b < x$ then $(b, x) in E(pi_1)$, same argument.
  So it's impossible to have $E$ be the smallest transitive set containing $E(pi_1) union E(pi_2)$ while $macron(E)$ be non-transitive $qed$
  ]

13. [M23] It is well known that half of the terms in the expansion of a determinant have a plus sign, and half have a minus sign. In other words, there are just as many permutations with an _even_ number of inversions as with an _odd_ number, when $n ≥ 2$. Show that, in general, the number of permutations having a number of inversions congruent to $t$ modulo $m$ is $n!/m$, regardless of the integer $t$, whenever $n ≥ m$.

  #answer()[
  it means there are $n!/m$ permutations that have $b_i$ inversion such $b_i equiv t(mod m)$]

14. [M24] (F. Franklin.) A partition of n into k distinct parts is a representation $n = p_1+ p_2+ · · · + p_k$, where $p_1 > p_2 > · · · > p_k> 0$. For example, the partitions of 7 into distinct parts are 7, 6 + 1, 5 + 2, 4 + 3, 4 + 2 + 1. Let $f_k(n)$ be the number of partitions of n into k distinct parts; prove that $sum_k (−1)^k f_k(n) = 0$, unless n has the form $(3j^2± j)\/2$, for some nonnegative integer $j$; in the latter case the sum is $(−1)^j$.

15. [M23] Prove that (16) is the generating function for partitions into at most $n$ parts; that is, prove that the coefficient of $z^m "in" 1\/(1 − z)(1 − z^2) . . . (1 − z^n)$ is the number of ways to write $m = p_1 + p_2 + · · · + p_n$ with $p_1 ≥ p_2 ≥ · · · ≥ p_n$ ≥ 0.

  #answer()[]

== 5.1.2 Permutations of a Multiset

$ binom(n, n_1\, n_2\, ...) = (n!)/(n_1!n_2!...) $

*intercalation*

= 5.2 Internal Sorting



