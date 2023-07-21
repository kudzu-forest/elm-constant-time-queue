This module provides an implementation of one-way queue whose _worst-case_ time complexity of `enqueue` and `dequeue` is _O(1)_.

The queue is implemented with a technique called _Recursive Slow-Down_, inspired by a paper for computer science.([Haim Kaplan _et.al_.](https://dl.acm.org/doi/pdf/10.1145/225058.225090))

If you are interested in the inner structure,
interactive demonstration is given at
[https://ellie-app.com/nqXvQBsz4Zfa1](https://ellie-app.com/nqXvQBsz4Zfa1).
(It is recomended to read the third section of the paper above(_Persistent Deques without Catenation_), where double-ended version is precisely explained.)
