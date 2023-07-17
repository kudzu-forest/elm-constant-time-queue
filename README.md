This module provides an implementation of one-way queue whose _worst-case_ time complexity of `enqueue` and `dequeue` is _O(1)_.

The queue is implemented with a technique called _Recursive Slow-Down_, inspired by a paper for computer science.([Haim Kaplan _et.al_.](https://dl.acm.org/doi/pdf/10.1145/225058.225090))
