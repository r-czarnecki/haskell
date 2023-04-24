# Individual Programming Project - Haskell

This project was created as part of the University of Warsaw's Individual Programming Project course.

The repository contains an implementation of sets and directed graphs in Haskell. 

Both sets and graphs can use `union` operation. Additionally graphs have access to operations:
- `connect` which given two graphs (V1, E1) and (V2, E2) creates a graph (V1 ∪ V2, E1 ∪ E2 ∪ V1 × V2)
- `fromBasic` which converts algebraic representation of a graph to the relational representation
- `mergeV` which for vertices `a`, `b`, `c` and a graph `g`, creates a modified graph `g'`, whose vertices `a` and `b` are merged into a single vertex `c`
- `splitV` which for vertices `a`, `b`, `c` and a graph `g`, creates a modified graph `g'`, whose vertex `a` is split into its two identical copies `b` and `c`
- `todot` - represents a graph in a GraphViz (graphviz.org) format