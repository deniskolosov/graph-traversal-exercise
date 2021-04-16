Graph traversal exercise

Clone the repo:

```
git clone https://github.com/deniskolosov/graph-traversal-exercise.git
cd graph-traversal-exercise
```

Run lein (make sure it is installed):

```bash
lein repl 
```

Run functions:

```
(def random-graph (make-graph 10 10))
(shortest-path random-graph (first (keys random-graph)) (last (keys random-graph))

(eccentricity random-graph (first (keys random-graph))) ; => number expressing eccentricity for first vertex in random-graph

(radius random-graph) ; => minimal eccentricity
(diameter random-graph) ; => maximal eccentricity
```

Or just go to `src/user.clj` and run the repl.


