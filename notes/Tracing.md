There are two possible directions

- Forward in time. This implies that a traced chromosome simply disappears once it is not selected for breeding.
- Backward in time. If the population size is constant, it implies that any chromosome picked can be traced back
  to the very beginning of the process, because in each iteration it must be the result of either mutation or
  cross-over.
  
The event system is non-existent in this retrospective. We will have to "look" at the changes in the structure.
With the current implementation, access path grows linear with iterations, making the construction of previous
versions rather simple.

The visual system would maintain a sort of identifier-map, but we cannot use the standard map because prefixes
shrink instead of growing. If any visual object stores the identifier, we can iterate backwards by updating
all identifiers for each step and thus be able to look up objects in the previous iteration. We would find some
that exist, so the graphical hierarchies can be updated, and those that are "new", requiring the creation of
new objects. Indeed, objects can appear and disappear multiple times during the "ancestry" of one particular
chromosome. Keeping track of these recurrences is much more effort, and probably irrelevant with respect to
the visual transposition (?).

Time is reversible. We can assemble frames in the order in which they are constructed or in reverse order.
The physical modelling layout is probably "immune" ot this reversal, forces remain forces, time does not
have the shape of an arrow.

## Scenario: Mutation

- addition and removal of vertices is straight forward. More effort to identify the change in position of
  a vertex
- addition and removal of edges likewise
- altering of a constant simple

## Scenario: Cross-Over

- we'd find a second new object
- it would be kind of interesting to preserve some of the history of that second object
- thus there could be a sort of fade period during which the existence of that object is upheld,
  then it would either disappear or be again part of the main traced object.
- graphically we would/might keep the main object in the 'camera', so ancestors in cross-over
  might appear 'peripherally'.
- indeed than the periphery could be allowed to drift as long as it pleases and would be automatically
  discarded if it exits the view port, solving the problem of incremental growth of the graph.

## Algorithm: Step backwards

- basically what we want is to "grow" elements anywhere they "connect" to the current state.
- example: `(c, t1)` is cross-over from `(a, t0)` and `(b, t0)`
- we don't know where `a` and `b` are in the genome. Perhaps also the head element of `c`
  got mutated at the same time. I.e. it may be that neither `a.head` nor `b.head` equals `c.head`
- therefore, the only solution is an exhaustive traversal through the whole genome
- so what is the identity of `c` that we're supposed to focus on (e.g. with the view-port),
  it appears it _does not exist_ as such. That's interesting. We've got to draw an arbitrary boundary.
- i.e. under _melding_, identities multiply both in the forward and backward direction of time
- we could either focus on a random part in the case of a cross-over, or we could give up the
  focus altogether, letting bits disappear via the dynamic layout and the view port
- if we discover new "neighbourhood" of an existing element, we can either lazily expand only
  to the next linked element in each update, or we can traverse the whole chromosome; in
  the latter case, we need to either scan the chromosome multiple times or maintain a link-back list
  