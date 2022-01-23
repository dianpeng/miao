# Typer

The goal of typer is to broadcast the type information into the node as early
as possible.

## Feedback

Most of the high level type information comes from the feedback provided by the
interpreter. Each function have a feedback vector contains the feedback of
runtime bytecode interpretation. A value's type can be observed from the feedback.

The problem is that the type can only be observed at its use place instead of
the actual type mutation. Ie, only by using the value we snapshot the type
status of each value sparesly.

To avoid the expensive deoptimization, the runtime tries to guess the type in
a relative conservative manner, ie the type will become the used type only
when certain threshold reached, ie becomes stable. Plus since we try to generate
type guards during lowering, the type guard information will mostly come from
the operation's type feedback. 

So if a value a is not used enough then its type is unknown from a certain use
site. However, from another use site, its type may already be very stable. This
pass hope we can propagate the type information from other use site to all its
use site.


## Consistency

Since we can infer types from many places, most of the cases the type should
be consistent from multiple observation points due to the fact that graph is a
SSA one. Any modification of a name will result in a different node. Currently,
if we observe inconsitent type, ie 2 operation's guess of the same type doesn't
match, like one say int the other say string, we will bailout from typer and
JIT for now.
