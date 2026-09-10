# Reading a trajectory plot

Part of the [qualembed](../README.md) documentation.

### Looking at a trajectory

```r
plot_recurrence(emb, seg, doc = "P07")                       # start here
plot_arc(emb, seg, y = "forward_flow", null_band = 999)      # and here
plot_arc(emb, seg, y = "projection", high = H, low = L)      # on your own axis
plot_trajectory(emb, seg, doc = "P07")                       # the map, read with care

trajectory_stats(emb, seg)          # path length, step, straightness (full space)
trajectory_null(emb, seg, 999)      # is the order doing anything?
trajectory_fidelity(emb, seg)       # can the map be trusted?
recurrence_stats(emb, seg)          # RR, DET, LAM at a fixed recurrence rate
```

**The order of that list is the recommendation.** The first two displays are
projection-free: every quantity they show is computed in the full space, and the
axes carry nothing but position. The map comes last because it is the only one of
the three whose geometry you must qualify while reading it, and a display you have
to caveat is a poor first look at your data.

`plot_recurrence()` is a segment × segment cosine matrix for one document. Every
cell is computed in the **full** space and both axes are just position, so nothing
is projected at all. Near-diagonal blocks are topic episodes; an off-diagonal block
is the speaker returning to an earlier theme; a bright vertical stripe is one early
passage the rest of the interview keeps referring back to. Read it against
`trajectory_null()`, which asks whether what you are reading off the plot
survives a permutation of the segment order. Shifts and returns do not
necessarily fare alike, and which of them holds up is worth knowing before you
write either into a paper.

`plot_arc()` draws one full-space quantity per segment against narrative position:
a projection onto an axis you defined, the distance from the previous segment, or
the mean distance from everything said so far. `null_band` shuffles the segment
order and shades where a bag of the same segments would have fallen.

`plot_trajectory()` is the picture most people picture: each segment placed by a
two-dimensional layout (non-metric MDS by default), joined in the order it
was spoken, with arrows.
**The order is exact — projection cannot distort which segment follows which — so
"the account went out and came back" is a reading you may take from the arrows.
Distance is not exact, so "this person travelled further" is not.** That is a
full-space quantity and `trajectory_stats()` measures it. The subtitle prints how
well the on-page distances rank-correlate with the measured ones, and beside it
the same quantity for a null: the same count of segments drawn at random from the
pool. Read the difference, not the raw number — a handful of points lands in a
plane easily whether or not they form a path. Pass `scope = "shared"` when you want
panels that can be compared with one another; the basis and the axis limits then
come from the whole pool rather than from each document's handful of points.

**Three cautions the package enforces rather than merely documents.**

- Raw **path length is not a measure of how far the account travelled**. It is a
  sum of steps, so it grows with the number of steps: it mostly counts how many
  times you cut. Use `step_mean`,
  `straightness`, or the arc; report `path_length` only beside the segment count.
- **A distance measured on a two-dimensional picture is a property of the
  picture.** `trajectory_length()`, which did exactly that, is deprecated and
  warns; `plot_trajectory()` prints its own fidelity so you are never guessing.
- **The same summary number is compatible with different trajectories.** Report the
  scalar and the plot together; neither alone is the finding.

None of these statistics are new — segment-embed-trajectory has been done at scale
in marketing and in clinical speech research, and psychology has its own
chained-utterance measure with a published dispute about what it means. What this
package adds is that they run in R, on commercial APIs, with a permutation null
attached. Their validity on interview-length material has not been established by
anyone, including us.

