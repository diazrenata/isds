# isds - Analysis of individual size distributions (of mammals)

### Status as of 12/2/2019

This repo has spawned a number of branches, the most recently active of which is `empty-ids`. They all swirl around trying to find a way to detect modes, or valleys, in an ISD in a reproducible way. The trouble stems from, these are kind of subjective determinations. I'm now considering a human-classified system, where a human looks at a plot and answers a few questions about it. You might eventually be able to feed this into machine learning, but right now I think even just a few hours of clicking would get us through this one analysis.

OK So the questions.

- Some theories (Holling) are making a statement about the existence of *gaps* between clumps. The question here is, "is there/how many *regions of low probability* are there in this ISD"? Plus some additional specifications: the regions must be distinct, interspersed with regions of *appreciably high probability/density*. It doesn't count if it's a blip down, and it doesn't count if it never picks up again afteward. It doesn't count if it only barely picks up. *It also doesn't count if it's a drop compared to the two centers of mass beside it, but still not super low-probability.* That last is a syndrome I think gets its own label...
- ESS seems to me more about there being multiple modes where you're similar. I think of this as centers-of-mass; if there's multiple distinct high-probability regions interspersed by lower-probability, but not necessarily extremely low. 
- Then there's modes, which is basically turnpoints.

A human looking at an ISD can make a determination of gaps or not. There can be centers of mass without gaps. There can be modes without centers of mass. These are kind of increasing in how much of a smush it is.
