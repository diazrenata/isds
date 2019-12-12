Measuring overlap in SBSDs
================

SBSD = species body size distribution. The frequency/probability density of body sizes across all individuals of a species.

Rationale & background of using overlap
---------------------------------------

The ISD (the size distribution across all individuals of all species) is...slippery and often subject to interpretation.

Both Holling and ESS would predict polarized/bimodal distribution of overlap among all pairings of species. Species should either be quite similar or quite different, so high or low, but not intermediate, overlap.

I've imported the SBSD overlap metric from [Read et al 2018](https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.03641):

1.  For all pairs of species in a community, construct the KDEs across the entire body size range and standardize so each KDE integrates to 1.
2.  Integrate the minimum of the two KDEs at each evaluation point.

In principle this varies from 0 to 1 for each pair of species.

Portal data
-----------

![](portal_overlap_files/figure-markdown_github/show%20plots-1.png)

Upsample with error propagation
-------------------------------

Here's the outcomes:

![](portal_overlap_files/figure-markdown_github/show%20error%20prop%20outcomes-1.png)
