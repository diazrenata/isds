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

Foreshadowing artefacts from rare species
-----------------------------------------

1.  Pointy KDEs
2.  Higher uncertainty wrt the true SBSD mean, sd, and indeed entire distribution.
3.  We may inherently want to downweight rare species, but *this is separate from* the above.

Demo with simple cases
----------------------

Let's leave rare species aside for the moment, and focus on how we expect the overlap distributions to come out under a few toy scenarios:

1.  All species are essentially the same; complete overlap
2.  Two clusters of species. Within clusters, species are essentially the same. The clusters do not overlap.
3.  All species are distinct and do not overlap.
4.  All species are different but overlap significantly.

For now, we will give all species an equal, reasonably large, number of individuals (50). This is wildly unrealistic for real communities.

We will assume all species' SBSDs are normal distributions, and that the standard deviation scales with the mean according to some coefficient. `.15` seems like a plausible coefficient to me.

![](overlap_explainer_files/figure-markdown_github/show%20plots-1.png)![](overlap_explainer_files/figure-markdown_github/show%20plots-2.png)![](overlap_explainer_files/figure-markdown_github/show%20plots-3.png)![](overlap_explainer_files/figure-markdown_github/show%20plots-4.png)

Demo SAD distortions
--------------------

Let's add this SAD, drawn from the METE logseries:

![](overlap_explainer_files/figure-markdown_github/plot%20sad-1.png)

We're assigning abundance to species at random.

![](overlap_explainer_files/figure-markdown_github/plot%20ls%20outcomes-1.png)![](overlap_explainer_files/figure-markdown_github/plot%20ls%20outcomes-2.png)![](overlap_explainer_files/figure-markdown_github/plot%20ls%20outcomes-3.png)![](overlap_explainer_files/figure-markdown_github/plot%20ls%20outcomes-4.png)
