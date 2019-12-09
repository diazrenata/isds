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

In these scenarios, the overlap metric behaves as expected. We have high overlap values when we have species we think should overlap fully, near 0 when there are either clumps that do not overlap or when there's no overlap between any species, and a spectrum of intermediate values when all the species overlap partially.

Adding a logseries SAD
----------------------

Let's add this SAD, drawn from the METE logseries. We'll assign abundance to species at random.

![](overlap_explainer_files/figure-markdown_github/plot%20sad-1.png)

![](overlap_explainer_files/figure-markdown_github/plot%20ls%20outcomes-1.png)![](overlap_explainer_files/figure-markdown_github/plot%20ls%20outcomes-2.png)![](overlap_explainer_files/figure-markdown_github/plot%20ls%20outcomes-3.png)![](overlap_explainer_files/figure-markdown_github/plot%20ls%20outcomes-4.png)

Just adding the SAD - and drawing all our samples from the *same distributions as above* - turns up unexpected overlap results. We get unexpectedly low values for the complete overlap scenario, because we are constructing kdes for low abundance species based on very few samples. Those samples may be off the "true" mean, and the resulting kde is much pointier than one constructed off of more samples. I think the complete\_overlap purple species illustrates being off the mean, and the complete\_overlap purple species illustrates the pointy kde. We also got some overlaps pushed downards in the partial\_overlap scenario, again because of artifiically pointy kdes.

Upsampling the logseries SAD
----------------------------

We can potentially get around the artificial pointiness by *up*-sampling our logseries SAD. Let's say the least abundant species now has 50 individuals, and everyone else increases proportionally.

For now we will dodge the error in estimating the mean by doing the upsampling with knowledge of the "true" distribution. Next we will go after the error in the mean.

Note the new axes for the SAD:

![](overlap_explainer_files/figure-markdown_github/show%20new%20sad-1.png)

![](overlap_explainer_files/figure-markdown_github/show%20upsampled%20LS%20outcomes-1.png)![](overlap_explainer_files/figure-markdown_github/show%20upsampled%20LS%20outcomes-2.png)![](overlap_explainer_files/figure-markdown_github/show%20upsampled%20LS%20outcomes-3.png)![](overlap_explainer_files/figure-markdown_github/show%20upsampled%20LS%20outcomes-4.png)

This has gotten us back to our expected outcomes. Now let's fold in the error propagation we expect from low abundances.

Upsample with error propagation
-------------------------------

``` r
ls_new_means <- lapply(ls_communities, FUN = function(community_df) 
  return((community_df %>% group_by(species) %>% summarize(meanwgt = mean(wgt)) %>% ungroup())$meanwgt))

ls_up_error_communities <- lapply(ls_new_means, FUN = sample_community, abunds = ls_abunds_up)

ls_up_error_plots <- lapply(ls_up_error_communities, FUN = plot_pipeline)
```

Here's how well we recovered the true means: (actually pretty well)

![](overlap_explainer_files/figure-markdown_github/show%20mean%20recovery-1.png)

And here's the outcomes:

![](overlap_explainer_files/figure-markdown_github/show%20error%20prop%20outcomes-1.png)![](overlap_explainer_files/figure-markdown_github/show%20error%20prop%20outcomes-2.png)![](overlap_explainer_files/figure-markdown_github/show%20error%20prop%20outcomes-3.png)![](overlap_explainer_files/figure-markdown_github/show%20error%20prop%20outcomes-4.png)

**We have gotten drift**, most visible in the complete\_overlap scenario.
