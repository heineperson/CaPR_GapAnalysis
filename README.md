# CaPR_GapAnalysis
California Plant Rescue Gap Analysis 2019

The purposes of this repo is to share the analysis and results of California Plant Rescue Gap Analysis. The data is not available publicly because of the sensitivity of rare plants.

## 02 Phylogenetic Tree
We constructed the phylogenetic tree used for this analysis following the S.PhyloMaker R script described in (Qian & Jin, 2016). This program matched the genera and families in our species list to an angiosperm mega-phylogeny (PhytoPhylo), which is an expansion of the tree published in (Zanne et al., 2014). We selected “Scenario 3” in S.PhyloMaker to construct our tree using the same methodology as phylomatic (Webb & Donoghue, 2005) and assign branch lengths in accordance with BLADJ in phylocom (Webb, Ackerly, & Kembel, 2008).

We also attempte to make phylogenetic trees using the updated version of the S.PhylaMaker package V.Phylomaker, which uses a newer version of the phylogeny

The file "R Codes from S.Phylomaker" is from the following repo: https://github.com/jinyizju/S.PhyloMaker, which asks that you cite the following paper: Qian, H. and Y. Jin. (2016) An updated megaphylogeny of plants, a tool for generating plant phylogenies and an analysis of phylogenetic community structure. Journal of Plant Ecology 9(2): 233–239.

The file "phylomaker_for_VPhylomaker.R" is from the following repo: https://github.com/jinyizju/V.PhyloMaker, which asks that you cite the following paper:  Jin Y & Qian H (2019) V.PhyloMaker: an R package that can generate very large phylogenies for vascular plants, Ecography, DOI: 10.1111/ecog.04434



