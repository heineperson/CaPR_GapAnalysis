# CaPR_GapAnalysis
California Plant Rescue Gap Analysis 2019

The purposes of this repo is to share the analysis and results of California Plant Rescue Gap Analysis. The data is not available publicly because of the sensitivity of rare plants.

## 02 Phylogenetic Tree Construction

We constructed the phylogenetic tree used for this analysis following the V.PhyloMaker R script described in (Jin & Qian, 2019). This program matched the genera and families in our species list to an angiosperm mega-phylogeny (GBOTB.extended.rda). We selected “Scenario 3” in V.PhyloMaker to construct our tree using the same methodology as phylomatic (Webb & Donoghue, 2005) and assign branch lengths in accordance with BLADJ in phylocom (Webb, Ackerly, & Kembel, 2008). We also have functioning code for creating the phylogeny in S.Phylomaker ("xx_PhylogeneticTreeSPhyloMaker_worksButNotInUse", "R_codes for S.Phylomaker"), which is from an earlier paper (Qian & Jin 2016). However, we do not currently use S Phylomaker in this pipeline.

To add subspecies to this tree, I used regular expressions to create a infraspecies polytomy in place of the species level taxon on the phylogenic tree. I made assumptions about branch length that will need to be revisited.

V phylomaker package can be downloaded at repo: https://github.com/jinyizju/V.PhyloMaker, which asks that you cite the following paper:  Jin Y & Qian H (2019) V.PhyloMaker: an R package that can generate very large phylogenies for vascular plants, Ecography, DOI: 10.1111/ecog.04434

The file "R Codes from S.Phylomaker" is from the following repo: https://github.com/jinyizju/S.PhyloMaker, which asks that you cite the following paper: Qian, H. and Y. Jin. (2016) An updated megaphylogeny of plants, a tool for generating plant phylogenies and an analysis of phylogenetic community structure. Journal of Plant Ecology 9(2): 233–239.


