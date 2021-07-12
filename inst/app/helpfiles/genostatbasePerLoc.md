The following statistics, defined in eq.7.38– 7.43 pp.164–5 of Nei (1987) are estimated:

    • Ho = observed heterozygosity (not calculated for haploid data)
    • Hs= Within population gene diversity (sometimes misleadingly called expected heterozygosity)
    • Ht= overall gene diversity
    • Dst=Ht-Hs = the amount of gene diversity among samples 
    • Dst'=Dstp=np/(np-1)Dst, with np being the number of samples. 
    • Ht'=Hs+Dst'
    • Fst=Dst/Ht.(This is not the same as Nei's Gst, Nei's Gst is an estimator of Fst based on allele frequencies only)
    • Fst'=Dst'/Ht'
    • Fis=1-Ho/Hs : inbreeding coefficient (not calculated for haploid data)
    • Dest=np/(np-1) (Ht'-Hs)/(1-Hs) a measure of population differentiation as defined by Jost (2008).
    
Here, the p_{ki} are unweighted by sample size. These statistics are estimated for each locus and an overall loci estimates is also given, as the unweighted average of the per locus estimates. In this way, monomorphic loci are accounted for (with estimated value of 0) in the overall estimates.
Note that the equations used here all rely on genotypic rather than allelic number and are corrected for heterozygosity.
Written from Notes of the hierfstat ::basic.stats function (Author :Jerome Goudet (jerome.goudet@unil.ch)). For further details, please consult these notes.