tabItem(
  tabName ="menu",
  tabsetPanel(
    tabPanel("Home", 
             h3("Overview"),
             p("Welcome to the DAPC Server Help Section. Under each section heading below you can find a brief description and useful information
               about the content of each tab on the Server. If you want to know more about a specific item or are seeking the definition of a term,
               you may be more interested in the Glossary on the adjacent tab."),
             
             p("The DAPC Server aims to provide a user-friendly interactive application of some of the functions contained in the R package adegenet.
               More information about adegenet can be found on the adegenet website:"),
             
             a("http://adegenet.r-forge.r-project.org/", href="http://adegenet.r-forge.r-project.org/", target="_blank"),
             
             p(br(),"On the DAPC Server, users can explore the Discriminant Analysis of Principal Components (DAPC) method.
               DAPC is a multivariate method that uses genetic data to describe the differences between pre-defined biological populations.
               DAPC is a dimension reduction approach that generates synthetic variables composed of weighted combinations of the original variables
               in a dataset to optimally capture between-group variation. DAPC uses Principal Components Analysis (PCA) as a prior step
               to Discriminant Analysis (DA) to identify up to (K - 1) linearly uncorrelated discriminant axes that can optimally discriminate between
               K groups of individuals. Unlike DA alone, DAPC is able to perform this procedure when the number of variables (alleles) greatly
               exceeds the number of individuals, and also in the presence of correlations between variables."),
             
             p("DAPC is a supervised method, which means that the clusters of individuals to be analysed must be pre-defined by the user.
               In cases where individuals are not classified into groups, procedures (like that of the find.clusters function in R) can be
               used to identify clusters so that a DAPC analysis can be carried out. These procedures are not included in the present version of the
               DAPC Server, however, which requires all data to be imported with inherently defined populations."),
             
             p("For any dataset containing a set of genetic variables for any number of individuals and a population grouping factor,
               DAPC can be used to explore between-population differentiation,
               estimate the probabilities of individual assignment to all possible groups,
               and to examine the contribution of individual alleles to population structuring."),
             
             p("Acceptable file types for input datasets include 'gtx/gen/dat/GTX/GEN/DAT/RData/Rdata/Rda/rda' and 'GENETIX/genepop/Fstat/R data'."),
             
             ## CITATION ##
             
             h3(br(),"Citing the DAPC Server"),
             h5("Citation for adegenet:"),
             p("Jombart T.(2008) adegenet: a R package for the multivariate analysis of genetic markers. Bioinformatics 24: 1403-1405. doi:10.1093/bioinformatics/btn129", a("[link to paper]", href="http://bioinformatics.oxfordjournals.org/cgi/reprint/btn129?ijkey=6sqx5BTXCdYtBZz&keytype=ref", target="_blank")),
             p("Jombart T. and Ahmed I. (2011) adegenet 1.3-1: new tools for the analysis of genome-wide SNP data. Bioinformatics. doi: 10.1093/bioinformatics/btr521"),
             
             h5("Citation for the DAPC:"),
             p("Jombart T, Devillard S and Balloux F (2010) Discriminant analysis of principal components: a new method for the analysis of genetically structured populations. BMC Genetics 11:94. doi:10.1186/1471-2156-11-94", a("[link to paper]", href="http://www.biomedcentral.com/1471-2156/11/94", target="_blank"))
             ),
    tabPanel("Glossary",
             tabPanel("Glossary",
                      h3("Compoplot"),
                      p("A compoplot is a bar plot showing the probabilities of assignment of individuals to the different clusters."),
                      h3("Cross-validation"),
                      p("Cross-validation is an optimisation procedure that is used in the context of DAPC to identify the number of principal components
                        that gives rise to the model with the highest predictive capacity. In cross-validation for DAPC, the data is divided into a training
                        set and a validation set (by default, comprising 90% and 10% of the data, respectively). The analysis is run on the training set with
                        variable numbers of PCs retained, and the degree to which the analysis is able to accurately predict the group membership of
                        excluded individuals (those in the validation set) is used to select the optimal number of PCs to retain."),
                      p("Note: Performing cross-validation can substantially improve the results of DAPC; however, the amount of computational time
                        required increases with the size of the dataset in question and the number of replicates carried out."),
                      h3("DA"),
                      p("Discriminant Analysis (DA) is a procedure for optimally describing the differences between groups of individuals.
                        DAPC uses DA subsequent to Principal Component Analysis to maximise discrimination between groups in conditions
                        where DA alone would be inappropriate."),
                      h3("DA axis"),
                      p("DAPC uses Discriminant Analysis (DA) to describe the differences between K groups of individuals along
                        a maximum of (K - 1) Discriminant Analysis axes (DA axes)."),
                      h3("DAPC"),
                      p("Discriminant Analysis of Principal Components (DAPC) is a multivariate method that uses genetic data to describe the
                        differences between pre-defined biological populations. DAPC uses Principal Component Analysis as a prior step to
                        Discriminant Analysis to identify weighted linear combinations of the original variables which give rise to optimal
                        between-group discrimination."),
                      p("For more, see: Jombart T, Devillard S and Balloux F (2010) Discriminant analysis of principal components: a new method for the
                        analysis of genetically structured populations. BMC Genetics11:94. doi:10.1186/1471-2156-11-94."),
                      h3("Inertia ellipse"),
                      p("The inertia ellipses displayed optionally on the DAPC scatter plot provide graphical summaries of a cloud of points.
           They are meant to give shape to groups of individuals, and do not necessarily represent a 95% confidence
           interval on the position of the centroid of a cluster."),
                      h3("Loading"),
                      p("Loadings provide a measure of the contribution of each original variable to the discrimination between groups along a given
           discriminant axis. A loading plot is used to visualise these loadings so that one can, for example, assess the weight of each
           variable and identify those variables whose contributions exceed a threshold of interest."),
                      h3("Minimum spanning tree"),
                      p("A minimum spanning tree is a graph theoretical phenomenon in which all of the vertices (in our case, cluster centroids) of
           a graph are connected into the tree that contains the shortest set of possible paths between vertices."),
                      h3("PCA"),
                      p("Principal Component Analysis (PCA) is a multivariate statistical method that generates linearly
           uncorrelated principal components (PCs) composed of weighted linear combinations of the original variables
           to represent overall variation in the data in a reduced space."),
                      h3("PC, PCA axis"),
                      p("Principal components (PCs, or PCA axes) are sets of linearly uncorrelated synthetic variables composed of
           weighted linear combinations of original variables. PCs are used to describe multivariate phenomenon in a smaller (or equal)
           number of dimensions. The 'optimal' number of PCs depends on the data. In DAPC, retaining too few PCs will cause useful
           information to be excluded from the analysis (and hence discriminative power to be lost), while retaining too many PCs
           may lead to problems of overfitting and uninterpretability."),
                      h3("RMSE"),
                      p("The root mean squared error (RMSE) is a measure of the error of an estimator. On the DAPC Server, RMSE is used in
           cross-validation to assess the ability of the model, with variable numbers of principal components retained, to achieve
           perfect prediction of individuals into the correct group."),
                      p("RMSE = sqrt((1/n)*sum(i=1 to n)(Yhat.i - Yi)^2), where Yhat is a vector of n predictions,
           and Y is the vector of the true values."),
                      h3("Screeplot"),
                      p("A graphical representation of the variance, or cumulative variance, contained in the set of principal components
           or discriminant functions. Components shaded in black represent those that have been retained in the analysis."),
                      h3("Training set"),
                      p("The training set, in cross-validation, is the set of individuals retained in the analysis.
           The complement of the training set is the 'validation set', which contains the remaining individuals excluded from the analysis
           who are used to test the performance of the model which varies as a function of the number of PCs retained.",br(),br(),br())
                      )
                      )
             )
            )