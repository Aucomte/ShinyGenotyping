tabItem(
  tabName ="menu",
  tabsetPanel(
    tabPanel("Home", 
         h3("Overview"),
         p("Welcome to the Shiny Genotyping Server"),

         ## CITATION ##
         
         p("The DAPC Server is designed for the analysis of multilocus genotyping data, specifically micro/minisatellite data and Multilocus VNTR analysis (MLVA), on haploid microorganisms (bacteria and fungi)"),
        
         h3(br(),"Github"),
         
         p("I recommand to download the code and use the application from github to avoid several server problems and for more feedback"),
         a("https://github.com/Aucomte/ShinyGenotyping", href="https://github.com/Aucomte/ShinyGenotyping", target="_blank"),
         
         h3(br(),"Citations"),
         
         br(),
         box(width = 12,
          p("the DAPC tab was adapted from the code written by the adegenet team. They have their own DAPC shiny application : https://github.com/thibautjombart/adegenet")
         ), 
         h4("Citation for adegenet:"),
         p("Jombart T.(2008) adegenet: a R package for the multivariate analysis of genetic markers. Bioinformatics 24: 1403-1405. doi:10.1093/bioinformatics/btn129", a("[link to paper]", href="http://bioinformatics.oxfordjournals.org/cgi/reprint/btn129?ijkey=6sqx5BTXCdYtBZz&keytype=ref", target="_blank")),
         p("Jombart T. and Ahmed I. (2011) adegenet 1.3-1: new tools for the analysis of genome-wide SNP data. Bioinformatics. doi: 10.1093/bioinformatics/btr521"),
         
         h4("Citation for the DAPC:"),
         p("Jombart T, Devillard S and Balloux F (2010) Discriminant analysis of principal components: a new method for the analysis of genetically structured populations. BMC Genetics 11:94. doi:10.1186/1471-2156-11-94", a("[link to paper]", href="http://www.biomedcentral.com/1471-2156/11/94", target="_blank")),
         
         a("http://adegenet.r-forge.r-project.org/", href="http://adegenet.r-forge.r-project.org/", target="_blank"),
         
         ## poppr
         br(),
         box(width = 12,
          p("The minimum spaning network (msn) tab was adapted from the code written by the adegenet team. They have their own msn shiny application (named imsn) : https://github.com/grunwaldlab/poppr")
         ),
         h4("Citation for poppr:"),
         
         p("Kamvar ZN, Tabima JF, Grünwald NJ (2014). “\textit{Poppr}: an R package for genetic analysis of populations with clonal, partially clonal, and/or sexual reproduction.” PeerJ, 2, e281. ISSN 2167-8359, doi: 10.7717/peerj.281, https://doi.org/10.7717/peerj.281."),
         
         p("Kamvar ZN, Brooks JC, Grünwald NJ (2015). “Novel R tools for analysis of genome-wide population genetic data with emphasis on clonality.” Front. Genet., 6, 208. doi: 10.3389/fgene.2015.00208, https://doi.org/10.3389/fgene.2015.00208.")
         
         )
        )
)