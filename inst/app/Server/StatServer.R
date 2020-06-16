
archiveCreation <- observeEvent(input$submitarchive, ignoreInit = TRUE, {
  sr$archive = diversitybyloc(sr$Genind)
  sr$xvm.stat = genind2hierfstat(sr$Genind)
  sr$pairwisefst = as.matrix(genet.dist(sr$Genind, method="WC84"))
  X = allelic.richness(sr$xvm.stat,diploid=F)
  sr$richness = X$Ar
  sr$richnessMIN = X$min.all
  colnames(sr$richness) = levels(X$pop)
  
  if(sr$ploidy_number == 2){
    sr$stats =rbind(basic.stats(sr$Genind, diploid=T, digits=2)$perloc, rbind(basic.stats(sr$Genind, diploid=T, digits=2)$overall))
    rownames(sr$stats)[nrow(sr$stats)] = "Overall"
  }
  else{
    sr$stats =rbind(basic.stats(sr$Genind, diploid=F, digits=2)$perloc, rbind(basic.stats(sr$Genind, diploid=F, digits=2)$overall))
    rownames(sr$stats)[nrow(sr$stats)] = "Overall"
  }
  
  output$heatmapDiv <- renderPlot({
    info_table(sr$Genind, plot=TRUE)
  })
  
  output$genind2hierfstat<- DT::renderDataTable({
    DT::datatable(
      sr$xvm.stat,
      extensions = 'Buttons',
      rownames= FALSE,
      filter = list(position = 'top', clear = TRUE, plain = FALSE),
      options = list(
        scrollX=TRUE,
        dom = 'Blfrtip', 
        buttons = list(
          'copy', 
          'print',
          list(
            extend = "collection", 
            text = "Download entire dataset",
            #buttons = c("csv","excel","pdf"),
            action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test2', true, {priority: 'event'});}")
          )
        ),
        lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
          "}"
        )
      )
    )
  })
  output$download2 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(sr$xvm.stat, file, sep="\t", dec= ",", col.names = T, row.names = F)
    }
  )
  myModal2 <- function() {
    div(id = "test2",
        modalDialog(downloadButton("download2","Download as csv"),easyClose = TRUE, title = "Download Table")
    )
  }
  observeEvent(input$test2, {
    showModal(myModal2())
  })
  
  output$pairwiseFST<- DT::renderDataTable({
    DT::datatable(
      sr$pairwisefst,
      filter = list(position = 'top', clear = TRUE, plain = FALSE),
      extensions = 'Buttons', 
      options = list(
        scrollX=TRUE,
        dom = 'Blfrtip', 
        buttons = list(
          'copy', 
          'print',
          list(
            extend = "collection", 
            text = "Download entire dataset",
            #buttons = c("csv","excel","pdf"),
            action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test3', true, {priority: 'event'});}")
          )
        ),
        lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
          "}"
        )
      )
    )
  })
  
  output$genostatbasePerLoc <- DT::renderDataTable({
    DT::datatable(
      sr$stats,
      filter = list(position = 'top', clear = TRUE, plain = FALSE),
      extensions = 'Buttons', 
      options = list(
        scrollX=TRUE,
        dom = 'Blfrtip', 
        buttons = list(
          'copy', 
          'print',
          list(
            extend = "collection", 
            text = "Download entire dataset",
            #buttons = c("csv","excel","pdf"),
            action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test3', true, {priority: 'event'});}")
          )
        ),
        lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
          "}"
        )
      )
    )
  })
  output$download3 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(sr$stats, file, sep="\t", dec= ",", col.names = T, row.names = F)
    }
  )
  myModal3 <- function() {
    div(id = "test3",
        modalDialog(downloadButton("download3","Download as csv"),easyClose = TRUE, title = "Download Table")
    )
  }
  observeEvent(input$test3, {
    showModal(myModal3())
  })
})


observeEvent(input$Submitcurve,{
  sr$drop = input$drop
  sr$dropna = input$dropna
  sr$thresgeno = input$thresgeno
  sr$samplegeno = input$samplegeno
       output$genocurve <- renderPlot({
           genotype_curve(sr$Genind, sample=sr$samplegeno, drop=sr$drop, dropna=sr$dropna, thresh=sr$thresgeno) + theme_bw() + geom_smooth()
       })
})


observeEvent(input$Submitstat,{
     #nameStrata(sr$Genind) <-~sr$strata
     sr$samplepoppr = input$samplepoppr
     sr$minsamp = input$minsamp
     sr$missingpopp = input$missingpopp
    sr$resume<-poppr(sr$Genind, missing=sr$missingpopp, sample=sr$samplepoppr, minsamp=sr$minsamp, strata=sr$strata, plot=T, legend=T)
   
     output$popprplot <- renderPlot({
       sr$resume
     })
     
     output$popprtab <- DT::renderDataTable({
       DT::datatable(
         sr$resume,
         filter = list(position = 'top', clear = TRUE, plain = FALSE),
         extensions = 'Buttons', 
         options = list(
           scrollX=TRUE,
           dom = 'Blfrtip', 
           buttons = list(
             'copy', 
             'print',
             list(
               extend = "collection", 
               text = "Download entire dataset",
               #buttons = c("csv","excel","pdf"),
               action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test3', true, {priority: 'event'});}")
             )
           ),
           lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
           initComplete = JS(
             "function(settings, json) {",
             "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
             "}"
           )
         )
       )
     })
})


#download tar
output$downloadDiv <- downloadHandler(
  filename = function() {
    paste("output", "zip", sep=".")
  },
  content = function(file) {
    file.copy(sr$archive$path, file)
  },
  contentType = "application/zip"
)

# MST

output$MSTpop <- renderPlot(
  aboot(x = sr$Genind, strata=sr$Genind@pop)
)
output$MSTind <- renderPlot(
  aboot(x = sr$Genind, strata=NULL)
)

output$AllelicRichness<- DT::renderDataTable({
  DT::datatable(
    sr$richness,
    filter = list(position = 'top', clear = TRUE, plain = FALSE),
    extensions = 'Buttons', 
    options = list(
      scrollX=TRUE,
      dom = 'Blfrtip', 
      buttons = list(
        'copy', 
        'print',
        list(
          extend = "collection", 
          text = "Download entire dataset",
          #buttons = c("csv","excel","pdf"),
          action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test3', true, {priority: 'event'});}")
        )
      ),
      lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
        "}"
      )
    )
  )
})

output$AllelicRichnessMIN <- renderText(
  sr$richnessMIN
)

# ismsn()

in_dataset <- reactive({
  if (input$datasetMSN == "genind"){
    dat <- sr$Genind 
  }
  return(dat)
})

#-------------------------------------
# This is the first field to change
# dynamically with user input. It will
# show the user a series of checkboxes
# representing the populations 
# available.
#-------------------------------------
output$selectPops <- renderUI({
  input$datasetMSN
  checkboxGroupInput("sublist",
                     "choose populations",
                     choices = popNames(in_dataset()),
                     inline = TRUE,
                     selected = popNames(in_dataset()))
})
#-------------------------------------
# Simply a reactive for the input above.
#-------------------------------------
sub_list <- reactive({
  if (is.null(input$sublist)) NA else input$sublist
})
#-------------------------------------
# This parses the data according to 
# the selected populations. Notice that
# it is only controlled by the 
# buttons and not the data set. If this
# were controlled by the data set, it
# would throw an error every time the
# user switches data sets. 
#-------------------------------------
dataset <- reactive({
  input$`update-data`
  input$submit
  isolate({
    popsub(in_dataset(), sub_list(), drop = FALSE)
  })
})
#-------------------------------------
# This grabs the name of the data set
# for the command tab.
#-------------------------------------
dataname <- reactive({
  if (input$datasetMSN == "genind"){
      dat <- "genind object"
  } else {
    dat <- "no data"
  }
  return(dat)
})

#-------------------------------------
# If the user selects "Custom" for the
# distance function, they must supply
# the name of the function. By default
# an example of euclidean distance is
# displayed.
#-------------------------------------
output$customDist <- renderUI({
  textInput("custom_distance", label = "Custom Distance Function", "function(x) dist(tab(x))")
})
#-------------------------------------
# If the distance is a custom function,
# it must be treated, otherwise it must
# be translated.
#-------------------------------------
distfun <- reactive({ 
  if (input$distance == "Custom"){
    the_dist <- parse_distfun(input$custom_distance)
  } else {
    the_dist <- get_dist(input$distance) 
    if (inherits(in_dataset(), "genlight") && the_dist == "diss.dist"){
      the_dist <- "bitwise.dist"
    }
  }
  return(the_dist)
})

#-------------------------------------
# All the functions have arguments 
# associated with them. This displays
# the arguments as text that can be
# later parsed as proper R code.
#-------------------------------------
output$distargsUI <- renderUI({
  the_fun <- eval(parse(text = distfun()))
  the_args <- formals(the_fun)[-1]
  the_args <- paste(names(the_args), the_args, sep = " = ", 
                    collapse = ", ")
  textInput("distargs", label = "Distance arguments", the_args)
})
#-------------------------------------
# Distance Arguments from above.
#-------------------------------------
distargs <- reactive({
  input$distargs     
})
#-------------------------------------
# Should the minimum spanning network
# contain reticulate nodes?
#-------------------------------------
reticulation <- reactive({
  input$reticulate        
})

#-------------------------------------
# The below reactives represent the
# situation in which the user chooses
# Bruvo's distance. When this is the
# case, since we know the specific
# combination of the different models
# is not immediately inherent, we give
# them the choice. 
#-------------------------------------
addloss <- reactive({
  switch(input$bruvo_model,
         "Genome Addition" = "add = TRUE, loss = FALSE",
         "Genome Loss" = "add = FALSE, loss = TRUE",
         "Infinite" = "add = FALSE, loss = FALSE",
         "Average Addition/Loss" = "add = TRUE, loss = TRUE")
})
#-------------------------------------
# The repeat lengths can be comma
# separated numbers or any R expression
# or object that's valid. 
#-------------------------------------
replen <- reactive({
  if (!grepl("\\(", input$replen)){
    paste0("replen = c(", input$replen, ")")
  } else {
    paste0("replen = ", input$replen)
  }
})

#----------------------------------------------------------------------------#
# Distance Matrix and Minimum Spanning Network Construction -----------------|
# ---------------------------------------------------------------------------|
# 
# This single reactive controls the generation of both the distance matrix and
# the minimum spanning network. This is controlled by the two buttons, reData
# and submit, but it's also controlled by the "reticulate" checkbox. 
#
# The reason why this is in one function is because it's much more efficient
# to process the minimum spanning network with bruvo's distance using
# bruvo.msn. One thing to note about this function is that missing data is
# always treated with "mean".
#============================================================================#

#-------------------------------------
# If the user selects "Custom" for the
# distance function, they must supply
# the name of the function. By default
# an example of euclidean distance is
# displayed.
#-------------------------------------
output$customLayout <- renderUI({
  textInput("custom_layout", label = "Custom Layout Function", "function(x) matrix(rnorm(igraph::vcount(x)*2), ncol = 2)")
})

layfun <- reactive({ 
  if (input$layout == "Custom"){
    the_lay <- parse_distfun(input$custom_layout)
  } else {
    the_lay <- paste0("igraph::", input$layout)
  }
  return(the_lay)
})
#-------------------------------------
# This reactive calculates the distance
# by parsing the distance and then
# running the minimum spanning network
# on that matrix.
#-------------------------------------
minspan <- reactive({
  # input$dataset
  input$`update-data`
  input$reticulate
  input$submit
  isolate({
    indist <- distfun()
    ret    <- reticulation()
    args   <- distargs()
    if (input$distance == "Bruvo"){
      args <- paste(replen(), addloss(), sep = ", ")
      fun <- paste0("bruvo.msn(dataset(), ", args, ", showplot = FALSE, include.ties = ret)")
      out <- eval(parse(text = fun))
    } else {
      if (indist != "diss.dist" && inherits(dataset(), "genind")){
        dat <- missingno(dataset(), "mean")
      } else {
        dat <- dataset()
      }
      if (length(args) == 1 && args == ""){
        fun <- paste0(indist, "(dat)")
      } else {
        fun <- paste0(indist, "(dat, ", args, ")")
      }
      dist <- eval(parse(text = fun))
      out <- poppr.msn(dataset(), dist, showplot = FALSE, include.ties = ret)
    }
    return(out)
  })
})

#----------------------------------------------------------------------------#
# Aesthetic Processing ------------------------------------------------------|
# ---------------------------------------------------------------------------|
# 
# This section contains all of the reactive functions to return basic values
# for plotting. They do not affect how the minimum spanning network is
# constructed. I will not comment on the ones that are simply one-line
# reactive functions.
#============================================================================#
slide <- reactive({
  input$greyslide
})

seed <- reactive({
  input$seed 
})

nodescale <- reactive({
  input$nodescale
})

#-------------------------------------
# plot_poppr_msn allows the user to
# display what node a specific sample
# lies. This will take the user input
# as a comma separated list of sample
# names and pass them on.
#-------------------------------------
inds <- reactive({
  inds <- strsplit(input$inds, "[[:blank:]]*,[[:blank:]]*")[[1]]
  if (input$ind_or_mlg == "sample names" || inds == "ALL" || inds == ""){
    return(inds)
  } else {
    return(as.numeric(inds))
  }
})

#-------------------------------------
# The user palette can be a custom
# palette. I'm not sure why this is
# protected by the reactive here...
#-------------------------------------
usrPal <- reactive({
  input$`update-data`
  input$`update-graph`
  input$submit
  isolate({
    if (input$pal == 'custom'){
      eval(parse(text = input$custom_pal))
    } else {
      input$pal
    }
  })
})

popLeg <- reactive({
  input$pop.leg
})

sizeLeg <- reactive({
  input$size.leg
})

scaleLeg <- reactive({
  input$scale.leg
})

cutoff <- reactive({
  cutoff <- as.numeric(input$cutoff)
  if (is.na(cutoff)) cutoff <- NULL
  cutoff      
})

bcut <- reactive({
  input$beforecut
})

#----------------------------------------------------------------------------#
# User-facing Command Construction ------------------------------------------|
# ---------------------------------------------------------------------------|
# 
# The following reactives construct the command the user needs to recreate the
# minimum spanning network that has been created. 
#============================================================================#

#-------------------------------------
# This constructs the command that
# Processes the data, constructs the
# distance, and constructs the minimum
# spanning network. 
#-------------------------------------
distcmd <- reactive({
  dat      <- dataname()
  distfunk <- distfun()
  args     <- distargs()
  the_pops <- popNames(in_dataset())
  match_pops <- the_pops %in% input$sublist
  
  # If the number of population selected is greater than half the total
  # populations, place the unselected populations in the blacklist argument.
  half <- ceiling(length(the_pops)/2)
  if (sum(match_pops) < half){
    first_dat <- paste0(dat, "_sub <- popsub(", dat, ", sublist = ", make_dput(input$sublist), ")\n")
  } else {
    first_dat <- paste0(dat, "_sub <- popsub(", dat, ", blacklist = ", make_dput(the_pops[!match_pops]), ")\n")
  }
  closer   <- paste0("showplot = FALSE, include.ties = ", reticulation(), ")")
  has_no_args <- length(args) == 1 && args == ""
  if (distfunk == "bruvo.dist"){
    args <- paste(replen(), addloss(), sep = ", ")
    distfunk <- "min_span_net <- bruvo.msn"
    closer <- paste0(", ", args, ", ", closer)
    return_cmd <- paste0(distfunk, "(", dat, "_sub", closer)
  } else { 
    if (distfunk == "diss.dist"){
      missfunk <- character(0)
      distfunk <- paste0(distfunk, "(", dat, "_sub, ", args, ")\n")        
    } else {
      missfunk <- paste0(dat, "_nomiss <- ", "missingno(", dat, 
                         ", type = 'mean')\n")
      args <- ifelse(has_no_args, "", paste0(", ", args))
      distfunk <- paste0(distfunk, "(", dat, "_nomiss", args, ")\n")        
    }
    msnfunk <- paste0("poppr.msn(", dat, "_sub, ", dat, "_dist, ", closer, "\n")
    return_cmd <- paste0(missfunk, 
                         dat, "_dist <- ", distfunk,
                         "min_span_net <- ", msnfunk)
  }
  return(paste0(first_dat, return_cmd))
})

#-------------------------------------
# This one is relatively easy as it
# simply just constructs the plotting
# function with it's uncomplicated
# processing.
#-------------------------------------
cmd <- reactive({
  dat <- dataname()
  pal <- ifelse(input$pal == 'custom', input$custom_pal, input$pal)
  padding <- paste(rep(" ", 15), collapse = "")
  paste0("plot_poppr_msn(", dat, 
         ",\n", padding, "min_span_net", 
         ",\n", padding, "inds = ", make_dput(inds()), 
         ",\n", padding, "mlg = ", input$mlgs,
         ",\n", padding, "gadj = ", input$greyslide,
         ",\n", padding, "nodescale = ", input$nodescale,
         ",\n", padding, "palette = ", pal,
         ",\n", padding, "cutoff = ", ifelse(is.null(cutoff()), "NULL", cutoff()),
         ",\n", padding, "quantiles = FALSE",
         ",\n", padding, "beforecut = ", bcut(), 
         ",\n", padding, "pop.leg = ", popLeg(), 
         ",\n", padding, "size.leg = ", sizeLeg(),
         ",\n", padding, "scale.leg = ", scaleLeg(),
         ",\n", padding, "layfun = ", layfun(), 
         ")")
})

#----------------------------------------------------------------------------#
# Output --------------------------------------------------------------------|
# ---------------------------------------------------------------------------|
# 
# Below are all the tabs for output.
#============================================================================#

#-------------------------------------
# The first thing the user sees is the
# plot, so it's important to check if
# the user has hit "submit" or not.
#-------------------------------------
output$plotMSN <- renderPlot({
  input$pop.leg
  input$scale.leg
  input$beforecut
  input$nodescale
  input$inds
  input$mlgs
  input$`update-graph`
  if(!input$submit) {
    plot.new() 
    rect(0, 1, 1, 0.8, col = "indianred2", border = 'transparent' ) + 
      text(x = 0.5, y = 0.9, "Please select data and click\nthe 'Go!' button.", 
           cex = 1.6, col = "white")
  } else {
    set.seed(seed())
    plot_poppr_msn(dataset(), 
                   minspan(), 
                   ind = inds(), 
                   gadj = slide(), 
                   mlg = input$mlgs,
                   palette = usrPal(), 
                   cutoff = cutoff(), 
                   quantiles = FALSE, 
                   beforecut = bcut(), 
                   nodescale = nodescale(),
                   pop.leg = popLeg(), 
                   size.leg = sizeLeg(),
                   scale.leg = scaleLeg(),
                   layfun = eval(parse(text = layfun()))
    )      
  }
})

#-------------------------------------
# Saving output as PDF
#-------------------------------------
output$save_pdf <- downloadHandler(
  filename = function() paste0('msn-', Sys.Date(), '.pdf'),
  content = function(file) {
    isolate({
      # Generate a pdf
      pdf(file, width = input$pdf_plot_width, height = input$pdf_plot_height)
      set.seed(seed())
      plot_poppr_msn(dataset(),
                     minspan(),
                     ind = inds(),
                     gadj = slide(),
                     mlg = input$mlgs,
                     palette = usrPal(),
                     cutoff = cutoff(),
                     quantiles = FALSE, 
                     beforecut = bcut(),
                     nodescale = nodescale(),
                     pop.leg = popLeg(),
                     size.leg = sizeLeg(),
                     scale.leg = scaleLeg(),
                     layfun = eval(parse(text = layfun()))
      )
      dev.off()
    })      
  }
)

#-------------------------------------
# Saving output as PNG
#-------------------------------------
output$save_png <- downloadHandler(
  filename = function() paste0('msn-', Sys.Date(), '.png'),
  content = function(file) {
    isolate({
      # Generate a png
      png(file, width = input$png_plot_width, height = input$png_plot_height)
      set.seed(seed())
      plot_poppr_msn(dataset(),
                     minspan(),
                     ind = inds(),
                     gadj = slide(),
                     mlg = input$mlgs,
                     palette = usrPal(),
                     cutoff = cutoff(),
                     quantiles = FALSE, 
                     beforecut = bcut(),
                     nodescale = nodescale(),
                     pop.leg = popLeg(),
                     size.leg = sizeLeg(),
                     scale.leg = scaleLeg(),
                     layfun = eval(parse(text = layfun()))
      )
      dev.off()
    })      
  }
)

