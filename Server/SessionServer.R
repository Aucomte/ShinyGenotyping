## Panel Session info

output$urlText1 <- renderText({
  si <- sessioninfo::session_info()
  pckgs <- map2(si$packages$package, 
                si$packages$loadedversion,
                ~ paste0(.x, " ", .y)) %>% 
    simplify()
  paste(sep="",
        "- Date: ", si$platform$date,"\n",
        "- OS: ", si$platform$os,"\n",
        "- Version: ", si$platform$version,"\n"
  )
})
output$urlText2 <- renderText({
  si <- sessioninfo::session_info()
  pckgs <- map2(si$packages$package, 
                si$packages$loadedversion,
                ~ paste0(.x, " ", .y)) %>% 
    simplify()
  paste(sep="",
        pckgs,"\n"
  )
})