
render_report <- function(input, RMD = NULL, ...) {
  
  if(is.null(RMD)) {
    
    RMD = system.file("DAMATO_report.Rmd", package = "DAMATO") %>%
      readLines(encoding = "UTF-8")
  
  }
  
  # create a tmp directory to render RMD file
  tmpdir <- tempdir(check = TRUE) %>% gsub("\\\\", "/", .)
  
  # create filenames of tmp rData and RMD files
  tmprData <- file.path(tmpdir, "tmp.rData")
  tmpRMD <- file.path(tmpdir, "DAMATO_report.Rmd")
  tmpHTML <- file.path(tmpdir, "DAMATO_report.html")
  
  # save to the tmpdir
  save(input, file = tmprData)
  
  # find the placehold position and replace it by the defined path
  w_tmp <- str_detect(RMD, "# <--input_file-->") %>%
    which() %>% 
    {. + 1}
  
  RMD[w_tmp] <- gsub("<replaceme>", tmprData, RMD[w_tmp])
  
  # save the new RMD to tmpdir
  readr::write_lines(RMD, file = tmpRMD)
  
  
  # render it 
  rmarkdown::render(input = tmpRMD, ...)
  
  # find it in tmpdir and copy to the wd if exists
  if(file.exists(tmpHTML)) {
    
    tmp = file.copy(tmpHTML, "./DMATO_report.html")
    
  }
  
  unlink(tmpdir, recursive = TRUE)
  
}





