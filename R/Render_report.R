#' @name render_report
#' @title render_report
#' @description render_report
#' @param input Input list
#' @param RMD RMD file path
#' @param output_format Param in rmarkdown::render, try \code{"html_document"},
#'   \code{"rmdformats::readthedown"}, \code{"pagedown::html_paged"}, or 
#'   \code{"slidy_presentation"}.
#' @param ... ...
#' @export
render_report <- function(input, 
                          RMD = NULL,
                          output_format = NULL,
                          ...) {
  
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
  if(is.null(output_format)) {
    output_format = "html_document"
  }
  
  rmarkdown::render(input = tmpRMD, 
                    output_format = output_format, 
                    quiet = TRUE,
                    ...)
  
  # find it in tmpdir and copy to the wd if exists
  if(file.exists(tmpHTML)) {
    
    tmp = file.copy(tmpHTML, "./DMATO_report.html", overwrite = TRUE)
    
  }
  
  unlink(tmpdir, recursive = TRUE)
  
}





