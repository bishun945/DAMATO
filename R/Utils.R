utils::globalVariables(c('.', 'dataset_format_1', 'dataset_format_1_folders'))

#' @name Transpose_spec_df
#' @title Transpose the spectra data as a data.frame with wavelength being colnames
#' @param input Result of Check_base_info
#' @param dataset_format Dataset_format (default as 1)
#' @return A list or NULL (error output)
#' @export

Transpose_spec_df <- function(input, dataset_format = 1){
  
  if(input$Status_sheet_name == "Pass" &
     input$Status_sample_id == "Pass" &
     input$Status_base_info == "Pass"){
    
    if(dataset_format == 1){
      dt = input$dt
      for(i in 2:7){
        tmp = t(dt[[i]])[-1,]
        colnames(tmp) = t(dt[[i]])[1,]
        tmp = data.frame(SampleID = colnames(dt[[i]])[-1], tmp)
        rownames(tmp) = seq(1, nrow(tmp))
        colnames(tmp)[-1] = gsub("X","",colnames(tmp)[-1])
        dt[[i]] = tmp
      }
      input$dt = dt
    }
    
    return(input)
    
  }else{
    
    stop("Please pass sheet_name, sample_id, and base_info check!")
    return(NULL)
    
  }
}





#' @name level_to_variable
#' @noRd
level_to_variable <- function(dt, warn=F){
  if(!is.data.frame(dt))
    stop('Input must be a data.frame')
  w <- NULL
  for(i in names(dt)){w<-c(w,is.factor(dt[,i]))}
  if(sum(w) != 0){
    for(i in names(dt)[w]){
      tmp <- levels(dt[,i])[dt[,i]]
      dt[,i] <- tmp
    }
    if(warn) message('Factors turned to variables are: ')
    if(warn) message(paste(names(dt)[w],collapse=' '))
    return(dt)
  }else{
    if(warn) message('No factor in this data.frame.')
    return(dt)
  }
}

