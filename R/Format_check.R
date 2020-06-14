#' @name Check_spread_file
#' @title Check the readable of spread files 
#' @importFrom readxl excel_format
#' @param fn fn
#' @return Logical
#' @export
#' 
Check_spread_file <- function(fn){
  
  if(excel_format(fn) != 'xls' & excel_format(fn) != 'xlsx')
    stop("The selected input should be an Excel file with `xls` or `xlsx` format!")

  return(TRUE)
  
}



#' @name Check_sheet_name
#' @title Check the format of sheet names of the spread
#' 
#' @importFrom stringr str_detect
#' @importFrom cli cat_bullet
#' @importFrom readxl excel_sheets read_excel
#' @importFrom magrittr %>%
#' @importFrom stringi stri_escape_unicode
#' 
#' @param fn fn
#' @param dataset_format dataset_format (default as 1)
#' @return List
#' @export
#' @encoding UTF-8
#' 
Check_sheet_name <- function(fn, dataset_format = 1){
  
  cli_control("head", "Sheet name check")
  
  cat("\n")
  
  num_er <- num_wr <-  0
   
  if(!Check_spread_file(fn)) stop("File check failed!")
  
  Sheets_found <- excel_sheets(fn)
  cli_control("info",
              c("Following sheets are found: ", paste(Sheets_found, collapse = " ")))
  
  if(dataset_format == 1){
    Sheets_required <- c(stri_unescape_unicode("\\u57fa\\u672c\\u53c2\\u6570"),"1.Rrs","2.ap","3.aph","4.anap","5.aCDOM","6.apc")
    general_name    <- c("base","Rrs","ap","aph","anap","aCDOM","apc")
    base_colnames_type <- dataset_format_1$base_info[,"base_colnames_type"]
  }
  
  # showNonASCIIfile(file = './R/DAMATO.R')
  
  Sheets_used <- NULL
  
  for(Sheet in Sheets_required){
    w <- str_detect(Sheets_found, Sheet)
    if(!any(w)){
      cli_control("error", c("[", Sheet, "] does not detected!"))
      num_er = num_er + 1
      next
    }
    if(sum(w) > 1){
      cli_control("error",
                  c(Sheet, " should be only one matched element! ",
                    "But [", paste(Sheets_found[w], collapse = " "), "] were found!"))
      num_er = num_er + 1
      next
    }
    Sheets_used <- c(Sheets_used, Sheet)
  }

  cli_control("right",
              c("Following sheets are used for the next step: ", paste(Sheets_used, collapse = " ")))
  
  dt <- list()
  # for(Sheet in Sheets_used){
  #   if(Sheet == stri_unescape_unicode("\\u57fa\\u672c\\u53c2\\u6570") | Sheet == "base"){
  #     dt[[Sheet]] <- read_excel(fn, sheet=Sheet, col_types = base_colnames_type)
  #   }else{
  #     dt[[Sheet]] <- read_excel(fn, sheet=Sheet, col_types = "numeric")
  #   }
  # }
  for(Sheet in Sheets_used){
    dt[[Sheet]] <- read_excel(fn, sheet=Sheet)
  }
  
  names(dt) <- general_name
  
  cli_control("info",
              c("To simplify the follow-up process, change the sheet name to: ", 
                paste(general_name, collapse = " ")))
  
  if(any(str_detect(names(dt[['base']]), "\\r\\n"))){
    old_name <- names(dt[['base']])
    names(dt[['base']]) <- names(dt[['base']]) %>% gsub("\\r\\n",'', .) # To remove the return line of colnames
    cli_control("warning",
                c("We found carriage return or newline in the sheet [base]: ",
                  paste(names(dt[['base']])[which(str_detect(old_name ,"\\r\\n"))], collapse = " ")))
    cli_control("warning", "Fully recomand to use only one line character to describe colnames in [base].")
    cli_control("warning", "Anyway, the carriage return or newline symbols are removed in this process!")

    num_wr = num_wr + 1
    
  }
  
  # final report 
  Status <- ifelse(num_er == 0, "Pass", "Error")
  col    <- ifelse(num_er == 0, {ifelse(num_wr == 0, "green", "yellow")}, "red")
  
  cat("\n")
  
  cli_control("status", c(
    "Sheet name status: [", Status,"] ", "Error: [", num_er, "] Warning: [", num_wr, "]"
  ), col_status = col)
  
  cat("\n")
  
  cli_control("end", "Sheet name check")

  result <- list(
    fn = fn, 
    Sheets_used = Sheets_used,
    general_name = general_name,
    dt = dt,
    Status_sheet_name = Status
  )
  
  return(result)
  
}



#' @name Check_sample_id
#' @title Check the format of sample ids of the spread
#' 
#' @importFrom cli cat_bullet
#' @importFrom stringr str_detect str_match str_c
#' @importFrom stringi stri_unescape_unicode
#' 
#' @param input input
#' @param dataset_format dataset_format (default as 1)
#' @param fix_Rrs_sample_id fix_Rrs_sample_id (default as \code{TRUE})
#' @return List
#' 
#' @export
#' @encoding UTF-8
#' 
Check_sample_id <- function(input, dataset_format = 1, fix_Rrs_sample_id = TRUE){
  
  # sample id should be checked into twp parts
  # 1) generally, the number of samples should follow some rules;
  # 2) for each sheet, check the string of samples about unmatched regrexp pattern such as "(" or "["
  
  # symbols: good-tick green; error-cross red; warning-bullet yellow
  
  cli_control("head", "Sample ID check")
  
  cat("\n")
  
  num_er <- num_wr <-  0
  
  dt <- input$dt
  general_name <- input$general_name
  
  list_sample_id <- list()
  
  # SampleID head
  for(name in names(dt)){
    
    dt_sub <- dt[[name]]
    
    if(name == general_name[1] | name == stri_unescape_unicode("\\u57fa\\u672c\\u53c2\\u6570")){   # for base sheet
      
      pattern <- str_c(stri_unescape_unicode("\\u7f16\\u53f7"),"|",stri_unescape_unicode("\\u901a\\u7528"),"|SampleID") #Bianhao | Tongyong
      w <- str_detect(names(dt_sub), pattern) %>% which()
      if(sum(w) == 1){
        list_sample_id[[name]] <- dt_sub[, w] %>% as.matrix %>% c
        cli_control("right", c("'SampleID' found in [", name, "]"))
      }else{
        cli_control("error", c("'SampleID' CANNOT found in [", name, "]"))
        num_er = num_er + 1
      }
      
    }else{                                                              # for following sheets
      
      if(!all(str_detect(names(dt_sub), pattern))){
        cli_control("warning", c("'SampleID' CANNOT be found in [", name, "]."))
        cli_control("warning", "The first colname is used here")
        cli_control("warning", c("Recommand to change '", names(dt_sub)[1], "' as 'SampleID'."))
        num_wr = num_wr + 1
      }
      
      wrong_pattern <- str_c(stri_unescape_unicode("\\u5907\\u6ce8"),
                             '|Note|note|\\[|\\]|\\{|\\}|\\(|\\)|',
                             stri_unescape_unicode("\\uff08"),'|',
                             stri_unescape_unicode("\\uff08"),'|-')
      
      if(any(str_detect(names(dt_sub)[-1], wrong_pattern))){
        cli_control("error",
                    c("Found '", 
                      paste(names(dt_sub)[-1][str_detect(names(dt_sub)[-1], wrong_pattern)], collapse = " "),
                      "' in colnames of [", name, 
                      "] with the wrong format 'YYYYMMXXNN_Z'. They have been removed. Please check and re-submit!"))
        num_er = num_er + 1
        w <- which(str_detect(names(dt_sub), str_c(stri_unescape_unicode("\\u5907\\u6ce8"),'|Note|note')))
        list_sample_id[[name]] <- names(dt_sub)[c(-1, -w)]
      }else{
        list_sample_id[[name]] <- names(dt_sub)[-1]
      }
      
    }
  }
  
  # SampleID details - format
  length_sample_id <- lapply(list_sample_id, length) %>% as.data.frame
  
  if(names(length_sample_id)[which.max(length_sample_id)] != 'base'){
    cli_control("error",
                c("Error! Full sample id was selected by the sheet [",
                  names(length_sample_id)[which.max(length_sample_id)], '].',
                  " It shoud be [base]!"))
    sample_id_full <- list_sample_id[['base']]
    num_er = num_er + 1
  }else{
    cli_control("okay", c(
      "Full sample id was selected by the sheet [", names(length_sample_id)[which.max(length_sample_id)], '].'
      ))
    sample_id_full <- list_sample_id[[which.max(length_sample_id)]]
  }
  
  # The sample id definition of base and Rrs is a kind of different. Have to check.
  w = which(!(list_sample_id$Rrs %in% list_sample_id$base))
  if(length(w) != 0){
    cli_control("error",c(
      "In Rrs sheet, the detected sample id(s) are different from the base: ", paste(list_sample_id$Rrs[w], collapse = " ")
    ))
    for(i in 1:length(w)){
      w_maybe = which(str_detect(list_sample_id$base, str_c(list_sample_id$Rrs[w][i],"_1")))
      if(length(w_maybe) == 0){
        cat_bullet("No.", i, " Cannot find matched '", list_sample_id$Rrs[w][i], "' from the base sample id!",
                   bullet = "radio_on", bullet_col = "red", col = 'red')
        stop("Check process exist! See info and resubmit!")
      }else{
        cat_bullet("No.", i, " Is '", list_sample_id$Rrs[w][i], "'matched with '",
                   paste(list_sample_id$base[w_maybe], collapse = ", ") ,"' from the base sample id?",
                   bullet = "radio_on", bullet_col = "yellow", col = 'yellow')
        if(fix_Rrs_sample_id & length(w_maybe) == 1){
          colnames(dt$Rrs)[-1][w][i] <- str_c(list_sample_id$Rrs[w][i],"_1") # replace the former one
          cli_control("okay", c(
            "Automatically modified as ", str_c(list_sample_id$Rrs[w][i],"_1")
          ))
        }else{
          stop("Check process exist! See info and resubmit!")
        }
      }
    }
  }
  
  # Detect whether the input spread satisfy the format of the required.
  for(i in names(length_sample_id)[!{names(length_sample_id) %in% c("base","Rrs")}]){
    sample_missing <- sample_id_full %in% list_sample_id[[i]] %>% {!.} %>% sample_id_full[.] %>%
      paste0(., collapse = ", ")
    if(sample_missing == ""){
      cli_control("right", c(
        "Sheet [", i, '] includes same samples with [base]!'
      ))
    }else{
      cli_control("error", c(
        "Sheet [", i, '] includes missing sample ids: ', paste(sample_missing, collapse = " ")
      ))
      num_er = num_er + 1
    }
    
  }

  # final report
  Status <- ifelse(num_er == 0, "Pass", "Error")
  col    <- ifelse(num_er == 0, {ifelse(num_wr == 0, "green", "yellow")}, "red")
  
  cat("\n")
  
  cli_control("status", c(
    "Sample ID status: [", Status,"] ", "Error: [", num_er, "] Warning: [", num_wr, "]"
  ), col_status = col)
  
  cat("\n")
  
  cli_control("end", "Sample ID check")


  result <- input
  result$Status_sample_id = Status
  result$dt = dt
  
  return(result)
  
}



#' @name Check_base_info
#' @title Check the format of sheet 'base'
#' @importFrom cli cat_bullet
#' @importFrom readxl excel_sheets cell_cols
#' @import stringr
#' @import stringi
#' @import dplyr
#' @param input input
#' @param dataset_format dataset_format (default as 1)
#' @param as_EN_colnames Convert colnames to EN format (default as \code{TRUE})
#' @return List
#' @export
#' 
Check_base_info <- function(input, dataset_format = 1, as_EN_colnames = TRUE){

  cli_control("head", "Base info check")
  
  cat("\n")
  
  num_er <- num_wr <-  0
  
  dt = input$dt
  dt_base <- dt[['base']]
  
  name_ <- colnames(dt_base)
  
  unit <- str_extract_all(name_, "(\\[.*\\])", simplify = T) %>% c
  name <- str_replace_all(name_, "(\\[.*\\])", "")
  
  cli_control("okay", c(
    "Found colnames (unit removed) are: ", paste(name, collapse = ", ")
  ))
  
  if(dataset_format == 1){
    base_colnames_CN <- dataset_format_1$base_info[,"base_colnames_CN"]
    base_colnames_EN <- dataset_format_1$base_info[,"base_colnames_EN"]
    base_colnames_type = dataset_format_1$base_info[,"base_colnames_type"]
  }

  cli_control("info", c(
    "Formatted colnames (CN): ", paste(base_colnames_CN, collapse = ", ")
  ))
  
  cli_control("info", c(
    "Formatted colnames (EN): ", paste(base_colnames_EN, collapse = ", ")
  ))
  
  # The function will detect the out-formatted columns in the raw spread and stop checking
  # The user requires to inspect these columns manually.
  if(length(name) > length(base_colnames_CN)){
    w = ((str_to_lower(name) %in% str_to_lower(base_colnames_CN)) | (str_to_lower(name) %in% str_to_lower(base_colnames_EN)))
    w = which(!w)
    cli_control("error",c(
      paste(name[w], collapse = ", "), " found in the current spread. ",
      "They are not required colnames. If you want to save them, just keep them into the column Note. ",
      "Or if you confirm these parameters as the often-used. Please contact me to modify the code. ", 
      "Now please check and re-submit!"
    ))
    stop("Exit check process!")
  }
  
  w = which(!{{str_to_lower(base_colnames_CN) %in% str_to_lower(name)} | {str_to_lower(base_colnames_EN) %in% str_to_lower(name)}})
  
  if(length(w) == 0){
    cli_control("right", "Great! All required colnames in [base] are detected!")
    w_sort = seq(length(base_colnames_CN))
    for(i in 1:length(w_sort)){
      w_sort[i] = which(base_colnames_CN[i] == name | str_to_lower(base_colnames_EN[i]) == str_to_lower(name))
    }
    dt_base <- dt_base[, w_sort]
    if(as_EN_colnames){
      names(dt_base) <- base_colnames_EN
      cli_control("info", "Colnames of [base] are converted to EN characters.")
    }else{
      names(dt_base) <- base_colnames_CN
      cli_control("info", "Colnames of [base] are converted to CN characters.")
    }
  }else{
    cli_control("error",c(
      paste(base_colnames_CN[w], collapse = ", "), ' (or ', 
      paste(base_colnames_EN[w], collapse = ", "),
      ") CANNOT be found in [base] correctly. Please check and re-submit!"
    ))
    num_er = num_er + 1
    
    cat("  Try to find the matched pattern (just guess) ...\n")
    
    for(i in 1:length(w)){
      w_CN = which(!is.na(str_match(name, base_colnames_CN[w[i]] %>% 
                                      stri_split_boundaries(., type="word", skip_word_none = TRUE, simplify = T) %>% 
                                      str_c(., collapse = "|"))))
      w_EN = which(!is.na(str_match(str_to_lower(name), 
                                    str_to_lower(base_colnames_EN[w[i]]))))
      w_ = union(w_CN, w_EN)
      if(length(w_) != 0){
        cat_bullet("No.", i, " Is '", base_colnames_CN[w][i], "' or ('", base_colnames_EN[w][i], 
                   "') matched with '", paste(name[w_], collapse = ", ") ,"' from the input colnames?",
                   bullet = "radio_on", bullet_col = "yellow", col = 'yellow')
      }else{
        cat_bullet("No.", i, " Could not find matched patterns for '", 
                   base_colnames_CN[w][i], "' or ('", base_colnames_EN[w][i], "')", " from the input colnames.",
                   bullet = "radio_off", bullet_col = "red", col = "red")
      }
    }
  }

  # final report
  Status <- ifelse(num_er == 0, "Pass", "Error")
  col    <- ifelse(num_er == 0, {ifelse(num_wr == 0, "green", "yellow")}, "red")
  
  # If base info check pass, the base data should be re-read as the col_types should be defined again.
  if(Status == "Pass"){
    Sheets = excel_sheets(input$fn)
    w_sheet = which(Sheets == stri_unescape_unicode("\\u57fa\\u672c\\u53c2\\u6570") | Sheets == "base")
    dt_base <- read_excel(input$fn, sheet=w_sheet, 
                          cell_cols(w_sort),
                          col_types = base_colnames_type)[, w_sort]
    if(as_EN_colnames){
      names(dt_base) <- base_colnames_EN
      cli_control("info", "Colnames of [base] are converted to EN characters.")
    }else{
      names(dt_base) <- base_colnames_CN
      cli_control("info", "Colnames of [base] are converted to CN characters.")
    }
  }
  
  cat("\n")
  
  cli_control("status", c(
    "Base info status: [", Status,"] ", "Error: [", num_er, "] Warning: [", num_wr, "]"
  ), col_status = col)
  
  cat("\n")
  
  cli_control("end", "Base info check")
  
  result <- input
  result$dt$base <- dt_base
  result$Status_base_info = Status
  result$dt$unit <- data.frame(name = name, unit = unit)
  result$base_colnames_CN = base_colnames_CN
  result$base_colnames_EN = base_colnames_EN
  result$as_EN_colnames = as_EN_colnames
  
  return(result)
  
}

#' @name Check_format
#' @title Main function to check the spread format using Check_sheet_name, Check_sample_id,
#'   and Check_base_info.
#' 
#' @param fn Filename
#' @param dataset_format The format of dataset to be checked.
#' @param as_EN_colnames Convert colnames to EN format (default as \code{TRUE})
#' @return A list
#' @export
#' 
#' @importFrom cli cat_boxx
#' 
Check_format <- function(fn,
                         dataset_format = 1,
                         as_EN_colnames = TRUE){
  
  # cat_boxx("Format check begin!")
  res_1 = Check_sheet_name(fn, dataset_format = dataset_format)
  cat("\n\n")
  # cat_boxx("Check_sheet_name() finished!")
  res_2 = Check_sample_id(res_1, dataset_format = dataset_format)
  cat("\n\n")
  # cat_boxx("Check_sample_id() finished!")
  res_3 = Check_base_info(res_2, dataset_format = dataset_format, as_EN_colnames = as_EN_colnames)
  cat("\n\n")
  # cat_boxx("Check_base_info() finished!")
  
  return(res_3)
  
}


# files <- lapply(list.files(system.file('extdata', package = 'DAMATO'), full.names = TRUE), read.csv)

Check_folder <- function(){
  
}
