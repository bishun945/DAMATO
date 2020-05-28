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
  
  cat_bullet("--- Sheet name check begin!", 
             bullet = 'info', col = "steelblue")
  
  num_er <- num_wr <-  0
   
  if(!Check_spread_file(fn)) stop("File check failed!")
  
  Sheets_found <- excel_sheets(fn)
  cat_bullet("Following sheets are found: ", paste(Sheets_found, collapse = " "), bullet = 'heart', bullet_col = "blue")
  
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
      cat_bullet("[", Sheet, "] does not detected!", bullet = 'cross', bullet_col = 'red')
      num_er = num_er + 1
      next
    }
    if(sum(w) > 1){
      cat_bullet(Sheet, " should be only one matched element! But [",
                 paste(Sheets_found[w], collapse = " "), "] were found!", 
                 bullet = 'cross', bullet_col = 'red')
      num_er = num_er + 1
      next
    }
    Sheets_used <- c(Sheets_used, Sheet)
  }
  
  cat_bullet("Following sheets are used for the next step: ", 
             paste(Sheets_used, collapse = " "), bullet = 'heart', bullet_col = "blue")
  
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
  
  cat_bullet("To simplify the follow-up process, change the sheet name to: ", 
             paste(general_name, collapse = " "), bullet = 'heart', bullet_col = "blue")
  
  if(any(str_detect(names(dt[['base']]), "\\r\\n"))){
    old_name <- names(dt[['base']])
    names(dt[['base']]) <- names(dt[['base']]) %>% gsub("\\r\\n",'', .) # To remove the return line of colnames
    cat_bullet("We found carriage return or newline in the sheet [base]: ",
               paste(names(dt[['base']])[which(str_detect(old_name ,"\\r\\n"))], collapse = " "),
               ".\n  Recomand use one line character to describe colnames in [base].",
               " Anyway, the carriage return or newline symbols are removed!",
               bullet = 'bullet', bullet_col = "yellow")
    num_wr = num_wr + 1
    
  }
  
  # final report 
  Status <- ifelse(num_er == 0, "Pass", "Error")
  col    <- ifelse(num_er == 0, {ifelse(num_wr == 0, "steelblue", "yellow")}, "red")
  
  cat_bullet("--- Sheet name check finished ! --- Status: [", Status,"] --- ",
             "Error number: [", num_er, 
             "] --- Warning number: [", num_wr, "]",
             col = col,
             bullet = 'info')
  
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
  
  cat_bullet("--- Sample ID check begin!", 
             bullet = 'info', col = "steelblue")
  
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
        cat_bullet("'SampleID' found in [", name, "]", bullet = "tick", bullet_col = "green")
      }else{
        cat_bullet("'SampleID' CANNOT found in [", name, "]", bullet = 'cross', bullet_col = 'red')
        num_er = num_er + 1
      }
      
    }else{                                                              # for following sheets
      
      if(!all(str_detect(names(dt_sub), pattern))){
        cat_bullet("'SampleID' CANNOT be found in [", name, 
                   "]. The first colname is used here. Recommand to change '", names(dt_sub)[1], "' as 'SampleID'.", 
                   bullet = 'bullet', bullet_col = "yellow")
        num_wr = num_wr + 1
      }
      
      wrong_pattern <- str_c(stri_unescape_unicode("\\u5907\\u6ce8"),
                             '|Note|note|\\[|\\]|\\{|\\}|\\(|\\)|',
                             stri_unescape_unicode("\\uff08"),'|',
                             stri_unescape_unicode("\\uff08"),'|-')
      
      if(any(str_detect(names(dt_sub)[-1], wrong_pattern))){
        cat_bullet("Found '", 
                   paste(names(dt_sub)[-1][str_detect(names(dt_sub)[-1], wrong_pattern)], collapse = " "),
                   "' in colnames of [", name, 
                   "] with the wrong format 'YYYYMMXXNN_Z'. They have been removed. Please check and re-submit!",
                   bullet = "cross", bullet_col = 'red')
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
    cat_bullet("Error! Full sample id was selected by the sheet [", names(length_sample_id)[which.max(length_sample_id)], '].',
               " It shoud be [base]!",
               bullet = 'cross', bullet_col = 'red')
    sample_id_full <- list_sample_id[['base']]
    num_er = num_er + 1
  }else{
    cat_bullet("Full sample id was selected by the sheet [", names(length_sample_id)[which.max(length_sample_id)], '].',
               bullet = 'heart', bullet_col = 'blue')
    sample_id_full <- list_sample_id[[which.max(length_sample_id)]]
  }
  
  # The sample id definition of base and Rrs is a kind of different. Have to check.
  w = which(!(list_sample_id$Rrs %in% list_sample_id$base))
  if(length(w) != 0){
    cat_bullet("In Rrs sheet, there found sample id(s) are different from the base: ", paste(list_sample_id$Rrs[w], collapse = " "),
               bullet = "cross", bullet_col = "red")
    for(i in 1:length(w)){
      w_maybe = which(str_detect(list_sample_id$base, str_c(list_sample_id$Rrs[w][i],"_1")))
      if(length(w_maybe) == 0){
        cat_bullet("No.", i, " Cannot find matched '", list_sample_id$Rrs[w][i], "' from the base sample id!",
                   bullet = "radio_on", bullet_col = "pink", col = 'pink')
        stop("Check process exist!")
      }else{
        cat_bullet("No.", i, " Is '", list_sample_id$Rrs[w][i], "'matched with '",
                   paste(list_sample_id$base[w_maybe], collapse = ", ") ,"' from the base sample id?",
                   bullet = "radio_on", bullet_col = "pink", col = 'pink')
        if(fix_Rrs_sample_id & length(w_maybe) == 1){
          colnames(dt$Rrs)[-1][w][i] <- str_c(list_sample_id$Rrs[w][i],"_1") # replace the former one
          cat_bullet("Automatically modified as ", str_c(list_sample_id$Rrs[w][i],"_1"), bullet = 'heart', bullet_col = 'blue')
        }else{
          stop("Check process exist!")
        }
      }
    }
  }
  
  # Detect whether the input spread satisfy the format of the required.
  for(i in names(length_sample_id)[!{names(length_sample_id) %in% c("base","Rrs")}]){
    sample_missing <- sample_id_full %in% list_sample_id[[i]] %>% {!.} %>% sample_id_full[.] %>%
      paste0(., collapse = ", ")
    if(sample_missing == ""){
      cat_bullet("Sheet [", i, '] includes same samples with [base]!', 
                 bullet = "tick", bullet_col = 'green')
    }else{
      cat_bullet("Sheet [", i, '] includes missing sample ids: ', paste(sample_missing, collapse = " "),
                 bullet = "cross", bullet_col = "red")
      num_er = num_er + 1
    }
    
  }

  # final report
  Status <- ifelse(num_er == 0, "Pass", "Error")
  col    <- ifelse(num_er == 0, {ifelse(num_wr == 0, "steelblue", "yellow")}, "red")
  
  cat_bullet("--- Sheet name check finished ! --- Status: [", Status,"] --- ",
             "Error number: [", num_er, 
             "] --- Warning number: [", num_wr, "]",
             col = col,
             bullet = 'info')
  
  result <- input
  result$Status_sample_id = Status
  result$dt = dt
  
  return(result)
  
}



#' @name Check_base_info
#' @title Check the format of sheet 'base'
#' @importFrom cli cat_bullet
#' @importFrom readxl excel_sheets
#' @import stringr
#' @import stringi
#' @import dplyr
#' @param input input
#' @param dataset_format dataset_format (default as 1)
#' @param as_EN_colnames Convert colnames to EN format (default as TRUE)
#' @return List
#' @export
#' 
Check_base_info <- function(input, dataset_format = 1, as_EN_colnames = TRUE){
  
  cat_bullet("--- Base info check begin!", 
             bullet = 'info', col = "steelblue")
  
  num_er <- num_wr <-  0
  
  dt = input$dt
  dt_base <- dt[['base']]
  
  name_ <- colnames(dt_base)
  
  unit <- str_extract_all(name_, "(\\[.*\\])", simplify = T) %>% c
  name <- str_replace_all(name_, "(\\[.*\\])", "")
  
  cat_bullet("Found colnames (unit removed) are: ", paste(name, collapse = ", "),
             bullet = "heart", bullet_col = "blue")
  
  if(dataset_format == 1){
    base_colnames_CN <- dataset_format_1$base_info[,"base_colnames_CN"]
    base_colnames_EN <- dataset_format_1$base_info[,"base_colnames_EN"]
    base_colnames_type = dataset_format_1$base_info[,"base_colnames_type"]
  }
  
  cat_bullet("Formatted colnames (CN): ", paste(base_colnames_CN, collapse = ", "), 
             bullet = "heart", bullet_col = "blue", col = "blue")
  
  cat_bullet("Formatted colnames (EN): ", paste(base_colnames_EN, collapse = ", "),  
             bullet = "heart", bullet_col = "blue", col = "blue")
  
  # The function will detect the out-formatted columns in the raw spread and stop checking
  # The user requires to inspect these columns manually.
  if(length(name) > length(base_colnames_CN)){
    w = ((str_to_lower(name) %in% str_to_lower(base_colnames_CN)) | (str_to_lower(name) %in% str_to_lower(base_colnames_EN)))
    w = which(!w)
    cat_bullet(paste(name[w], collapse = ", "), " found in the current spread. ",
               "They are not required colnames. If you want to save them, just keep them into the column Note. ",
               "Or if you confirm these parameters as the often-used. Please contact me to modify the code. ", 
               "Now please check and re-submit!",
               bullet = "cross", bullet_col = 'red')
    stop("Exit check process!")
  }
  
  w = which(!{{str_to_lower(base_colnames_CN) %in% str_to_lower(name)} | {str_to_lower(base_colnames_EN) %in% str_to_lower(name)}})
  
  if(length(w) == 0){
    cat_bullet("Great! All required colnames in [base] are detected!",
               bullet = "tick", bullet_col = "green")
    w_sort = seq(length(base_colnames_CN))
    for(i in 1:length(w_sort)){
      w_sort[i] = which(base_colnames_CN[i] == name | str_to_lower(base_colnames_EN[i]) == str_to_lower(name))
    }
    dt_base <- dt_base[, w_sort]
    if(as_EN_colnames){
      names(dt_base) <- base_colnames_EN
      cat("  Colnames of [base] are converted to EN characters.\n")
    }else{
      names(dt_base) <- base_colnames_CN
      cat("  Colnames of [base] are converted to CN characters.\n")
    }
  }else{
    cat_bullet(paste(base_colnames_CN[w], collapse = ", "), ' (or ', 
               paste(base_colnames_EN[w], collapse = ", "),
               ") CANNOT be found in [base] correctly. Please check and re-submit!",
               bullet = "cross", bullet_col = 'red')
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
                   bullet = "radio_on", bullet_col = "pink", col = 'pink')
      }else{
        cat_bullet("No.", i, " Could not find matched patterns for '", 
                   base_colnames_CN[w][i], "' or ('", base_colnames_EN[w][i], "')", " from the input colnames.",
                   bullet = "radio_off", bullet_col = "pink", col = "pink")
      }
    }
  }

  # final report
  Status <- ifelse(num_er == 0, "Pass", "Error")
  col    <- ifelse(num_er == 0, {ifelse(num_wr == 0, "steelblue", "yellow")}, "red")
  
  # If base info check pass, the base data should be re-read as the col_types should be defined again.
  if(Status == "Pass"){
    Sheets = excel_sheets(input$fn)
    w_sheet = which(Sheets == stri_unescape_unicode("\\u57fa\\u672c\\u53c2\\u6570") | Sheets == "base")
    dt_base <- read_excel(input$fn, sheet=w_sheet, 
                          cell_cols(w_sort),
                          col_types = base_colnames_type)[, w_sort]
    if(as_EN_colnames){
      names(dt_base) <- base_colnames_EN
      cat("  Colnames of [base] are converted to EN characters.\n")
    }else{
      names(dt_base) <- base_colnames_CN
      cat("  Colnames of [base] are converted to CN characters.\n")
    }
  }
  
  cat_bullet("--- Base info check finished ! --- Status: [", Status,"] --- ",
             "Error number: [", num_er, 
             "] --- Warning number: [", num_wr, "]",
             col = col,
             bullet = 'info')
  
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
#' @title Main function to check spread format using Check_sheet_name, Check_sample_id,
#'   and Check_base_info.
#' 
#' @param fn Filename
#' @return A list
#' @export
#' 
#' @importFrom cli cat_boxx
#' 
Check_format <- function(fn,
                         dataset_format = 1,
                         as_EN_colnames = TRUE){
  
  cat_boxx("Format check begin!")
  res_1 = Check_sheet_name(fn, dataset_format = dataset_format)
  cat_boxx("Check_sheet_name() finished!")
  res_2 = Check_sample_id(res_1, dataset_format = dataset_format)
  cat_boxx("Check_sample_id() finished!")
  res_3 = Check_base_info(res_2, dataset_format = dataset_format, as_EN_colnames = as_EN_colnames)
  cat_boxx("Check_base_info() finished!")
  
  return(res_3)
  
}

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

#' @name Check_geo_location
#' @title Check the geo-location through heatmap
#' 
#' @import leaflet
#' @import htmltools
#' @importFrom cli cat_bullet
#' @importFrom stats median
#' @importFrom magrittr %<>%
#' @importFrom htmlwidgets saveWidget
#' 
#' @param input input
#' @return List
#' @export
#' 
Check_geo_location <- function(input){
  
  if(is.null(input$Status_base_info))
    stop("Run 'Check_base_info' at first!")
  
  if(input$Status_base_info != 'Pass')
    stop("'Check_base_info' does not pass! Check and re-submit!")
  
  dt_base = input$dt[['base']] %>% as.data.frame
  
  if(!input$as_EN_colnames) names(dt_base) = input$as_EN_colnames
  
  w <- which(dt_base$Depth == 0)
  df <- data.frame(name=dt_base$SampleID[w],
                   lon=dt_base$LON[w], 
                   lat=dt_base$LAT[w]) %>% level_to_variable()
  for(i in 2:3) df[,i] <- as.numeric(df[,i])
  
  m = leaflet(df) %>% addTiles()
  m = m %>% setView(median(df$lon), median(df$lat), zoom = 9)
  label.opt <- labelOptions(noHide=F, textOnly=T, textsize="9px") 
  m %<>% 
    addScaleBar() %>%
    addCircleMarkers(~lon, ~lat, label=~htmlEscape(name), labelOptions=label.opt,
                     radius=2, opacity=0, fillOpacity=1, color="red")
  
  print(m)
  
  fn_map <- "Geolocation.html"
  saveWidget(m, file=fn_map)
  if(!dir.exists('html_widgets')) dir.create('html_widgets')
  a = file.copy(fn_map, 'html_widgets', overwrite = TRUE)
  a = file.remove(fn_map)
  fn_map_ <- file.path('html_widgets', 
                      paste(str_remove(basename(input$fn), '\\..{3,4}$'), basename(fn_map), sep = "_"))
  a = file.rename("./html_widgets/Geolocation.html", fn_map_)
  
  cat_bullet("The html map was saved to: ", fn_map_, " Please inspect it visually!",
             bullet = 'heart', bullet_col = 'blue')
  
  cat_bullet("If any mistake found, please modify it manualy!",
             bullet = 'heart', bullet_col = 'blue')
  
  result <- input
  result$map <- m
  result$fn_map <- fn_map_
  
  return(result)
  
}



#' @name Check_param_quality
#' @title Check the quality of parameters
#' 
#' @importFrom cli cat_bullet
#' @import htmltools
#' @import plotly
#' 
#' @param input input
#' @return List
#' @export
#' 
Check_param_quality <- function(input){
  
  return(NULL)
  
}



#' @name Check_spectra_quality
#' @title Check the quality of spectra
#' 
#' @importFrom cli cat_bullet
#' @import htmltools
#' @import plotly
#' 
#' @param input input
#' @return List
#' @export
#' 
Check_spectra_quality <- function(input){
  
  return(NULL)
  
}

#' @name build_spectra
#' @title Randomly build spectra for Rrs, aph, anap and aCDOM
#' @param Chla Chla concentration. If NULL then randomly produce one.
#' @param adg400 adg at 400 nm. If NULL then randomly produce one.
#' @importFrom stats runif
#' @noRd
#' 
build_spectra <- function(Chla = NULL, adg400 = NULL){
  
  wv = c(404, 412, 442, 490, 510, 560, 620, 666, 674, 681, 708,
         754, 761, 764, 768, 779)
  dt_aphs = c(0.1766, 0.1814, 0.169, 0.1246, 0.1088, 0.0791,
              0.0776, 0.0834, 0.0902, 0.0873, 0.0472, 0.0313,
              0.0302, 0.0298, 0.0294, 0.0279)
  dt_aw = c(0.0062, 0.0045, 0.0068, 0.015, 0.0325, 0.0619, 
            0.2755, 0.4314, 0.4459, 0.4688, 0.817, 2.7808, 
            2.6701, 2.6114, 2.5303, 2.3324)
  percent_anap <- c(0.78, 0.78, 0.81, 0.81, 0.81, 0.79, 0.83, 
                    0.93, 0.91, 0.93, 0.88, 0.47, 0.47, 0.46, 0.45, 0.43)
  S_adg = -0.0035
  
  Chla = ifelse(is.null(Chla), 10^runif(1, -0.070, 2.564), Chla)
  adg400 = ifelse(is.null(adg400), 10^runif(1, 0.296, 1.171), adg400)
  
  aph = dt_aphs * Chla
  adg = adg400 * exp(S_adg*wv)
  anap= adg * percent_anap
  aCDOM=adg-anap
  
  a <- dt_aw + adg + aph
  bb = 5 * exp(-0.0035*wv)
  
  rrs = 0.089*(bb/(a+bb)) + 0.125*(bb/(a+bb))^2
  Rrs = 0.52*rrs / (1-1.7*rrs)
  Rrs = round(Rrs, 4)
  
  result <- list(Chla=Chla, 
                 adg400=adg400, 
                 Rrs=Rrs,
                 ap=aph+anap,
                 aph=aph,
                 anap=anap,
                 aCDOM=aCDOM)
  
  return(result)
}



#' @name Generate_ref_spread
#' @title Generate spreads of the built-in format (recommand)
#' @import openxlsx
#' @param dataset_format dataset_format (default as 1)
#' @param file_format file_format (default as 'xlsx')
#' @param overwrite Default as \code{FALSE}, just in case you forget to copy demo spread to other folders.
#' @param use_CN Use CN colnames (default as \code{TRUE})
#' @param QF_sheet Qualtiy Flag sheets (default as \code{FALSE})
#' @param Meta_sheet Metadata info sheets (default as \code{TRUE})
#' @param add_demo Add demo samples (TRUE)
#' @param add_unit Add unit (TRUE)
#' @return List 
#' @export
#' 
#' @importFrom cli cat_bullet
#' @importFrom stringr str_sub
#' 
Generate_ref_spread <- function(dataset_format = 1, 
                                file_format = "xlsx", 
                                overwrite = FALSE,
                                use_CN = TRUE,
                                QF_sheet = FALSE, 
                                Meta_sheet = TRUE,
                                add_demo = TRUE,
                                add_unit = TRUE){
  
  # openxlsx package used here to specify cell range
  
  options("openxlsx.dateFormat" = "YYYY-mm-dd")
  options("openxlsx.datetimeFormat" = "YYYY-mm-dd")
  
  wb <- createWorkbook()
  
  modifyBaseFont(wb, fontSize = 12, fontName = "Arial")
  
  if(dataset_format == 1){
    # data("dataset_format_1")
    sheetnames = dataset_format_1$sheets_info[,"Sheets_required"]
    if(QF_sheet) sheetnames <- c("QF", sheetnames)
    if(Meta_sheet) sheetnames <- c("Meta", sheetnames)
    demo_samples <- dataset_format_1$demo_samples
    if(use_CN){
      base_colnames <- dataset_format_1$base_info[,c("base_colnames_CN","base_unit_CN")]
    }else{
      base_colnames <- dataset_format_1$base_info[,c("base_colnames_EN","base_unit_EN")]
    }
    base_colnames <- str_c(base_colnames[,1], base_colnames[,2])
    colnames(demo_samples) <- base_colnames
    Sample_head <- str_c(format(Sys.Date(),"%Y%m"),"XXX")
    SampleID <- paste0(Sample_head, demo_samples[,1])
    demo_samples[,1] <- SampleID
    fn_output <- sprintf("%sDataCollection_v1.0.%s", Sample_head, file_format)
    cat_bullet("The output filename is ", fn_output, bullet = 'heart', bullet_col = "blue")
  }
  
  # build ms style
  headstyle <- createStyle(textDecoration = "bold", fgFill = "#EBF1DE")
  fgFills <- c("#FDE9D9","#DAEEF3","#E4DFEC","#F2DCDB","#DCE6F1","#C5D9F1","#DDD9C4","#D9D9D9")
  SampleIDstyle <- createStyle(textDecoration = "bold", fgFill = sample(fgFills, 1))
  
  # generage demo Spectra
  build_demo_spectra <- function(x){
    wv <- c(404, 412, 442, 490, 510, 560, 620, 666, 674, 681, 708,
            754, 761, 764, 768, 779)
    demo_spectra <- matrix(data = rep(NA,length(wv)*length(SampleID)),
                           ncol = length(SampleID),
                           nrow = length(wv))
    for(i in 1:ncol(demo_spectra)){
      demo_spectra[,i] = build_spectra(Chla=demo_samples[i,24])[[x]]
    }
    demo_spectra <- data.frame(wv=wv, demo_spectra)
    colnames(demo_spectra) <- c(base_colnames[1], SampleID)
    return(demo_spectra)
  }
  
  # add worksheet
  for(sheet in sheetnames){
    addWorksheet(wb, sheet)
    cat_bullet("Added sheet ", sheet, " to ", fn_output, bullet = 'heart', bullet_col = "blue")
  }
  
  # add and write Meta info
  if(Meta_sheet){
    pkg_version <- gsub("Version: ","",readLines('DESCRIPTION')[stringr::str_which(readLines('DESCRIPTION'), 'Version')])
    pkg_version <- str_c("DAMATO v", pkg_version, " by Shun Bi")
    dt_meta <- data.frame(
      Item = c("Filename","Data_Collect_Date","File_Create_Date","Maintainer(s)","Package_Version","Notes"),
      Content = c(fn_output,"YYYY-MM-DD",as.character(Sys.Date()),"Add your name(s)",pkg_version,"Add notes here.")
    )
    writeData(wb, sheet = "Meta", x = dt_meta,
              startRow = 1, startCol = "A",
              borders = "surrounding")
    addStyle(wb, 
             sheet = "Meta",
             style = headstyle,
             gridExpand = TRUE, stack = TRUE,
             cols = 1:ncol(dt_meta),
             rows = 1)
    cat_bullet("Added meta info to sheet Meta", bullet = 'heart', bullet_col = "blue")
    
    dt_version <- data.frame(
      Date = c(as.character(Sys.Date()),"",""),
      Version = c("1.0","",""),
      Modifiers = c("Add name(s) who modified the spreadsheet in that version.","",""),
      Change_details = c("What are news in this version? Note that all previous version records should be listed above the current one.","","")
    )
    writeData(wb, sheet = "Meta", x = dt_version, 
              startRow = {nrow(dt_meta) + 4}, startCol = "A",
              borders = "surrounding")
    addStyle(wb, 
             sheet = "Meta",
             style = headstyle,
             gridExpand = TRUE, stack = TRUE,
             cols = 1:ncol(dt_version),
             rows = {nrow(dt_meta) + 4})
    setColWidths(wb, sheet = "Meta", cols = 1:3, widths = "auto")
    cat_bullet("Added version record to sheet Meta", bullet = 'heart', bullet_col = "blue")
  }
  
  # write demos to the sheet base
  writeData(wb, sheet = dataset_format_1$sheets_info[,"Sheets_required"][1],
            x = demo_samples,
            borders = "surrounding")
  addStyle(wb, 
           sheet = dataset_format_1$sheets_info[,"Sheets_required"][1],
           style = headstyle,
           gridExpand = TRUE, stack = TRUE,
           cols = 1:ncol(demo_samples),
           rows = 1)
  addStyle(wb, 
           sheet = dataset_format_1$sheets_info[,"Sheets_required"][1],
           style = SampleIDstyle,
           gridExpand = TRUE, stack = TRUE,
           cols = 1,
           rows = (1:nrow(demo_samples))+1)
  setColWidths(wb, sheet = dataset_format_1$sheets_info[,"Sheets_required"][1],
               cols = 1:ncol(demo_samples), widths = "auto")
  cat_bullet("Added demo samples to sheet ", dataset_format_1$sheets_info[,"Sheets_required"][1], 
             bullet = 'heart', bullet_col = "blue")
  
  # write Rrs data to sheet 1.Rrs
  demo_spectra = build_demo_spectra("Rrs")
  writeData(wb, sheet = dataset_format_1$sheets_info[,"Sheets_required"][2],
            x = demo_spectra[, c(1,which(demo_samples[,6] == 0) + 1)],
            borders = "surrounding")
  setColWidths(wb, sheet = dataset_format_1$sheets_info[,"Sheets_required"][2],
               cols = 1:ncol(demo_spectra[, c(1,which(demo_samples[,6] == 0) + 1)]),
               widths = "auto")
  addStyle(wb,
           sheet = dataset_format_1$sheets_info[,"Sheets_required"][2],
           style = SampleIDstyle, stack = TRUE,
           cols = 2:ncol(demo_spectra[, c(1,which(demo_samples[,6] == 0) + 1)]),
           rows = 1)
  cat_bullet("Added demo spectra (surface only) to sheet ", dataset_format_1$sheets_info[,"Sheets_required"][2], 
             bullet = 'heart', bullet_col = "blue")
  
  # write absorption data to sheets
  if(dataset_format == 1){
    for(sheet in dataset_format_1$sheets_info[,"Sheets_required"][-c(1,2)]){
      x = str_sub(sheet,3)
      if(x == "apc"){
        demo_spectra = build_demo_spectra("aph") # use aph to replace apc
      }else{
        demo_spectra = build_demo_spectra(x)
      }
      writeData(wb, sheet = sheet,
                x = demo_spectra,
                borders = "surrounding")
      setColWidths(wb, sheet = sheet,
                   cols = 1:ncol(demo_spectra),
                   widths = "auto")
      addStyle(wb,
               sheet = sheet,
               style = SampleIDstyle, stack = TRUE,
               cols = 2:ncol(demo_spectra),
               rows = 1)
      cat_bullet("Added demo spectra to sheet ", sheet, 
                 bullet = 'heart', bullet_col = "blue")
      
    }
  }
  
  if(add_demo == FALSE){
    
  }
  
  # Add unit
  if(add_unit){
    addWorksheet(wb, 'unit')
    writeData(wb,
              sheet = 'unit',
              x = dataset_format_1$base_info,
              borders = "surrounding")
    setColWidths(wb, sheet = "unit", cols = 1:ncol(dataset_format_1$base_info), widths = "auto")
    addStyle(wb, sheet = "unit", style = headstyle, stack = TRUE,
             cols = 1:ncol(dataset_format_1$base_info), rows = 1)
    
    if(dataset_format == 1){
      unit_spec = dataset_format_1$sheets_info
    }
    writeData(wb,
              sheet = 'unit',
              x = unit_spec,
              startCol = 1, startRow = nrow(dataset_format_1$base_info) + 5, 
              borders = "surrounding")
    setColWidths(wb, sheet = "unit", cols = 1:ncol(unit_spec), widths = "auto")
    addStyle(wb, sheet = "unit", style = headstyle,  stack = TRUE,
             cols = 1:ncol(unit_spec), rows = nrow(dataset_format_1$base_info) + 5)
    
    cat_bullet("Added the unit sheet", 
               bullet = 'heart', bullet_col = "blue")
  }
  
  # save spread
  if(!dir.exists("spreads")){
    dir.create("spreads")
    cat_bullet("No 'spreads' fold found, create sucessfully", bullet = 'heart', bullet_col = "blue")
  }
  
  saveWorkbook(wb, file = file.path("spreads", fn_output), overwrite = overwrite)
  
  cat_bullet("Okay! The spreadsheet is sucessfully generated!", bullet = "tick", bullet_col = "green")
  
  return( file.path("spreads", fn_output) )
  
}


# files <- lapply(list.files(system.file('extdata', package = 'DAMATO'), full.names = TRUE), read.csv)


#' @name dataset_format_1
#' @title Format_1 used for generating reference spreads
#' @docType data
#' @usage dataset_format_1
#' @encoding UTF-8
NULL



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


utils::globalVariables(c('.', 'dataset_format_1'))
                         