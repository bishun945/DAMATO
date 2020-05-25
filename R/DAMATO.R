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
#' @importFrom stringr str_detect
#' @importFrom cli cat_bullet
#' @param fn fn
#' @param dataset_format dataset_format (default as 1)
#' @return List
#' @export
#' 
Check_sheet_name <- function(fn, dataset_format = 1){
  
  cat_bullet("--- Sheet name check begin!", 
             bullet = 'info', col = "steelblue")
  
  num_er <- num_wr <-  0
   
  if(!Check_spread_file(fn)) stop("File check failed!")
  
  Sheets_found <- excel_sheets(fn)
  cat_bullet("Following sheets are found: ", paste(Sheets_found, collapse = " "), bullet = 'heart', bullet_col = "blue")
  
  if(dataset_format == 1){
    Sheets_required <- c("基本参数","1.Rrs","2.ap","3.aph","4.anap","5.aCDOM","6.apc")
    general_name    <- c("base","Rrs","ap","aph","anap","aCDOM","apc")
  }
  
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
#' @importFrom cli cat_bullet
#' @importFrom stringr str_detect str_match
#' @param input input
#' @param dataset_format dataset_format (default as 1)
#' @return List
#' @export
#' 
Check_sample_id <- function(input, dataset_format = 1){
  
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
    
    if(name == general_name[1] | name == '基本参数'){                   # for base sheet
      
      w <- str_detect(names(dt_sub), '编号|通用|SampleID') %>% which()
      if(sum(w) == 1){
        list_sample_id[[name]] <- dt_sub[, w] %>% as.matrix %>% c
        cat_bullet("'SampleID' found in [", name, "]", bullet = "tick", bullet_col = "green")
      }else{
        cat_bullet("'SampleID' CANNOT found in [", name, "]", bullet = 'cross', bullet_col = 'red')
        num_er = num_er + 1
      }
      
    }else{                                                              # for following sheets
      
      if(!all(str_detect(names(dt_sub), '编号|通用|SampleID'))){
        cat_bullet("'SampleID' CANNOT be found in [", name, 
                   "]. The first colname is used here. Recommand to change '", names(dt_sub)[1], "' as 'SampleID'.", 
                   bullet = 'bullet', bullet_col = "yellow")
        num_wr = num_wr + 1
      }
      
      wrong_pattern <- '备注|Note|note|\\[|\\]|\\{|\\}|\\(|\\)|（|）|-'
      
      if(any(str_detect(names(dt_sub)[-1], wrong_pattern))){
        cat_bullet("Found '", 
                   paste(names(dt_sub)[-1][str_detect(names(dt_sub)[-1], wrong_pattern)], collapse = " "),
                   "' in colnames of [", name, 
                   "] with the wrong format 'YYYYMMXXNN_Z'. They have been removed. Please check and re-submit!",
                   bullet = "cross", bullet_col = 'red')
        num_er = num_er + 1
        w <- which(str_detect(names(dt_sub), '备注|Note|note'))
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
  
  return(result)
  
}



#' @name Check_base_info
#' @title Check the format of sheet 'base'
#' @importFrom cli cat_bullet
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
    base_colnames_CN <- c('通用编号','测量日期','测量时间','经度','纬度','采水深度',
                          '是否垂向','天气状况','云量状况','大气能见度','风向','pH','Eh','DO',
                          '海拔','气压','空气温度','相对湿度','风速','水深','水温','水表温','透明度',
                          '叶绿素a','藻蓝蛋白','总藻毒素','胞外藻毒素','胞内藻毒素','颗粒有机碳','溶解有机碳','总悬浮物','无机悬浮物',
                          '有机悬浮物','总氮','总磷','溶解总氮','溶解总磷','光合有效辐射','周围情况描述','备注')
    base_colnames_EN <- c('SampleID','Date','Time','LON','LAT','Depth',
                          'IfVertial','Weather','Cloudiness','Visibility','Wind_Dir.','pH','Eh','DO',
                          'Altitude','Air_Pres.','Air_Temp.','R.H.','Wind_Spe.','Bathymetry','Water_Body_Temp.','Water_Surf._Temp.','SSD',
                          'Chla','PC','TMC','EMC','IMC','POC','DOC','TSM','ISM',
                          'OSM','TN','TP','DTN','DTP','PAR','Surroundings','Notes')
  }
  
  cat_bullet("Formatted colnames (CN): ", paste(base_colnames_CN, collapse = ", "), 
             bullet = "heart", bullet_col = "blue", col = "blue")
  
  cat_bullet("Formatted colnames (EN): ", paste(base_colnames_EN, collapse = ", "),  
             bullet = "heart", bullet_col = "blue", col = "blue")
  
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



#' @name Check_geo_location
#' @title Check the geo-location through heatmap
#' @import leaflet
#' @import htmltools
#' @importFrom cli cat_bullet
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
                   lat=dt_base$LAT[w]) %>% .level_to_variable()
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
  a = file.copy(fn_map, 'html_widgets')
  a = file.remove(fn_map)
  fn_map_ <- file.path('html_widgets', 
                      paste(str_remove(basename(input$fn), '\\..{3,4}$'), basename(fn_map), sep = "_"))
  
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
#' @importFrom cli cat_bullet
#' @import htmltools
#' @import plotly
#' @param input input
#' @return List
#' @export
#' 
Check_param_quality <- function(input){
  
}



#' @name Check_spectra_quality
#' @title Check the quality of spectra
#' @importFrom cli cat_bullet
#' @import htmltools
#' @import plotly
#' @param input input
#' @return 
#' @export
#' 
Check_spectra_quality <- function(input){
  
}



#' @name Generate_ref_spread
#' @title Generate spreads of the built-in format (recommand)
#' @import openxlsx
#' @param dataset_format dataset_format (default as 1)
#' @param file_format file_format (default as 'xlsx')
#' @return List 
#' @export
Generate_ref_spread <- function(dataset_format = 1, file_format = "xlsx"){
  
  # openxlsx package used here to specify cell range
  
}


