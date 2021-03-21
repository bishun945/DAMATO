#' @name Generate_ref_spread
#' @title Generate spreads of the built-in format (recommend)
#' @import openxlsx
#' @param dataset_format dataset_format (default as 1)
#' @param file_format file_format (default as 'xlsx')
#' @param overwrite Default as \code{FALSE}, just in case you forget to copy demo spread to other folders.
#' @param use_CN Use CN colnames (default as \code{TRUE})
#' @param QF_sheet Quality Flag sheets (default as \code{FALSE})
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



#' @name Generate_ref_folders
#' @title Generate_ref_folders
#' @param dataset_format dataset_format
#' @return The created templated folders shoud be at the working directory.
#' 
#' @importFrom cli tree
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' 
#' @export
Generate_ref_folders <- function(dataset_format = 1){
  
  cli_control("head", sprintf("Generating reference folders with the format %s", dataset_format))
  
  cat("\n")
  
  if(dataset_format == 1){
    
    cli_control("info", "The folder structure as the template is:")
    tree(dataset_format_1_folders$folders_tree) %>%
      style_bold %>%
      col_green %>%
      cat_line
    cat("\n")
    
    cli_control("info", "Folder structure analyzing ...")
    ft = solve_tree(dataset_format_1_folders$folders_tree)
    if(sum(names(ft) %in% c("path", "depth")) == 2) cli_control("right", "Folder solved sucessful!")
    cat("\n")
    
    cli_control("info", "Folders creating...")
    for(i in sort(unique(ft$depth))){
      for(path in ft$path[ft$depth == i]){
        if(str_detect(path, "[txt]")){
          write("", path, append = TRUE)
        }else{
          dir.create(path)
        }
        cli_control("okay", path)
      }
    }
    
  }
  
  cat("\n")
  
  cli_control("end", sprintf("Generating reference folders with the format %s", dataset_format))
  
}



#' @name solve_tree
#' @title solve file trees created by cli::tree
#' @param ft tree data.frame from cli::tree
#' @return ft with appended columns father, path and depth used for folder creating
#' @noRd
#' 
#' @importFrom stringr str_split str_count
#' 
solve_tree <- function(ft){
  
  # ft = dataset_format_1_folders$folders_tree
  
  ft$father = NA
  
  w_fathers = which((lapply(ft$deps, length) %>% unlist) != 0)
  
  # find its close father for each line
  for(i in 1:nrow(ft)){
    
    item = ft$name[i]
    
    for(w_father in w_fathers){
      
      fathers = ft$deps[[w_father]]
      tmp = fathers %in% item
      if(any(tmp)){
        ft$father[i] = ft$name[w_father]
        next
      } 
      
    }
    ## detect the big father (the main fold)
    if(is.na(ft$father[i])){
      ft$father[i] = "."
      big_father_w = i  
      big_father   = item
    }
    
  }
  
  # path is the fold to be created 
  ft$path = ft$father
  
  w = (str_split(ft$path, "\\/", simplify = TRUE)[-big_father_w,1] %in% big_father)
  
  # add fathers to each line until they all reach the big_father in the head
  while(sum(!w) != 0){
    
    for(j in seq(nrow(ft))[-big_father_w][which(!w)]){
      j_father = str_split(ft$path[j], "\\/")[[1]][1]
      ft$path[j] <- file.path(ft$father[which(ft$name == j_father)], ft$path[j])
    }
    
    w = (str_split(ft$path, "\\/", simplify = TRUE)[-big_father_w,1] %in% big_father)
    
  }
  
  ft$path[big_father_w] <- file.path(ft$path[big_father_w], ft$name[big_father_w])
  ft$path[-big_father_w] <- file.path(".",ft$path[-big_father_w], ft$name[-big_father_w])
  
  # detect the depth of folder to decide the sort of dir.create
  ft$depth = str_count(ft$path, "\\/")
  
  return(ft)
  
}





