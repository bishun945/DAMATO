fd <- list(
  
  #
  MAIN   = c("INSITU", "LABD", "MAIN_OTHERS", "QC_RECORD_xlsx.txt", "QC_RECORD_docx.txt", "QC_RECORD_pptx.txt", "README.txt"),
  
  ##
  INSITU = c("ASD", "PHOTO", "INSITU_OTHERS"),
  LABD   = c("IOPS", "CHL", "SPM", "PC", "LABD_OTHERS"),
  MAIN_OTHERS = character(),
  "QC_RECORD_xlsx.txt" = character(),
  "QC_RECORD_docx.txt" = character(),
  "QC_RECORD_pptx.txt" = character(),
  "README.txt"     = character(),
  
  ###
  ASD    = character(),
  PHOTO  = c("RECORDING_PAPAER", "SCENE_PHOTO", "PHOTO_OTHERS"),
  INSITU_OTHERS = character(),
  IOPS   = character(),
  CHL    = character(),
  SPM    = character(),
  PC     = character(),
  LABD_OTHERS = character(),
  
  ####
  RECORDING_PAPAER = character(),
  SCENE_PHOTO = character(),
  PHOTO_OTHERS = character()
)

fd_tree = data.frame(
  stringsAsFactors = FALSE,
  name = names(fd),
  deps = I(unname(fd))
)

# fd_tree$depth = c(
#   0, 
#   1, 1, 1, 1, 1, 1, 1,
#   2, 2, 2, 
# )

# tree(fd_tree)

dataset_format_1_folders <- list(folders = fd, folders_tree = fd_tree)

save(dataset_format_1_folders, file = "./data/dataset_format_1_folders.rda", compression_level = 9)
