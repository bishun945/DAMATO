
#' @name cli_control
#' @title Control the position and information should be printed
#' 
#' @param pos The position of cli function should be
#' @param text The character to print
#' @param col_status The status color for pos = end
#'  
#' @import cli
#' @importFrom utils packageVersion
#' @importFrom magrittr %>%
#' 
#' @export
#' 
#' @examples 
#' cli_control("head", "Message 1")
#' cli_control("okay", "Okay message")
#' cli_control("warning", "Warning message")
#' cli_control("error", "Error message")
#' cli_control("info", "Plain information")
#' cli_control("end", "End of message")
#' 
cli_control <- function(pos = "info", text, col_status = NULL){
  
  if(!is.character(text)) text = as.character(text)
  if(length(text) > 1) text = paste(text, collapse = "")
  
  pos = match.arg(pos, c("head", "end", "error", "warning", "okay", "info", "right", "status"))
  right = paste0(Sys.time()," by DAMATO v", packageVersion('DAMATO'))
  
  if(pos == "head"){
    
    rule(left = style_bold(text), right = style_bold(paste("Start at",right)),
         line = 2, col = "cyan") %>% cat(., "\n")
    
  }else if(pos == "end"){
    
    rule(left = style_bold(text), right = style_bold(paste("Finish at",right)),
         line = 2, col = "cyan") %>% cat(., "\n")
    
  }else if(pos == "error"){
    
    cat_bullet(col_red(style_bold(text)), bullet = "cross", bullet_col = "red")
    
  }else if(pos == "warning"){
    
    cat_bullet(col_yellow(text), bullet = "bullet", bullet_col = "yellow")
    
  }else if(pos == "okay"){
    
    cat_bullet(col_green(text), bullet = "heart", bullet_col = "green")
    
  }else if(pos == "info"){
    
    cat_bullet(col_cyan(text), bullet = "info", bullet_col = "cyan")
    
  }else if(pos == "right"){
    
    cat_bullet(style_bold(col_green(text)), bullet = "tick", bullet_col = "green")
    
  }else if(pos == "status"){
    
    rule(center = style_bold(text), line = " ", col = col_status) %>% cat(., "\n")
    
  }
  
}
