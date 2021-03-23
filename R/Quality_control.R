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


#' @import FCMm
#' @importFrom dplyr full_join
#' @importFrom ggplot2 ggplot geom_point geom_abline facet_wrap aes 
#' @importFrom ggplot2 scale_x_log10 scale_y_log10 scale_color_viridis_c
#' @importFrom reshape2 melt
#' @noRd
Rrs_and_Chla <- function(x) {
  
  stopifnot(
    x$Status_base_info  == "Pass",
    x$Status_sample_id  == "Pass", 
    x$Status_sheet_name == "Pass"
    )
  
  dt_base <- x$dt$base
  dt_Rrs  <- x$dt$Rrs
  
  dt_Rrs_tmpp <- FCMm::SRF_simulate(dt_Rrs, 
                                    select_sensor = "OLCI", 
                                    input_wv_as_column = FALSE)
  dt_Rrs_OLCI <- dt_Rrs_tmpp$OLCI$Rrs_simu %>%
    as.data.frame() %>%
    setNames(., dt_Rrs_tmpp$OLCI$cw_med)
  
  dt_Rrs_OLCI <- 
    dt_Rrs_OLCI[, !(colnames(dt_Rrs_OLCI) %in% c(761, 764, 768, 900, 940, 1012))]
  
  
  Blend_result<- FCMm::Blend_FCMm(dt_Rrs_OLCI)
  dt_Chla <- data.frame(
    Blend_FCMm = Blend_result$Chla_blend,
    Blend_FCMm_r = Blend_result$Chla_reparam,
    Blend_result$dt_Chla
  )
  rownames(dt_Chla) <- rownames(dt_Rrs_OLCI)
  dt_Chla <- cbind(SampleID = rownames(dt_Chla), dt_Chla)
  
  dt_comp <- dplyr::full_join(dt_Chla, dt_base[, c("SampleID", "Chla")], by = "SampleID")
  
  p <- 
    dt_comp %>%
    reshape2::melt(id = c("SampleID", "Chla")) %>%
    ggplot(aes(x = Chla, y = value, color = Chla)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0, linetype = 2) + 
    facet_wrap(~variable) + 
    scale_x_log10(limits = range(dt_comp$Chla, na.rm = TRUE) * c(0.9, 1.1)) + 
    scale_y_log10(limits = range(dt_comp$Chla, na.rm = TRUE) * c(0.9, 1.1)) + 
    scale_color_viridis_c(trans = "log10")
  
  return(list(
    dt_Chla = dt_comp,
    p_Chla = p,
    Blend_result = Blend_result
  ))
  
}





