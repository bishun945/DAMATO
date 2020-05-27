## Chla
dt_Chla <- dt_base$Chla[-11] # remove bloom spec.
range(dt_Chla) %>% range %>% log10 %>% round(3) # -0.070  2.564

## ISM
dt_ISM  <- dt_base$ISM[-11]

## aph
aph_OLCI <- SRF_simulate(dt$aph,"OLCI")
dt_aph <- aph_OLCI$OLCI$Rrs_simu %>% as.data.frame
colnames(dt_aph) <- aph_OLCI$OLCI$cw_med[1:ncol(dt_aph)]
wv = aph_OLCI$OLCI$cw_med[1:ncol(dt_aph)]
w = which(dt_base$Depth == 0)
dt_aph <- dt_aph[w,]
dt_aph <- dt_aph[-11,]
dt_aphs = {dt_aph / dt_Chla} %>% apply(., 2, mean) %>% as.numeric ##

## anap
anap_OLCI <- SRF_simulate(dt$anap,"OLCI")
dt_anap <- anap_OLCI$OLCI$Rrs_simu %>% as.data.frame
colnames(dt_anap) <- anap_OLCI$OLCI$cw_med[1:ncol(dt_anap)]
w = which(dt_base$Depth == 0)
dt_anap <- dt_anap[w,]
dt_anap <- dt_anap[-11,]

## aCDOM
aCDOM_OLCI <- SRF_simulate(dt$aCDOM,"OLCI")
dt_aCDOM <- aCDOM_OLCI$OLCI$Rrs_simu %>% as.data.frame
colnames(dt_aCDOM) <- aCDOM_OLCI$OLCI$cw_med[1:ncol(dt_aCDOM)]
w = which(dt_base$Depth == 0)
dt_aCDOM <- dt_aCDOM[w,]
dt_aCDOM <- dt_aCDOM[-11,]

## adg
dt_adg <- dt_anap + dt_aCDOM
dt_adg_ <- dt_adg[,wv<=700]
plot_spec_from_df(dt_adg_)
dt_adg_ <- {dt_adg_ / dt_adg_$`404`} %>% log %>% {./wv[wv<=700]}
S_adg = -0.0035
dt_adg$`404` %>% range %>% log10 %>% round(3) # c(0.296 1.171) ##

{dt_anap / dt_adg} %>% colMeans %>% as.numeric %>% round(2) %>% str_c(., collapse = ", ") %>% str_c("c(",.,")") %>% cat

## aw
dt_aw <- FCMm::dt_water$aw[dt_water$nm %in% aph_OLCI$OLCI$cw_med][1:ncol(dt_aph)] ##


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
                 adg440=adg440, 
                 Rrs=Rrs,
                 aph=aph,
                 anap=anap,
                 aCDOM=aCDOM)
  
  return(result)
}









