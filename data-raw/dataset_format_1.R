## code to prepare `dataset_format_1` dataset goes here

dataset_format_1 <- list()

dataset_format_1$base_info <- cbind(
  base_colnames_CN = c('通用编号', '测量日期', '测量时间', '经度', '纬度', '采水深度', '是否垂向',
                       '天气状况', '云量状况', '大气能见度', '风向', 'pH', 'Eh', 'DO', '海拔', '气压', 
                       '空气温度', '相对湿度', '风速', '水深', '水温', '水表温', '透明度', '叶绿素a', 
                       '藻蓝蛋白', '总藻毒素', '胞外藻毒素', '胞内藻毒素', '颗粒有机碳', '溶解有机碳', 
                       '总悬浮物', '无机悬浮物', '有机悬浮物', '总氮', '总磷', '溶解总氮', '溶解总磷', 
                       '光合有效辐射', '周围情况描述', '备注'),
  base_colnames_EN = c('SampleID', 'Date', 'Time', 'LON', 'LAT', 'Depth', 'IfVertial', 'Weather', 'Cloudiness',
                       'Visibility', 'Wind_Dir.', 'pH', 'Eh', 'DO', 'Altitude', 'Air_Pres.', 'Air_Temp.', 'R.H.',
                       'Wind_Spe.', 'Bathymetry', 'Water_Body_Temp.', 'Water_Surf._Temp.', 'SSD', 'Chla', 'PC',
                       'TMC', 'EMC', 'IMC', 'POC', 'DOC', 'TSM', 'ISM', 'OSM', 'TN', 'TP', 'TDN', 'TDP', 'PAR',
                       'Ambient_description', 'Notes'),
  base_unit_CN = c('', '[年月日]', '[时分]', '[°]', '[°]', '[m]', '[0|1]', '', '', '', '', '', '[mV]', '[%]',
                   '[m]', '[hPa]', '[℃]', '[%]', '[m/s]', '[m]', '[℃]', '[℃]', '[m]', '[μg/L]', '[μg/L]', '[ug/L]',
                   '[ug/L]', '[ug/L]', '[mg/L]', '[mg/L]', '[mg/L]', '[mg/L]', '[mg/L]', '[mg/L]', '[mg/L]',
                   '[mg/L]', '[mg/L]', '[m^-1]', '', ''),
  base_unit_EN = c('', '[YYYY-MM-DD]', '[HH:MM:SS]', '[°]', '[°]', '[m]', '[0|1]', '', '', '', '', '', '[mV]',
                   '[%]', '[m]', '[hPa]', '[℃]', '[%]', '[m/s]', '[m]', '[℃]', '[℃]', '[m]', '[μg/L]', '[μg/L]',
                   '[ug/L]', '[ug/L]', '[ug/L]', '[mg/L]', '[mg/L]', '[mg/L]', '[mg/L]', '[mg/L]', '[mg/L]',
                   '[mg/L]', '[mg/L]', '[mg/L]', '[m^-1]', '', ''),
  base_colnames_EN_long = c('', '', '', '', '', '', '', '', '', '', 'Wind Direction', '', '', 'Dissolved Oxygen',
                            '', 'Air Pressure', 'Air Temperature', 'Relative Humidity', 'Wind Speed', '',
                            'Water Body Temperature', 'Water Surface Temperature', 'Secchi Disk Depth',
                            'Chlorophyll a concentration', 'Phycocyanin concentration', 'Total Microcystin concentration',
                            'Extracelluar Microcystin concentration', 'Intracelluar Microcystin concentration',
                            'Particulate Organic Carbon concentration', 'Dissolved Organic Carbon concentration',
                            'Total Suspended Matter concentration', 'Inorganic Suspended Matter concentration',
                            'Organic Suspended Matter concentration', 'Total Nitrogen concentration',
                            'Total Phosphorus concentration', 'Total Dissolved Nitrogen concentration',
                            'Total Dissolved Phosphorus concentration', 'Photosynthetically Active Radiation', '', '')
)

dataset_format_1$sheets_info <- cbind(
  Sheets_required = c('基本参数', '1.Rrs', '2.ap', '3.aph', '4.anap', '5.aCDOM', '6.apc'),
  general_name    = c('base', 'Rrs', 'ap', 'aph', 'anap', 'aCDOM', 'apc')
)

today <- as.character(Sys.Date())
demo_samples <- data.frame(
  `通用编号` = c('1_1', '1_2', '1_3', '2_1', '2_2', '2_3', '4_1', '5_1') ,
  `测量日期[年月日]` = c(today, 'NA', 'NA', today, 'NA', 'NA', today, today) ,
  `测量时间[时分]` = c('10:30:00', '10:30:00', '10:30:00', '11:30:00', '11:30:00', '11:30:00', '13:00:00', '13:30:00') ,
  `经度[°]` = c('120.035', 'NA', 'NA', '120.188', 'NA', 'NA', '119.932', '120.127') ,
  `纬度[°]` = c('31.334', 'NA', 'NA', '31.346', 'NA', 'NA', '31.321', '30.942') ,
  `采水深度[m]` = c('0', '1', '2', '0', '1', '2', '0', '0') ,
  `是否垂向[0|1]` = c('1', '1', '1', '1', '1', '1', '0', '0') ,
  `天气状况` = c('晴', 'NA', 'NA', '晴', 'NA', 'NA', '晴', '晴') ,
  `云量状况` = c('无云', 'NA', 'NA', '无云', 'NA', 'NA', '无云', '无云') ,
  `大气能见度` = c('15km', 'NA', 'NA', '20km', 'NA', 'NA', '15km', '35km') ,
  `风向` = c('东南', 'NA', 'NA', '南', 'NA', 'NA', '东南', '南') ,
  `pH` = c('1', '2', '3', '4', '5', '6', '7', '8') ,
  `Eh[mV]` = c('1', '2', '3', '4', '5', '6', '7', '8') ,
  `DO[%]` = c('1', '2', '3', '4', '5', '6', '7', '8') ,
  `海拔[m]` = c('NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA') ,
  `气压[hPa]` = c('1030', 'NA', 'NA', '1030', 'NA', 'NA', '1030', '1030') ,
  `空气温度[℃]` = c('20', 'NA', 'NA', '20', 'NA', 'NA', '20', '20') ,
  `相对湿度[%]` = c('60', 'NA', 'NA', '60', 'NA', 'NA', '60', '60') ,
  `风速[m/s]` = c('1.5', 'NA', 'NA', '1.5', 'NA', 'NA', '1.5', '1.5') ,
  `水深[m]` = c('5', 'NA', 'NA', '5', 'NA', 'NA', '5', '5') ,
  `水温[℃]` = c('15', 'NA', 'NA', '15', 'NA', 'NA', '15', '15') ,
  `水表温[℃]` = c('14', 'NA', 'NA', '14', 'NA', 'NA', '14', '14') ,
  `透明度[m]` = c('0.5', 'NA', 'NA', '0.5', 'NA', 'NA', '0.5', '0.5') ,
  `叶绿素a[μg/L]` = round(runif(8,0,10),2) ,
  `藻蓝蛋白[μg/L]` = round(runif(8,0,10),2) ,
  `总藻毒素[ug/L]` = round(runif(8,0,10),2) ,
  `胞外藻毒素[ug/L]` = round(runif(8,0,10),2) ,
  `胞内藻毒素[ug/L]` = round(runif(8,0,10),2) ,
  `颗粒有机碳[mg/L]` = round(runif(8,0,10),2) ,
  `溶解有机碳[mg/L]` = round(runif(8,0,10),2) ,
  `总悬浮物[mg/L]` = round(runif(8,0,10),2) ,
  `无机悬浮物[mg/L]` = round(runif(8,0,10),2) ,
  `有机悬浮物[mg/L]` = round(runif(8,0,10),2) ,
  `总氮[mg/L]` = round(runif(8,0,10),2) ,
  `总磷[mg/L]` = round(runif(8,0,10),2) ,
  `溶解总氮[mg/L]` = round(runif(8,0,10),2) ,
  `溶解总磷[mg/L]` = round(runif(8,0,10),2) ,
  `光合有效辐射[m^-1]` = round(runif(8,0,10),2) ,
  `周围情况描述` = c('淡绿色，清澈，无漂浮物', 'NA', 'NA', '灰绿色，少量泡沫漂浮，藻颗粒比较多', 'NA', 'NA', '有水葫芦漂浮', '水色偏灰黄') ,
  `备注` = c('ASD编号', 'NA', 'NA', 'ASD编号', 'NA', 'NA', 'ASD编号', 'ASD编号')
)
colnames(demo_samples) <- dataset_format_1$base_info[,"base_colnames_CN"]
w = which(demo_samples == "NA", arr.ind = T)
demo_samples <- replace(demo_samples, w, NA)
dataset_format_1$demo_samples <- demo_samples
rm(w, today, demo_samples)

save(dataset_format_1, file="./data/dataset_format_1.rda", compression_level = 9)
