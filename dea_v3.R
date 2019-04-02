### Practicum Project: State HealthCare ###
### Data Envenlopment Analysis (version 3) ###
### Xiang Chen ###
### 2018.11.30 ###

setwd("~/Desktop/Practicum")
library('dplyr')
library('rDEA')
df = read.csv("500_Cities__Local_Data_for_Better_Health__2017_release.csv", header = T)
df_d <- df %>% select(Short_Question_Text, MeasureId) %>% distinct
df = select(df, 'GeographicLevel', 'CityName', 'StateAbbr', 'CategoryID', 'MeasureId', 'Data_Value', 'DataValueTypeID', 'PopulationCount')
df_citylevel <- df[which(df$GeographicLevel == 'City' & df$DataValueTypeID == 'AgeAdjPrv'), c('CityName', 'StateAbbr', 'CategoryID', 'MeasureId', 'Data_Value', 'PopulationCount')]
df_citylevel$CategoryID <- as.character(df_citylevel$CategoryID)

df_citylevel$Data_Value <- ifelse(df_citylevel$CategoryID == 'PREVENT', 100 - df_citylevel$Data_Value, df_citylevel$Data_Value)

#output_list <- c('ARTHRITIS', 'BPHIGH', 'CANCER', 'CASTHMA', 'CHD')
in_and_out_pairs <- read.csv("Pairs.csv", header = T)

outcome <- 'COPD'
high_risk_cluster <- c('AL','DE','KY','LA','MD','MI','MS','OH','TN','WV')
input_list <- as.character(in_and_out_pairs[which(in_and_out_pairs$Output == outcome), 'Input'])
total <- data.frame()
for (state in high_risk_cluster){
  
  #output
  output <- df_citylevel[which(df_citylevel$StateAbbr == state & 
                                 df_citylevel$MeasureId == outcome), c('StateAbbr', 'Data_Value')]
  colnames(output) <- c('State', outcome)
  
  citynames <- as.character(unique(df_citylevel[which(df_citylevel$StateAbbr == state),'CityName']))
  dea_data <- cbind(City = citynames, output)
  for (n in seq_along(input_list)){
    input <- df_citylevel[which(df_citylevel$StateAbbr == state &
                                  df_citylevel$MeasureId == input_list[n]), c('Data_Value')]
    dea_data <- cbind(dea_data, input)
    #View(dea_data)
  }
  colnames(dea_data) <- c('City', 'State', outcome, input_list)
  total <- rbind(total, dea_data)
}

Target <- data.frame()
for (state in high_risk_cluster){
  #print(state)
  dea_ready <- total[which(total$State == state),]
  inp_var <- select(dea_ready, input_list)
  out_var <- select(dea_ready, outcome)
  model <- dea(XREF = inp_var, YREF = out_var, X = inp_var[,], Y = out_var[,], model="input", RTS="constant") 
  
  result <- cbind(City = as.character(dea_ready$City), round(model$thetaOpt,4),round(model$lambda,4))
  colnames(result) <- c('City', 'Efficiency', as.character(dea_ready$City))
  result <- as.data.frame(result)
  
  c <- dea_ready[-3] %>% 
    gather(input_list, key = "Measure", value = "Value")
  
  for (measure in input_list){
    #c <- dea_ready[-3] %>% 
    #gather(input_list, key = "Measure", value = "Value")
    d <- c[which(c$Measure == measure), 'Value']
    for (city in dea_ready$City){
      #print(city)
      b <- as.numeric(as.matrix(result[which(result$City == city),-c(1, 2)]))
      effi <- as.numeric(as.matrix(result[which(result$City == city),2]))
      c[which(c$State == state & c$City == city & c$Measure== measure), 'Target'] <- d%*%b
      c[which(c$State == state & c$City == city & c$Measure== measure), 'Efficiency'] <- effi
      #Target <- rbind(Target, c)
    }
    #Target <- rbind(Target, c)
  }
  Target <- rbind(Target, c)
} 
View(Target)
write.csv(Target, "COPD_dea.csv")
