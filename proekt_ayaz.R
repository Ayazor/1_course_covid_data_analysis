library('dplyr')
library('ggplot2')
library('xtable')

dataframe <- read.csv(file = 'D:/R/CMF//owid-covid-data.csv')
dataframe$date <- as.Date(dataframe$date, '%Y-%m-%d')

plot(x = dataframe[dataframe$location == c('Russia'),]$date , y = dataframe[dataframe$location == c('Russia'),]$new_cases, 
     type ='l', xlab = 'Время', ylab = 'кол-во случаев', main = "Волны Covid-19")

data_russia <- subset(dataframe, location == 'Russia')

volna_1_y <- max(data_russia$new_cases[0:180])
volna_1_x <- data_russia$date[data_russia$new_cases == volna_1_y]
points(volna_1_x, volna_1_y, col ='red', lwd = 5)

volna_2_y <- max(data_russia$new_cases[180:400])
volna_2_x <- data_russia$date[data_russia$new_cases == volna_2_y]
points(volna_2_x, volna_2_y, col ='red', lwd = 5)

volna_3_y <- max(data_russia$new_cases[400 : 600])
volna_3_x <- data_russia$date[data_russia$new_cases == volna_3_y]
points(volna_3_x, volna_3_y, col ='red', lwd = 5)

volna_4_y <- max(data_russia$new_cases[600:700])
volna_4_x <- data_russia$date[data_russia$new_cases == volna_4_y]
points(volna_4_x, volna_4_y, col ='red', lwd = 5)

volna_5_y <- max(data_russia$new_cases[700:915])
volna_5_x <- data_russia$date[data_russia$new_cases == volna_5_y]
points(volna_5_x, volna_5_y, col ='red', lwd = 5)

text(volna_1_x, volna_1_y+5000, paste(as.character(volna_1_y),'человек'))
text(volna_2_x, volna_2_y+5000, paste(as.character(volna_2_y),'человек'))
text(volna_3_x, volna_3_y+5000, paste(as.character(volna_3_y),'человек'))
text(volna_4_x, volna_4_y+5000, paste(as.character(volna_4_y),'человек'))
text(volna_5_x, volna_5_y+5000, paste(as.character(volna_5_y),'человек'))

text(volna_1_x+10, volna_1_y-5000, volna_1_x)
text(volna_2_x+10, volna_2_y-5000, volna_2_x)
text(volna_3_x+10, volna_3_y-5000, volna_3_x)
text(volna_4_x+10, volna_4_y-5000, volna_4_x)
text(volna_5_x+10, volna_5_y-5000, volna_5_x)



ggplot( dataframe[dataframe$location == c( 'European Union'),],
        aes( date, new_cases_smoothed_per_million, color = location)) +
  geom_line(show.legend = F)


ggplot( dataframe[dataframe$location == c( 'United States'),],
        aes( date, new_cases_smoothed_per_million, color = location)) +
  geom_line(show.legend = F)

ggplot( dataframe[dataframe$location == c( 'Chile'),],
        aes( date, new_cases_smoothed_per_million, color = location)) +
  geom_line(show.legend = F)

ggplot( dataframe[dataframe$location == c( 'China'),],
        aes( date, new_cases_smoothed_per_million, color = location)) +
  geom_line(show.legend = F)


plot(x = data_russia$date[400:650], y = data_russia$new_cases_smoothed_per_million[400:650], type ='l', xlab = 'Время', ylab = 'кол-во случаев',
     main = "Случаи заболеваний(3 волна)")
plot(x = data_russia$date[400:650], y = data_russia$new_vaccinations_smoothed_per_million[400:650], type ='l', xlab = 'Время', ylab = 'кол-во случаев',
     main = "Вакцинированные(3 волна)", col = 'red')

plot(x = data_russia$date[600:700], y = data_russia$new_cases_smoothed_per_million[600:700], type ='l', xlab = 'Время', ylab = 'кол-во случаев',
     main = "Случаи заболеваний(4 волна)")
plot(x = data_russia$date[600:700], y = data_russia$new_vaccinations_smoothed_per_million[600:700], type ='l', xlab = 'Время', ylab = 'кол-во случаев',
     main = "Вакцинированные(4 волна)", col = 'red')

plot(x = data_russia$date[700:915], y = data_russia$new_cases_smoothed_per_million[700:915], type ='l', xlab = 'Время', ylab = 'кол-во случаев',
     main = "Случаи заболеваний(5 волна)")
plot(x = data_russia$date[700:915], y = data_russia$new_vaccinations_smoothed_per_million[700:915], type ='l', xlab = 'Время', ylab = 'кол-во случаев',
     main = "Вакцинированные(5 волна)", col = 'red')


fit5<- aov(new_cases_smoothed_per_million~location, data =dataframe[dataframe$continent == c( 'Europe'),] )
summary(fit5)
abv <- TukeyHSD(fit5)
abv_data<-as.data.frame(abv[1]) 
cases_analysis <- abv_data[abv_data$location.p.adj > 0.05 & abv_data$location.p.adj != 0 & abv_data$location.p.adj != 1.0, ]
#cas_analysis_table <- xtable(cases_analysis)
#print( cas_analysis_table, type = 'html', file = 'D:/R/CMF/Europe_cases.html')

fit6<- aov(new_vaccinations_smoothed_per_million~location, data =dataframe[dataframe$continent == c( 'Europe'),] )
summary(fit6)
abv_2 <- TukeyHSD(fit6)
abv_data_2<-as.data.frame(abv_2[1]) 
vaccinations_analysis <- abv_data_2[abv_data_2$location.p.adj > 0.05 & abv_data_2$location.p.adj != 0 & abv_data_2$location.p.adj != 1.0, ]
#vaccinations_analysis_table <- xtable(vaccinations_analysis )
#print( vaccinations_analysis_table, type = 'html', file = 'D:/R/CMF/Europe_vaccinations.html')



fit7<- aov(new_cases_smoothed_per_million~location, data =dataframe[dataframe$location == c( 
  'United States', 'Russia', 'China', 'India', 'Japan', 'France', 'Germany' , 'Brazil' , 'Indonesia', 'United Kingdom'),] )
summary(fit7)
bbv <- TukeyHSD(fit7)
bbv_data<-as.data.frame(bbv[1]) 
vaccinations_analysis <- bbv_data[bbv_data$location.p.adj > 0.05 & bbv_data$location.p.adj != 0 & bbv_data$location.p.adj != 1.0, ]
#vvp_table <- xtable(vac_par_1)
#print( vvp_table, type = 'html', file = 'D:/R/CMF/top_10_cases.html')

fit8<- aov(new_cases_smoothed_per_million~location, data =dataframe[dataframe$location == c( 
  'United States', 'Russia', 'China', 'India', 'Japan', 'France', 'Germany' , 'Brazil' , 'Indonesia', 'United Kingdom'),] )
summary(fit8)
bbv_2<- TukeyHSD(fit8)
bbv_data_2<-as.data.frame(bbv_2[1]) 
vaccinations_analysis <- bbv_data_2[bbv_data_2$location.p.adj > 0.05 & bbv_data_2$location.p.adj != 0 & bbv_data_2$location.p.adj != 1.0, ]
#vvp_table <- xtable(vac_par_1)
#print( vvp_table, type = 'html', file = 'D:/R/CMF/top_10_cases.html')



coint = c('United States', 'Russia', 'China', 'India', 'Japan', 'France', 'Germany' , 'Brazil' , 'Indonesia', 'United Kingdom')
corr_znach_cases<- list()
for (i in 1:length(coint)){
corr_znach_cases <- list(corr_znach_cases,  cor.test(x = dataframe[dataframe$location == c('Russia') & dataframe$date > '2021-03-01',]$new_cases,
                 y = dataframe[dataframe$location == coint[i] & dataframe$date > '2021-03-01',]$new_cases))
  
} 

corr_znach_vaccinations<- list()
for (i in 1:length(coint)){
  corr_znach_vaccinations <- list(corr_znach_vaccinations,  cor.test(x = dataframe[dataframe$location == c('Russia') & dataframe$date > '2021-03-01',]$new_cases,
                                                       y = dataframe[dataframe$location == coint[i] & dataframe$date > '2021-03-01',]$new_cases))
  
} 

corr_znach_deaths<- list()
for (i in 1:length(coint)){
  corr_znach_deaths <- list(corr_znach_deaths,  cor.test(x = dataframe[dataframe$location == c('Russia') & dataframe$date > '2021-03-01',]$new_cases,
                                                                     y = dataframe[dataframe$location == coint[i] & dataframe$date > '2021-03-01',]$new_cases))
  
} 


ggplot(dataframe[dataframe$location == c('Indonesia') & dataframe$date>'2021-03-01'  ,], aes(new_vaccinations,new_cases))+
  geom_point()+
  geom_smooth()+
  labs(x = 'Новые вакцинации', y = 'Новые заболевания', title = 'Индонезия')

ggplot(dataframe[dataframe$location == c('Russia') & dataframe$date>'2021-03-01'  ,], aes(new_vaccinations,new_cases))+
  geom_point()+
  geom_smooth()+
  labs(x = 'Новые вакцинации', y = 'Новые заболевания', title = 'Россия')


lm2_cases_vaccinations <- list()
for (i in 1:length(coint)){
  lm2 <- lm(new_vaccinations ~ new_cases,dataframe[dataframe$location == coint[i] & dataframe$date>'2021-03-01',])
  lm2_cases_vaccinations <- list(lm2_cases_vaccinations, summary(lm2))
}
lm2_cases_vaccinations 