library('dplyr')
library('ggplot2')
library('xtable')

dataframe <- read.csv(file = 'D:/R/CMF//owid-covid-data.csv')
dataframe$date <- as.Date(dataframe$date, '%Y-%m-%d')

ggplot( dataframe[dataframe$location == c( "Russia", 'Germany'),],
       aes( date, new_vaccinations_smoothed_per_million, color = location)) +
geom_line()


hist(dataframe[dataframe$location == c('Russia'),]$ new_vaccinations_smoothed_per_million)


dgs <- data_russia[data_russia$date == volna_5_x,]$total_vaccinations

print(dgs / data_russia$population[1])

shapiro.test( dataframe[dataframe$location == c('European Union', 'United States'),]$new_vaccinations)
bartlett.test(new_vaccinations ~ location,  dataframe[dataframe$location == c('United States', 'Germany'),])



ggplot( dataframe[dataframe$location == c( 'Russia', 'European Union'),],
        aes( date, new_vaccinations_smoothed_per_million, color = location)) +
  geom_line()

boxplot(new_vaccinations_smoothed_per_million ~ location,data = dataframe[dataframe$location == c('European Union', 'Russia' ),] )



fit <- aov(data =dataframe[dataframe$location == c( 'Finland', "Sweden"),], new_vaccinations_smoothed_per_million~location)
summary(fit)
model.tables(fit, 'means')
fit2<- aov( new_cases_smoothed_per_million~location, data = dataframe[dataframe$continent == c( 'Europe', 'Asia'),])
summary(fit2)
model.tables(fit2, 'means')#lesson 2.3 and third video

fit5<- aov(new_cases_smoothed_per_million~location, data =dataframe[dataframe$continent == c( 'Europe'),] )
summary(fit5)
abv <- TukeyHSD(fit5)
abv1<-(abv)
abv_data<-as.data.frame(abv[1]) 
cas_par <- abv_data[abv_data$location.p.adj > 0.05 & abv_data$location.p.adj != 0 & abv_data$location.p.adj != 1.0, ]
cas_table <- xtable(cas_par)
print( cas_table, type = 'html', file = 'D:/R/CMF/Europe_cases.html')

fit6<- aov(new_cases_smoothed_per_million~location, data =dataframe[dataframe$location == c( 
  'United States', 'Russia', 'China', 'India', 'Japan', 'France', 'Germany' , 'Brazil' , 'Indonesia', 'United Kingdom'),] )
summary(fit6)
bbv <- TukeyHSD(fit6)
bbv1<-(bbv)
bbv_data<-as.data.frame(bbv[1]) 
vac_par_1 <- bbv_data[bbv_data$location.p.adj > 0.05 & bbv_data$location.p.adj != 0 & bbv_data$location.p.adj != 1.0, ]
vvp_table <- xtable(vac_par_1)
print( vvp_table, type = 'html', file = 'D:/R/CMF/top_10_cases.html')#lesson 2.3 and the sixth video

fit6<- aov(new_vaccinations_smoothed_per_million~location, data =dataframe[dataframe$location == c( 
  'United States', 'Russia', 'China', 'India', 'Japan', 'France', 'Germany' , 'Brazil' , 'Indonesia', 'United Kingdom'),] )
summary(fit6)
bbv <- TukeyHSD(fit6)
bbv1<-(bbv)
bbv_data<-as.data.frame(bbv[1]) 
vac_par_2 <- bbv_data[bbv_data$location.p.adj < 0.05 & bbv_data$location.p.adj != 0 & bbv_data$location.p.adj != 1.0, ]
vvp_table_2 <- xtable(vac_par_2)
print( vvp_table_2, type = 'html', file = 'D:/R/CMF/top_10_vaccinations.html')

coint = c('United States', 'Russia', 'China', 'India', 'Japan', 'France', 'Germany' , 'Brazil' , 'Indonesia', 'United Kingdom')
for (i in 1:length(coint)){
print(cor.test(x = dataframe[dataframe$location == c('Russia') & dataframe$date > '2021-03-01',]$new_deaths,
         y = dataframe[dataframe$location == coint[i] & dataframe$date > '2021-03-01',]$new_deaths)
)
}

plot(x = dataframe[dataframe$location == c('Italy') & dataframe$date > '2021-03-01',]$new_vaccinations,
     y = dataframe[dataframe$location == c('Italy') & dataframe$date > '2021-03-01',]$new_cases_smoothed)
