#("D:/R/CMF/")
install.packages("Hmisc")
library(Hmisc)

dataframe <- read.csv(file = 'D:/R/CMF//owid-covid-data.csv')
dataframe$date <- as.Date(dataframe$date, '%Y-%m-%d')

data_russia <- subset(dataframe, location == 'Russia')

plot(x = data_russia$date, y = data_russia$new_vaccinations_smoothed_per_million, type ='l', xlab = 'Время', ylab = 'кол-во случаев',
     main = "Волны Covid-19")
lines(x = data_russia$date, y = data_russia$new_cases_per_million, type ='l', xlab = 'Время', ylab = 'кол-во случаев',
     main = "Волны Covid-19", col = 'green')
legend("topleft", legend = c("вакцинированные","заболевшие"),
       lwd = 3, col = c( "black","green"), bty = "n")

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


hist(x = data_russia$date[400:600], data_russia$new_cases_smoothed_per_million[400:600])
hist(data_russia$new_vaccinations_smoothed_per_million[400:600])




paste(as.character(volna_1_y) , 'dsb')

data_russia$total_cases[data_russia$date == volna_3_x]

data_russia$total_vaccinations[data_russia$date == volna_3_x-80]
data_russia$total_vaccinations[data_russia$date == volna_3_x]
data_russia$total_vaccinations[data_russia$date == volna_3_x+80]




lines(x = data_russia$date, y = data_russia$positive_rate, lwd = 5, type = 'l')
plot(x = data_russia$date, y = data_russia$tests_per_case, lwd = 2, col = 'red', type = 'l')#opredelili volny covid

plot(x = data_russia$date, y = data_russia$new_cases_per_million, col ='green', type ='l')



data_germany <- subset(dataframe, location == 'European union')
plot(x = data_germany$date, y = data_germany$new_cases_per_smoothed_per_million, type ='l', xlab = 'Время', ylab = 'кол-во случаев',
     main = "Германия" , xlim = '2022-11-30' , ylim = 100000)


ggplot( dataframe[dataframe$location == c( 'China'),],
        aes( date, new_cases_smoothed_per_million, color = location)) +
  geom_line(show.legend = T)

data_japan <- subset(dataframe, location == 'Japan')
data_japan$date <- as.Date(data_japan$date, '%Y-%m-%d')
plot(x = data_japan$date, y = data_japan$new_cases, type = 'l', col = 'red')


data_europe <- subset(dataframe, continent == 'Europe')
data_europe$date <- as.Date(data_europe$date, '%Y-%m-%d')
plot(x = data_europe$date, y = data_europe$total_vaccinations_per_hundred, type = 'l')


data_asia <- subset(dataframe, continent == 'Asia')
data_asia$date <- as.Date(data_asia$date, '%Y-%m-%d')
lines(x = data_asia$date, y = data_asia$total_vaccinations_per_hundred, col = 'red')

q<-aggregate(people_fully_vaccinated~location+population+gdp_per_capita,dataframe,max,na.rm=T)
q$otnos<-(q$people_fully_vaccinated/q$population)*100


ch <- dataframe[dataframe$location == c( "China") & dataframe$date > '2021-03-23',]




git <- lm(new_vaccinations~new_cases ,dataframe[dataframe$location == c( "Russia") & dataframe$date>'2021-02-01',])
summary(git)


ggplot(dataframe[dataframe$location == c( 'Indonesia') & dataframe$date>'2021-03-01' ,], aes( new_vaccinations,new_cases))+
  geom_point()+
  geom_smooth()

print(cor.test(x = dataframe[dataframe$location == c('Indonesia') & dataframe$date > '2021-03-01',]$new_cases,
               y = dataframe[dataframe$location == 'Indonesia' & dataframe$date > '2021-03-01',]$new_vaccinations))


    
hist(log(dataframe[dataframe$location == c( "Russia") & dataframe$date > '2021-03-01',]$new_vaccinations))
hist(log(dataframe[dataframe$location == c( 'Russia') & dataframe$date>'2021-06-09' & dataframe$date<'2021-08-31' ,]$new_cases))









ggplot(dataframe[dataframe$location == c('Russia') & dataframe$date>'2021-03-01'  ,], aes(new_vaccinations,new_cases))+
  geom_point()+
  geom_smooth()+
  labs(x = 'Новые вакцинации', y = 'Новые заболевания', title = 'Россия')

dataframe$cases_squared <- (dataframe$new_cases)^2
dataframe$vaccinations_squared <- (dataframe$new_vaccinations)^2

lm2_cases_vaccinations <- list()
for (i in 1:length(coint)){
lm2 <- lm(new_vaccinations ~ new_cases,dataframe[dataframe$location == coint[i] & dataframe$date>'2021-03-01',])
lm2_cases_vaccinations <- list(lm2_cases_vaccinations, summary(lm2))
}
lm2_cases_vaccinations 


