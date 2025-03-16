#Healthcare Coverage Analysis in Mexico Using R

#Installing required packages and loading them

install.packages("rio")
library(ggplot2) #Load the library
library("psych") #Load the package Psych
install.packages("statip")
library(statip)
install.packages("DescTools")
library(DescTools)
install.packages("patchwork")
library(patchwork)
library("rio")
library(dplyr)

#Exporting and filtering the downloaded data base provided by INEGI

#Exporting process
data_set_1 <- import("C:/Users/DELL/Downloads/morbilidad2023/sap.csv", encoding = 'Latin-1')

#Filtering process
data_set_1_2010 <- data_set_1[data_set_1$Periodo == 2010, ]
data_set_1_2015 <- data_set_1[data_set_1$ Periodo==2015,]
data_set_1_2015 <- data_set_1_2015 %>%
  filter(`Entidad Federativa` != "Estados Unidos Mexicanos")
data_set_1_2010 <- data_set_1_2010 %>%
  filter(`Entidad Federativa` != "Estados Unidos Mexicanos")


#Bar plot of the percentage of access to a work healthcare service per state in Mexico

plot1<- ggplot(data_set_1_2010, aes(x = reorder(`Entidad Federativa`, -`Porcentaje de la población afiliada o derechohabiente a algún servicio de salud`), 
                            y = `Porcentaje de la población afiliada o derechohabiente a algún servicio de salud`)) +
  geom_bar(stat = "identity", fill = "lightblue") + 
  labs(
    title = "Porcentaje de población derechohabiente por Estado en 2010",
    x = "Estados",
    y = "Porcentaje de la población afiliada en %"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

#Creating summaries to understand the data behivor

summary(data_set_1_2010$`Porcentaje de la población afiliada o derechohabiente a algún servicio de salud`)
describe(data_set_1_2010$`Porcentaje de la población afiliada o derechohabiente a algún servicio de salud`)
Mode(data_set_1_2010$`Porcentaje de la población afiliada o derechohabiente a algún servicio de salud`)

#Conclusion based on the provided information and using descriptive statistics

#Since the percentage of population who has access to a work healtcare provider has a mean of 68.14, median=69.4.
#The distribution is a normal distribution. Also among all states of Mexico, in average, the 69.4% of population of each state have access to a work healtcare provider.
#A remarkable fact about data is that there is no outliers, meaning no irregularities on the distribution of the data.



#Bar plot of the percentage of access to a work healthcare service per state in Mexico in 2015
plot2 <-ggplot(data_set_1_2015, aes(x = reorder(`Entidad Federativa`, 
                                        -`Porcentaje de la población afiliada o derechohabiente a algún servicio de salud`),  
                            y = `Porcentaje de la población afiliada o derechohabiente a algún servicio de salud`)) + 
  geom_col(fill = "purple", color = "black") + 
  labs(
    title = "Porcentaje de población derechohabiente por Estado en 2015",
    x = "Estados",
    y = "Porcentaje de la población afiliada en %"
  ) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

describe(data_set_1_2015$`Porcentaje de la población afiliada o derechohabiente a algún servicio de salud`)

#Conclusion based on the provided information and using descriptive statistics
#The data from 2015 suggest that the distribution is normal because mean=83.7 and median=84.7 which are small in difference.
#Since the mean is 83.7% it suggest that in average, the 83.7 percent of population arround each individual state have access to a healtacre service.

########################################Comparison between  two periods of time#################################################################################################################
#Join both plots into one single plot for comparison.
plot1|plot2

#Conclusion: From 2010 to 2015 we appreciate a remarkable increase rate in the access to healthcare providers from work, passing from 68.4% in average in 2010 to a 83.7% in 2015.
#both distributions were normal with aubsence of outliers or irregular data.
#In the visualization of both data sets we appreciate the the increment of the percentage of population who has access to a healtcare provider service by their work. It can be saw looking both bar charts and observing that this percentage increases in each state.







