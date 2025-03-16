install.packages("readxl")
library(readxl)
library(rio)
library(dplyr)
library(ggplot2)
library(psych)

#Registered deaths of men due to malignant prostate tumor in Zacatecas by usual residence of the deceased and five-year age group from 2010 to 2023

###Exporting and filtering the downloaded data base provided by INEGI
#Exporting process and correct formatting of the whole data base.
data <- import("C:/Users/DELL/Downloads/morbilidad2023/Mortalidad_06.csv", encoding = 'UTF-8')
#Filtering data in order to maintain a proper formatting and focusing in the state of Zacatecas
filtered_data<- data[data$V1 == 'Zacatecas', ]
#Labeling the columns of the filtered data to maintain the original format
colnames(filtered_data) <- c('Entidad federativa', 'Grupo de Edad',2010:2023)
#Modifying the values of the columns for percentages for better understanding
filtered_data<- filtered_data %>%
  mutate(across(`2010`:`2023`, ~ . / sum(. [ !(. %in% c(94, 89,103,106,128,102,119,126,110,113,140,137,108,120)) ]) * 100))
#Eliminate rows that do not add useful data into the report and a unnecessary row
filtered_data_2010<- filtered_data %>% filter(`2010`!= 0, `2010`!= 100)
#Plotting Registered deaths of men due to malignant prostate tumor in Zacatecas in 2010

prostate_cancer_2010 <- ggplot(filtered_data_2010, aes(x = `Grupo de Edad`, y = `2010`, fill = `2010`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2010") +
  xlab("Age Group") + 
  ylab("% of the total numer of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

#Analyzing the quantitative variable
describe(filtered_data_2010$`2010`)

#Function to calculate the mode
get_mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}

#Central Tendency measures
#Calculate the mode
mode_prostate_cancer_2010 <- get_mode(filtered_data_2010$`2010`)
mode_prostate_cancer_2010
#Since the mode is 3.1314%, this indicates that across all age groups, the most frequent percentage of deaths due to prostate cancer in 2010 was 3.1314%. In other words, this was the percentage observed most often in the dataset for each age group.
#Calculate the Mean:
#Since the mean is 12.5%, it means that, on average, 12.5% of the total deaths in 2010 in the data set were due to malignant prostate cancer across all age groups.
#Median:
#Since the median is 12.77%, this implies that 50% of the observations fall below 12.77% of deaths. In the data set of 94 registered deaths of men due to malignant prostate tumors in 2010 in Zacatecas, half of the groups had a death percentage lower than 12.77%, while the other half had percentages higher than this value.
#Dispersion measures
#A variance of 68.22% in your data set means that the squared deviations of death percentages from the mean (12.5%) are, on average, 68.22%. This shows a moderate spread in the data.
#The 8.26% SD tells us that there is a moderate variation in the percentage of deaths due to prostate cancer across different groups. The actual percentages for each group will typically fall within a range of about 4.24% to 20.76% in relation to the average percentage of 12.5%. This provides a sense of the diversity in the data for prostate cancer deaths across age groups or regions in Zacatecas.

#Coefficient of variation cv
CV_filtered_data_2010<- (8.26/12.5)*100
CV_filtered_data_2010
#A CV of 66.08% suggests that the data has high relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that the rates of deaths due to prostate cancer are not consistent across age groups or regions.
#A skew=0.15 This means that the death percentages due to prostate cancer has a small tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.
#Test for normality
shapiro.test(filtered_data_2010$`2010`)
#Since the p-value (0.2348) is greater than 0.05, we fail to reject the null hypothesis. This means there is no significant evidence to suggest that the data is not normally distributed. Therefore, we can conclude that the data is approximately normally distributed.
#Thus we can assume that data follows a normal distribution.

####################Analysis of 2011

#Eliminate rows that do not add useful data into the report and a unnecessary row
filtered_data_2011<- filtered_data %>% filter(`2011`!= 0, `2011`!= 100)

#Plotting Registered deaths of men due to malignant prostate tumor in Zacatecas in 2011
ggplot(filtered_data_2011, aes(x = `Grupo de Edad`, y = `2011`, fill = `2011`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#FFCCCB", high = "red") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2011") +
  xlab("Age Group") + 
  ylab("% of the total numer of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

#cental tedency measures.

describe(filtered_data_2011$`2011`)

#Mode
#Calculating the mode
mode_prostate_cancer_2011 <- get_mode(filtered_data_2011$`2011`)
mode_prostate_cancer_2011
#Since mode=15.73 it means that across all age groups the most popular death rate in Zacatecas by malignant prostate cancer in 2011 was 15.73%.
#Mean
#Since the mean is 12.5%, it means that, on average, 12.5% of the total deaths in 2011 in the data set were due to malignant prostate cancer across all age groups.
#Median
#Since the median=12.92 it implies that across the total registered deaths in men due to malignant prostate cancer in 2011 the 50% of observations fall below 12.92%, while the other 50% of deaths fall next to 12.92%
#Dispersion measures
#Variance
#A variance of 12.96% in your data set means that the squared deviations of death percentages from the mean (12.5%) are, on average, 12.92%. This shows a low-moderate spread in the data.
#Standard deviation
#The 3.6% SD tells us that there is a low-moderate variation in the percentage of deaths due to prostate cancer across different groups. The actual percentages for each group will typically fall within a range of about 8.9% to 16.1% in relation to the average percentage of 12.5%. This provides a sense of not too much diversity of measurements of deaths by prostate cancer in 2011.

#Coefficient of variation
CV_filtered_data_2011<- (3.6/12.5)*100
CV_filtered_data_2011
#A CV of 28.8% suggests that the data has low-moderate relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that the rates of deaths due to prostate cancer are more stable in coparison to the 2010 year.
#A skew=0.65 This means that the death percentages due to prostate cancer has a quite big tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2011$`2011`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.

####################Analysis of 2012


#Eliminate rows that do not add useful data into the report and a unnecessary row
filtered_data_2012<- filtered_data %>% filter(`2012`!= 0, `2012`!= 100)

#Plotting Registered deaths of men due to malignant prostate tumor in Zacatecas in 2011
ggplot(filtered_data_2012, aes(x = `Grupo de Edad`, y = `2012`, fill = `2012`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#90EE90", high = "green") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2012") +
  xlab("Age Group") + 
  ylab("% of the total numer of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

#cental tedency measures.

describe(filtered_data_2012$`2012`)

#Mode

#Calculating the mode
mode_prostate_cancer_2012 <- get_mode(filtered_data_2012$`2012`)
mode_prostate_cancer_2012
#Since mode=1.93 it means that across all age groups the most popular death rate in Zacatecas by malignant prostate cancer in 2012 was 1.94%.
#Mean
#Since the mean is 12.5%, it means that, on average, 12.5% of the total deaths in 2012 in the data set were due to malignant prostate cancer across all age groups.
#Median
#Since the median=8.74 it implies that across the total registered deaths in men due to malignant prostate cancer in 2012 the 50% of observations fall below 8.74%, while the other 50% of deaths fall next to 8.74%
#Dispersion measures
#Variance
#A variance of 14.13% in your data set means that the squared deviations of death percentages from the mean (12.5%) are, on average, 12.92%. This shows a low-moderate spread in the data.
#Standard deviation
#The 3.76% SD tells us that there is a low-moderate variation in the percentage of deaths due to prostate cancer across different groups. The actual percentages for each group will typically fall within a range of about 8.74% to 16.26% in relation to the average percentage of 12.5%. This provides a sense of not too much diversity of measurements of deaths by prostate cancer in 2011.

#Coefficient of variation
CV_filtered_data_2012<- (3.76/12.5)*100
CV_filtered_data_2012
#A CV of 30.08% suggests that the data has low-moderate relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that the rates of deaths due to prostate cancer are more stable in coparison to the 2010 year, but quite less stable in comparison to 2011.
#A skew=0.65 This means that the death percentages due to prostate cancer has a quite big tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2012$`2012`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.

#########################Analysis of 2013

#Eliminate rows that do not add useful data into the report and a unnecessary row
filtered_data_2013<- filtered_data %>% filter(`2013`!= 0, `2013`!= 100)

#Plotting Registered deaths of men due to malignant prostate tumor in Zacatecas in 2013
ggplot(filtered_data_2012, aes(x = `Grupo de Edad`, y = `2013`, fill = `2013`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#FFDAB9", high = "orange") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2013") +
  xlab("Age Group") + 
  ylab("% of the total numer of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

#cental tedency measures.

describe(filtered_data_2013$`2013`)

#Mode

#Calculating the mode
mode_prostate_cancer_2013 <- get_mode(filtered_data_2013$`2013`)
mode_prostate_cancer_2013
#Since mode=1.88 it means that across all age groups the most popular death rate in Zacatecas by malignant prostate cancer in 2013 was 1.94%.
#Mean
#Since the mean is 12.5%, it means that, on average, 12.5% of the total deaths in 2013 in the data set were due to malignant prostate cancer across all age groups.
#Median
#Since the median=11.79 it implies that across the total registered deaths in men due to malignant prostate cancer in 2012 the 50% of observations fall below 11.79%, while the other 50% of deaths fall next to 11.79%
#Dispersion measures
#Variance
#A variance of 11.15% in your data set means that the squared deviations of death percentages from the mean (12.5%) are, on average, 11.15%. This shows a low-moderate spread in the data.
#Standard deviation
#The 3.34% SD tells us that there is a low-moderate variation in the percentage of deaths due to prostate cancer across different groups. The actual percentages for each group will typically fall within a range of about 9.16% to 15.84% in relation to the average percentage of 12.5%. This provides a sense of not too much diversity of measurements of deaths by prostate cancer in 2013.

#Coefficient of variation
CV_filtered_data_2013<- (3.34/12.5)*100
CV_filtered_data_2013
#A CV of 26.72% suggests that the data has low-moderate relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that the rates of deaths due to prostate cancer are more stable in coparison to previous years.
#A skew=0.22 This means that the death percentages due to prostate cancer has a small tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2013$`2013`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.

###############################Analysis 2014


#Eliminate rows that do not add useful data into the report and a unnecessary row
filtered_data_2014<- filtered_data %>% filter(`2014`!= 0, `2014`!= 100, )
#Eliminate non specified group of age
filtered_data_2014 <- filtered_data_2014 %>% slice(-9)

#Plotting Registered deaths of men due to malignant prostate tumor in Zacatecas in 2014
ggplot(filtered_data_2014, aes(x = `Grupo de Edad`, y = `2014`, fill = `2014`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#FFB6C1", high = "#FF69B4") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2014") +
  xlab("Age Group") + 
  ylab("% of the total numer of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

#cental tedency measures.

describe(filtered_data_2014$`2014`)

#Mode

#Calculating the mode
mode_prostate_cancer_2014 <- get_mode(filtered_data_2014$`2014`)
mode_prostate_cancer_2014
#Since mode=5.46% it means that across all age groups the most popular death rate in Zacatecas by malignant prostate cancer in 2014 was 5.46%.
#Mean
#Since the mean is 12.4%, it means that, on average, 12.4% of the total deaths in 2013 in the data set were due to malignant prostate cancer across all age groups.
#Median
#Since the median=8.59 it implies that across the total registered deaths in men due to malignant prostate cancer in 2014 the 50% of observations fall below 8.59%, while the other 50% of deaths fall next to 8.59%
#Dispersion measures
#Variance
#A variance of 15.36% in your data set means that the squared deviations of death percentages from the mean (12.4%) are, on average, 15.36%. This shows a low-moderate spread in the data.
#Standard deviation
#The 3.92% SD tells us that there is a low-moderate variation in the percentage of deaths due to prostate cancer across different groups. The actual percentages for each group will typically fall within a range of about 8.48% to 16.32% in relation to the average percentage of 12.4%. This provides a sense of not too much diversity of measurements of deaths by prostate cancer in 2014.

#Coefficient of variation
CV_filtered_data_2014<- (3.92/12.4)*100
CV_filtered_data_2014
#A CV of 31.62% suggests that the data has low-moderate relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that the rates of deaths due to prostate cancer are more stable in coparison to previous years.
#A skew=0.7 This means that the death percentages due to prostate cancer has a small tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2014$`2014`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.

############################################Analysis of 2015


#Eliminate rows that do not add useful data into the report and a unnecessary row
filtered_data_2015<- filtered_data %>% filter(`2015`!= 0, `2015`!= 100, )

#Plotting Registered deaths of men due to malignant prostate tumor in Zacatecas in 2015
ggplot(filtered_data_2015, aes(x = `Grupo de Edad`, y = `2015`, fill = `2015`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#D8BFD8", high = "#DDA0DD") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2015") +
  xlab("Age Group") + 
  ylab("% of the total numer of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

#cental tedency measures.

describe(filtered_data_2015$`2015`)

#Mode

#Calculating the mode
mode_prostate_cancer_2015 <- get_mode(filtered_data_2015$`2015`)
mode_prostate_cancer_2015
#Since mode=20.58% it means that across all age groups the most popular death rate in Zacatecas by malignant prostate cancer in 2014 was 29.58%.
#Mean
#Since the mean is 14.29%, it means that, on average, 14.29% of the total deaths in 2015 in the data set were due to malignant prostate cancer across all age groups.
#Median
#Since the median=11.76 it implies that across the total registered deaths in men due to malignant prostate cancer in 2015 the 50% of observations fall below 11.76%, while the other 50% of deaths fall next to 11.76%
#Dispersion measures
#Variance
#A variance of 14.82% in your data set means that the squared deviations of death percentages from the mean (12.4%) are, on average, 15.36%. This shows a low-moderate spread in the data.
#Standard deviation
#The 3.85% SD tells us that there is a low-moderate variation in the percentage of deaths due to prostate cancer across different groups. The actual percentages for each group will typically fall within a range of about 10.97% to 18.85% in relation to the average percentage of 14.82%. This provides a sense of not too much diversity of measurements of deaths by prostate cancer in 2014.

#Coefficient of variation
CV_filtered_data_2015<- (3.85/14.82)*100
CV_filtered_data_2015
#A CV of 25.97% suggests that the data has moderate relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that the rates of deaths due to prostate cancer are more stable in comparison to 2014.
#A skew=0.19 This means that the death percentages due to prostate cancer has a moderate tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2015$`2015`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.


##################################Analysis of 2016

# Eliminate rows that do not add useful data into the report and unnecessary rows
filtered_data_2016 <- filtered_data %>% filter(`2016` != 0, `2016` != 100)

# Plotting Registered Deaths of Men due to Malignant Prostate Tumor in Zacatecas in 2016
ggplot(filtered_data_2016, aes(x = `Grupo de Edad`, y = `2016`, fill = `2016`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#C1E1C1", high = "#98FB98") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2016") +
  xlab("Age Group") + 
  ylab("% of the total number of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

# Central Tendency Measures

describe(filtered_data_2016$`2016`)

# Mode
mode_prostate_cancer_2016 <- get_mode(filtered_data_2016$`2016`)
mode_prostate_cancer_2016

#Since the mode= 1.68, it represents the most common death percentage due to malignant prostate cancer in Zacatecas for 2016 

# Mean
# Since the mean=14.29% then the average proportion of deaths due to prostate cancer across all age groups in 2016 was 14.29%.

##Median
#Since the median=15.97 it implies that across the total registered deaths in men due to malignant prostate cancer in 2016 the 50% of observations fall below 15.97%, while the other 50% of deaths fall next to 15.97%.

#Dispersion Measures
#Variance
#A variance of 14.59% in your data set means that the squared deviations of death percentages from the mean (14.29%) are, on average, 15.36%. This shows a low-moderate spread in the data.
#Standard deviation
#The 3.82% SD tells us that there is a low-moderate variation in the percentage of deaths due to prostate cancer across different groups. The actual percentages for each group will typically fall within a range of about 10.47% to 18.11% in relation to the average percentage of 12.4%. This provides a sense of not too much diversity of measurements of deaths by prostate cancer in 2016.

#Coefficient of variation
CV_filtered_data_2016<- (3.82/14.29)*100
CV_filtered_data_2016
#A CV of 26.73% suggests that the data has low-moderate relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that the rates of deaths due to prostate cancer are slightly less stable in comparison to 2015.
#A skew=-0.16 This means that the death percentages due to prostate cancer has a smallleft tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2016$`2016`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.


#################################Analysis of 2017


# Eliminate rows that do not add useful data into the report and unnecessary rows
filtered_data_2017 <- filtered_data %>% filter(`2017` != 0, `2017` != 100)

# Plotting Registered Deaths of Men due to Malignant Prostate Tumor in Zacatecas in 2017
ggplot(filtered_data_2017, aes(x = `Grupo de Edad`, y = `2017`, fill = `2017`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#D500A3", high = "#FF1493") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2017") +
  xlab("Age Group") + 
  ylab("% of the total number of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

# Central Tendency Measures

describe(filtered_data_2017$`2017`)

# Mode
mode_prostate_cancer_2017 <- get_mode(filtered_data_2017$`2017`)
mode_prostate_cancer_2017
#Since the mode= 0.79, it represents the most common death percentage due to malignant prostate cancer in Zacatecas for 2017 

# Mean
# Since the mean=12.5% then the average proportion of deaths due to prostate cancer across all age groups in 2017 was 12.5%.

##Median
#Since the median=11.9% it implies that across the total registered deaths in men due to malignant prostate cancer in 2017 the 50% of observations fall below 11.9%, while the other 50% of deaths fall next to 11.9%.

#Dispersion Measures
#Standard deviation
#The 10.9% SD tells us that there is a quite high variation in the percentage of deaths due to prostate cancer across different age groups. The actual percentages for each group will typically fall within a range of about 1.6% to 23.4% in relation to the average percentage of 12.5%. This provides a sense of much diversity of measurements of deaths by prostate cancer in 2017.

#Coefficient of variation
CV_filtered_data_2017<- (10.9/12.5)*100
CV_filtered_data_2017
#A CV of 87.2% suggests that the data has high relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that data is highly dispersed.
#A skew=0.41 This means that the death percentages due to prostate cancer has a small right tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2017$`2017`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.

##################################Analysis 2018


# Eliminate rows that do not add useful data into the report and unnecessary rows
filtered_data_2018 <- filtered_data %>% filter(`2018` != 0, `2018` != 100)

# Plotting Registered Deaths of Men due to Malignant Prostate Tumor in Zacatecas in 2018
ggplot(filtered_data_2018, aes(x = `Grupo de Edad`, y = `2018`, fill = `2018`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#FEBAAD", high = "#FFD9C0") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2018") +
  xlab("Age Group") + 
  ylab("% of the total number of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

# Central Tendency Measures

describe(filtered_data_2018$`2018`)

# Mode
mode_prostate_cancer_2018 <- get_mode(filtered_data_2018$`2018`)
mode_prostate_cancer_2018
#Since the mode= 25.45%, it represents the most common death percentage due to malignant prostate cancer in Zacatecas for 2018 

# Mean
# Since the mean=12.5% then the average proportion of deaths due to prostate cancer across all age groups in 2018 was 12.5%.

##Median
#Since the median=10.91% it implies that across the total registered deaths in men due to malignant prostate cancer in 2017 the 50% of observations fall below 10.91%, while the other 50% of deaths fall next to 10.91%.

#Dispersion Measures
#Standard deviation
#The 10.26% SD tells us that there is a quite high variation in the percentage of deaths due to prostate cancer across different age groups. The actual percentages for each group will typically fall within a range of about 2.24% to 22.76% in relation to the average percentage of 12.5%. This provides a sense of much diversity of measurements of deaths by prostate cancer in 2017.

#Coefficient of variation
CV_filtered_data_2018<- (10.26/12.5)*100
CV_filtered_data_2018
#A CV of 82.08% suggests that the data has high relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that data is highly dispersed.
#A skew=0.16 This means that the death percentages due to prostate cancer has a small right tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2018$`2018`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.

#The data does not significantly deviate from a normal distribution, based on the Shapiro-Wilk test.

###############################Analysis of 2019


# Eliminate rows that do not add useful data into the report and unnecessary rows
filtered_data_2019 <- filtered_data %>% filter(`2019` != 0, `2019` != 100)

# Plotting Registered Deaths of Men due to Malignant Prostate Tumor in Zacatecas in 2019
ggplot(filtered_data_2019, aes(x = `Grupo de Edad`, y = `2019`, fill = `2019`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#76EEC6", high = "#008080") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2019") +
  xlab("Age Group") + 
  ylab("% of the total number of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

# Central Tendency Measures

describe(filtered_data_2019$`2019`)

# Mode
mode_prostate_cancer_2019 <- get_mode(filtered_data_2018$`2019`)
mode_prostate_cancer_2019
#Since the mode= 1.76%, it represents the most common death percentage due to malignant prostate cancer in Zacatecas for 2019 

# Mean
# Since the mean=11.11% then the average proportion of deaths due to prostate cancer across all age groups in 2019 was 11.11%.

##Median
#Since the median=7.96% it implies that across the total registered deaths in men due to malignant prostate cancer in 2019 the 50% of observations fall below 7.96%, while the other 50% of deaths fall next to 7.96%.

#Dispersion Measures
#Standard deviation
#The 10.29% SD tells us that there is a quite high variation in the percentage of deaths due to prostate cancer across different age groups. The actual percentages for each group will typically fall within a range of about 0.82% to 21.4% in relation to the average percentage of 11.11%. This provides a sense of much diversity of measurements of deaths by prostate cancer in 2019.

#Coefficient of variation
CV_filtered_data_2019<- (10.29/11.11)*100
CV_filtered_data_2019
#A CV of 92.61% suggests that the data has high relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that data is highly dispersed.
#A skew=0.52 This means that the death percentages due to prostate cancer has a small right tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2019$`2019`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.

#The data does not significantly deviate from a normal distribution, based on the Shapiro-Wilk test.

##############################Analysis of 2020

# Eliminate rows that do not add useful data into the report and unnecessary rows
filtered_data_2020 <- filtered_data %>% filter(`2020` != 0, `2020` != 100)

# Plotting Registered Deaths of Men due to Malignant Prostate Tumor in Zacatecas in 2019
ggplot(filtered_data_2020, aes(x = `Grupo de Edad`, y = `2020`, fill = `2020`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#A75D67", high = "#800020") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2020") +
  xlab("Age Group") + 
  ylab("% of the total number of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

# Central Tendency Measures

describe(filtered_data_2020$`2020`)

# Mode
mode_prostate_cancer_2020 <- get_mode(filtered_data_2020$`2020`)
mode_prostate_cancer_2020
#Since the mode= 12.14%, it represents the most common death percentage due to malignant prostate cancer in Zacatecas for 2020 

# Mean
# Since the mean=12.5% then the average proportion of deaths due to prostate cancer across all age groups in 2020 was 12.5%.

##Median
#Since the median=12.14% it implies that across the total registered deaths in men due to malignant prostate cancer in 2020 the 50% of observations fall below 12.4%, while the other 50% of deaths fall next to 12.14%.

#Dispersion Measures
#Standard deviation
#The 10.37% SD tells us that there is a quite high variation in the percentage of deaths due to prostate cancer across different age groups. The actual percentages for each group will typically fall within a range of about 2.13% to 22.87% in relation to the average percentage of 12.5%. This provides a sense of much diversity of measurements of deaths by prostate cancer in 2020.

#Coefficient of variation
CV_filtered_data_2020<- (10.37/12.5)*100
CV_filtered_data_2020
#A CV of 82.96% suggests that the data has high relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that data is highly dispersed.
#A skew=0.47 This means that the death percentages due to prostate cancer has a small right tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2020$`2020`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.

#The data does not significantly deviate from a normal distribution, based on the Shapiro-Wilk test.

###########################Analysis of 2021

# Eliminate rows that do not add useful data into the report and unnecessary rows
filtered_data_2021 <- filtered_data %>% filter(`2021` != 0, `2021` != 100)

# Plotting Registered Deaths of Men due to Malignant Prostate Tumor in Zacatecas in 2019
ggplot(filtered_data_2021, aes(x = `Grupo de Edad`, y = `2021`, fill = `2021`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#C69C72", high = "#5C4033") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2021") +
  xlab("Age Group") + 
  ylab("% of the total number of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

# Central Tendency Measures
describe(filtered_data_2021$`2021`)

# Mode
mode_prostate_cancer_2021 <- get_mode(filtered_data_2021$`2021`)
mode_prostate_cancer_2021
#Since the mode= 0.72%, it represents the most common death percentage due to malignant prostate cancer in Zacatecas for 2021 

# Mean
# Since the mean=12.5% then the average proportion of deaths due to prostate cancer across all age groups in 2021 was 12.5%.

#Median
#Since the median=10.58% it implies that across the total registered deaths in men due to malignant prostate cancer in 2021 the 50% of observations fall below 10.58%, while the other 50% of deaths fall next to 10.58%.

#Dispersion Measures
#Standard deviation
#The 10.96% SD tells us that there is a quite high variation in the percentage of deaths due to prostate cancer across different age groups. The actual percentages for each group will typically fall within a range of about 1.54% to 23.46% in relation to the average percentage of 12.5%. This provides a sense of much diversity of measurements of deaths by prostate cancer in 2021.

#Coefficient of variation
CV_filtered_data_2021<- (10.96/12.5)*100
CV_filtered_data_2021
#A CV of 87.68% suggests that the data has high relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that data is highly dispersed.
#A skew=0.48 This means that the death percentages due to prostate cancer has a small right tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2021$`2021`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.

#The data does not significantly deviate from a normal distribution, based on the Shapiro-Wilk test.

###################Analysis 2022


# Eliminate rows that do not add useful data into the report and unnecessary rows
filtered_data_2022 <- filtered_data %>% filter(`2022` != 0, `2022` != 100)

# Plotting Registered Deaths of Men due to Malignant Prostate Tumor in Zacatecas in 2019
ggplot(filtered_data_2022, aes(x = `Grupo de Edad`, y = `2022`, fill = `2022`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#FFF9C4", high = "#FFD700") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2022") +
  xlab("Age Group") + 
  ylab("% of the total number of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

# Central Tendency Measures
describe(filtered_data_2022$`2022`)

# Mode
mode_prostate_cancer_2022 <- get_mode(filtered_data_2022$`2022`)
mode_prostate_cancer_2022
#Since the mode= 6.48%, it represents the most common death percentage due to malignant prostate cancer in Zacatecas for 2022 

# Mean
# Since the mean=12.5% then the average proportion of deaths due to prostate cancer across all age groups in 2022 was 12.5%.

#Median
#Since the median=8.8% it implies that across the total registered deaths in men due to malignant prostate cancer in 2022 the 50% of observations fall below 8.8%, while the other 50% of deaths fall next to 8.8%.

#Dispersion Measures
#Standard deviation
#The 11.35% SD tells us that there is a quite high variation in the percentage of deaths due to prostate cancer across different age groups. The actual percentages for each group will typically fall within a range of about 1.15% to 23.85% in relation to the average percentage of 12.5%. This provides a sense of much diversity of measurements of deaths by prostate cancer in 2022.

#Coefficient of variation
CV_filtered_data_2022<- (11.35/12.5)*100
CV_filtered_data_2022
#A CV of 90.68% suggests that the data has high relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that data is highly dispersed.
#A skew=0.93 This means that the death percentages due to prostate cancer has a quite big right tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2022$`2022`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.

#The data does not significantly deviate from a normal distribution, based on the Shapiro-Wilk test.


################################Analysis 2023


# Eliminate rows that do not add useful data into the report and unnecessary rows
filtered_data_2023 <- filtered_data %>% filter(`2023` != 0, `2023` != 100)

# Plotting Registered Deaths of Men due to Malignant Prostate Tumor in Zacatecas in 2023
ggplot(filtered_data_2023, aes(x = `Grupo de Edad`, y = `2023`, fill = `2023`)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Black border for elegance
  scale_fill_gradient(low = "#D3D3D3", high = "#4F4F4F") +  # Gradient color
  ggtitle("Registered Deaths of Men Due to Malignant Prostate Tumor in 2023") +
  xlab("Age Group") + 
  ylab("% of the total number of deaths by prostate cancer") +
  theme_minimal(base_size = 10) +  # Elegant, clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centered bold title
    legend.position = "none"  # Hide legend if unnecessary
  )

# Central Tendency Measures
describe(filtered_data_2023$`2023`)

# Mode
mode_prostate_cancer_2023 <- get_mode(filtered_data_2023$`2023`)
mode_prostate_cancer_2023
#Since the mode= 1.66%, it represents the most common death percentage due to malignant prostate cancer in Zacatecas for 2023 

# Mean
# Since the mean=12.5% then the average proportion of deaths due to prostate cancer across all age groups in 2023 was 12.5%.

#Median
#Since the median=10.83% it implies that across the total registered deaths in men due to malignant prostate cancer in 2023 the 50% of observations fall below 10.83%, while the other 50% of deaths fall next to 10.83%.

#Dispersion Measures
#Standard deviation
#The 10.04% SD tells us that there is a quite high variation in the percentage of deaths due to prostate cancer across different age groups. The actual percentages for each group will typically fall within a range of about 2.46% to 22.54% in relation to the average percentage of 12.5%. This provides a sense of much diversity of measurements of deaths by prostate cancer in 2023.

#Coefficient of variation
CV_filtered_data_2023<- (10.04/12.5)*100
CV_filtered_data_2023
#A CV of 80.32% suggests that the data has high relative variability in the percentages of prostate cancer deaths across different groups in Zacatecas. This highlights that data is highly dispersed.
#A skew=0.32 This means that the death percentages due to prostate cancer has a quite small right tail of higher values. There are still some regions or age groups that have slightly higher percentages of deaths, but the difference is not substantial.

#Test for normality
shapiro.test(filtered_data_2023$`2023`)
#p-value > 0.05 indicates that your data does not significantly deviate from a normal distribution.
#Therefore, you fail to reject the null hypothesis, suggesting that data can be assumed to be normally distributed.

#The data does not significantly deviate from a normal distribution, based on the Shapiro-Wilk test.





# Data: Mean mortality percentage per year
years <- 2010:2023
mean_mortality <- c(12.5, 12.5, 12.5, 12.5, 12.4, 14.29, 14.29, 12.5, 11.11, 12.5, 
                    12.5, 12.5, 12.5, 12.5)

# Create a data frame
mortality_data <- data.frame(Year = years, Mortality = mean_mortality)

# Plot the trend line
ggplot(mortality_data, aes(x = Year, y = Mortality)) +
  geom_line(color = "blue", size = 1) +  # Line plot
  geom_point(color = "red", size = 2) +  # Add points for each year
  labs(title = "Prostate Cancer Mortality Trend in Zacatecas (2010-2023)",
       x = "Year",
       y = "Mean Mortality Percentage") +
  theme_minimal()




































































