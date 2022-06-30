rm(list=ls())

# Load the Data
Data_Day = read.csv('Dataset/day_data.csv', header = T )

# Exploratory Data Analysis
class(Data_Day)
dim(Data_Day)
head(Data_Day,10)
names(Data_Day)
str(Data_Day)
summary(Data_Day)


# Dropping few columns since they are not important for our analysis
Data_Day = subset(Data_Day, select = -c(instant, dteday))
dim(Data_Day)
names(Data_Day)


# Separate Numeric and Categorical variables
numeric_var = c('temp', 'atemp', 'hum', 'windspeed', 'cnt')
categorical_var = c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday', 'weathersit')


# Missing Value Analysis
summary(is.na(Data_Day))
sum(is.na(Data_Day))


# Outlier Analysis
df = Data_Day
Data_Day = df

library(ggplot2)

# BoxPlots - Outlier Check

for (i in 1:length(numeric_var))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (numeric_var[i]), x = "cnt"), data = subset(Data_Day))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=numeric_var[i],x="count")+
           ggtitle(paste("Box plot of count for",numeric_var[i])))
}

# Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5, ncol=2)

# Replacing outliers with NA

for(i in numeric_var){
  print(i)
  outlier = Data_Day[,i][Data_Day[,i] %in% boxplot.stats(Data_Day[,i])$out]
  print(length(outlier))
  Data_Day[,i][Data_Day[,i] %in% outlier] = NA
}
sum(is.na(Data_Day))


# Data Understanding

# Barplot with x axis as season and y axis as count
ggplot(Data_Day, aes(x = Data_Day$season, y = Data_Day$cnt))+
  geom_bar(stat = "identity", fill = "blue")+labs(title = "Number of bikes rented with respect to season", x = "Seasons", y = "cnt")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))


# Barplot with x axis as year and y axis as count
ggplot(Data_Day, aes(x = Data_Day$yr, y = Data_Day$cnt))+
  geom_bar(stat = "identity", fill = "red")+labs(title = "Number of bikes rented with respect to year", x = "yr", y = "cnt")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))


# Barplot with x axis as weekday and y axis as count
ggplot(Data_Day, aes(x = Data_Day$weekday, y = Data_Day$cnt))+
  geom_bar(stat = "identity", fill = "navyblue")+labs(title = "Number of bikes rented with respect to days", x = "Days of the week", y = "count")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))


#Count with respect to temperature and humidity together
ggplot(Data_Day,aes(temp,cnt)) + 
  geom_point(aes(color=hum),alpha=0.5) +labs(title = "Bikes count vs temperature and humidity", x = "Normalized temperature", y = "Count")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()


# Count with respect to windspeed and weather together
ggplot(Data_Day, aes(x = windspeed, y = cnt))+
  geom_point(aes(color= weathersit ), alpha=0.5) +labs(title = "Bikes count vs windspeed and weather", x = "Windspeed", y = "Count")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()


# Count with respect to temperature and season together
ggplot(Data_Day, aes(x = temp, y = cnt))+
  geom_point(aes(color=season),alpha=0.5) +labs(title = "Bikes count vs temperature and season", x = "Normalized temperature", y = "Count")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()



# Feature Selection

df2 = Data_Day
Data_Day = df2

# Correlation Analysis and Anova test is done.

# Correlation Analysis for Numeric Variable

library(corrgram)

corrgram(Data_Day[,numeric_var],order=FALSE,upper.panel = panel.pie,
         text.panel = panel.txt,
         main= "Correlation Analysis between Numeric Variables")


# Anova Test for Categorical Variables

for(i in categorical_var){
  print(i)
  Anova_test_result = summary(aov(formula = cnt~Data_Day[,i],Data_Day))
  print(Anova_test_result)
}


# Dimension redusction , removing variables that ar not required

Data_Day = subset(Data_Day, select=-c(atemp,holiday,weekday,workingday))



# Feature Scaling


numeric_var = c("temp","hum","windspeed","cnt")
catergorical_var = c("season", "yr", "mnth", "weathersit")

library(propagate)

# Identify range and check min max of the variables to check noramility

for(i in numeric_var){
  print(summary(Data_Day[,i]))
}


# Visualizing Normality Check 

hist(Data_Day$temp, col="Navyblue", xlab="Temperature", ylab="Frequency",
     main="Temperature Distribution")

hist(Data_Day$hum, col="Blue", xlab="Humidity", ylab="Frequency",
     main="Humidity Distribution")

hist(Data_Day$windspeed,col="Dark green",xlab="Windspeed",ylab="Frequency",
     main="Windspeed Distribution")

# Quantile - Quantile Normal Plot
qqnorm(Data_Day$cnt)

# Quantile - Quantile Line
qqline(Data_Day$cnt)


# Split the dataset into Train and Test Dataset

# Load some libraries
library(purrr)
library(dplyr)

train_data<-filter(Data_Day, yr==0)
dim(train_data)

test_data<- filter(Data_Day, yr==1)
dim(test_data)

# Read the train and test data
head(train_data,5)
head(test_data,5)

# create a new subset for train categorical attributes
train_cat_attributes<-subset(train_data,select=c('season','weathersit','yr'))

# create a new subset for test categorical attributes
test_cat_attributes<-subset(test_data,select=c('season','weathersit','yr'))

# create a new subset for train numerical attributes
train_num_attributes<-subset(train_data,select=c('mnth','temp','hum','windspeed','cnt'))

# create a new subset for test numerical attributes
test_num_attributes<-subset(test_data,select=c('mnth','temp', 'hum','windspeed','cnt'))

# Load the caret library
library(caret)


# Train_encoded_attributes

# other variables along with target variable to get dummy variables
othervars<-c('mnth','temp','hum','windspeed','cnt')
set.seed(1818)

# Categorical variables
vars<-setdiff(colnames(train),c(train$cnt,othervars))

# Formula pass through encoder to get dummy variables
f <- paste('~', paste(vars, collapse = ' + '))

# Encoder is encoded the categorical variables to numeric
encoder<-dummyVars(as.formula(f), train)

# Predicting the encode attributes
encode_attributes<-predict(encoder,train)

# Binding the train_num_attributes and encode_attributes
train_encoded_attributes<-cbind(train_num_attributes,encode_attributes)
head(train_encoded_attributes,5)



# Test_encoded_attributes

set.seed(8181)

# Categorical variables
vars<-setdiff(colnames(test_cat_attributes),c(test_cat_attributes$cnt,othervars))

# Formula pass through encoder to get dummy variables
f<- paste('~',paste(vars,collapse='+'))

# Encoder is encoded the categorical variables to numeric
encoder<-dummyVars(as.formula(f),test_cat_attributes)

# Predicting the encoder attributes
encode_attributes<-predict(encoder,test_cat_attributes)

# Binding the test_num_attributes and encode_attributes
test_encoded_attributes<-cbind(test_num_attributes,encode_attributes)
head(test_encoded_attributes,5)

# Forecast

library(forecast)

count <- Data_Day$cnt %>%
  ts(frequency = 365) %>%
  stl("periodic")

plot(forecast(count)) 


