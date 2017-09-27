#set working directory 
setwd("C:/Users/Amanda/Desktop/BigData")

#read in dataset
dataset <- read.csv("C:/Users/Amanda/Desktop/BigData/data.csv")
colnames(dataset)[colnames(dataset)=="ï..Country.Name"] <- "Country.Name"

#View(dataset)

dataset[rowSums(is.na(dataset)) == 0,]
#View(dataset)

dataset[complete.cases(dataset),]

# change any empty values to 'NA'
dataset[dataset==""] <- NA

# removing 2 empty columns from the dataset
dataset <- dataset[, -c(60,61)]

# removed all incomplete data from dataset
datasetZ <- na.omit(dataset)
#View(datasetZ)

#write.csv("C:/Users/Amanda/Desktop/BigData/dataProcessed.csv")

#write new dataset to csv file to read in excel
write.csv(datasetZ, file = "MyData1.csv")

data <- read.csv('C:/Users/Amanda/Desktop/BigData/MyData1.csv')
View(data)

##install.packages('dplyr')
##install.packages('ggplot2')
##install.packages('reshape')

library(reshape)
library(dplyr)
library(ggplot2)

## filter data by income level, birth rate and death rate
income_data <- data %>% 
  filter(Country.Name %in% c('Low income','Low & middle income','High income','Middle income')
         ,Indicator.Name %in% c('Birth rate, crude (per 1,000 people)','Death rate, crude (per 1,000 people)'))

## remove unwanted coulmns and pivot table
income_data <-
  income_data %>% 
  select(-X,-Country.Code,-Indicator.Code) %>% 
  melt(id=c('Country.Name','Indicator.Name')) %>% 
  mutate(variable=as.numeric(gsub('X','',variable))
         ,Country.Name=factor(Country.Name,levels=c('Low income', 'Low & middle income','Middle income','High income'))
  )

dev.off()
## plot trend line over years
income_data %>% 
  ggplot(aes(variable,value,group=Country.Name,color=Country.Name))+
  geom_line()+
  geom_point()+
  expand_limits(y=0)+
  facet_wrap(~Indicator.Name,scales='free')+
  scale_x_continuous(breaks=c(seq(1960,2014,4)))+
  scale_color_manual(values=c('blue','red','black','orange'))+
  labs(x='years','value',title='Birth rate & death rate by year and income level')+
  theme(
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    legend.text=element_text(size=15),
    legend.position="top",
    legend.title = element_blank(),
    axis.text.x=element_text(angle=90,size=15),
    axis.text.y=element_text(size=15),
    axis.title.x=element_text(size=15), 
    axis.title.y=element_text(size=15), 
    strip.text.x = element_text(size=15), 
    strip.text.y = element_text(angle=0,size=15), 
    plot.title = element_text(size=15))


## Looking at the graph, it can be seen that low and middle class income have significant higher birth rate.
## comparing low & middle to middle, it can be seen that low & middle is bit higher however not significant
## Looking at the death rate, it can be seen that until ~1987, low/middle and middle class had significant higher death rate.
## Since 1987, it's quite the same, and event a bit lower.
## its possible that change in technology and medicine which in past were affordable only by rich people changed the trend

##########################################################
## t.test comparing birth rate for low/middle and middile 
##########################################################

low_class <- 
  income_data %>% 
  filter(Country.Name=='Low income',Indicator.Name=='Birth rate, crude (per 1,000 people)') %>% 
  select(value)

low_middle_class <-
  income_data %>% 
  filter(Country.Name=='Low & middle income',Indicator.Name=='Birth rate, crude (per 1,000 people)') %>% 
  select(value)

middle_class <- 
  income_data %>% 
  filter(Country.Name=='Middle income',Indicator.Name=='Birth rate, crude (per 1,000 people)') %>% 
  select(value) 

high_class <- 
  income_data %>% 
  filter(Country.Name=='High income',Indicator.Name=='Birth rate, crude (per 1,000 people)') %>% 
  select(value)

## get p-value
t.test(low_class,low_middle_class)$p.value
t.test(low_class, middle_class)$p.value
t.test(low_class,high_class)$p.value
t.test(middle_class,high_class)$p.value
t.test(low_middle_class, middle_class)$p.value

## pvalue is greater than 0.05 hence it can be concluded that diffrences in birth rate 
## between those 2 classes are not significant

low_class_death <- 
  income_data %>% 
  filter(Country.Name=='Low income',Indicator.Name=='Death rate, crude (per 1,000 people)') %>% 
  select(value)

low_middle_class_death <-
  income_data %>% 
  filter(Country.Name=='Low & middle income',Indicator.Name=='Death rate, crude (per 1,000 people)') %>% 
  select(value)

middle_class_death <- 
  income_data %>% 
  filter(Country.Name=='Middle income',Indicator.Name=='Death rate, crude (per 1,000 people)') %>% 
  select(value) 

high_class_death <- 
  income_data %>% 
  filter(Country.Name=='High income',Indicator.Name=='Death rate, crude (per 1,000 people)') %>% 
  select(value)


t.test(low_class_death,low_middle_class_death)$p.value
t.test(low_class_death, middle_class_death)$p.value
t.test(low_class_death,high_class_death)$p.value
t.test(middle_class_death,high_class_death)$p.value
t.test(low_middle_class_death, middle_class_death)$p.value



#####################################



birth_data <- income %>% 
  filter(Indicator.Name == "Birth rate, crude (per 1,000 people)") %>% 
  droplevels() 

death_data <- income %>% 
  filter(Indicator.Name == "Death rate, crude (per 1,000 people)") %>% 
  droplevels() 



kc_birth <- birth_data[,4]

(kc_b <- kmeans(kc_birth, 4))

table(birth_data$Country.Name, kc_b$cluster)

prop.table(table(birth_data$Country.Name, kc_b$cluster))* 100

plot(birth_data[c("Country.Name", "value")], col=kc$cluster)
IQR(birth_data$value)



kc_death <- death_data[,4]
(kc_d <- kmeans(kc_death,4))

table(death_data$Country.Name, kc_d$cluster)
prop.table(table(death_data$Country.Name, kc_d$cluster))* 100

plot(death_data[c("Country.Name", "value")], col=kc_d$cluster)
IQR(death_data$value)



