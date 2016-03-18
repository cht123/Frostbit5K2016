#---------------- Setup ----------------#
# Source functions
source("~/Classes/DSS/Functions.R")

# Load packages
library("rvest")
library("lubridate")
library("ggplot2")
library("xtable")
library("scales")
library("dplyr")
library("plyr")
library("PerformanceAnalytics")
library("psych")
library("stringr") 



#---------------- Extract race data ----------------#
url <- "http://championship-racing.com/results/2016/2016-01-30-Frostbite5K.htm"

race_data <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/table') %>%
  html_table()
race_data <- race_data[[1]]

# add column names
colnames(race_data) <- race_data[4,]

## CLEAN THE DATA
# identify rows to be removed
text_rows <- as.data.frame(grepl("[A-Z]", race_data[,1]))
colnames(text_rows) <- "textrows"
race_data <- cbind(race_data, text_rows)

# CREATE FLAG FOR GENDER
Female <- as.data.frame(grepl("Female", race_data[,1]))
Male <- as.data.frame(grepl("Male", race_data[,1]))
Gender <- cbind(Female, Male)
colnames(Gender) <- c('Female', 'Male')
# combine the flags into one field
Gender$Gender <- ifelse(Gender$Female == TRUE, 'Female',ifelse(Gender$Male == TRUE, 'Male', 'Error' ))

# create gender variable
gender <- vector()
for(i in 1:length(Gender$Gender)){
  if(grepl("^[Male]", Gender$Gender[i])){
    gender <- c(gender, "Male")
  }else if(grepl("^[Female]", Gender$Gender[i])){
    gender <- c(gender, "Female")
  }else{gender <- c(gender, "Replace")}
}

gender2 <- vector()
gender2 <- c(gender2, "Replace")
for(i in 1:length(gender)){
  if(!gender[i]=="Replace"){
    gender2 <- c(gender2, gender[i])
  }else if(gender[i]=="Replace"){
    gender2 <- c(gender2, gender2[i-1])
  }else{gender2[i]=="Replace"}
}


gender3 <- as.data.frame(gender)
gender4 <- as.data.frame(gender2)

# combine the columns
race_data <- cbind(race_data, Gender$Gender, gender3, gender4)

# select only good rows
clean_race_data <- race_data%>%filter(textrows == FALSE)

## Convert time to time

split_time <- str_split(clean_race_data$`Total Time`, ":")


# functions to extract time elements
get_minutes<- function(time_string){
  min <- vector()  
  sec <- vector()
  for (i in 1:length(time_string)) {                                   
    min<-c(min, time_string[[i]][1])        
    sec<-c(sec, time_string[[i]][2])
  } 
  min
}

get_seconds<- function(time_string){  
  sec <- vector()
  for (i in 1:length(time_string)) {                                           
    sec<-c(sec, time_string[[i]][2])
  } 
  sec
}

# extract time elements
mins <- as.numeric(get_minutes(split_time))
sec <- as.numeric(get_seconds(split_time))

# calculate times
min_sec <- mins * 60 
tot_sec <- sec + min_sec
min_dec <- tot_sec / 60
race_time <- as.data.frame(min_dec)

# add min_dec column to main data frame
clean_race_data <- cbind(clean_race_data, race_time)
clean_race_data$numAge <- as.numeric(clean_race_data$Age)

# get the means of each group
cdat <- ddply(clean_race_data, "gender2", summarise, time.mean=mean(min_dec))
cdat

# Create histogram by gender
g <- ggplot(clean_race_data, aes(x = min_dec, fill = gender2)) +
  geom_histogram(binwidth = 3, alpha = .5, position = "identity") +
  ggtitle("Frost Bite 5K Time Distributions by Gender") +
  labs(x = "race time (minutes)",y = "Count") 
g <- g + geom_vline(
    aes(xintercept = 30.6),
    linetype = "dashed", size =
      1, colour = "red"
  )
g <- g + geom_vline(
  aes(xintercept = 24.9),
  linetype = "dashed", size = 1, colour = "blue"
)
g <- g + geom_vline(
  aes(xintercept = 24.7),
  linetype = "dashed", size = 1, colour = "darkgreen"
)

g <- g + geom_vline(
  aes(xintercept = median(clean_race_data$min_dec)),
  linetype = "solid", size = 1, colour = "black"
)
g <- g + annotate(geom="text", x=45, y=17, label="Sam = 24:47, 16th",
           color="darkgreen")
g <- g + annotate(geom="text", x=45, y=16, label="Alex = 30:36, 54th",
                  color="Red")
g <- g + annotate(geom="text", x=45, y=15, label="Nate = 24:54, 18th",
                  color="blue")
g <- g + annotate(geom="text", x=45, y=14, label="Median = 31:36",
                  color="black")
g <- g + annotate(geom="text", x=45, y=12, label="Racers = 122",
                  color="black")
g



# create linear model by gender
cleaned <- subset(clean_race_data, Age < 90, 
                  select=c(Place:numAge))
ggplot(cleaned, aes(x=numAge, y=min_dec, color=gender2)) + geom_point(shape=1) +
  facet_grid(gender2 ~ .) + scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  ggtitle("Frost Bite 5K Linear Regression by Gender") +
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines
full<-lm(min_dec~numAge, data=cleaned)
summary(full)
pairsdata <- cleaned[c("numAge", "gender2", "min_dec")]
pairs.panels(pairsdata)

pairs.panels(pairsdata,lm = TRUE, rug = TRUE,
             pch=21,main="Fisher Iris data by Species")
cleaned$gender2 <- factor(cleaned$gender2)
# t test 
t.test(cleaned$min_dec~cleaned$gender2)
boxplot(cleaned$min_dec~cleaned$gender2, 
        main = "Boxplot of Male versus Female 5K Times", 
        notch = TRUE, col=c("pink","blue"),
        ylab = "Race Time (minutes)")

# create a model controlling for gender
male_cleaned <- filter(cleaned, gender2 =='Female')

time_lm <- lm(min_dec~numAge , data=cleaned)
summary(time_lm)
anova(time_lm)
par(mfrow=c(2,2))
plot(time_lm)
fitted(time_lm)
cleaned_add_fit <- cbind(cleaned, fitted(time_lm))
plot(cleaned_add_fit)
