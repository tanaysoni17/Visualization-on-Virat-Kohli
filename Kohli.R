#Loading Libraries.
library(data.table)
library(ggplot2)
library(csv)

#Copying the dataset in batsman variable so as no data gets erased if I mess up.
Virat.Kohli <- read.csv("C:/Users/Tanay Soni/Desktop/Rutgers 1 Sem/DAV/Virat Kohli.csv")
batsman = Virat.Kohli
setDT(batsman)

#Printing the structure of the dataset.
str(batsman)
summary(batsman)

#Creating catagory variables for the ease of EDA.
unique(batsman$Inns)

batsman[,Inns:=factor(Inns, levels = c(1,2),
                      labels = c('First', 'Second'))]

unique(batsman$Dismissal)

batsman[,Dismissal:=factor(Dismissal, 
                           levels = c("lbw", "caught", "run out", "bowled", "not out", "stumped", "hit wicket"), labels = c("lbw", "caught", "run out", "bowled", "not out", "stumped", "hit wicket"))]

#Printing the summary
summary(batsman)

#Converting variables to numeric form as they were in character form.
batsman$SR <- as.numeric(batsman$SR)
batsman$Mins <- as.numeric(batsman$Mins)

# batsman$Start.Date <- as.Date(batsman$Start.Date,
#                               "%Y-%m-%d")

str(batsman)

#Looking for NA values
table(is.na(batsman))
grep("NA",batsman)

#Removed NA Values. 
batsman <- na.omit(batsman)
summary(batsman)

#Creating the CSV file of cleaned data.
write.csv(batsman, "C:\\Users\\Tanay Soni\\Desktop\\Rutgers 1 Sem\\DAV\\batsman.csv")

#Looking for the dependent variable.
library(GGally)
ggpairs(batsman[1:9])
#From here I come to know that Run should be my dependent variable with BF, Mins and X4s as independent variables.

str(batsman)

#Creating the linear model for Runs as dependent variable and BF, Mins and X4s as independent variables.
model1 <- lm(Runs~BF + Mins + X4s, data = batsman)
summary(model1)

# From here I come to know that I have to use simple linear model.
#Creating the linear model for Runs and BF.
model2 = lm(Runs~BF, data=batsman)
summary(model2)

#Plotting the model.
ggplot(batsman, aes(x=BF, y=Runs))+
  geom_point(color='black')+
  geom_smooth(method = "lm")+
  theme_bw()

#Creating the linear model for Runs and Mins.
model3 <- lm(Runs~Mins, data=batsman)
summary(model3)

#Plotting the second model.
ggplot(batsman, aes(x=Mins, y=Runs))+
  geom_point(color='hot pink')+
  geom_smooth(method = "lm")+
  theme_bw()

