#WAGE: Wage  paid for an hour (USD), Class: Float
#EXPERIENCE: Work experience Class: Intiger
#weeks:Weeks worked in a year. 
#Occupation: Departments in the business department , Class:Character
#Ethnictiry : Race/ethnicity of the individual
#Industry: Industry type
#sOUTH: Region: Southern USA
#SMSA: Standard Metropolitan Statistical Area (Urban vs. Rural)
#Union: Union membership , Class: Factor
#Gender: whether male or female, Class:Character
#Married: Marital status, Class: Factor
#EDUCATION: Years of education completed.

df <- PSID1982
str(df)
head(df)
summary(df)

colSums(is.na(df)) #Are there any missing values? 

hist(df$wage , main ="Wage Histogram ", xlab="Wage",col="yellow")
mean(df$wage) #per hours wage 

hist(df$experience, main ="Experience Histogram",xlab="Experience",col="brown")
mean(df$experience)

boxplot(wage ~ gender, data=df,main="Boxplot of wage and gender",xlab="gender",ylab="wage ",col=c("white","grey"))

Q1 <- quantile(df$wage,0.25)
Q3 <- quantile(df$wage,0.75)
IQR= Q3-Q1

lower_bound <- Q1-1.5*IQR
upper_bound <-Q3+1.5*IQR

outliers <- df$wage[df$wage <lower_bound | df$wage >upper_bound]
length(outliers)

reg_model1 <- lm(wage~experience+education+gender+union, data=df )
summary(reg_model1)

#Experience  p value is too small which is <0.05 this mean statistically signifiecnt
# Only a small portion of the data can be explained, which is not a good thing.
#The model is statistically significant but the model does not have a very good explanatory power because it has a rate of 0.29%.
#It would be better to build another model instead.


#gaussian glm model

glm_model1 <- glm(education ~experience, data=df,family=gaussian("identity"))
summary(glm_model1)


#education p value is very small < 0.05 this show us the test is statistically significent
#null intercept, 4623 > 4396 made a positive contribution to the data
#AIC is quite a lot,  smaller  AIC better for data set 


glm_model2 <- glm(union ~education+experience, data=df,family=gaussian("identity"))
summary(glm_model2)

#education and experience both smaller than 0.05 show us the test is statistically signifiecnt
#null intercept, 138.13 > 127.91 made a positive contribution to the data
#AIC is smaller then first model this analysis better than previsly one 

correlation <- cor(df$wage, df$education)
cat("correlation coefficient",correlation)

#there is a positive relationship between two varibles but relationship is Moderate relationship.Which is  0.4407

#binomial glm model
df$union <- ifelse(df$union =="yes",1,0)
glm_model3 <- glm(union ~wage ,data=df, family=binomial(link="logit"))
summary(glm_model3)

#wage is not statistically signifienct because of 0.06>0.05
#null > residual  made a positive contribution to the data
#AIC is 782.17 (A smaller AIC generally indicates a better model selection.)


#possion glm model
glm_model_poisson <- glm(experience ~  married + education+wage  , data = df, family = poisson(link="log"))
summary(glm_model_poisson)

#married and educaiton, wage  values < 0.05 this mean is statistically signifiecnt 
#null> deviance (21877>10224)  made a negative contribution to the data
#AIC: 5613 quite a lot 

corelation2 <- cor(df$experience, df$education)
cat("corelation coefficient",corelation2)

#there is a negative  relationship between two varibles but relationship is also weak relationship .Which is  -0.221

