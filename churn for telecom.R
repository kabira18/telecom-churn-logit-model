
setwd("C:/Users/amans/OneDrive/Desktop/capstone simplilearn telecom churn")
library(dplyr)


library(ggplot2)
library(effects)
library(sjPlot)
library(emmeans)



merged_data <- inner_join(churn,customer, by = "customer_ID")


d <- inner_join(merged_data,internet, by = "customer_ID")


summary(d)
str(d)
tc <- d %>%
  mutate(
    across(c(PhoneService,Contract,PaperlessBilling,PaymentMethod,Churn,gender,partner,dependents,MultipleLines,InternetService,OnlineSecurity,OnlineBackup,DeviceProtection,TechSupport,StreamingTV,StreamingMovies), factor)
  )

str(tc)


is.na(tc)




# Create box plot
ggplot(tc, aes(y = MonthlyCharges )) +
  geom_boxplot() +
  labs(title = "Box Plot for monthlycharges", x = "", y = "Value")



# Create box plot
ggplot(tc, aes(y = TotalCharges)) +
  geom_boxplot() +
  labs(title = "Box Plot for total charges", x = "", y = "Value")




table(tc$Churn)



tc<-tc %>% 
  select(2:21)








str(tc)



tc<- tc %>%
  mutate(
    across(c(Senior_Citizen), factor)
  )



rg.limit = 60000






tc <- tc%>%
  relocate(Churn, .after = 20)
str(tc)
tc$Churn <- as.numeric(as.factor(tc$Churn))

str(tc$Churn)
tc$Churn[tc$Churn==1]=0
tc$Churn[tc$Churn==2]=1


indx=sample(1:nrow(tc),as.integer(0.8*nrow(tc)))
churn_train=tc[indx,]
churn_test=tc[-indx,]



dim(churn_train)
dim(churn_test)



prop.table(table(tc$Churn))*100


prop.table(table(churn_train$Churn))*100
prop.table(table(churn_test$Churn))*100



churn_train_labels=tc[indx,20]
churn_test_labels=tc[-indx,20]


library(Amelia)
library(Rcpp)
library(effects)
missmap(tc,main="missing vs observed value")




model=glm(tc$Churn~.,family = binomial(link = "logit"),data = tc)
m<-model
plot(allEffects(m))


summary(m)
library(margins)

margins(m)


anova(m,test = "Chisq")

fitted.results=predict(m,newdata = churn_test,type = "response")
fitted.results=ifelse(fitted.results>0.7,1,0)

head(fitted.results)
head(churn_test$Churn)


### roc curve

library(ROCR)

p=predict(m,newdata = churn_test,type = "response")
pr=prediction(p,churn_test$Churn)

prf=performance(pr,measure = "tpr",x.measure = "fpr")

plot(prf)





m1<-glm(Churn ~ Senior_Citizen * MonthlyCharges, data = tc ,family = "binomial")
plot(allEffects(m1))
plot_model(m1,type="int")
summary(m1)

m2<-glm(Churn ~ gender * tenure, data = tc ,family = "binomial")
plot(allEffects(m2))
plot_model(m2,type="int")


library(emmeans)

emmeans(m1,pairwise~ Senior_Citizen | MonthlyCharges,adjust="fdr")$contrasts



##visualization

pwpp(emmeans(m1,~Senior_Citizen* MonthlyCharges),type="response",adjust="fdr")+theme_minimal()
