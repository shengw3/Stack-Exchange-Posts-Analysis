#data cleaning
#install.packages("pscl") # 
#install.packages("e1071") run confusion matrix 
#install.packages("randomForest")
#install.packages('xgboost')
#install.packages('fastAdaboost')
#install.packages("adabag")
#install.packages("pROC")#roc 
#install.packages("epiDisplay")#roc 
#install.packages("caret") # cross-valudation
library(pROC)#roc 
library(epiDisplay)#roc 
library(class)
library(adabag)
library(fastAdaboost)
library(xgboost)
library(randomForest)
library(e1071)
library(pscl)
library(car)
library(stringr)
library(caret)
library(MASS)
wholedata <- read.csv("~/Desktop/stats_project2/nondeletedposts.csv")
#View(wholedata)
d1 <-subset(wholedata,select = -c(Id,ParentId,DeletionDate,OwnerDisplayName,LastEditorUserId,
                           LastEditorDisplayName,ClosedDate,CommunityOwnedDate,AcceptedAnswerId,
                           OwnerUserId))
d1$CreationDate = strptime(d1$CreationDate,"%Y-%m-%d %H:%M:%S")
d1$LastActivityDate = strptime(d1$LastActivityDate,"%Y-%m-%d %H:%M:%S")
d1$timeDuration <- round(abs(as.numeric(difftime(d1$LastActivityDate,d1$CreationDate, units = "days"))))
d1$Body <- as.character(d1$Body)
d1$bodyLine <-str_count(d1$Body, "</p>")
d1$LastEditDate <- as.character(d1$LastEditDate)
d1$edited <- ifelse(d1$LastEditDate=="","No","Yes")
d1 <-subset(d1,select = -c(LastActivityDate,CreationDate,Body,LastEditDate))
######################################################
###################################################### Answer DataFrame 
######################################################
#View(d1)
###we split our data into answer type and question type 
ans <- d1[d1$PostTypeId==2,]
#dim(ans) #26116    14
#sum(is.na(ans$AnswerCount)) #all missing 
#sum(is.na(ans$ViewCount)) #all missing 
#sum(is.na(ans$FavoriteCount))# all missing 
#sum(is.na(ans$Score))# no missing 


#AnswerCount,ViewCount,FavoriteCount are all missing in the answer data set
# and we only have one obs have information for Tag and Title
# drop them from the data set 
ans <-subset(ans,select = -c(AnswerCount,ViewCount,FavoriteCount,Tags,Title))
#View(ans)
ans <- na.omit(ans)# stop the only one observation with NA 
plot(ans$timeDuration,ans$Score) # there is one obs has an extreme value, which seems a wrong value 
ans <- ans[-which(ans$Score==max(ans$Score)),]
m1 <-lm(Score~CommentCount + timeDuration + bodyLine + edited,data = ans)
#residual plot 
ex <- studres(m1)
plot(ex,xlab = 'Time Duration', ylab = 'Score')
plot(m1, which = 1)
plot(m1, which = 2)#qqnorm 

#check leverage point 
plot(m1, which = 5) # point 4161, 11596, 9479 is highly influential to the data, we decide to drop them 
ans1 <- ans[c("4161","11596","9479"),] # three influential points 
#which(ans$Score==444)#2282
#which(ans$Score==368)#6229
#which(ans$Score==308) #5144
ans2 <-ans[-c(2282,6229,5144),]
ans2$edited <-as.factor(ans2$edited)
m2 <-lm(Score~CommentCount + timeDuration + bodyLine + edited,data = ans2)
#residual vs.fitted 
plot(m2, which = 1) 
ex2 <- studres(m2)
plot(fitted.values(m2), ex2)
#Normality Histogram 

x_grid <- seq(min(ex2)-.5, max(ex2)+.5, by = .01)
dfRes <- nrow(ans2) - 4 - 1 # 7 predictors and one intercept
hist(ex2, probability = T, ylim = c(0,.5), xlim = c(-6,8),xlab = "External Residuals")
t_densities <- dt(x_grid, dfRes)
points(x_grid, t_densities, type = "l", col = "red")


######################################################
#poisson model 
ans3 <- ans2
ans3$Score <- ans3$Score + 8
#poisson 
poi_mod <-glm(Score~., data = ans3, family = poisson)
summary(poi_mod)
plot(poi_mod, which = 2)
ex_poi <-studres(poi_mod)
plot(fitted.values(poi_mod), ex_poi)#homoskestacity not good 



#boxcox to do transformation to response variable
bc_res <- boxcox(Score ~ CommentCount + timeDuration + bodyLine + edited, data = ans3)
best_lambda <- bc_res$x[bc_res$y == max(bc_res$y)] # best lambda is -0.95
ans3$new_response_bc <- (ans3$Score^best_lambda - 1)/best_lambda

#build Box-cox suggested model 
box_cox_model <- lm(new_response_bc~CommentCount + timeDuration + bodyLine + edited,data = ans3)
plot(box_cox_model , which = 2)     # Normal qq plot 
plot(box_cox_model , which = 1)     # residual 
ex_bc <-studres(box_cox_model) #External Residual 
plot(fitted.values(box_cox_model), ex_bc)# Homoskestacity(good)
ex_bc_data <- data.frame(fitted.values(box_cox_model),ex_bc)
summary(box_cox_model)

ggplot(aes(x = fitted.values(box_cox_model), y = ex_bc), data = ex_bc_data )+
  xlab(" Fitted Value ")+
  ylab("External Residuals")+
  geom_point()+
  stat_smooth()+
  ggtitle('Homoskestacity')

########################### 
# Box-Cox Transformation using poisson regression (not good drop it)
bc_poi_mod <- glm(new_response_bc~CommentCount + timeDuration + bodyLine + edited,data = ans3, family = poisson(link = "log"))
plot(bc_poi_mod, which = 2)
ex_bc_poi <-studres(bc_poi_mod)
plot(fitted.values(bc_poi_mod), ex_bc_poi)#homoskestacity (pretty constant)
summary(bc_poi_mod)# no predictors are significant 

###########################
#do transformation to predictor 
ans3$CommentCountadd <- ans3$CommentCount + 0.0001
ans3$bodyLineadd <- ans3$bodyLine + 0.0001
plot(fitted.values(m3), ex_m3)

## FINAL MODEL
m4 <-lm(new_response_bc ~ log(CommentCountadd) + timeDuration + bodyLineadd + edited,data = ans3)
summary(m4)
ex_m4 <-studres(m4)
plot(m4, which = 1) #residual 
plot(m4, which = 2) #normal qq 
#homoskedasiticity 
plot(fitted.values(m4), ex_m4)
######################################################
###################################################### Question DataFrame 
######################################################
que <- d1[d1$PostTypeId==1,]
#dim(que) #23413    14
#sum(is.na(que$AnswerCount))# no missing 
#sum(is.na(que$ViewCount)) #no missing 
#sum(is.na(que$Score)) # no missing 
que$response <- ifelse(que$AnswerCount==0,0,1)
que$response <- as.factor(que$response)
mis_favct <- que[c(is.na(que$FavoriteCount)),]
favct <-que[!c(is.na(que$FavoriteCount)),]
fav_model <-lm(FavoriteCount~ Score+ ViewCount + CommentCount + timeDuration +  bodyLine + edited + response, data = favct )
summary(fav_model)
fav_model2 <-lm(FavoriteCount~ Score+ ViewCount + CommentCount + timeDuration  + edited + response, data = favct )
summary(fav_model2)
anova(fav_model2,fav_model)
# we reject the longer model, keep using the reduced model without bodyLine 
fav_ex <- studres(fav_model)
plot(fav_ex,ylab = 'External Residuals')
plot(fav_model, which = 1)
newdata <-data.frame('Score' = mis_favct$Score,'ViewCount' = mis_favct$ViewCount, 
                     'timeDuration' = mis_favct$timeDuration,'edited' = mis_favct$edited,
                     'CommentCount' = mis_favct$CommentCount,'response' = mis_favct$response
                     )
pred_favct <-round(predict(fav_model2,newdata))
mis_favct$fav_count <- pred_favct 
mis_favct = subset(mis_favct, select = -c(FavoriteCount))
colnames(favct)[colnames(favct) == 'FavoriteCount'] <- 'fav_count'
que2 <- rbind(favct,mis_favct)
que2 <-subset (que2, select = -c(PostTypeId,Tags,Title,AnswerCount))
que2 <-na.omit(que2)
que2$edited <- as.factor(que2$edited)
#build model 
que2_m1 <-glm(response~., data  = que2, family = binomial(link = "logit"),control = list(maxit = 50))
que2_m2 <-glm(response~ Score + ViewCount + CommentCount + timeDuration + bodyLine + edited, data  = que2, family = binomial(link = "logit"),,control = list(maxit = 50))
plot(que2_m1,which =1)
#Likelihood Ratio Test 
que2_null <-glm(response~1, data  = que2, family = binomial(link = "logit"),control = list(maxit = 50))
result1 <- anova(que2_null,que2_m1,test="LRT")
result <-anova(que2_m2,que2_m1,test="LRT")# we should include favoriate count in our model 

#reject the null pypothesis, we should use the full model 

#psudo r square 
pR2(que2_m1)[['McFadden']]*100 
#wald test 
confint(que2_m1)
summary(que2_m2)# all predictors are significant 
?step

# check correlation 
cor(que2[,-c(7,8)])

#View(que2)
exp(coef(que2_m1))
#This informs us that for every one unit increase in ViewCount, the odds of getting post answered increases by a factor of 0.8.

######################################################
######################################################
######################################################

# AIC Forward/Backward selection 
reg0 <- glm(response~1,data=que2,family=binomial(link = "logit"))
step(reg0,scope=formula(que2_m1),direction="forward",k=2)
step(que2_m1)
step(reg0, list(lower=formula(reg0),upper=formula(que2_m1)),direction="both",trace=0)
# all give the same model (full model)

#roc plot 
resLroc <- lroc(que2_m1)

######################################################
######################################################
######################################################

# Do it in training data set and testing data set 

Train <- createDataPartition(que2$response, p=0.7, list=FALSE)
training <- que2[ Train, ]
testing <- que2[ -Train, ]
train_mod <- glm(response~., data  = training, family = binomial(link = "logit"))
pred <- predict(train_mod, newdata=testing,type = "response")
pred <- as.factor(ifelse(pred>0.5,1,0))
confusionMatrix(data=pred, testing$response) #accuracy  86% 
######################################################
#random forest 
set.seed(1)
ran_mod <-randomForest(response~., data = training,mtry=3,importance=TRUE)
impt <- importance(ran_mod)
varImpPlot(ran_mod)
rownames(impt)[order(impt[,4], decreasing = TRUE)]
#"fav_count" -->"ViewCount" -->"Score"-->"timeDuration" -->"CommentCount" -->"bodyLine"--> "edited"
ran_pred<-predict(ran_mod,testing,type="class")
table(ran_pred,testing$response)
(1617+4817)/(359 + 230 +1617+4817)#91%s

#agaboost
ada_mod <-adaboost(response~., data = training,15)
ada_pred <-predict(ada_mod,testing)$class
table(ada_pred,testing$response)
(1583+4794)/(393 + 253 +1583+4794) #90.8%

#svm
svm_mod <- svm(response~., data = training,kernel="radial", gamma=1)
svm_pred<-predict(svm_mod,testing)
table(svm_pred,testing$response)
(1497+4584)/(479 + 463 +4584+1497) #86% 





