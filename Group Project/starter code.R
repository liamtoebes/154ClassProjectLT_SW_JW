epl=data.frame(read.csv("C:/Users/liamt/OneDrive/Documents/College/STAT154/Group Project/epldata_final.csv"))

head(epl)
epl$name=NULL
epl$club=NULL
epl$position=NULL
epl$page_views=NULL
epl$fpl_value=NULL
epl$fpl_sel=NULL
epl$fpl_points=NULL
epl$nationality=NULL
epl$age_cat=NULL
epl$club_id=NULL

head(epl)
epl=na.omit(epl)
factors=c('position_cat','region','big_club','new_signing','new_foreign')
epl[factors] <- lapply(epl[factors] , factor)

summary(epl.lmfit)

require(splines)

#boxplots for all the categorical variables
par(mfrow=c(2,3))
boxplot(epl$market_value~epl$position_cat)
boxplot(epl$market_value~epl$big_club)
boxplot(epl$market_value~epl$region)
boxplot(epl$market_value~epl$new_foreign)
boxplot(epl$market_value~epl$new_signing)
#plot + cubic spline for the ages column
plot(epl$age,epl$market_value)

age.spline=lm(market_value~ns(age,4),epl)
x=seq(min(epl$age),max(epl$age),length=100)
y=predict(age.spline,newdata=data.frame(age=x),se=T)

lines(x,y$fit,lwd=2)
lines(x,y$fit+2*y$se.fit,lty='dotted')
lines(x,y$fit-2*y$se.fit,lty='dotted')

#linear model with forward/backward selection with age as linear term
epl.lmfit1=lm(market_value~age+position_cat+region+big_club+new_signing+new_foreign,epl)
summary(epl.lmfit1)
start.model1=lm(market_value~1,data=epl)
end.model1  =lm(market_value~age+position_cat+region+big_club+new_signing+new_foreign,data=epl)

step(start.model1, scope=list(lower=start.model1,upper=end.model1),direction="forward")
step(end.model1, scope=list(lower=start.model1,upper=end.model1),direction="backward")

#both forward and backward model selection give the same outbut
epl.forwardfit1=lm(market_value~big_club+position_cat+new_signing+new_foreign,data=epl)
summary(epl.forwardfit1)

#linear model with forward/backward selection with splines for age term
epl.lmfit2=lm(market_value~ns(age,3)+position_cat+region+big_club+new_signing+new_foreign,epl)
summary(epl.lmfit2)

start.model2=lm(market_value~1,data=epl)
end.model2  =lm(market_value~ns(age,3)+position_cat+region+big_club+new_signing+new_foreign,data=epl)

step(start.model2, scope=list(lower=start.model2,upper=end.model2),direction="forward")
step(end.model2, scope=list(lower=start.model2,upper=end.model2),direction="backward")

#both forward and backward model selection give the same outbut
epl.forwardfit2=lm(market_value~big_club+ns(age,3)+position_cat+new_signing,data=epl)

summary(epl.forwardfit2)


library(tree)

train.index=sample(nrow(epl),nrow(epl)*0.75)
train.index
train=epl[train.index,]
test=epl[-train.index,]
par(mfrow=c(2,2))
epl.tree =tree(market_value~age+factor(position_cat)+factor(region)+factor(big_club)+factor(new_signing)+factor(new_foreign),train )
summary(epl.tree)

plot(epl.tree)
text(epl.tree,pretty=0)

cv.epl=cv.tree(epl.tree)
plot(cv.epl$size,cv.epl$dev,type='b')

prune.epl=prune.tree(epl.tree,best=8)
plot(prune.epl)
text(prune.epl,pretty=1)

mean((test$market_value-predict(epl.tree,newdata=test))^2)
mean((test$market_value-predict(prune.epl,newdata=test))^2)

mean((test$market_value-mean(train$market_value))^2)

