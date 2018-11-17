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
epl.lmfit=lm(market_value~age+factor(position_cat)+factor(region)+factor(big_club)+factor(new_signing)+factor(new_foreign),epl)
summary(epl.lmfit)


plot(epl$position_cat,epl$market_value)
start.model=lm(market_value~1,data=epl)
end.model  =lm(market_value~age+factor(position_cat)+factor(region)+factor(big_club)+factor(new_signing)+factor(new_foreign),data=epl)
step(start.model, scope=list(lower=start.model,upper=end.model),direction="forward")

epl.forwardfit=lm(market_value~factor(big_club)+factor(position_cat)+factor(new_signing)+factor(new_foreign),data=epl)

summary(epl.forwardfit)

library(tree)
epl.tree =tree(market_value~age+factor(position_cat)+factor(region)+factor(big_club)+factor(new_signing)+factor(new_foreign),epl )
summary(epl.tree)
par(mfrow=c(1,1))
plot(epl.tree)
text(epl.tree,pretty=0)
prune.epl=prune.tree(epl.tree,best=7)
plot(prune.epl)
text(prune.epl,pretty=1)
mean((epl$market_value-predict(epl.tree))^2)
var(epl$market_value)

cv.epl=cv.tree(epl.tree)
plot(cv.epl$size,cv.epl$dev,type='b')

