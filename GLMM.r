cancer<-read.csv("~/Desktop/PROJECT/data.csv",  sep=",", header=T)
head(cancer)
cancer$InRate<-cancer$InRate/100000
cancer[,2:10]<-cancer[,2:10]/100
y <- round(cancer$InRate * cancer$Populations)
x <- scale(cancer[,2:10])
newcancer = data.frame(y, x, Superfund=cancer$Superfund,total=cancer$Populations,county=cancer$county)
head(newcancer)

#cluster analysis for education
edu <- cancer[,2:3]
dist.r <- dist(edu, method="euclidean")  #euclidean distance
heatmap(as.matrix(dist.r),labRow = F, labCol = F)
race.f <- hclust(dist.r, method="ward.D")  #Ward's method
plot(race.f) #display dendogram
groups.r <- cutree(race.f,k=3)

mds.r=cmdscale(dist.r,k=2,eig=T)
x = mds.r$points[,1]
y = mds.r$points[,2]

#install.packages("ggplot2")
library(ggplot2)
p=ggplot(data.frame(x,y),aes(x,y))
p+geom_point(size=3,alpha=0.8, aes(colour=factor(groups.r))) 
p+geom_point(size=3,alpha=0.8, aes(colour=factor(groups.r))) 

groups.r

##?????cmdscale() ???????
##?????handle Superfund???????


#glm
require(glmnet)
glm1<-glm(y/total~ x+Superfund, family=binomial, weights=total, data=newcancer)
summary(glm1)
AIC(glm1) #1331.391
glm1$dev #782.4622

glm2<- glm(y/total~ White + Black + Bachelor + Female + older65 
           + OwnHouseRate + MIncome + NonInsurance, family=binomial, weights=total, data=newcancer)
summary(glm2)
AIC(glm2) #1328.709
glm2$dev #783.7807


#random effects model

library(glmmML)
fit.glmm <- glmmML(response ~ gender + z1 + z2,
                     +           cluster=abortion$case, family=binomial, data=abortion,
                     +           method = "ghq", n.points=70, start.sigma=9)
summary(fit.glmm)

library(lme4)
m1<-glmer(y/total~ Bachelor + Female + older65 
            + OwnHouseRate + MIncome + NonInsurance +  (1 | groups.r), family=binomial, weights=total, data=newcancer)
summary(m1)
AIC(m1) #1347.847
