
n <- nrow(chap07_ex2_soccer) 

all(levels(chap07_ex2_soccer$ht)==levels(chap07_ex2_soccer$at))


goals <-c(chap07_ex2_soccer$goals1,chap07_ex2_soccer$goals2) 
game <- c(1:n, 1:n ) 
home <- c( rep(1,n), rep(0,n)  ) # Binary indicator ( 1 for the home team , 0 for the away team , n times )
att <- factor(c(chap07_ex2_soccer$ht, chap07_ex2_soccer$at) )
def <- factor(c(chap07_ex2_soccer$at, chap07_ex2_soccer$ht))

levels(att) <- levels(chap07_ex2_soccer$ht)
levels(def) <- levels(chap07_ex2_soccer$ht)

premier <- data.frame( game=game, att=att, def=def, home=home, goals=goals) # Create a data frame with the results of the championship
head(premier)
tail(premier)



i<-order(game)
premier<-premier[i,] # Create a df with the results of the championship ( home team vs away team and away team vs home team, every two observations ) 
head(premier) 

# STZ constraints
contrasts(premier$att)<-contr.sum(20)
contrasts(premier$def)<-contr.sum(20)

model <- glm( goals~home+att+def, family=poisson, data=premier ) # Fit a Poisson glm model
summary(model)
round(summary(model)$coef,3)

abilities <- matrix( nrow=20,ncol=4 )
abilities[1:19,1:2] <- summary(model)$coef[2+1:19,1:2]
abilities[1:19,3:4] <- summary(model)$coef[21+1:19,1:2]
abilities[20,1] <- -sum(summary(model)$coef[2+1:19,1]) # Attack of node 20 ( by definition the minus sum of the rest ) 
abilities[20,3] <- -sum(summary(model)$coef[21+1:19,1]) # Defense of node 20 ( by definition the minus sum of the rest ) 

rownames(abilities)<-levels(premier$att)
colnames(abilities)<-c( "Att", "SD-Att", "Def", "SD-Def" )
round(abilities,3)

stz_att <- model$contrasts$att # Matrix of the pseudo variables
stz_att

stz_def <- model$contrasts$def
stz_def

# library(MASS) # Needed for the next function ( in order to export them in txt rectangular format )
# There's no need to use them:
# write.matrix(stz_att,file="C:/Users/mihal/OneDrive/stz_att_data.txt",sep =" ") 
# write.matrix(stz_def,file="C:/Users/mihal/OneDrive/stz_def_data.txt",sep =" ") 







library(BAS)
res1 <- bas.lm(goals~home+att+def,data = premier,n.models = 10000,prior = "BIC",alpha = 3,modelprior = beta.binomial(1,1))
res1
names(res1)
a1 <- summary(res1)
b1 <- a[,2]
round(summary(res1),1)
round(t(summary(res1)),2)
t(summary(res1)[,-1])
round(t(summary(res1, 5)[,-1]),2)


coef(res1)# Marginal Posterior Summaries of Coefficients using BMA ( based on the top 524288 models initially )

image(res1, top.models=5, intensity=TRUE, prob=TRUE, log=TRUE, rotate=TRUE, color="rainbow", 
subset=NULL, offset=.75, digits=3, 
vlas=2,plas=0,rlas=0)

d1 <- coef(res1,estimator = "MPM") # Extract posterior means and standard deviations, marginal posterior means and standard 
# deviations, and marginal inclusions probabilities under median probability model
e1 <- d1$probne0
e1





res2 <- bas.lm(goals~home+att+def,data = premier,n.models = 10000,prior = "BIC",alpha = 3,modelprior = uniform())
res2
a2 <- summary(res2)
b2 <- a2[,2]
round(t(summary(res2, 5)[,-1]),2)
coef(res2)

image(res2, top.models=5, intensity=TRUE, prob=TRUE, log=TRUE, rotate=TRUE, color="rainbow", 
      subset=NULL, offset=.75, digits=3, 
      vlas=2,plas=0,rlas=0)

d2 <- coef(res2,estimator = "MPM") # Extract posterior means and standard deviations, marginal posterior means and standard 
# deviations, and marginal inclusions probabilities under median probability model
e2 <- d2$probne0



res3 <- bas.lm(goals~home+att+def,data = premier,n.models = 10000,prior = "g-prior",alpha = 3,modelprior = beta.binomial(1,1))
res3 # Error: cannot allocate vector of size 10 Kb hence change the n.models to 10
a3 <- summary(res3)
b3 <- a3[,2]
round(t(summary(res3, 10)[,-1]),2)
coef(res3)

image(res3, top.models=10, intensity=TRUE, prob=TRUE, log=TRUE, rotate=TRUE, color="rainbow", 
      subset=NULL, offset=.75, digits=3, 
      vlas=2,plas=0,rlas=0)


d3 <- coef(res3,estimator = "MPM") # Extract posterior means and standard deviations, marginal posterior means and standard 
# deviations, and marginal inclusions probabilities under median probability model
e3 <- d3$probne0





res4 <- bas.lm(goals~home+att+def,data = premier,n.models = 10000,prior = "g-prior",alpha = 3,modelprior = uniform())
res4
a4 <- summary(res4)
b4 <- a4[,2]
round(t(summary(res4, 10)[,-1]),2)

image(res4, top.models=10, intensity=TRUE, prob=TRUE, log=TRUE, rotate=TRUE, color="rainbow", 
      subset=NULL, offset=.75, digits=3, 
      vlas=2,plas=0,rlas=0)

d4 <- coef(res4,estimator = "MPM") # Extract posterior means and standard deviations, marginal posterior means and standard 
# deviations, and marginal inclusions probabilities under median probability model
e4 <- d4$probne0








res5 <- bas.lm(goals~home+att+def,data = premier,n.models = 10000,prior = "hyper-g",alpha = 3,modelprior = beta.binomial(1,1))
res5
a5 <- summary(res5)
b5 <- a5[,2]
round(t(summary(res5, 10)[,-1]),2)

image(res5, top.models=10, intensity=TRUE, prob=TRUE, log=TRUE, rotate=TRUE, color="rainbow", 
      subset=NULL, offset=.75, digits=3, 
      vlas=2,plas=0,rlas=0)


d5 <- coef(res5,estimator = "MPM") # Extract posterior means and standard deviations, marginal posterior means and standard 
# deviations, and marginal inclusions probabilities under median probability model
e5 <- d5$probne0








res6 <- bas.lm(goals~home+att+def,data = premier,n.models = 10000,prior = "hyper-g",alpha = 3,modelprior = uniform())
res6
a6 <- summary(res6)
b6 <- a6[,2]

round(t(summary(res6, 10)[,-1]),2)


image(res6, top.models=10, intensity=TRUE, prob=TRUE, log=TRUE, rotate=TRUE, color="rainbow", 
      subset=NULL, offset=.75, digits=3, 
      vlas=2,plas=0,rlas=0)


d6 <- coef(res6,estimator = "MPM") # Extract posterior means and standard deviations, marginal posterior means and standard 
# deviations, and marginal inclusions probabilities under median probability model
e6 <- d6$probne0







mat <- matrix(c(b1,b2,b3,b4,b5,b6),nrow=6,ncol=45,byrow=T)
mat
rownames(mat) <- c("BIC+beta.binomial","BIC+uniform","g-prior+beta.binomial","g-prior+uniform","hyper-g+beta.binomial"
,"hyper-g+uniform")

c1 <- paste(c("att"),1:19,sep ="")
c1
c2 <- paste(c("def"),1:19,sep="")
c2
c <- c("Intercept","home",c1,c2,"BF","PostProbs","R2","dim","logmarg")

colnames(mat) <- c
mat



mat2 <- matrix(c(e1,e2,e3,e4,e5,e6),nrow=6,byrow = T)
mat2

g1 <- c("Intercept","home",c1,c2)
colnames(mat2) <- g1
rownames(mat2) <- c("BIC+beta.binomial","BIC+uniform","g-prior+beta.binomial","g-prior+uniform",
"hyper-g+beta.binomial","hyper-g+uniform")
mat2







library(MCMCpack)
posterior <- MCMCpoisson(goals~home+att+def,data = premier,burnin = 500,mcmc=1000 )
summary(posterior)
plot(posterior)









