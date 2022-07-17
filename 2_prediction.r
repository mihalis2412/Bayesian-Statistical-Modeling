
i1 <- premier$game<371 # Select all the games of the championship minus the last 10 observations ( minus the last match day i.e. matchday 38 in the Premier league )

premier2 <- premier[i1,]
model2 <- glm( goals~home+att+def, family=poisson, data=premier2 )

lambda <- exp(predict(model2, premier[!i1,])) # Expected number of goals
lambda

names(lambda) <- levels(premier$att)[c(4,2,6,19,8,1,10,3,12,18,13,5,14,11,17,9,20,7,16,15)]
names(lambda)


np <- nrow(premier[!i1,])
B <- 10000 # Number of iterations
G <- matrix(nrow=B, ncol=np)
for (i in 1:B){
	G[i,]<-rpois(np, lambda) # Bootstrap simulations
} 

i2 <- seq(1,np,2)
diff <- G[,i2] - G[,i2+1] 

apply(diff,2,mean) # Means
apply(diff,2,sd) # Sds
t(apply(diff,2,quantile, probs=c(0.025,0.975)))

tabres <- matrix("", 10,8) # Create a 3x8 matrix

tabres[,1:2] <- matrix( c("Burnley","Bournemouth","Crystal Palace","West Brom","Huddersfield","Arsenal","Liverpool","Brighton",  
"Manchester United","Watford","Newcastle","Chelsea","Southampton","Manchester City","Swansea","Stoke","Tottenham","Leicester",
"West Ham","Everton"), 10,2, byrow=T )
tabres[,3] <- paste(premier[!i1,]$goals[i2],premier[!i1,]$goals[i2+1],sep="-")
med <- apply(G,2,median)
tabres[,4] <- paste(med[i2],med[i2+1],sep="-")
l<-round(lambda,2)
tabres[,5] <- paste(l[i2],l[i2+1],sep="-")
tabres[,6] <- round(apply(diff,2,mean),2)
tabres[,7] <- round(apply(diff,2,sd),2) 
temp <- t(apply(diff,2,quantile, probs=c(0.025,0.975)))
tabres[,8] <-paste("(",apply(temp,1,paste,collapse=','),")", sep='')

rownames(tabres)<- premier[!i1,]$game[i2]
colnames(tabres)<- c("Home Team", "Away Team", "Actual Score", "Median", "Expected", "Exp.Diff", "SD Diff", "95% CI Diff")
noquote(tabres)

probs<-rbind( 
	c(mean(diff[,1]>0), mean(diff[,1]==0), mean(diff[,1]<0)),
	c(mean(diff[,2]>0), mean(diff[,2]==0), mean(diff[,2]<0)), 
	c(mean(diff[,3]>0), mean(diff[,3]==0), mean(diff[,3]<0)),
	c(mean(diff[,4]>0), mean(diff[,4]==0), mean(diff[,4]<0)),
	c(mean(diff[,5]>0), mean(diff[,5]==0), mean(diff[,5]<0)), 
	c(mean(diff[,6]>0), mean(diff[,6]==0), mean(diff[,6]<0)),
	c(mean(diff[,7]>0), mean(diff[,7]==0), mean(diff[,7]<0)), 
	c(mean(diff[,8]>0), mean(diff[,8]==0), mean(diff[,8]<0)),
	c(mean(diff[,9]>0), mean(diff[,9]==0), mean(diff[,9]<0)), 
	c(mean(diff[,10]>0), mean(diff[,10]==0), mean(diff[,10]<0))
)
probs 

tabres2<-cbind(tabres[,1:8], round(probs,2))
colnames(tabres2)[9:11]<-c("HomeWins","Draw","AwayWins")
noquote(tabres2) # Probabilities of each outcome

round(table(diff[,1])/B,3) # Probs of the goal difference for each of the 10 matches 
round(table(diff[,2])/B,3)
round(table(diff[,3])/B,3) 
round(table(diff[,4])/B,3)
round(table(diff[,5])/B,3) 
round(table(diff[,6])/B,3)
round(table(diff[,7])/B,3) 
round(table(diff[,8])/B,3)
round(table(diff[,9])/B,3) 
round(table(diff[,10])/B,3)

















