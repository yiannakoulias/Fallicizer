#------------------------------------------------
#'The Fallacizer'
#Simple experiment in ecological fallacy
#------------------------------------------------

no_obs <- 1000
no_classes <- 50 #needs to be a divisor of no_obs

#generate data
A <- runif(no_obs,0,1)
B <- runif(no_obs,0,1)

#create n  classes
S <- rep(1:no_obs, each = no_obs/no_classes, len = no_obs)

#create an identifier
I <- seq(1:no_obs)

#aggregate both
AA <-aggregate(A, by=list(S), FUN=mean)
AB <-aggregate(B, by=list(S), FUN=mean)

#calculate OF
OF <- cor(AA,AB)[2,2]

for(i in 1: 1000){

#identify break points that are feasible, and put them into two vectors
#library(data.table) #shift function
f <- I[(shift(S,1L) != S)]
f <- f[-1]

#select a breakpoint at random
r <- sample(1:(no_classes-1),1)

#change the randomly selected breakpoint to the adjacent different value
#create a temporary class variable
t <- S
t[f[r]] <- t[f[r]-1]

#aggregate both
AA <-aggregate(A, by=list(t), FUN=mean)
AB <-aggregate(B, by=list(t), FUN=mean)

#calculate OF
OF2 <- cor(AA,AB)[2,2]

#keep if an improvement
if(abs(OF2) > abs(OF)){
  S <- t
  OF <- OF2
}

}
AA <-aggregate(A, by=list(S), FUN=mean)
AB <-aggregate(B, by=list(S), FUN=mean)

plot(A,B, main="Correlation between original variables", xlab="X", ylab="y", xmin=0, xmax=1, ymin=0, ymax=1)
abline(fit1<-lm(A~B), col="blue")
legend(x='bottomright', legend=paste('Cor =',round(cor(B,A),2)))

plot(AA$x,AB$x, main="Correlation between Fallicized variables", xlab="X", ylab="y", xmin=0, xmax=1, ymin=0, ymax=1)
abline(fit2<-lm(AB$x~AA$x), col="red")
legend(x='bottomright', legend=paste('Cor =',round(cor(AB$x,AA$x),2)))

