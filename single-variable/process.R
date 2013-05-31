library(ggplot2)

# A Sample Survey plots
for(theta in c(0:10)/10){
    d=dbinom(c(0:100),100,theta)
    plot(d)
    lines(d)
}

# To calculate p(theta | data) on 0.0,0.1,...,1.0
thetas=c(0:10)/10
p_thetas=(function(theta){theta^57*(1-theta)^43/sum((function(theta){theta^57*(1-theta)^43})(thetas))})(thetas)

pdf(file="figure1.pdf", height=3, width=4)

plot(thetas,p_thetas)

dev.off()

pdf(file="figure2.pdf", height=3, width=4)

plot(c(1:100)/100,dbeta(c(1:100)/100,1+57,1+100-57))

dev.off()

num_samples=5000
theta1=rbeta(num_samples,1+57,1+100-57)
theta2=rbeta(num_samples,1+30,1+50-30)
x=table(theta1<theta2)
print(c("theta1<theta2",x[2]/num_samples))

#Sensitivity Analysis

print("Sensitivity Analysis")
theta_0=c(1:9)/10
n_0=c(1:32)
rowcount=length(theta_0)*length(n_0)
result=data.frame(theta=rep(NA,rowcount),n=rep(NA,rowcount),a=rep(NA,rowcount),b=rep(NA,rowcount),p=rep(NA,rowcount))

print(length(theta_0))
print(length(n_0))
num=1
for(i in 1:length(theta_0)){
    for(j in 1:length(n_0)){
        theta=theta_0[i]
	n=n_0[j]
	a=theta*n
	b=(1-theta)*n
	p=pbeta(c(1,0.5),a+57,b+100-57)
	result[num,]=c(theta,n,a,b,p[1]-p[2])
	num=num+1
    }
}
print(result)

library(ggplot2)
pdf(file="figure3.pdf", height=3, width=6)
ggplot(result,aes(theta,n,fill=p))+geom_tile()+labs(title="Probability of Theta>0.5 for Different Priors")
dev.off()

#Count Data
yA=c(12,9,12,14,13,13,15,8,15,6)
yB=c(11,11,10,9,9,8,7,10,6,8,8,9,7)

stats<-function(alpha,beta){
    print(c(alpha,beta,alpha/beta,alpha/beta/beta,qgamma(c(0.025,0.975),alpha,beta)))
}
#Posteriors
alphaA=120+sum(yA)
betaA=10+length(yA)
stats(alphaA,betaA)
alphaB=12+sum(yB)
betaB=1+length(yB)
stats(alphaB,betaB)

# Sensitivity of the posterior for B to other priors

n0=c(1:50)
for(n in n0){
    alpha=(12*n)+sum(yB)
    beta=n+length(yB)
    x=c(1:200)/10
    y=dgamma(x,alpha,beta)
    y_a=dgamma(x,alphaA,betaA)
    plot(x,y)
    points(x,y_a,col="red")
    #print(c(n,alpha,beta,alpha/beta,alpha/beta/beta,qgamma(c(0.025,0.975),alpha,beta)))
    
}

# Model checking
poissoncheck=function(data,prior_alpha,prior_beta){
    alpha=prior_alpha+sum(data)
    beta=prior_beta+length(data)

    t.mc<-NULL
    for(s in 1:10000){
        theta1<-rgamma(1,alpha,beta)
        y1.mc<-rpois(10,theta1)
        t.mc<-c(t.mc,mean(y1.mc)/sd(y1.mc))
    }
    t.mc=sort(t.mc)

    print(mean(data)/sd(data))
    quantile(t.mc,probs=seq(0,1,0.05))

}

poissoncheck(yA,120,10)
poissoncheck(yB,12,1)

#Probability of Differenceapproximately
thetaA=rgamma(10000,alphaA,betaA)
thetaB=rgamma(10000,alphaB,betaB)
table(thetaA>thetaB)

#Differences in Members of Population

t.mc<-NULL
for(s in 1:10000){
    thetaA=rgamma(1,alphaA,betaA)
    thetaB=rgamma(1,alphaB,betaB)
    t.mc<-c(t.mc,rpois(1,thetaA)>rpois(1,thetaB))
}
table(t.mc)

#Independence of the Two Distributions
