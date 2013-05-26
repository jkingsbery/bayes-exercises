

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

