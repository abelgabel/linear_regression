
real_a<-2
real_b<-1




estimated<-matrix(0, nrow=1000, ncol=2)
estimated2<-matrix(0, nrow=1000, ncol=2)
for(i in 1:1000){

# exponential tdistribution
simulation_reg2<-regression_sim(N= 100,a=real_a,b=real_b, typeerror ='uniform', sd_chosen =1, inter=5,lambda=.01,degf=2)


lg2<-lm(simulation_reg2[,2]~ simulation_reg2[,1])
estimated[i,]<-summary(lg2)$coef[,1]
estimated2[i,]<-summary(lg2)$coef[,2]

#(summary(lg2)$coef[,1]-c(real_a, real_b))/summary(lg2)$coef[,2]
}

estim<-(estimated[,1]-real_a)/estimated2[,1]
estim2<-(estimated[,2]-real_b)/estimated2[,2]

simul<-simulated_regression(N=100,real_a=2,real_b=3, typeerror='normal', sd_chosen=1, inter,lambda,degf)

ggplot(simul, aes(est_intercept))+geom_histogram(fill='#4484CE', color='#F9CF00')+theme(   axis.text = element_text(size = 14),
    legend.key = element_rect(fill = "gray80"),
    legend.background = element_rect(fill = "white"),
    legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
	)+xlab(expression(hat(alpha)))+ylab('')







par(mfrow=c(2,2))
hist(simul[,1], breaks=70, col='blue',freq=F)
hist(simul[,2], breaks=70, col='blue',freq=F)

hist(simul[,3], breaks=70, col='blue',freq=F, xlim=c(-4,4),ylim=c(0,.5))
hist(simul[,4], breaks=70, col='blue',freq=F, xlim=c(-4,4),ylim=c(0,.5))


par(mfrow=c(1,2))

qqnorm(estimated[,2])
qqline(estimated[,2])
qqnorm(estimated2[,2])
qqline(estimated2[,2])


#########################
plot_regression(simulation_reg1,real_a= real_a,real_b= real_b)
plot_residuals_hist(simulation_reg1)
plot_residuals(simulation_reg1)







simulation_reg<-regression_sim(N= 400,a=real_a,b=real_b, typeerror ='exponential', sd_chosen =NA,int=NA,lambda=10,degf=NA)



plot_regression(simulation_reg,real_a= real_a,real_b= real_b)
plot_residuals_hist(simulation_reg)
plot_residuals(simulation_reg)


simulation_reg<-regression_sim(N= 400,a=real_a,b=real_b, typeerror ='tdist', sd_chosen =NA,int=NA,lambda=NA,degf=2)



plot_regression(simulation_reg,real_a= real_a,real_b= real_b)
plot_residuals_hist(simulation_reg)
plot_residuals(simulation_reg)