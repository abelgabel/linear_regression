
 
simulated_regression<-function(N,a,b, typeerror, sd_chosen, inter,lambda,degf, omittedvar, corrOVM, corrOVMpositive){
estimated<-matrix(0, nrow=250, ncol=2)
estimated2<-matrix(0, nrow=250, ncol=2)
for(i in 1:250){

# exponential tdistribution
simulation_reg2<-regression_sim(N= N,a=a,b=b, typeerror = typeerror, sd_chosen = sd_chosen, inter= inter,lambda= lambda,degf= degf, intercept2=omittedvar, corrOVM= corrOVM, corrOVMpositive= corrOVM)


lg2<-lm(simulation_reg2[,2]~ simulation_reg2[,1])
estimated[i,]<-summary(lg2)$coef[,1]
estimated2[i,]<-summary(lg2)$coef[,2]

}

estim<-(estimated[,1]-a)/estimated2[,1]
estim2<-(estimated[,2]-b)/estimated2[,2]


output<-matrix(0,nrow=250, ncol=4)

output[,1:2]<-estimated
output[,3]<-estim
output[,4]<-estim2

output<-data.frame(output)
names(output)<-c('est_intercept','est_slope','est_inter_norm','est_slope_norm')

return(output)
}
