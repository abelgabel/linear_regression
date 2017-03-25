
regression_sim<-function(N,a,b, typeerror, sd_chosen,inter,lambda,degf,intercept2){

# fixed x
x<-seq(5,25,0.05)

x<-sample(x,N, replace=T)



if(typeerror=='normal'){
	error<-rnorm(N,0,sd_chosen)
}
if(typeerror=='exponential'){
	error<-rexp(N, lambda)-1/lambda
}

if(typeerror =='uniform'){
	error<-runif(N, min=-inter,max= inter)
}

if(typeerror =='tdistribution'){
	error<-rt(N,df= degf)
}

if(typeerror =='omittedvar'){
	
	z<-rnorm(N,0,1)
    # z<-sample(z,N, replace=T)
	error<-rnorm(N,0,1)+ intercept2*z
}

y<- b*x+a+error



data_reg<-matrix(0, nrow=N , ncol=2)
data_reg[,1]<-x
data_reg[,2]<-y
data_reg<-data.frame(data_reg)
names(data_reg)<-c('x','y')


return(data_reg)
}