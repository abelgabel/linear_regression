

plot_residuals<-function(data_reg,real_a,real_b){
	
	
	lg<-lm(data_reg[,2]~ data_reg[,1])

	N<-nrow(data_reg)
	 data_reg<-matrix(0,nrow=N,ncol=2)
	data_reg[,1]<-as.numeric(lg$fitted)
	data_reg[,2]<-as.numeric(lg$residuals)
	data_reg<-data.frame(data_reg)
	names(data_reg)<-c('pred','res') 
lg_plot<-ggplot(data_reg)+geom_point(aes(x=pred,y=res))+geom_abline(intercept=0,slope=0)+theme( plot.title = element_text(size = rel(2.2), colour = "#4484CE", hjust=.5),  axis.text = element_text(size = 14),
    legend.key = element_rect(fill = "gray80"),
    legend.background = element_rect(fill = "white"),
    legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
	)+xlab('Predicted values')+ylab('Residuals')+labs(title='Residuals versus Fitted values')


lg_plot<-lg_plot+geom_segment(x=data_reg[,1],y= 0, xend= data_reg[,1], yend= data_reg[,2], color='red')	

		lg_plot
}