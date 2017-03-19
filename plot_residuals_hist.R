

plot_residuals_hist<-function(data_reg,real_a,real_b){
	
	
	lg<-lm(data_reg[,2]~ data_reg[,1])

	N<-nrow(data_reg)
	 data_reg<-matrix(0,nrow=N,ncol=2)
	data_reg[,1]<-as.numeric(lg$fitted)
	data_reg[,2]<-as.numeric(lg$residuals)
	data_reg<-data.frame(data_reg)
	names(data_reg)<-c('pred','res') 
lg_plot<-ggplot(data_reg, aes(res))+geom_histogram(fill='#4484CE', color='#F9CF00')+theme( plot.title = element_text(size = rel(2.2), colour = "#4484CE", hjust=.5),  axis.text = element_text(size = 14),
    legend.key = element_rect(fill = "gray80"),
    legend.background = element_rect(fill = "white"),
    legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
	)+xlab('Residuals')+ylab('')+labs(title='Histogram of Residuals')



		lg_plot
}