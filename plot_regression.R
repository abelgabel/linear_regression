

plot_regression<-function(data_reg,real_a,real_b){
	
	
	lg<-lm(data_reg[,2]~ data_reg[,1])

	
	names(data_reg)<-c('x','y')
	
	
	
    a<-as.numeric(lg$coef[1])
	b<-as.numeric(lg$coef[2])
lg_plot<-ggplot(data_reg)+geom_point(aes(x=x,y=y))+geom_abline(aes(intercept=a,slope=b, colour='Regression line') )+geom_abline(aes(intercept=real_a,slope=real_b, colour='Real line'))+
theme(plot.title = element_text(size = rel(2.2), colour = "#4484CE", hjust=.5),   axis.text = element_text(size = 14),
    legend.key = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
	)+xlab('Independent variable')+ylab('Dependent variable')+scale_colour_manual("", breaks = c("Regression line", "Real line"), values = c("#4484CE", "black"))+labs(title='Regression')


lg_plot<-lg_plot+geom_segment(x= data_reg[,1],y= data_reg[,2], xend= data_reg[,1], yend= predict(lg, data_reg, interval = "prediction")[,1], color='red')	

		lg_plot
}