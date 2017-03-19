
plot_hist_estimpar_normal<-function(simul){
	 
gplot1<-ggplot(simul, aes(est_inter_norm))+geom_histogram(fill='#4484CE', color='#F9CF00')+theme( plot.title = element_text(size = rel(2.2), colour = "#4484CE", hjust=.5),  axis.text = element_text(size = 14),
    legend.key = element_rect(fill = "gray80"),
    legend.background = element_rect(fill = "white"),
    legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
	)+xlab('')+ylab('') +labs(title = expression((hat(alpha)-alpha)/hat(sd)(hat(alpha)))) 




gplot2<-ggplot(simul, aes(est_slope_norm))+geom_histogram(fill='#4484CE', color='#F9CF00')+theme( plot.title = element_text(size = rel(2.2), colour = "#4484CE", hjust=.5),  axis.text = element_text(size = 14),
    legend.key = element_rect(fill = "gray80"),
    legend.background = element_rect(fill = "white"),
    legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
	)+xlab('')+ylab('') +labs(title =expression((hat(beta)-beta)/hat(sd)(hat(beta))))


grid.arrange(gplot1, gplot2, ncol=2)
}
