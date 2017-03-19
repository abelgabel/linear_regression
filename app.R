# #####################################################
#
#
#
# #####################################################
library(shiny)
library(ggplot2)
library(gridExtra)
library(grid)

# #####################################################
# Load functions
# #####################################################
 
folder_dir<-getwd()
source(paste0(folder_dir,'/simulations/create_regression.R'))
source(paste0(folder_dir,'/simulations/simulate_severalregression.R'))
source(paste0(folder_dir,'/plot_regression.R'))  
source(paste0(folder_dir,'/plot_residuals_hist.R'))  
source(paste0(folder_dir,'/plot_pred_residuals.R'))  
 source(paste0(folder_dir,'/plot_estimated_parameters.R'))  
 source(paste0(folder_dir,'/plot_estimated_parameters_normalised.R'))  



server <- function(input, output, session) {


 observeEvent(input$simulation, {


intercept<-input$intercept
slope<-input$slope
 	
typeerror<-input$selectdistribution
 sd_chosen<-input$sdev
 degf <-input$degf
 interv<-input$interv
 lambda<-input$lambda
 omittedvar<-input$omittedvar

 sample_size <-input$sample_size


intercept <-as.numeric(intercept)
slope <-as.numeric(slope)
sd_chosen <-as.numeric(sd_chosen)
degf<-as.numeric(degf)
interv<-as.numeric(interv)
lambda<-as.numeric(lambda)
omittedvar<-as.numeric(omittedvar)

N<-as.numeric(sample_size)
distribution_chosen<-input$distribution_chosen


    simulated_model<-regression_sim(N, a=intercept, b=slope, typeerror= typeerror , sd_chosen=sd_chosen, inter=interv, lambda =lambda, degf=degf, intercept2 =omittedvar)

                  
 	 # Plot
	 output$regression<-renderPlot({
  
plot_regression(simulated_model,intercept, slope)			 
	 })		
	 	 
	 output$residuals<-renderPlot({
plot_residuals_hist(simulated_model,intercept, slope)		
	 
	 })		 	 
	
	
	 output$pred_residuals <-renderPlot({
plot_residuals(simulated_model,intercept, slope)		
 })		

lg1<-lm(simulated_model[,2]~ simulated_model[,1])
results<-summary(lg1)$coef
results <-round(results,3)

results<-data.frame(results)
rownames(results)<-c('intercept','slope')
n<-nrow(results)+1
results[n,]<-'-----'
rownames(results)[n]<-''
n2<-n+1
results[n2,1]<-''
results[n2,2]<-round(summary(lg1)$r.squared,3)
results[n2,3:4]<-''

n3<-n2+1
results[n3,1]<-''
results[n3,2]<-summary(lg1)$df[2]
results[n3,3:4]<-''


rownames(results)[c(n2,n3)]<-c('R-squared','Degrees of freedom')

output$table <- renderTable({
results
 }, rownames=T)	
 	
 	
 	
############################################################################
 	
 	 	 # Plot
 
 observeEvent(input$simulation2, {
 	aaa<-input$simulation2
	simul2<-simulated_regression(N= N,a=intercept,b=slope, typeerror= typeerror, sd_chosen= sd_chosen, inter= interv,lambda= lambda,degf= degf, omittedvar= omittedvar)
	 	 
	 output$estimatedpar<-renderPlot({
plot_hist_estimpar(simul2)
	 
	 })		 	 
	
	
	 output$estimatedpar_normalised<-renderPlot({
plot_hist_estimpar_normal(simul2)
 })		
}) 	 	 
	 	
 #
 	}) 			
		
}



ui <- fluidPage(htmlTemplate( "simulations.html",
theme = "css/mystyle.css",



time_chosen=tags$input(id='time',type="number", value=10, min=5, max=500),

plot_regression=plotOutput('regression'),
plot_residuals=plotOutput('residuals'),

plot_predresiduals=plotOutput('pred_residuals'),
plot_estimatedpar=plotOutput('estimatedpar'),
plot_estimatedpar_normalised=plotOutput('estimatedpar_normalised'),
summary_reg=tableOutput('table')


)

  )
  
  
  shinyApp(ui=ui,server=server)