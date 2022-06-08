library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(readxl)

# "SQ" stands for "Status Quo" within this code

####Input data####

input_estimates <- data.frame(variable = c("Education_investment",
                                     "Economy_investment", "Economy_payout",
                                     "SQ_Resources_investment", 
                                     "SQ_Resources_payout",
                                     "Empowerment_Resources_investment", 
                                     "Empowerment_Resources_payout",
                                     "SQ_Workforce_investment",
                                     "SQ_Workforce_payout",
                                     "Empowerment_Workforce_investment",
                                     "Empowerment_Workforce_payout",
                                     "SQ_Husband_Workforce_investment",
                                     "Husband_Empowerment_Workforce_investment",
                                     "var_slight", "discount_rate",
                                     "payout_months", "investment_months",
                                     "safety_risk"),
                              lower = c(10,1,50,30,20,30,200,50,30,50,300,50,10,
                                        1,1,9,3,0.5),
                              median = NA,
                              upper = c(50,20,200,100,90,100,300,100,100,100,
                                        1000,100,50,1,1,9,3,0.5),
                              distribution = c("posnorm","posnorm","posnorm",
                                               "posnorm","posnorm","posnorm",
                                               "posnorm","posnorm","posnorm",
                                               "posnorm","posnorm","posnorm",
                                               "posnorm",
                                               "const","const","const","const",
                                               "const"),
                              label = c("Education investment (Dollar/Month)",
                     "Economy investment (Dollar/Month)",
                     "Economy payout (Dollar/Month)",
                     "Status Quo Resources investment (Dollar/Month)",
                     "Status Quo Resources payout (Dollar/Month)",
                     "Empowerment Resources investment (Dollar/Month)",
                     "Empowerment payout (Dollar/Month)",
                     "Status Quo Workforce investment (Dollar/Month)",
                     "Status Quo Workforce payout (Dollar/Month)",
                     "Empowerment Workforce investment (Dollar/Month)",
                     "Empowerment Workforce payout (Dollar/Month)",
                     "SQ Husband's Workforce investment (Dollar/Month)",
                     "Husband's Workforce investment (Dollar/Month)",
                     "Coefficient of variation",
                     "Discout rate",
                     "Months of receiving money (Dollar/Month)",
                     "Months of paying into empowerment efforts (Dollar/Month)",
                     "Risk Safety"),
                              Description = c("Education investment",
                                    "Economy investment",
                                    "Economy payout",
                                    "Status Quo Resources investment",
                                    "Status Quo Resources payout",
                                    "Empowerment Resources investment",
                                    "Empowerment Resources payout",
                                    "Status Quo Workforce investment",
                                    "Status Quo Workforce payout",
                                    "Empowerment Workforce investment",
                                    "Empowerment Workforce payout",
                                    "SQ Husband's Workforce investment",
                                    "Husband's Workforce investment",
                                    "Coefficient of variation",
                                    "Discout rate",
                                    "Months of receiving money",
                                    "Months of paying into empowerment efforts",
                                    "Risk Safety"))

input_estimates <- input_estimates %>%
  mutate(variable = as.character(variable),
         distribution = as.character(distribution),
         label = as.character(label),
         Description = as.character(Description))

#alternative for using the excel sheet

#input_table <-read_excel("inputs-femiaculture.xlsx")

#input_table <- input_table %>% 
#  mutate(Description = as.character(Description),
#         label = as.character(label),
#         variable = as.character(variable),
#         distribution = as.character(distribution),
#         lower = as.numeric(lower),
#         median = as.numeric(median),
#        upper = as.numeric(upper))





#Reminder about the make_variables function

# make_variables <- function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i,
#                               as.numeric(x[1,i]),envir=.GlobalEnv)
# }#Then call:
#   make_variables(as.estimate(input_table_gender))




####Start of decision function####

decision_function <- function(x, varnames){
  
  #Risk
  safety_payout <- chance_event(safety_risk, 1, 0, n = payout_months)
  safety_inv <- chance_event(safety_risk, 1, 0, n = investment_months)
  
  #Education
  Education_investment_A <-Education_investment * (1-safety_inv)
  
  Education_investment <- c(vv(var_mean = Education_investment_A, 
                          var_CV = var_slight, 
                          n = investment_months), rep(0,payout_months))
  #Economy
  Economy_payout_A <- Economy_payout * (1-safety_payout)
  
  Economy_payout <- c(rep (0,investment_months), 
                    vv(var_mean = Economy_payout_A, 
                    var_CV = var_slight, 
                    n = payout_months))
  
  Economy_investment_A <- Economy_investment * (1-safety_inv)
  
  Economy_investment <- c(vv(var_mean = Economy_investment_A, 
                        var_CV = var_slight, 
                        n = investment_months), rep(0,payout_months))

  #Status Quo Resources
  SQ_Resources_investment <- c(vv(var_mean = SQ_Resources_investment, 
                             var_CV = var_slight, 
                             n = investment_months), rep(0,payout_months))
  
  SQ_Resources_payout <- c(rep (0,investment_months),
                         vv(var_mean = SQ_Resources_payout, 
                         var_CV = var_slight, 
                         n = payout_months))
  #Empowerment Resources
  Empowerment_Resources_investment <- c(vv(var_mean = 
                                      Empowerment_Resources_investment, 
                                      var_CV = var_slight, 
                                      n = investment_months), 
                                      rep(0,payout_months))
  
  Empowerment_Resources_payout <- c(rep (0,investment_months),
                                  vv(var_mean = Empowerment_Resources_payout, 
                                  var_CV = var_slight, 
                                  n = payout_months))
  #Status Quo monthly Workforce
  SQ_Workforce_investment_A <-  SQ_Workforce_investment * (1-safety_inv)
  
  SQ_Workforce_investment <- c( vv(var_mean = SQ_Workforce_investment_A, 
                             var_CV = var_slight, 
                             n = investment_months), 
                             rep(0,payout_months))
  
  SQ_Workforce_payout <- c(rep (0,investment_months),
                         vv(var_mean = SQ_Workforce_payout, 
                         var_CV = var_slight, 
                         n = payout_months))

  #Empowerment monthly Workforce
  Empowerment_Workforce_investment <- c( vv(var_mean = 
                                              Empowerment_Workforce_investment, 
                                var_CV = var_slight, 
                                n = investment_months), rep(0,payout_months))
  
  Empowerment_Workforce_payout <- c(rep (0,investment_months),
                        vv(var_mean = Empowerment_Workforce_payout, 
                           var_CV = var_slight, 
                           n = payout_months))
  
  # Husband's investment
  SQ_Husband_Workforce_investment <- c( vv(var_mean = 
                                     SQ_Husband_Workforce_investment, 
                                     var_CV = var_slight, 
                                     n = investment_months),
                                     rep(0,payout_months))
  
  Husband_Empowerment_Workforce_investment <- c(rep (0,investment_months),
                                      vv(var_mean = 
                                      Husband_Empowerment_Workforce_investment, 
                                      var_CV = var_slight, 
                                      n = payout_months))
  
####Pathway calculations####  
###Computing the decision and Status Quo pathways###
#1 existent Branch: Status Quo pathway vs. Empowerment pathway#
  
##Status Quo pathway##
  
  PartA <- SQ_Workforce_payout + SQ_Resources_payout 
  PartB <- SQ_Resources_investment + SQ_Workforce_investment 
           + SQ_Husband_Workforce_investment
  Profit_SQ <- (PartA -PartB)
  
# It can be dangerous to use the money for herself, instead of the family.
# Women might be dependent on their husbands for health care and food. 
# This calculation shows how much money a woman would
# have for health care and food investments (= workforce investment)
  
#Computing the Status Quo NPV (Net present value)#
  
  NPV_no_empowerment_branch <- discount(Profit_SQ,
                            discount_rate = discount_rate, calculate_NPV = TRUE) 
  
##Empowerment pathway##
  
  PartA <- Economy_payout + Empowerment_Resources_payout + 
           Empowerment_Workforce_payout
  PartB <- Education_investment  
           + Economy_investment + Empowerment_Resources_investment + 
           Empowerment_Workforce_investment
           + Husband_Empowerment_Workforce_investment
  
# Safety risks occur for: Education and economy investments since time away
# from female connotated tasks might risk violence. 
# having her own money and not giving it to the husband or family might also
# be a risk for violence. 
  
# Husband's investment into food and health care (workforce investment)
# might be smaller within the empowerment pathway than status quo.
  
  
#Computing the Empowerment NPV (Net present value)#
  
  Empowerment_profit <-  (PartA - PartB)
  
  NPV_Empowerment_profit <- discount(Empowerment_profit,
                            discount_rate = discount_rate, calculate_NPV = TRUE)
  NPV_decision_profit_with_Empowerment <- NPV_Empowerment_profit - 
                                          NPV_no_empowerment_branch

  
####Return list####
  
  return(list(NPV_no_empowerment_branch =  NPV_no_empowerment_branch,
              NPV_Empowerment_profit = NPV_Empowerment_profit, 
    NPV_decision_profit_with_Empowerment = NPV_decision_profit_with_Empowerment,
    Cashflow_decision_empowerment =  Empowerment_profit
              
  )) 
  
}

####Monte Carlo Simulation####
mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_estimates),
  model_function = decision_function,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames"
)

####Plot NPV distributions####

#Plot Net Present Value (NPV) distributions
#We can use the plot_distributions() function to produce 
#one of the several plotting options for distribution outputs.
#There we show an overlay of the full results of the 
#Monte Carlo simulation (200 model runs) of the decision options, 
#i.e. the expected NPV if we choose to do the
#intervention. 

#Plot empowerment pathway
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                     vars = c("NPV_decision_profit_with_Empowerment" ),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)
#plot both
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_Empowerment",
                                             "NPV_no_empowerment_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

#Plot distributions one by one
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                     vars = "NPV_no_empowerment_branch",
                                     method = 'boxplot_density')

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision_profit_with_Empowerment",
                                    method = 'boxplot_density')


####Boxplots####

# We can use the same function to show the distributions of the
# decisions as boxplots. Boxplots show the median (central line), 
# the 25th and 75th percentiles (sides of boxes) and any outliers 
# (light circles outside of boxes).

#'boxplot' empowerment pathway
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                      vars = c("NPV_decision_profit_with_Empowerment"
                                    ),
                                    method = 'boxplot', 
                                    base_size = 7)


####Cashflow analysis####

#Here we plot the distribution of annual cashflow over the 
#entire simulated period for the intervention (n_months). 
#For this we use the plot_cashflow() function which uses the 
#specified cashflow outputs from the mcSimulation() function 
#(in our case Cashflow_decision_do) to show cashflow over time.


Cashflow <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                           cashflow_var_name = "Cashflow_decision_empowerment",
                           x_axis_name = "Month",
                           y_axis_name = "Cashflow in Dollar",
                           color_25_75 = "green4",
                           color_5_95 = "green1",
                           color_median = "red")

Cashflow

####PLS####

#We apply a post-hoc analysis to the mcSimulation() outputs
#with plsr.mcSimulation() to determine the Variable 
#Importance in the Projection (VIP) score and coefficients of 
#a Projection to Latent Structures (PLS) regression model. 
#This function uses the outputs of the mcSimulation() selecting
#all the input variables from the decision analysis function 
#in the parameter object and then runs a PLS regression with an 
#outcome variable defined in the parameter resultName. 
#We use the code names(mcSimulation_results$y)[n] to select the
#correct results. 
# Here we provide also a legend of the objects 
# in  mcSimulation_results$y

names(mcSimulation_results$x)
names(mcSimulation_results$y)


#	Pls of	 "NPV_decision_profit_with_Own_business_branch_1"

#pls_result_1 <- plsr.mcSimulation(object = mcSimulation_results,
#                                resultName = names(mcSimulation_results$y)[3],
#                                ncomp = 1)

#or...

pls_result_1 <- plsr.mcSimulation(object = mcSimulation_results,
                                  resultName = "NPV_decision_profit_with_Empowerment",
                                  ncomp = 1)


#We run the plot_pls() on the results from plsr.mcSimulation() 
#The colors of the bars represent the positive or negative coefficient 
#of the given input variable with the output variable.

plot_pls(pls_result_1, threshold = 0.8, input_table = input_estimates)


####EVPI####

# We calculate Value of Information (VoI) analysis 
# with the Expected Value of Perfect Information (EVPI). 
# As we learned in Lecture 8 on forecasts, EVPI measures 
# the expected opportunity loss that is incurred when the decision-maker 
# does not have perfect information about a particular variable. 
# EVPI is determined by examining the influence of that 
#variable on the output value of a decision model.
mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, 
                 first_out_var = "NPV_Empowerment_profit")
plot_evpi<-plot_evpi(evpi,
          decision_vars = "NPV_decision_profit_with_Empowerment")
#Check
names(mcSimulation_results$x)
names(mcSimulation_results$y[1:3])

colnames(mcSimulation_results$y)

###########################################################
chance_event?
?decisionSupport
?multi_EVPI
?plsr.mcSimulation
?stat_density

?plot_cashflow
?var_CV
var_CV
?input$slider
