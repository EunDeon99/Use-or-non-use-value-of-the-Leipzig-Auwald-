library(simulateDCE)
library(apollo)
library(dplyr)


data <- readRDS("C:/Users/Jackal/Desktop/Master/Leipzig/Sem 3/Interdisplinary research/data1/full_data_dce_clean.rds") %>% 
  rename("ID" = "RID")


# Define your priors (true parameter values for simulation)
priors <- c(
 # b_30d = 0,      # reference level
  b_60d = 0.1,
  b_30dw = 0.2,
  b_60dw = 0.3,
  b_t2 = 0.5,
  b_price = -0.01,
  b_asc = -0.2    # status quo constant
)




ul <- list(
  u1 = list(
    
    # Alternative 1
    v1 = V.1 ~ 
      b_60d   * a1_60d   +
      b_30dw  * a1_30dw  +
      b_60dw  * a1_60dw  +
      b_t2    * a1_t2    + 
      b_price * a1_price ,
    
    # Alternative 2
    v2 = V.2 ~  
      b_60d   * a2_60d   +
      b_30dw  * a2_30dw  +
      b_60dw  * a2_60dw  +
      b_t2    * a2_t2    +
      b_price * a2_price,
    
    # Status quo
    v3 = V.3 ~  b_asc
  )
)

# Simulate data based on the defined priors

set.seed(1253)  # For reproducibility



runs = 200 
models = list()

for (r in seq_along(1:runs)) {


database <- simulate_choices(data,bcoeff =  priors, utility = ul,setspp = 12) # should be 12 but not necessarily





#### Apollo #####


##Initialize Apollo

apollo_initialise()


### Set core controls
apollo_control = list(
  modelName  = "Clogit_Model1",
  modelDescr = "WTP for measures - model1",
  indivID    ="ID",
  mixing     = FALSE,
  HB= FALSE,
  nCores     = 1, # Set to number of cores - 1,
  outputDirectory = "clogit"
)


## Definition of model beta parameters
apollo_beta=c(
#  b_30d = 0,
  b_60d = 0,
  b_30dw = 0,
  b_60dw = 0,
  b_t2 = 0,
  b_price = 0,
  b_asc = 0
)

### specify parameters that should be kept fixed, here = none
apollo_fixed = c()

### Validate Input
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below) as defined in the equation in the paper
  V = list()
  
  ##Model1 WTP for measures
  V[['alt1']] = b_price *  a1_price+ b_60d * a1_60d + b_30dw * a1_30dw + b_60dw * a1_60dw + b_t2 * a1_t2  # WTP
  V[['alt2']] = b_price *  a2_price+ b_60d * a2_60d + b_30dw * a2_30dw + b_60dw * a2_60dw + b_t2 * a2_t2  # WTP
  V[['alt3']] = b_asc
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3),
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = CHOICE, #choice,
    V             = V#,  # tell function to use list vector defined above
    
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  #P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}



# ################################################################# #
#### MODEL ESTIMATION                                            ##
# ################################################################# #


clogit = apollo_estimate(apollo_beta, apollo_fixed,
                         apollo_probabilities, apollo_inputs, 
                         estimate_settings=list(maxIterations=400,
                                                estimationRoutine="bfgs",
                                                hessianRoutine="analytic"))



# ################################################################# #
#### MODEL OUTPUTS                                               ##
# ################################################################# #

saveOutput_settings = list(
  printClassical = TRUE,
  printPVal = 1
) 

apollo_saveOutput(clogit, saveOutput_settings)



print(apollo_modelOutput(clogit))


m <-apollo_modelOutput(clogit)


res <- as.data.frame(round(cbind(true = as.double(priors), m),3))


models[[r]] <- res
}





#### summarize results ####




sum <- as.data.frame(models) %>% 
  tibble::rownames_to_column() %>% 
  rowwise() %>%
  transmute(Attribute = rowname, true,
            mean_estimates= mean(c_across(contains("Estimate"))),
            sd_estimates  = sd(c_across(contains("Estimate"))) ,
            min           = min(c_across(contains("Estimate"))) ,
            max           = max(c_across(contains("Estimate"))) ,
            mean_robt     = mean(c_across(contains("Rob.t")))     ,
            sd_robt       = sd(c_across(contains("Rob.t"))) ,
            count_above_1_96 = sum(abs(c_across(starts_with("Rob.t"))) > 1.96),
            share_above_1_96 = count_above_1_96 / length(c_across(starts_with("Rob.t")))
  ) %>%
  ungroup() 

sum