
############################################################################################
################### DATA IMPORT #########################################################
###########################################################################################

  #install.packages("tidyverse")
  #install.packages("readxl")   
  library(tidyverse)
  library(readxl)
  #Import de Assessments (assess) and Recommendation (recc) cases
  #source of data:  https://iac.university/
  # my local path: C:/UNC/PROJETOS DE PESQUISAS/PerformanceAnalytics
  # please change the path

# via API google drive

  #install.packages("googledrive")
  #library(googledrive)
  #emp <- tempfile(fileext = ".xls")
  #dl <- drive_download(as_id("1FUWGbGRCgWXV5bptQvy-Paj54hs9MHww"), path = temp, overwrite = TRUE)
  #assess <- read_excel(temp, sheet = "ASSESS")
  #recc1 <- read_excel(temp, sheet = "RECC1")
  #recc2 <- read_excel(temp, sheet = "RECC2")
  #recc3 <- read_excel(temp, sheet = "RECC3")
  #recc4 <- read_excel(temp, sheet = "RECC4")
  #recc5 <- read_excel(temp, sheet = "RECC5")

  # via local folder
  assess <- read_excel("IAC_Database_20210429.xls", sheet = "ASSESS")
  recc1 <- read_excel("IAC_Database_20210429.xls", sheet = "RECC1")
  recc2 <- read_excel("IAC_Database_20210429.xls", sheet = "RECC2")
  recc3 <- read_excel("IAC_Database_20210429.xls", sheet = "RECC3")
  recc4 <- read_excel("IAC_Database_20210429.xls", sheet = "RECC4")
  recc5 <- read_excel("IAC_Database_20210429.xls", sheet = "RECC5")
  
    #Joining the recommendations
  recc <- rbind(recc1, recc2, recc3,recc4,recc5)
  dim(recc)

############################################################################################
################### DATA PROCESSING #########################################################
###########################################################################################

  #Please, see Sheet Terms in Excel file (https://iac.university/download)
  #filter I - implemented N - not implemented
  #filter PSAVED (Cost savings) > 0 
  #filter IMPCOST (Total implementation cost) > 0
  #group by ID (Assessment ID)
  # summarize by ID - sum of: AR_NUMBER (Recommendation Number); IMPCOST (Total implementation cost); PCONSERVED (Units conserved)
  # PSAVED (Cost savings); SCONSERVED (Units conserved); SSOURCONSV (Source units conserved); SSAVED (Cost savings)
  #TCONSERVED (Units conserved); TSOURCONSV (Source units conserved); TSAVED (Cost savings); QCONSERVED (Units conserved)
  #QSOURCONSV (Source units conserved); QSAVED (Cost savings); PAYBACK (Simple Payback (years))

  trat <- recc %>% filter(IMPSTATUS %in% c("I", "N") & PSAVED > 0 & IMPCOST > 0 ) %>%
  group_by (ID) %>%
  summarize (AR_NUMBER=sum(AR_NUMBER), IMPCOST =sum(IMPCOST), PCONSERVED=sum(PCONSERVED), PSOURCONSV=sum(PSOURCONSV), PSAVED=sum(PSAVED), SCONSERVED=sum(SCONSERVED),
             SSOURCONSV=sum(SSOURCONSV), SSAVED=sum(SSAVED), TCONSERVED=sum(TCONSERVED), TSOURCONSV=sum(TSOURCONSV), TSAVED=sum(TSAVED), QCONSERVED=sum(QCONSERVED), 
             QSOURCONSV=sum(QSOURCONSV),QSAVED=sum(QSAVED), PAYBACK=mean(PAYBACK) )
  head(trat)

  # inner join of assess with trat and 
  # filter SALES >=10000
  # filter EMPLOYEES >= 50 and not missing (NA)
  # filter PRODHOURS > 2000 (Total yearly hours of operation) and not missing (NA)
  # filter EC_plant_usage not missing (NA) (Total yearly electricity consumption (kWh))
  # filter EC_plant_cost not missing (NA) (Total yearly electricity consumption costs ($))
  # filter E2_plant_usage)  not missing (NA) and > 1000  (Total yearly natural gas consumption (MMBtu))
  # filter E2_plant_cost not missing (NA) (Total yearly natural gas consumption costs ($))

  inn <- assess %>%  inner_join(trat, by = "ID") %>% filter( SALES >= 10000  & EMPLOYEES >= 50 & !is.na(EMPLOYEES) & PRODHOURS > 2000 & !is.na(PRODHOURS)
                             & !is.na(EC_plant_usage) & !is.na(EC_plant_cost) & !is.na(E2_plant_usage) & E2_plant_usage > 1000 & !is.na(E2_plant_cost)) 
  dim(inn)

  #rename variables: USAGE_ELEC=EC_plant_usage; USAGE_NAT=E2_plant_usage; COST_ELEC=EC_plant_cost; COST_NAT=E2_plant_cost
  inn2 <- inn %>% select(SALES, EMPLOYEES, USAGE_ELEC=EC_plant_usage, USAGE_NAT=E2_plant_usage, PRODHOURS, COST_ELEC=EC_plant_cost, COST_NAT=E2_plant_cost)
  dim(inn2)

  # elimination outliers - SALES
  outliers1 <- boxplot(inn2$SALES, plot=FALSE)$out
  inn3 <- inn2[-which(inn2$SALES %in% outliers1),]
  dim(inn3)

  # elimination outliers - EMPLOYEES
  outliers2 <- boxplot(inn3$EMPLOYEES, plot=FALSE)$out
  inn4 <- inn3[-which(inn3$EMPLOYEES %in% outliers2),]
  dim(inn4)

  # elimination outliers - USAGE_ELEC
  outliers3 <- boxplot(inn4$USAGE_ELEC, plot=FALSE)$out
  inn5 <- inn4[-which(inn4$USAGE_ELEC %in% outliers3),]
  dim(inn5)

  # elimination outliers - USAGE_NAT
  outliers4 <- boxplot(inn5$USAGE_NAT, plot=FALSE)$out
  inn6 <- inn5[-which(inn5$USAGE_NAT %in% outliers4),]
  dim(inn6)

  # elimination outliers - PRODHOURS
  outliers5 <- boxplot(inn6$PRODHOURS, plot=FALSE)$out
  inn7 <- inn6[-which(inn6$PRODHOURS %in% outliers5),]
  dim(inn7)

  # elimination outliers - COST_ELEC
  outliers6 <- boxplot(inn7$COST_ELEC, plot=FALSE)$out
  inn8 <- inn7[-which(inn7$COST_ELEC %in% outliers6),]
  dim(inn8)

  # elimination outliers - COST_NAT
  outliers7 <- boxplot(inn8$COST_NAT, plot=FALSE)$out
  inn9 <- inn8[-which(inn8$COST_NAT %in% outliers7),]
  dim(inn9)

  # final data
  head(inn9)
  dim(inn9) #7549 lines and 7 columns

############################################################################################
###################STOCHASTIC FRONTIER ANALYSIS ###########################################
###########################################################################################

  #install.packages("Benchmarking")
  #install.packages("frontier")

  #OUTPUT - SALES
  #INPUT - EMPLOYEES
  #INPUT - USAGE_ELEC (Total yearly electricity consumption (kWh))
  #INPUT - USAGE_NAT (Total yearly natural gas consumption (MMBtu))
  #INPUT - PRODHOURS (Total yearly hours of operation)
  #INPUT - COST_ELEC (Total yearly electricity consumption costs ($))
  #INPUT -  COST_NAT (Total yearly natural gas consumption costs ($))

  #sfa lin-log
  x <- with(inn9, cbind( EMPLOYEES, USAGE_ELEC, USAGE_NAT, PRODHOURS, COST_ELEC, COST_NAT)) #inputs
  y <- with(inn9,(SALES)) # output
  iac <- Benchmarking::sfa(log(x), log(y))
  summary(iac)
  e_sfa_log <- Benchmarking::te.sfa(iac)

  #sfa translog
  mp <- data.frame(SALES=(y), x) # data for SFA
  #converting to log
  mp$lSALES <- log(mp$SALES)
  mp$lEMPLOYEES <- log(mp$EMPLOYEES)
  mp$lUSAGE_ELEC <- log(mp$USAGE_ELEC)
  mp$lUSAGE_NAT <- log(mp$USAGE_NAT)
  mp$lPRODHOURS <- log(mp$PRODHOURS)
  mp$lCOST_ELEC <- log(mp$COST_ELEC)
  mp$lCOST_NAT <- log(mp$COST_NAT)
  head(mp)

  tl <- frontier::sfa(lSALES ~ lEMPLOYEES + lUSAGE_ELEC + lUSAGE_NAT + lPRODHOURS + lCOST_ELEC + lCOST_NAT+
                      I(0.5*lEMPLOYEES^2)+I(0.5*lUSAGE_ELEC^2)+I(0.5*lUSAGE_NAT^2)+I(0.5*lPRODHOURS^2)+I(0.5*lCOST_ELEC^2)+I(0.5*lCOST_NAT^2)+
                      I(lEMPLOYEES*lUSAGE_ELEC)+I(lEMPLOYEES*lUSAGE_NAT)+I(lEMPLOYEES*lPRODHOURS)+I(lEMPLOYEES*lCOST_ELEC)+I(lEMPLOYEES*lCOST_NAT)+
                      I(lUSAGE_ELEC*lUSAGE_NAT)+I(lUSAGE_ELEC*lPRODHOURS)+I(lUSAGE_ELEC*lCOST_ELEC)+I(lUSAGE_ELEC*lCOST_NAT)+
                      I(lUSAGE_NAT*lPRODHOURS)+I(lUSAGE_NAT*lCOST_ELEC)+I(lUSAGE_NAT*lCOST_NAT)+I(lPRODHOURS*lCOST_ELEC)+I(lPRODHOURS*lCOST_NAT)+
                      I(lCOST_ELEC*lCOST_NAT),data=mp, timeEffect = FALSE)
  summary(tl)
  e_sfa_tl  <- frontier::efficiencies(tl)


############################################################################################
################### DATA ENVELOPMENT ANALYSIS ###########################################
###########################################################################################


  #RTS - RETURN TO SCALE - can be (drs, crs, irs, vrs for article )
  te <- Benchmarking::dea(x,y,RTS="vrs") # approximately 1 minute to compute - DELL i7, 8 gig RAM
  dea_eff <- te$eff
  summary(te)

############################################################################################
############################### DICRETIZATION ###########################################
###########################################################################################

  # function to place the data on the scale 0-1
  normFun <- function(x){(x-min(x))/(max(x)-min(x))}
  # TGH = continuous scale (efficiencies)
  TGH <- normFun(dea_eff) # can be: e_sfa_tl, e_sfa_log and DEA (drs, crs, irs)
  #install.packages('arules')
  library(arules)
  # TGH_cat - discretized scale (eficiencies)
  # breaks in article is 2, 3, 5 and 10
  TGH_cat <- discretize(TGH, method="interval", breaks=10, labels=c( 'J', 'I', 'H', 'G','F', 'E', 'D', 'C', 'B', 'A'))
  # another strategy for discretization was developed (based on the article by Hong et al. (1999), tier strategy)
  # this code can be shared later (I have to organize it, yet).

  # make variables visible in the environment
  attach(inn9)

  # generate new data fame with efficiencies variables: TGH and TGH_cat
  dfDrive <- data.frame(EMPLOYEES, USAGE_ELEC, USAGE_NAT, PRODHOURS, COST_ELEC, COST_NAT, SALES, TGH, TGH_cat)

############################################################################################
################### MACHINE LEARNING #########################################################
###########################################################################################

  # NOTE: in the machine learning process the target is TGH for regression and
  # TGH_cat for classification. The inputs and output are attributes.
  # please see Table 01 of article (Tabela 01)

  #install.packages("mlr3verse")
  library("mlr3verse")

  #install.packages("e1071")  
  #install.packages("ranger") 
  #install.packages("kknn") 
  library("e1071")
  library(ranger) 
  library(kknn)   


  ##################################################################

  #Machine learning - Regression problem - package mlr3
  #https://mlr3book.mlr-org.com/

  # data frame for ML regression
  df <- data.frame(EMPLOYEES, USAGE_ELEC, USAGE_NAT, PRODHOURS, COST_ELEC, COST_NAT, SALES, TGH)
  # task - type data organization for ml 
  task1 = TaskRegr$new(id = "desemp", backend = df, target = "TGH")

  #learning algorithms - please see https://mlr3book.mlr-org.com/
  learnerA = lrn("regr.lm") #(LM - Linear model)                                                                                                
  learnerB = lrn("regr.rpart") # (DT -Decision tree)
  learnerC = lrn("regr.ranger") # (RF - Ramdom forest)
  learnerD = lrn("regr.svm") # (SVM - Support vector machine)
  learnerE = lrn("regr.kknn") # (KNN - k-nearest-neighbor)

  #resampling strategy - cross validation (cv)
  # 5 folds for training and testing
  cv5 = rsmp("cv", folds = 5)
  #list of learners
  learners = list(learnerA, learnerB, learnerC, learnerD, learnerE )
  #benchmarking process
  bm_grid = benchmark_grid(task1, learners, cv5)
  bm = benchmark(bm_grid) # 01 min
  #print measure of benchmarking
  print(bm$aggregate(measures = msrs(c("regr.mse", "regr.mae","regr.bias", "regr.srho"))))

  ##################################################################
  #Machine learning - Classification problem - package mlr3
  #https://mlr3book.mlr-org.com/

  # data frame for ML classification
  dfc1 <- data.frame(EMPLOYEES, USAGE_ELEC, USAGE_NAT, PRODHOURS, COST_ELEC, COST_NAT, SALES, TGH_cat)

  # task - type data organization for ml
  task2 = TaskClassif$new(id = "desemp", backend = dfc1, target = "TGH_cat")

  #learning algorithms - please see https://mlr3book.mlr-org.com/
  learnerF = lrn("classif.lda") #(LDA - Linear discriminant analysis)
  learnerG = lrn("classif.rpart") #(DT -Decision tree)
  learnerH = lrn("classif.ranger") #(RF - Ramdom forest)
  learnerI = lrn("classif.svm" ) #(SVM - Support vector machine)
  learnerJ = lrn("classif.kknn") #(KNN - k-nearest-neighbor)
  #resampling strategy - cross validation (cv)
  # 5 folds for training and testing
  cv5 = rsmp("cv", folds = 5)

  # list of learners
  learners = list(learnerF, learnerG, learnerH, learnerI, learnerJ )
  #benchmarking process
  bm_grid = benchmark_grid(task2, learners, cv5)
  bm = benchmark(bm_grid) # 1 min
  #print measure of benchmarking
  print(bm$aggregate(measures = msrs(c("classif.acc", "classif.ce"))))

#######################################################################
##################classification problem #############################
#####Machine Learning with optimized hyperparameters #############
#############################################################

  #task
  task3 = TaskClassif$new(id = "desemp", backend = dfc1, target = "TGH_cat")
  #machine learning - roudout strategy
  # 70% of data for train
  train_set = sample(seq_len(task3$nrow), 0.7 * task3$nrow)
  # 30% of data for test
  test_set = setdiff(seq_len(task3$nrow), train_set)
  #Ramdom forest learner
  learnerC5 = lrn("classif.ranger") 
  # set new hyperparameters ( please see bellow)
  learnerC5$param_set$values$num.trees =1667 # number of trees
  learnerC5$param_set$values$alpha =0.7  # Cost-Complexity Pruning
  learnerC5$param_set$values$max.depth =278  # tree depth
  learnerC5$param_set$values$mtry =4  # number of variables available for division in each tree node
  learnerC5$train(task3, train_set)
  # predict with test set
  preds = learnerC5$predict(task3, test_set)
  # metrics accuracy (acc) and classification error (ce)
  s = preds$score(msrs(c("classif.acc", "classif.ce")))
  print(s)
  # confusion matrix
  conf <- preds$confusion
  print(conf)


  # Hyperparameter Tuning of Ramdom forest
  # Please see chapter 3 of https://mlr3book.mlr-org.com/tuning.html
  learner1 = lrn("classif.ranger") # learner
  learner1$param_set # ParamSet (atual values)

  # Tuning Search Spaces
  search_space = ps(
  alpha = p_dbl(lower = 0.1, upper = 1),
  num.trees = p_int(lower = 1, upper = 5000),
  max.depth = p_int(lower = 1, upper = 500),
  mtry = p_int(lower = 1, upper = 6))

  # resample strategy
  hout = rsmp("holdout",  ratio = 0.7)
  #measure
  measure = msr("classif.acc")
  # evaluation number
  evals20 = trm("evals", n_evals = 20)

  #Tuning problem 
  instance = TuningInstanceSingleCrit$new(
  task = task3,
  learner = learner1,
  resampling = hout,
  measure = measure,
  search_space = search_space,
  terminator = evals20)

  # tuning strategy - can be Random Search, Generalized Simulated Annealing
  #Non-Linear Optimization - https://mlr3book.mlr-org.com/tuning.html
  tuner = tnr("grid_search")
  # Optmization
  tuner$optimize(instance) # 05 minutes
  # best hyperparameters
  instance$result_learner_param_vals
  
  
  
  
