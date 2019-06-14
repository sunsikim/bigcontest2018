
# 0. Attach packages ------------------------------------------------------

# Basic Setup
library(dplyr)
library(foreach)
library(doParallel)

# For NA imputation
library(missForest)

# For RETIRE_NEED imputation
library(glmnet)       
library(randomForest)

# For PAM clustering
library(cluster)      

# For Bayesian Network learning
library(bnlearn)      
# if(!'gRain' %in% rownames(installed.packages())){
#   source('http://bioconductor.org/biocLite.R')
#   biocLite('gRain')
# }
library(gRain)

# 1. Data preprocessing ---------------------------------------------------

sampler <- function (data, ratio = 0.8, iamseed = 77) {
  if (is.numeric(iamseed)) set.seed(iamseed)
  traindex <- sample(1:nrow(data), round(nrow(data) * ratio, 0))
  train77 <<- data[traindex, ]
  testindex <- !1:nrow(data) %in% traindex
  test77 <<- data[testindex, ]
}

generate_eda <- function (include_index=FALSE) {
  # Integrated process of data processing as a result of EDA
  # One might skip this definition since it is redundant to the actual analysis process
  # (and thus not described specifically; read submitted ppt for more detail).
  eda <- read.csv('rawdata.csv', header=T)
  eda[is.na(eda$CHUNG_Y), ]$CHUNG_Y <- 'N'
  eda[eda$CHUNG_Y=='5', ]$CHUNG_Y   <- 'Y'
  eda[is.na(eda$DOUBLE_IN), ]$DOUBLE_IN <- 3
  eda[is.na(eda$NUMCHILD), ]$NUMCHILD   <- 0
  
  eda$SEX_GBN    <- factor(eda$SEX_GBN)
  eda$AGE_GBN    <- factor(eda$AGE_GBN)
  eda$JOB_GBN    <- factor(eda$JOB_GBN)
  eda$ADD_GBN    <- factor(eda$ADD_GBN)
  eda$INCOME_GBN <- factor(eda$INCOME_GBN)
  eda$DOUBLE_IN  <- factor(eda$DOUBLE_IN)
  eda$CHUNG_Y    <- factor(eda$CHUNG_Y)
  eda$MARRY_Y    <- factor(eda$MARRY_Y, levels = 1:3)
  eda$NUMCHILD   <- factor(eda$NUMCHILD, levels = 0:4)
  
  for(i in 1:length(eda)){
    if(is.integer(eda[,i])) eda[,i] <- as.numeric(eda[,i])
  }
  if(!include_index) eda <- eda %>% dplyr::select(-idx)
  eda <- eda %>% dplyr::select(-TOT_ASSET, -M_FUND_STOCK)
  
  attach(eda)
  M_ETC <- M_TOT_SAVING - (M_JEOK + M_FUND + M_STOCK + M_SAVING_INSUR + M_CHUNG)
  detach(eda)
  eda$M_TOT_SAVING <- M_ETC
  names(eda)[names(eda) == 'M_TOT_SAVING'] <- 'M_ETC'
  
  eda$D_DAMBO <- eda$D_DAMBO - eda$D_JUTEAKDAMBO
  names(eda)[names(eda)=='D_DAMBO'] <- 'D_DAMBO_ETC'
  
  D_ETC <- eda$TOT_DEBT - (eda$D_SHINYONG + eda$D_DAMBO_ETC + eda$D_JUTEAKDAMBO + eda$D_JEONSEA)
  eda$TOT_DEBT <- D_ETC
  names(eda)[names(eda)=='TOT_DEBT'] <- 'D_ETC'
  
  eda[is.na(eda$TOT_YEA), which(names(eda) == 'TOT_YEA')]         <- 0
  eda[is.na(eda$TOT_JEOK), which(names(eda) == 'TOT_JEOK')]       <- 0
  eda[is.na(eda$TOT_CHUNG), which(names(eda) == 'TOT_CHUNG')]     <- 0
  eda[is.na(eda$TOT_FUND), which(names(eda) == 'TOT_FUND')]       <- 0
  eda[is.na(eda$TOT_ELS_ETE), which(names(eda) == 'TOT_ELS_ETE')] <- 0
  
  main_ranfo_predict <- read.csv('prdrnid.csv', header=T)
  main_ranfo_predict <- main_ranfo_predict[, -1]
  eda[is.na(eda$RETIRE_NEED), which(names(eda) == 'RETIRE_NEED')] <- main_ranfo_predict
  
  eda <<- eda
}


# 3. Clustering based on posterior probability P(K|X) ---------------------

keyassigner <- function (data) {
  # This function assigns ID(key) to each observation based on the customer type.
  # ex. SEX AGE JOB ADD INC MRY DIN NCH   ->   ID(key)
  #       1   2   1   3   5   2   1   0   -> 12135210
  if (names(data)[1] != 'SEX_GBN') stop("Data is not in expected form.")
  values <- sapply(data[, 1:8], as.numeric)
  return(factor(values %*% matrix(10 ^ (7:0), ncol = 1)))
} 

as.custtype <- function (data, include_chungy = T) {
  # This function was used to obtain average of the customers' each financial information
  # ex.   SEX AGE JOB ADD INC MRY DIN NCH ASS_FIN TOT_SOBI CHUNG_Y
  #     -----------------------------------------------------------
  #         1   2   1   3   5   2   1   0     100     200        Y
  #         1   2   1   3   5   2   1   0     300     300        Y
  #         1   2   1   3   5   2   1   0     800     700        N
  #     -----------------------------------------------------------
  # return: 1   2   1   3   5   2   1   0     400     400        Y
  result <- data
  
  # Apply mean function to every categorized financial features
  result <- data %>% 
    group_by(SEX_GBN, AGE_GBN, JOB_GBN, ADD_GBN, INCOME_GBN, MARRY_Y, DOUBLE_IN, NUMCHILD) %>% 
    summarize_all(mean)
  
  # Then, since CHUNG_Y variable is boolean value, NA appear in this column if mean function is applied.
  # Accordingly, this variable should be treated separately from other varaibles as below:
  result$CHUNG_Y <- data %>% 
    group_by(SEX_GBN, AGE_GBN, JOB_GBN, ADD_GBN, INCOME_GBN, MARRY_Y, DOUBLE_IN, NUMCHILD) %>% 
    summarize(CHUNG_Y = factor(ifelse(which.max(table(CHUNG_Y)) == 1, 'N', 'Y'))) %>% 
    select(CHUNG_Y)
  
  if(!include_chungy) result$CHUNG_Y <- NULL # Option to exclude CHUNG_Y from the analysis
  result <- as.data.frame(result)            # that is, refrain from storing this information in tibble type
  return(result)
}

### With as.custtype function, provided data with 17,076 customer information is condensed to the data of 5,087 customer type ###

clust_assigner <- function (input_data, ncluster) {
  # ex. clust_assigner(input, 7) contains clustering result of 'input' when the number of cluster is set to 7
  ids              <- data.frame(id = keyassigner(input_data))
  # cluster_key : data.frame that stores the result of PAM clustering of 5,087 customer type
  #               Dimension of this data.frame is 5,087 x 15 because this stores the result of clustering when the number of cluster varies from 3 to 17
  #               (this means that ncluster should have value from 3 to 17)
  joinjoin         <- left_join(ids, cluster_key[, c(which(names(cluster_key)=='id'), ncluster - 2)], by='id')
  result           <- input_data
  result$clust_num <- joinjoin[, 2]
  return(result)
}

### User can assign cluster for 5,087 customer type using clust_assigner function,
### but the cluster of remaining 136,663 customer types should be assigned by calculating posterior probabilities.
### group_assigner calculates probability P(K = i|X) (i.e. probability of being assigned to ith group given 8 basic customer information)
