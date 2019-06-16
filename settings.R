
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
# Note : As gRain package is not uploaded in CRAN, execute following for installation:
# if (!'gRain' %in% rownames(installed.packages())) {
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
  
  return(eda)
}

# 2. Justification of the process -----------------------------------------
# No predefined function in this step

# 3. Clustering based on posterior probability P(K = i|X) -----------------

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
  result <- as.data.frame(result)            # Return the result in data.frame, not in tibble
  return(result)
}

### With as.custtype function, provided data with 17,076 customer information is condensed to the data of 5,087 customer type ###

clust_assigner <- function (input_data, ncluster) {
  # ex. clust_assigner(input, 7) contains clustering result of 'input' when the number of cluster is set to 7
  ids              <- data.frame(id = keyassigner(input_data))
  # cluster_key : data.frame that stores the result of PAM clustering of 5,087 customer type
  #               Dimension of this data.frame is 5,087 x 15 because this stores the result of clustering when the number of cluster
  #               varies from 3 to 17(this means that ncluster should have value from 3 to 17)
  joinjoin         <- left_join(ids, cluster_key[, c(which(names(cluster_key)=='id'), ncluster - 2)], by='id')
  result           <- input_data
  result$clust_num <- joinjoin[, 2]
  return(result)
}

### User can assign cluster for 5,087 customer type using clust_assigner function,
### but the cluster for remaining 136,663 customer types should be assigned by calculating posterior probabilities.
### The group_assigner function calculates P(K = i|X) : probability of being assigned to ith group given 8 basic customer information

group_assigner <- function(data, clustered_data, original_data, ncluster = 11, maxparams = TRUE){
  # data           : 141,750 * 8
  # clustered_data : 5,084 * p data stored separately
  # original_data  : 5,084 * p data itself
  
  # 1. Fitting Bayesian network to each cluster  ----------------------------
  # 1-1. Learning the structure
  ## Create 2000 graphs from 2000 bootstrapped data
  registerDoParallel(detectCores() - 1)
  arcs_boot       <- foreach(i = 1:ncluster, .combine = list, .multicombine = T, .packages = 'bnlearn') %dopar% {
    boot.strength(clustered_data[[i]][, 1:8], R=2000, algorithm = 'hc',
                  algorithm.args = list(score = 'bde', iss = 5))
  }
  ## Create averaged graph from that 2000 graphs
  network_cluster <- foreach(i = 1:ncluster, .combine = list, .multicombine = T, .packages = 'bnlearn') %dopar% {
    averaged.network(arcs_boot[[i]][arcs_boot[[i]]$direction > 0.5, ], threshold = 0.85)
  }
  ## Optional : if maximal value among the vector of number of parameters needed to fit BN for each cluster is needed
  if (maxparams) {
    iamnparams <- numeric(ncluster)
    for(i in 1:ncluster) iamnparams[i] <- nparams(network_cluster[[i]], clustered_data[[i]][, 1:8])
    print(paste("Maximal number of parameter required to fit BN is", max(iamnparams)))
    rm('iamnparams')
  }
  stopImplicitCluster()
  
  # 1-2. Assigning probabilities to that structure
  fitted_BNs <- vector("list", length(network_cluster))
  for(i in 1:length(network_cluster)){
    fitted_BNs[[i]] <- bn.fit(network_cluster[[i]], clustered_data[[i]][,1:8], method='bayes', iss=1)
    # Bayesian estimation is applied to prevent the probability from being estimated as 0
  }
  rm('arcs_boot', 'network_cluster')
  
  
  # 2. Calculate P(X = x|K = i) -----------------------------------------------
  for (i in 1:length(fitted_BNs)) {
    nodeprob <- matrix(0, nrow = nrow(data), ncol = 8) # placeholder for P(X_j | X^{(-j)}, K = i)
    tree     <- compile(as.grain(fitted_BNs[[i]]))     # grain object supports faster access to the information of each BNs.
    
    for (j in 1:8) {
      # 2-1. Calculation of P(X_j | X_{-j}, K = i)
      if (length(fitted_BNs[[i]][[j]]$parents) == 0) {
        # 2-1-1. If parent of j-th node in i-th cluster doesn't exist
        # (i.e. calculate P(X_j | X_{-j}, K = i) = P(X_j | K = i))
        nodeprob[,j] <- as.numeric(querygrain(tree, node = names(data)[j])[[1]][as.numeric(data[, j])])
      } else {
        # 2-1-2. If parent of j-th node in i-th cluster exist
        ## (1) assigning ID to rows of the data
        treeindex   <- which(names(fitted_BNs[[i]]) == names(data)[j]) 
        iamfamily   <- c(names(data)[j], fitted_BNs[[i]][[treeindex]]$parents) # store name of the node and its parents
        nfamily_jth <- length(iamfamily) # number of family members of j-th node in i-th cluster
        
        wherefamily <- numeric(nfamily_jth) # vector of indices of columns that correspond to 'iamfamily'
        for (p in 1:nfamily_jth) {wherefamily[p] <- which(names(data) == iamfamily[p])} 
        familylevel <- numeric(nfamily_jth) # vector of numbers of levels of variables corresponding to 'iamfamily'
        for (p in 1:nfamily_jth) {familylevel[p] <- nlevels(data[, wherefamily[p]])}  
        
        queryorder  <- sapply(data[, wherefamily], as.numeric)  # numerize factor to generate ID
        idmaker     <- matrix(0, nfamily_jth, ncol = 1)         # column vector to multiply to 'queryorder'
        for (a in 1:nfamily_jth) idmaker[a, 1] <- 10 ^ (a - 1)  # create t([1 10 ... 10 ^ (nfamily_jth - 1)])
        queryids    <- data.frame(ids = as.numeric(queryorder %*% idmaker))
        
        ## (2) assigning ID to probabilities
        idtable     <- data.frame(prob = as.numeric(tree$cptlist[[treeindex]]))
        ids         <- matrix(0, nrow = nrow(idtable), ncol = nfamily_jth)
        for(k in 1:nfamily_jth){
          if (k == 1) {
            ids[,k] <- rep(1:familylevel[k], length.out=nrow(idtable))
          } else {
            iterate <- cumprod(familylevel)[(k-1)]
            ids[,k] <- rep(1:familylevel[k], each = iterate, length.out = nrow(idtable))
          }
        }
        idtable$ids <- ids %*% idmaker
        
        ## (3) matching queries to probabilities
        queryids$ids <- factor(queryids$ids)
        idtable$ids  <- factor(idtable$ids)
        nodeprob[,j] <- left_join(queryids, idtable, by='ids') %>% select(prob) %>% unlist()
      }
    }
    
    # 3. get P(X=x|K=i) * P(K=i) = exp(logP(X=x|K=i) + logP(K=i)) -------------
    #    (For numerical stability, applying exponential function to calculated log probabilities is recommended.)
    pre_result    <- apply(log(nodeprob), 1, sum)
    probtable     <- matrix(0, nrow = nrow(data), ncol = length(fitted_BNs))
    probtable[,i] <- exp(pre_result + log(nrow(clustered_data[[i]]) / nrow(original_data)))
  }

  # 4. get argmax_k ------------------------------------------------------
  return(apply(probtable, 1, which.max))
}