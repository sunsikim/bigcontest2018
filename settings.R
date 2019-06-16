
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
# # Note : As gRain package is not uploaded in CRAN, execute following for installation:
# source('http://bioconductor.org/biocLite.R')
# biocLite('gRain')
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

# 2. Justification of the process -----------------------------------------
# No predefined function in this step

# 3. Clustering based on posterior probability P(K = i|X) -----------------
# 3-1. PAM cluster the customer type --------------------------------------

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


# 3-2. Assign posterior probability ---------------------------------------

### User can assign cluster for 5,087 customer type using clust_assigner function,
### but the cluster for remaining 136,663 customer types should be assigned by calculating posterior probabilities.
### The group_assigner function calculates P(K = i|X) : probability of being assigned to ith group given 8 basic customer information

group_assigner <- function (data, clustered_data, original_data, ncluster = 11, maxparams = TRUE) {
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
  ## Obtain averaged graph from that 2000 graphs
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
  fitted_BNs <- vector("list", ncluster)
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
    probtable     <- matrix(0, nrow = nrow(data), ncol = ncluster)
    probtable[,i] <- exp(pre_result + log(nrow(clustered_data[[i]]) / nrow(original_data)))
  }

  # 4. get argmax_k -----------------------------------------------------------
  return(apply(probtable, 1, which.max))
}


# 4. Bayesian estimation on financial information using Gibbs Sampling --------
# 4-1. Discretizing continuous variables --------------------------------------

data_transform <- function (datasets, clustnum, storage) {
  result    <- datasets[[clustnum]] # clustnum-th dataset to be discretized 
  cutpoints <- storage[[clustnum]]  # Cutpoints of clustnum-th dataset
  
  # Transformation begins
  for (i in 1:length(result)) {
    if (!names(result)[i] %in% cutpoints$varname) next # pass if revaluing i-th variable is unnecessary
    pointer      <- which(cutpoints$varname == names(result)[i]) # location of i-th variable in 'cutpoints'
    level_assign <- rep(ninterval, nrow(result)) # vector of discretized value
    
    for(j in ninterval:2){
      level_assign[result[, i] <= cutpoints[pointer, j]] <- (j - 1)
    }
    result[, i] <- factor(level_assign)
  }
  return(result)
}


# 4-2. Searching conditional independence using Bayesian Network ----------
# No predefined function in this step


# 4-3. Gibbs Sampling ----------------------------------------------------------

## Create transition_matrix(if arc between i-th, j-th variable exists, (i, j)th element is TRUE(FALSE elsewhere))
make_transition_byarcs <- function (col_names, network_info) {
  # col_names    : name of variables of data subsetted by cluster information
  # network_info : sub-object from network_cluster_full(fitted BN)
  transition_matrix <- matrix(0, length(col_names), length(col_names))
  from_to           <- network_info$arcs
  for (i in 1:nrow(from_to)) {
    transition_matrix[grep(from_to[i, 1], col_names), grep(from_to[i, 2], col_names)] <- 1
  }
  transition_matrix <- apply(transition_matrix, c(1, 2), as.logical)
  return(transition_matrix)
}

## Returns data.frame with variable names and cut_point for each data subsetted by clusters
## If cutpoint exists, store that cutpoint; store 0 otherwise
make_cutpoints <- function (col_names, cut_point) {
  # col_names : name of variables of data subsetted by cluster information
  # cut_point : sub-object from cutpoint_list(list of cutpoints)
  cutpoint_1 <- numeric(length(col_names))
  cutpoint_1[sapply(cut_point[, 1], function (x) {grep(x, col_names)})] <- cut_point[, 2]
  result <- cbind.data.frame(col_names, cutpoint_1)
  names(result) <- names(cut_point)
  return(result)
}

## Generate 'n' random numbers from empirical pdf derived from 'data_vec' using AR algorithm
AR_sim <- function(data_vec, n){
  # data_vec : vector of values to generate epdf
  # n : number of samples to generate from that epdf
  if (length(unique(data_vec)) == 1) return(rep(data_vec[1], n))
  
  # Create epdf and corresponding condition value
  data_vec_log <- log(data_vec + 1) # Minimum value of any cell of the data is 0: add 1 to prevent log0(-Inf)
  data_vec_std <- (data_vec_log - min(data_vec_log)) / (max(data_vec_log) - min(data_vec_log)) # minmax scaling
  epdf         <- approxfun(density(data_vec_std))  # Obtain empirical log pdf(support becomes (0,1) through minmax scaling)
  weight_c     <- max(epdf(seq(0, 1, 0.0001)))  # Obtain maximal value of the epdf
  
  # Generation begins
  X = c()
  while (TRUE) {
    Y = runif(n * 2)        # surrogate distribution : g(y) = 1, 0 < y < 1
    U = runif(n * 2)        # For acceptance evaluation
    C = epdf(Y) / weight_c  # acceptance condition : U <= epdf(Y) / weight_c * g(Y) = epdf(Y) / weight_c
    X = c(X, Y[U <= C])     # accept values that meets the condition
    if(length(X) >= n) break
  }
  X <- exp(X * (max(data_vec_log) - min(data_vec_log)) + min(data_vec_log)) - 1
  return(X[1:n])
}

generate_sample(i, c(), transition_matrix, factor_index, origin_data, filter_data, n/2)
generate_sample(var_index, var_condition, transition_matrix, factor_index, origin_data, filter_data, n)
# network_list : network_cluster_full -> result in transition_matrix
# cutpoint_list : cutpoint_list -> result in cut_point
# data_list : edas -> origin_data of gibbsSampler
# filter_data_list : edas_discretized -> filter_data of gibbsSampler
# answersheet : 141,750 x 8 dataset
# answer_clust : 141,750-length vector of argmax_i


## Function for obtaining data to sample from
deepen_filter <- function (origin_data, filter_data, output_col, parent_var, var_condition) {
  # If parent variable doesn't exist, just return the whole data without any filtering
  if (length(var_condition) == 0) return(origin_data[, output_col])
  
  # If parent variable exists
  filter_index <- apply(data.frame(filter_data[, parent_var]), 1, function(x){all(x == var_condition)})

  if(sum(filter_index) < 15){
    cond_count <- numeric(length(var_condition))
    for(i in 1:length(var_condition)){
      cond_count[i] <- sum(origin_data[, grep(names(origin_data)[parent_var][i], names(origin_data))] == unlist(var_condition[i]))
    }
    delete_var_name = names(origin_data)[parent_var][which.min(cond_count)]
    var_condition = var_condition[-grep(delete_var_name, names(origin_data)[parent_var])]
    parent_var[grep(delete_var_name, names(origin_data))] = F
    
    return(deepen_filter(origin_data, filter_data, output_col, parent_var, var_condition))
  }
  return(origin_data[filter_index, output_col])
}

## Wrapper function of 'deepen_filter' function and 'AR_sim' function
generate_sample <- function(var_index, var_condition, transition_matrix, factor_index, origin_data, filter_data, n){
  var_data_vec <- deepen_filter(origin_data = origin_data, filter_data = filter_data, output_col = var_index, 
                                parent_var = transition_matrix[, var_index], var_condition = var_condition)
  
  if(var_index %in% factor_index){
    return(sample(var_data_vec, n, replace = T))
  } else {
    return(AR_sim(var_data_vec, n)) 
  }
}

## The gibbs Sampler.
gibbsSampler = function(origin_data, filter_data, cut_point, transition_matrix, burnin, x_df){
  # origin_data       : one of edas
  # filter_data       : one of edas_discretized
  # cut_point         : one of cutpoint_list
  # transition_matrix : 32 x 32 boolean matrix
  # burnin            : scalar value to specify the length of burn.in sample
  # x_df              : subset of answersheet(141,750 x 8 dataset)
  factor_index <- which(sapply(origin_data, is.factor))
  n.samples <- burnin + nrow(x_df)
  
  sim_list <- vector("list", ncol(origin_data))
  for (i in 9:ncol(origin_data)) {
    # Note : 9 means index of first financial information
    # (i.e. this loop is excuted to every variable in the dataset)
    # Question : Then what does this loop do?
    # Answer   : Conduct simulation for every possible combination of conditioning variables of i-th variable
    
    # Case1. i-th variable is factor variable
    if (i %in% factor_index) {
      # encoded using DeMorgan's Law
      sim_list[[i]] <- NA
      next
    }
    
    num_parents <- sum(transition_matrix[, i])
    # Case2. i-th variable is numeric and has no parent variable
    if (num_parents == 0) {
      sim_list_i <- list()
      sim_list_i[[1]] <- generate_sample(i, c(), transition_matrix, factor_index, origin_data, filter_data, n / 2)
      sim_list[[i]]   <- sim_list_i
      next
    }
    rm('num_parents')
    
    # Case3. i-th variable is numeric and has parent variable(s)
    ## Sample the data given any possible combination of states of parent variables
    ## (Note that parent variables are discretized, since this part uses filter_data)
    parent_var    <- as.vector(filter_data[1, ])[transition_matrix[, i]]
    parent_levels <- lapply(parent_var, function(x) levels(x))        # list of levels of each parent variable
    n_cases       <- prod(sapply(parent_var, function(x) nlevels(x))) # number of possible combinations of levels of parent variables
    sim_list_i    <- vector("list", 1:n_cases)
    for (j in 1:n_cases) {
      j_condition <- numeric(length(parent_var))
      j2 <- j - 1
      for (k in length(parent_var):1) {
        j_condition[k] <- parent_levels[[k]][j2 %% length(parent_levels[[k]]) + 1]
        j2 <- j2 %/% length(parent_levels[[k]])
      }
      sim_list_i[[j]] <- generate_sample(var_index = i, var_condition = j_condition, transition_matrix = transition_matrix,
                                         factor_index = factor_index, origin_data = origin_data, filter_data = filter_data, n = round(n/2))
    }
    sim_list[[i]] <- sim_list_i
  }
  
  curr_vec_index <- sample(1:nrow(filter_data), 1)
  curr_vec       <- as.vector(origin_data[curr_vec_index, ])
  curr_vec[1:ncol(x_df)] <- x_df[1, ]
  curr_vec_logi  <- as.vector(filter_data[curr_vec_index, ])
  curr_vec_logi[1:ncol(x_df)] <- x_df[1, ]
  
  sim_result = origin_data[rep(1,nrow(x_df)),]  # output dataframe initialize
  rownames(sim_result) = NULL
  parent_logi = curr_vec_logi  # condition vector initialize
  k=0  # condition index initialize
  
  for (i in 1:n.samples) {
    for (j in 9:ncol(origin_data)) 
    {
      if (sum(transition_matrix[,j]) == 0)
      {
        # If parent of j-th variable doesn't exist
        if (length(sim_list[[j]][[1]]) == 0) {  
          # If simulated vectors inside the list are exhausted
          sim_list[[j]][[1]] <- generate_sample(j, c(), transition_matrix, factor_index, origin_data, filter_data, round(n/2))
        }
        curr_vec[j] <- sim_list[[j]][[1]][1]  # Update vector
        sim_list[[j]][[1]] <- sim_list[[j]][[1]][-1]  # Exclude used vector
        
      } 
      else 
      {
        parent_logi <- curr_vec_logi[transition_matrix[,j]]  # Vector presenting the conditions of parent variables
        k <- sum(sapply(parent_logi, function(x) {grep(x, levels(x)) - 1}) *  
                   c(rev(cumprod(rev(sapply(parent_logi, function(x){length(levels(x))})[-1]))), 1)) + 1
        if (length(sim_list[[j]][[k]]) == 0) {
          # If simulated vectors inside the list are exhausted
          sim_list[[j]][[k]] <- generate_sample(var_index = j, var_condition = parent_logi, transition_matrix = transition_matrix,
                                                factor_index = factor_index, origin_data = origin_data, filter_data = filter_data, n = round(n/2))
        }
        curr_vec[j] <- sim_list[[j]][[k]][1]  # Update vector
        sim_list[[j]][[k]] <- sim_list[[j]][[k]][-1]  # Exclude used vector
      }
      
      if (j %in% factor_index) {  
        curr_vec_logi[j] = curr_vec[j] # Update vector
      } else {
        curr_vec_logi[j] = factor(sum(rep(unlist(curr_vec[j]), ncol(cut_point)-1) > cut_point[j - ncol(x_df), -1]) + 1, levels = levels(filter_data[,j]))
      }
    }
    
    if (i <= burnin) {
      # Doesn't store the vector created in burn-in period
      next
    } else {
      # Store gibbs sample
      sim_result[i - burnin, ] <- curr_vec  
    }
    curr_vec[1:ncol(x_df)] <- x_df[i - burnin + 1, ]
    curr_vec_logi[1:ncol(x_df)] <- curr_vec[1:ncol(x_df)]
  }
  
  return(sim_result)
}

boot_gibbs_sampling = function(cluster_no, network_list, cutpoint_list, data_list, filter_data_list, answersheet, answer_clust = answer_clust,
                               burnin = 200, boot_n = 100, boot_ratio = 0.9){
  transition_matrix = make_transition_byarcs(names(data_list[[cluster_no]]), network_list[[cluster_no]])
  cut_point         = make_cutpoints(names(data_list[[cluster_no]]), cutpoint_list[[cluster_no]]) 
  boot_gibbs = foreach(i = 1:boot_n, .combine = list, .multicombine = T) %dopar% {
    boot_index  <- sample(1:nrow(data_list[[cluster_no]]), round(boot_ratio*nrow(data_list[[cluster_no]])))
    boot_data   <- data_list[[cluster_no]][boot_index,]
    boot_filter <- filter_data_list[[cluster_no]][boot_index,]
    sim_data    <- gibbsSampler(origin_data = boot_data, filter_data = boot_filter, cut_point = cut_point, 
                                transition_matrix = transition_matrix, burnin = burnin, x_df = answersheet[answer_clust == cluster_no,])
    return(sim_data)
  }
  return(boot_gibbs)
}