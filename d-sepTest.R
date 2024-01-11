

convert_matrix_dag <- function(graph_data_numeric) {
  # Get all unique nodes
  all_nodes <- seq_len(ncol(graph_data_numeric))
  
  # Convert to dagitty format
  dag_code <- paste0('dag {', '\n')
  
  # Add all nodes to dag_code
  for (node in all_nodes) {
    dag_code <- paste0(dag_code, "  V", node, "\n")
  }
  
  # Add edges to dag_code based on the numerical matrix
  edges <- which(graph_data_numeric == 1, arr.ind = TRUE)
  for (i in 1:nrow(edges)) {
    from_node <- paste("V", edges[i, 1], sep = "")
    to_node <- paste("V", edges[i, 2], sep = "")
    dag_code <- paste0(dag_code, "  ", from_node, " -> ", to_node, "\n")
  }
  
  dag_code <- paste0(dag_code, '}')
  
  # Create the dagitty graph
  dag <- dagitty(dag_code)
  return(dag)
}


monte_carlo_simulation <- function(true_graph,estimated_graph,node_set,nmc,data_matrix,alpha ){

  # Initialize counts d-separation
  TP <- TN <- FP <- FN <- 0
  
  # Initialize counts conditional independence
  TPC <- TNC <- FPC <- FNC <- 0

  # Set the length of nodes
  num_nodes <- length(node_set)

  # Monte Carlo sampling loop
  for (m in 1:nmc) {
    Xi <- sample(names(node_set), 1)
    
    # Diffrent from Xi
    Xj <- sample(setdiff(names(node_set), Xi), 1)
   
    ns <- sample(0:(num_nodes - 2), 1, prob = rep((num_nodes - 2)/(2^(num_nodes - 2)), num_nodes - 1))
    exclude_nodes <- c(Xi, Xj)
    node_set <- node_set[setdiff(seq_along(node_set), exclude_nodes)]
    # Generate a new set with size ns
    XS <- sample(names(node_set), size = ns, replace = FALSE)

    # d-seperation test for the true graph
    is_sep_true <- dconnected(true_graph, Xi, Xj,  XS)
    
    # d-seperation test for the estemated graph
    is_sep_est <- dconnected(estimated_graph, Xi, Xj,  XS)
    
    # Conditionally independent partial correlation
    index_Xi <- match(Xi, names(node_set))
    index_Xj <- match(Xj, names(node_set))
    indices_XS <- match(XS, names(node_set))
    
    # Partial corrolation on the  true dataset 
    par_cor_true <- pcor.test(data_matrix[, index_Xi], data_matrix[, index_Xj], data_matrix[, -indices_XS])

    is_correlated_true <- par_cor_true$p.value < alpha 
    
    # Update counts based on d-separation results
    if (is_sep_true) {
      if (is_sep_est) {
        TP <- TP + 1 

      } else {
        FN <- FN + 1 
      }
    } else {
      if (!is_sep_est) {
        TN <- TN + 1
      } else {
        FP <- FP + 1
      }
    }
    
    # Update counts based on d-separation results
    if (is_sep_true) {
      if (isTRUE(is_correlated_true)) {
        TPC <- TPC + 1 
        
      } else {
        FNC <- FNC + 1 
      }
    } else {
      if (!is.na(is_correlated_true) && !is_correlated_true) {
        TNC <- TNC + 1
      } else {
        FPC <- FPC + 1
      }
    }
    
  }
  # Return counts
  return(list(c(TP,FN,FP,TN), c(TPC,FNC,FPC,TNC)))
}

test_sep <- function(alg_name) {
  
  all_results_list <- list()
  all_results_listC <- list()
  alg_name<-alg_name
  print(alg_name)
  # Get a list of files in the "data" directory
  files_G <- list.files("data/true_amat", pattern = "\\.csv$", full.names = TRUE)
 
  files_EG <- list.files( paste("gen_data", alg_name, sep = "/"), pattern = "\\.csv$", full.names = TRUE)
 
  files_Data <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
  
  execution_times <- c()
  accuracy_sep <- c()
  accuracy_ind <- c()
  for (i in seq_along(files_EG)) {
    # Read the True graph
    true_graph <- read.csv(files_G[i])
    graph_data_numeric_true <- as.matrix(true_graph) + 0
    dag_true<-convert_matrix_dag(graph_data_numeric_true)
    
    # Read the estimated graph
    estimated_graph <- read.csv(files_EG[i])
    graph_data_numeric_est <- as.matrix(estimated_graph) + 0
    dag_est<-convert_matrix_dag(graph_data_numeric_est)
    
    # The dataset
    data_matrix <- read.csv(files_Data[i])
    alpha <- 0.05
    
    # Set of nodes
    node_set<-true_graph[0,]
    nmc <- 50  # Number of MC samples
    
    start_time <- Sys.time()
    result <- monte_carlo_simulation(dag_true,dag_est, node_set,nmc,data_matrix,alpha)
    end_time <- Sys.time()
    
    execution_time <- end_time - start_time
    execution_times <- c(execution_times, as.numeric(execution_time))
    
    #  Result for d-sep of true and est graph
    result_matrix <- matrix(result[[1]], nrow = 2, byrow = TRUE)
    # Result of test corrolation and true graph
    result_matrixC <- matrix(result[[2]], nrow = 2, byrow = TRUE)
    
    accuracy_sep <- c(accuracy_sep, as.numeric(result_matrix[1,][1] + result_matrix[2,][2]))
    accuracy_ind <- c(accuracy_ind, as.numeric(result_matrixC[1,][1] + result_matrixC[2,][2]))
 
    # Append matrices to the lists for the first 4 files
    all_results_list[[i]] <- result_matrix
    all_results_listC[[i]] <- result_matrixC
    
  }
  
  confusionAlg(all_results_list,substr(tools::file_path_sans_ext(basename(files_G)), 9, nchar(basename(files_G))),alg_name=alg_name, sep_con="d-sep")
  confusionAlg(all_results_listC,substr(tools::file_path_sans_ext(basename(files_EG)), 9, nchar(basename(files_G))),alg_name=alg_name,sep_con="corr")
 
  return(list(execution_times = execution_times, accuracy_sep = accuracy_sep,accuracy_ind = accuracy_ind))
}





