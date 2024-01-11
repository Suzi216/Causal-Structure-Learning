generate_and_save_graph <- function(num_nodes, density, sample_number) {
  set.seed(44)
  
  # Loop through each combination of variables
  for (nodes in nodes_list) {
    for (density in density_list) {
      for (sample_number in sample_number_list) {
        
        # randomly generate DAG
        graph <- r.gauss.pardag(nodes, prob=density/nodes, top.sort = FALSE, normalize = TRUE,
                                lbe = 0.5, ube = 2, neg.coef = TRUE, labels = as.character(1:nodes),
                                lbv = 0.5, ubv = 1)
        # retrieve and store info about DGP (weight matrix and error variance)
        proba <- round(nodes/density,digits=5) 
        weights <- graph$weight.mat()
        error_var <- graph$err.var()
        weights_filename <- paste("data/dgps/dag_", nodes,"_",density, "_",sample_number ,"_weights.csv", sep="")
        write.csv(weights, weights_filename, row.names = FALSE)
        
        variance_filename <- paste("data/dgps/dag_", nodes, "_",density,  "_",sample_number, "_error_var.csv", sep="")
        write.csv(error_var, variance_filename, row.names = FALSE)
        
        # retrieve Boolean adjacency matrix and store it
        amat <- as(graph, "matrix")
        amat_name <- paste("data/true_amat/dag_", nodes, "_",density, "_",sample_number, ".csv", sep="")
        write.csv(amat, amat_name, row.names = FALSE)
        
        # create and store data
        data <- graph$simulate(sample_number)
        filename <- paste("data/dag_", nodes, "_",density, "_",sample_number, ".csv", sep="")
        write.csv(data, filename, row.names = FALSE)
        # Define a unique filename for each combination
        
      }
    }
  }
}


