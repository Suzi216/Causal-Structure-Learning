function(alg) {
  execution_times <- c()
  # Get a list of files in the "datapipipeline" directory
  files <- list.files("data/", pattern = "\\.csv$", full.names = TRUE)
  files <- list.files("data/", pattern = "\\.csv$", full.names = TRUE)[order(as.numeric(sub("data/dag_(\\d+)_.*", "\\1", list.files("data/", pattern = "\\.csv$", full.names = TRUE))))]
  
  alg_name <- deparse(substitute(alg))
  # Loop through each file and perform some operation
  for (file in files) {
    
    # Read the CSV file
    data <- read.csv(file)
    # Convert to 0 and 1 
    adj_matrix_generated <- as.matrix(data) + 0
    data <- as.data.frame(adj_matrix_generated)

    start_time <- Sys.time()
    # The generated graph
    pc_result <- alg(data)
    end_time <- Sys.time()
    
    execution_time <- end_time - start_time
    execution_times <- c(execution_times, as.numeric(execution_time))
    
    adj_matrix_generated <- amat(pc_result)

    # Convert 0 and 1 to TRUE and FALSE
    adj_matrix_generated <- adj_matrix_generated == 1

    # Extract file name (without path) for naming the output
    file_name <- tools::file_path_sans_ext(basename(file))
    
    # Save the adjacency matrix to a CSV file with a name based on the input file
    weights_filename <- paste("gen_data/", alg_name, "/gen_", file_name, ".csv", sep = "")
    write.csv(adj_matrix_generated, weights_filename, row.names = FALSE)
    
    # Print a message indicating the processing is done for the current file
    cat("Processed file:", file, "\n")
  }
  return(execution_times)
  
}
