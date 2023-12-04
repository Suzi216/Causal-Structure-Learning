# Install and load the required packages if not already installed
# install.packages(c("caret", "pheatmap"))
library(caret)
library(pheatmap)

confusionAlg <- function(results_list){
  conf_matrix <- matrix(0, nrow = 2, ncol = 2)

  # Add each matrix to the confusion matrix
  for (mat in results_list) {
    conf_matrix <- conf_matrix + mat
  }
  
  # Convert the 2x2 matrix to a data frame
  conf_matrix_df <- as.data.frame(conf_matrix)
  
  # Create a confusion matrix object using the confusionMatrix function
  conf_matrix_object <- confusionMatrix(conf_matrix)
  
  # Visualize the confusion matrix using pheatmap
  pheatmap(conf_matrix_df,
           display_numbers = TRUE,
           cluster_rows = FALSE,
           cluster_cols = FALSE,
           fontsize_number = 12,
           fontsize_row = 12,
           fontsize_col = 12,
           main = "Confusion Matrix")
  
}

