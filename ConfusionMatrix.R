# Install and load the required packages if not already installed
# install.packages(c("caret", "pheatmap"))
library(caret)
library(pheatmap)

confusionAlg <- function(all_results_list, filename,row_labels = c("True_N", "True_P"), col_labels = c("Est_N", "Est_P"),files_name,alg_name,sep_con ) {
  alg_name<-alg_name

  plot_list=list()
  # Loop through each matrix in the list and plot it
  for (i in seq_along(all_results_list)) {
    # Open a PDF file for writing
    # Convert the matrix to a data frame
    conf_matrix_df <- as.data.frame(all_results_list[[i]])

    # Create a confusion matrix object using the confusionMatrix function
    # conf_matrix_object <- confusionMatrix(all_results_list[[i]])


    # # Visualize the confusion matrix using pheatmap
    plot <- pheatmap(conf_matrix_df,
                   
                     display_numbers = TRUE,
                     cluster_rows = FALSE,
                     cluster_cols = FALSE,
                     fontsize_number = 7,
                     fontsize_row = 7,
                     fontsize_col = 7,
                     fontsize = 3,
                     main = paste(filename[i]),
                     labels_row = row_labels,
                     labels_col = col_labels,
                     color =  colorRampPalette(c("lightblue", "burlywood", "lightgreen"))(50),
                     legend = FALSE,
                     show_colnames = FALSE,  # Set to FALSE to hide column labels
                     show_rownames = FALSE
                     )  # You can change the color palette
    plot_list[[i]] <- plot[[4]]

  }

 
  pdf( paste("confusion_matrices_", alg_name, "_", sep_con, ".pdf"))
  grid.arrange(grobs = plot_list, ncol = 8, nrow = 8, top = "Predicted Label", bottom=" ", left = "True Label", right=paste("Confusion Matrices for ", alg_name, " ", sep_con), margin = c(0, 0, unit(5, "points"), 0)) 
  
    grid.text("d-sep", x = unit(0.02, "npc"), y = unit( 0.1 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.02, "npc"), y = unit( 0.22 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.02, "npc"), y = unit( 0.34 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.02, "npc"), y = unit( 0.435 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.02, "npc"), y = unit( 0.56 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.02, "npc"), y = unit( 0.68 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.02, "npc"), y = unit( 0.8 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.02, "npc"), y = unit( 0.92 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    
    grid.text("d-/sep", x = unit(0.02, "npc"), y = unit( 0.06 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-/sep", x = unit(0.02, "npc"), y = unit( 0.18 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-/sep", x = unit(0.02, "npc"), y = unit( 0.3 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-/sep", x = unit(0.02, "npc"), y = unit( 0.4 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    # grid.text("d-/sep", x = unit(0.02, "npc"), y = unit( 0.5 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-/sep", x = unit(0.02, "npc"), y = unit( 0.64 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-/sep", x = unit(0.02, "npc"), y = unit( 0.76 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-/sep", x = unit(0.02, "npc"), y = unit( 0.88 , "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
   
      
    grid.text("d-sep", x = unit(0.065, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.18, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.3, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.43, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.54, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.66, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.78, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.9, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    
    grid.text("d-sep", x = unit(0.11, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.24, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.35, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.47, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.59, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.71, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.84, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    grid.text("d-sep", x = unit(0.95, "npc"), y = unit(0.02, "npc"), gp = gpar(fontsize = 5), just = "bottom", vp = viewport(angle = 0))
    
  
  # Close the PDF file
  dev.off()
}




