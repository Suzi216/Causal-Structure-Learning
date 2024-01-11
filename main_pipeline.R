library(pcalg)
library(caret)
library(pheatmap)
library(ggplot2)
library(gridGraphics)
library(grid)
library(gplots)
library(gridExtra)
library(dagitty)
library(ppcor)
library(fs)
library(bnlearn)
library(RColorBrewer)


setwd("C:/Users/suzan/OneDrive/Documents/")
dir.create("data")
dir.create("data/dgps")
dir.create("data/true_amat")
# Set the working directory to the location of the "data" directory
dir.create("gen_data")
dir.create("gen_data/tabu")
dir.create("gen_data/hc")
dir.create("gen_data/mmhc")
dir.create("gen_data/h2pc")

dir.create("gen_data_missing_values")

# Set the arguments
nodes_list <- c(10,20,50,100)
density_list <- c(2, 3, 4, 5)
sample_number_list <- c(100, 1000, 10000, 100000)

# Generate the graphs and data
generate_and_save_graph(num_nodes, density, sample_number)

# Apply the alg to get the est graph and time execution tabu/hc/gs/mmhc/h2pc
estG_time <- apply_pc_save(h2pc)
 
# Test for d-separtion and corrolation and obtain the Confusion matrixes in a pdf for  tabu/hc/gs/mmhc/h2pc
result<-test_sep("h2pc")

# Adding the execution time of learning the graph and checking for d-sep
result_time <- estG_time + result$execution_times
print(result$accuracy_sep)

# Plotting
plot_all(result_time,result$accuracy_sep,result$accuracy_ind,substr(tools::file_path_sans_ext(basename(files_G)), 5, nchar(basename(files_G))))


# VIOLATIONS TODO

# Apply the alg to get the est graph and time execution tabu/hc/gs/mmhc/h2pc with missing data values
estG_time <- apply_pc_save_missing_data(tabu)










