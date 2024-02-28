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


setwd("-")
dir.create("data")
dir.create("data/dgps")
dir.create("data/true_amat")
# Set the working directory to the location of the "data" directory
dir.create("gen_data")
dir.create("gen_data/tabu")
dir.create("gen_data/hc")
dir.create("gen_data/mmhc")
dir.create("gen_data/h2pc")
dir.create("gen_data/rsmax2")

dir.create("gen_data_missing_values")
dir.create("gen_data_missing_values/tabu")
dir.create("gen_data_missing_values/hc")
dir.create("gen_data_missing_values/mmhc")
dir.create("gen_data_missing_values/h2pc")
dir.create("gen_data_missing_values/rsmax2")

dir.create("gen_data_incorrect_values")
dir.create("gen_data_incorrect_values/tabu")
dir.create("gen_data_incorrect_values/hc")
dir.create("gen_data_incorrect_values/mmhc")
dir.create("gen_data_incorrect_values/h2pc")
dir.create("gen_data_incorrect_values/rsmax2")

# Set the arguments
nodes_list <- c(10,20,50,100)
density_list <- c(2, 3, 4, 5)
sample_number_list <- c(100, 1000, 10000, 100000)

# Generate the graphs and data
generate_and_save_graph(num_nodes, density, sample_number)

# Apply the alg to get the est graph and time execution tabu/hc/mmhc/h2pc/rsmax2
estG_time <- apply_pc_save(hc)

print(estG_time)

# Test for d-seperation and pcor and obtain the Confusion matrixes in a pdf for  tabu/hc/mmhc/h2pc/rsmax2
result<-test_sep("h2pc")
print(result)


# Adding the execution time of learning the graph and checking for d-sep
result_time_sep <- estG_time + result$execution_times_sep
# Adding the execution time of checking for pcor
result_time_pcor <- estG_time + result$execution_times_pcor




# VIOLATIONS 

#  Missing data of variables %5.

# Apply the alg to get the est graph and time execution tabu/hc/mmhc/h2pc/rsmax2 with missing data values
estG_time <- apply_pc_save_missing_data(tabu)

# Test for d-separation and corrolation and obtain the Confusion matrixes in a pdf for  tabu/hc/gs/mmhc/h2pc/rsmax2 with missing data 
result<-test_sep_missing_data("hc")
print(result)

# Incorrect values 

# Apply the alg to get the est graph and time execution tabu/hc/mmhc/h2pc/rsmax2 with incorrect data values
estG_time <- apply_pc_save_incorrect_data(tabu)

result<-test_sep_incorrect_data("hc")

print(result)







