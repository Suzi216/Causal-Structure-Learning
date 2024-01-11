plot_all <- function(execution_times,accuracy_sep,accuracy_ind,file_name ){
  
  # Create a data frame for d-sep
  data_sep <- data.frame(
    ExecutionTimes = execution_times,
    Accuracy = accuracy_sep,
    FileNames = file_name
  )
  my_plot_acc <- ggplot(data_sep, aes(x = rep(c("10", "100", "1000", "100000"), each = 16), group = FileNames)) +
    geom_bar(aes(y = Accuracy / 50, fill = rep(c("red", "blue", "gray", "green"), each = 16)),
             stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    geom_text(aes(y = (Accuracy / 50), label = rep(c(2, 3, 4, 5), each = 4, length.out = 64)),
              position = position_dodge(width = 0.9), vjust = -0.9, size = 3) +
    labs(
      y = "Accuracy",
      x= " "
    ) +
    scale_fill_manual(values = c("red", "blue", "gray", "green"),
                      breaks = c("red", "blue", "gray", "green"),
                      labels = c("10D", "100D", "20D", "50D")) +
    scale_x_discrete(
      breaks = c("10", "100", "1000", "100000"),
      labels = c("10", "100", "1000", "100000")
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6)) +
    guides(fill = guide_legend(title = NULL))
  
  ggsave("my_plot_acc_sep.pdf", plot = my_plot_acc, width = 10, height = 6, units = "in")

  # Create the plot
  my_plot_exe <- ggplot(data_sep, aes(x = FileNames)) +
    geom_line(aes(y = ExecutionTimes, group = 1), color = "black", size = 1) +
    labs(
      y = "Execution Times",
    ) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6))

  ggsave("my_plot_exe_sep.pdf", plot = my_plot_exe, width = 10, height = 6, units = "in")
  
  
  
  # Create a data frame for independence test
  data_ind <- data.frame(
    ExecutionTimes = execution_times,
    Accuracy = accuracy_ind,
    FileNames = file_name
  )
  
  my_plot_acc <- ggplot(data_ind, aes(x = rep(c("10", "100", "1000", "100000"), each = 16), group = FileNames)) +
    geom_bar(aes(y = Accuracy / 50, fill = rep(c("red", "blue", "gray", "green"), each = 16)),
             stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    geom_text(aes(y = (Accuracy / 50), label = rep(c(2, 3, 4, 5), each = 4, length.out = 64)),
              position = position_dodge(width = 0.9), vjust = -0.9, size = 3) +
    labs(
      y = "Accuracy",
      x= " "
    ) +
    scale_fill_manual(values = c("red", "blue", "gray", "green"),
                      breaks = c("red", "blue", "gray", "green"),
                      labels = c("10D", "100D", "20D", "50D")) +
    scale_x_discrete(
      breaks = c("10", "100", "1000", "100000"),
      labels = c("10", "100", "1000", "100000")
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6)) +
    guides(fill = guide_legend(title = NULL))
  
  
  ggsave("my_plot_acc_ind.pdf", plot = my_plot_acc, width = 10, height = 6, units = "in")
  

} 
