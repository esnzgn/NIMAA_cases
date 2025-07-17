
# Load required packages
library(NIMAA)
library(tidyverse)
library(cowplot)

# Define paths and dataset names
data_files <- list(
  fish_catches = "fish_catches.rds",
  HR_Analytics = "HR_Analytics.rds",
  student_discussion = "student_discussion.rds",
  Restaurant_user = "Restaurant_user.rds",
  online_retail = "online_retail.rds"
)

data_path <- "../NIMAA_cases_ext"

# Generate plots for each dataset
# all_plots <- map2(data_files, names(data_files), function(file, name) {
  for (i in seq_along(data_files)){
    cat("Processing:", names(data_files)[i], "\n")
  
    data <- readRDS(file.path(data_path, data_files[i]))
  
    inc <- plotIncMatrix(
      x = data,
      index_nominal = c(2, 1),
      index_numeric = 3,
      print_skim = FALSE,
      plot_weight = TRUE,
      verbose = FALSE
    )
  
    bip <- plotBipartite(inc_mat = inc, vertex.label.display = FALSE) +
      ggtitle(paste0(name, " bipartite"))
  
    subm <- extractSubMatrix(
      x = inc,
      shape = c("Square", "Rectangular_element_max"),
      row.vars = names(data)[2],
      col.vars = names(data)[1],
      plot_weight = FALSE,
      print_skim = FALSE
    )
  
    rect_submatrix_plot <- plotMatrix(
      mat = subm$Rectangular_element_max$matrix,
      title = paste0(name, " Rectangular Submatrix")
    )
  
    square_submatrix_plot <- plotMatrix(
      mat = subm$Square$matrix,
      title = paste0(name, " Square Submatrix")
    )
  
    return(bip / rect_submatrix_plot / square_submatrix_plot)
    
  }
# })

# Add beatAML data
data <- NIMAA::beatAML
name <- "beatAML"

inc <- plotIncMatrix(
  x = data,
  index_nominal = c(2, 1),
  index_numeric = 3,
  print_skim = FALSE,
  plot_weight = TRUE,
  verbose = FALSE
)

bip <- plotBipartite(inc_mat = inc, vertex.label.display = FALSE) +
  ggtitle(paste0(name, " bipartite"))

subm <- extractSubMatrix(
  x = inc,
  shape = c("Square", "Rectangular_element_max"),
  row.vars = names(data)[2],
  col.vars = names(data)[1],
  plot_weight = FALSE,
  print_skim = FALSE
)

rect_submatrix_plot <- plotMatrix(
  mat = subm$Rectangular_element_max$matrix,
  title = paste0(name, " Rectangular Submatrix")
)

square_submatrix_plot <- plotMatrix(
  mat = subm$Square$matrix,
  title = paste0(name, " Square Submatrix")
)

all_plots <- c(all_plots, list(bip / rect_submatrix_plot / square_submatrix_plot))

# Combine and save the multi-panel plot
multi_panel_plot <- wrap_plots(all_plots, ncol = 2) +
  plot_annotation(title = "Multi-Dataset Bipartite and Submatrix Visualization",
                  theme = theme(plot.title = element_text(size = 18, hjust = 0.5)))

ggsave("multi_panel_nimaa_datasets.png", multi_panel_plot, width = 16, height = 20, dpi = 300)
