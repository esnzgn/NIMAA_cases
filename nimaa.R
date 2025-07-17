# ==== Load required libraries ====
library(NIMAA)
library(igraph)
library(ggplot2)
library(pheatmap)
library(dplyr)

# ==== Load example data ====
data("nominal_data")
head(nominal_data)

# ==== Step 1: Plot Incidence Matrix ====
inc <- plotIncMatrix(
  data = nominal_data,
  part1 = "Label1",
  part2 = "Label2",
  value = "value",
  return_matrix = TRUE
)

# View incidence matrix
print(dim(inc$matrix))
# Heatmap of incidence matrix
pheatmap(inc$matrix, main = "Original Incidence Matrix")

# ==== Step 2: Extract Submatrix (Largest Non-Missing Block) ====
subm <- extractSubMatrix(
  incidence_matrix = inc$matrix,
  method = "binmatnest"
)
# Visualize
pheatmap(subm$matrix, main = "Extracted Submatrix")

# ==== Step 3: Clustering on Projected Networks ====
cl <- findCluster(
  incidence_matrix = subm$matrix,
  method = "louvain"
)
# Plot clustering result
plotBipartiteGraph(
  incidence_matrix = subm$matrix,
  clusters = cl,
  layout = "bipartite"
)

# ==== Step 4: Projected Graphs ====
proj <- projectMatrix(
  incidence_matrix = subm$matrix,
  part = "both"
)
# Visualize each projection
par(mfrow = c(1, 2))
plot(proj$Label1, main = "Projection: Label1", vertex.label.cex = 0.7)
plot(proj$Label2, main = "Projection: Label2", vertex.label.cex = 0.7)

# ==== Step 5: Data Imputation ====
# Create a simulated missing version of the submatrix
subm_missing <- subm$matrix
subm_missing[sample(length(subm_missing), size = floor(0.1 * length(subm_missing)))] <- NA

# Run imputation methods
imputations <- imputeMissingValue(
  incidence_matrix = subm_missing,
  methods = c("mean", "median", "svd", "als", "lls")
)

# ==== Step 6: Evaluate Imputation ====
# Evaluate with ground truth (original submatrix)
evals <- evaluateImputation(
  imputations = imputations,
  original_matrix = subm$matrix,
  metrics = c("rmse", "cor")
)
print(evals)

# ==== Step 7: Plot Imputation Results ====
# Heatmap of one imputation (e.g., SVD)
pheatmap(imputations$svd, main = "SVD Imputed Matrix")
