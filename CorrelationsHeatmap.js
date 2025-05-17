# Load libraries
library(Hmisc)
library(ggplot2)
library(reshape2)

# Select your variables
your_vars <- Thesis_for_Process[, c("SDO", "LUSO", "PVJ", "RPC", "CA", "Education", "Political.orientation")]

# Compute correlation and p-values
result <- rcorr(as.matrix(your_vars), type = "pearson")
cor_mat <- result$r
p_mat <- result$P

# Replace diagonal p-values with 0 (or 1 if you want no stars on diagonal)
diag(p_mat) <- 0

# Create significance stars
star_matrix <- ifelse(p_mat < 0.001, "***",
                      ifelse(p_mat < 0.01, "**",
                             ifelse(p_mat < 0.05, "*", "")))

# Combine correlation values with stars
cor_with_stars <- matrix(paste0(round(cor_mat, 2), star_matrix),
                         nrow = nrow(cor_mat),
                         dimnames = dimnames(cor_mat))

# Ensure clean 1.00 values on the diagonal (no stars if preferred)
diag(cor_with_stars) <- "1.00"

# Melt data for plotting
melted_cor <- melt(cor_mat)
melted_text <- melt(cor_with_stars)
colnames(melted_cor) <- c("Var1", "Var2", "cor")
colnames(melted_text) <- c("Var1", "Var2", "label")
plot_data <- merge(melted_cor, melted_text, by = c("Var1", "Var2"))

# Plot heatmap with smaller text and axis labels
ggplot(plot_data, aes(x = Var1, y = Var2, fill = cor)) +
  geom_tile(color = "white", width = 1, height = 1) +
  geom_text(aes(label = label), color = "black", size = 3) +  # Cell label text
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal(base_size = 11) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9),  # Smaller X labels
    axis.text.y = element_text(size = 9),  # Smaller Y labels
    panel.grid = element_blank()
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))
