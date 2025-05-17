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

# Replace diagonal p-values with 0 to prevent NA in stars
diag(p_mat) <- 0

# Create significance stars
star_matrix <- ifelse(p_mat < 0.001, "***",
                      ifelse(p_mat < 0.01, "**",
                             ifelse(p_mat < 0.05, "*", "")))

# Combine with rounded correlations
cor_with_stars <- matrix(paste0(round(cor_mat, 2), star_matrix),
                         nrow = nrow(cor_mat),
                         dimnames = dimnames(cor_mat))

# Fix diagonal to show clean 1.00
diag(cor_with_stars) <- "1.00"

# Melt data
melted_cor <- melt(cor_mat)
melted_text <- melt(cor_with_stars)
colnames(melted_cor) <- c("Var1", "Var2", "cor")
colnames(melted_text) <- c("Var1", "Var2", "label")
plot_data <- merge(melted_cor, melted_text, by = c("Var1", "Var2"))

# Keep only lower triangle
var_levels <- colnames(cor_mat)
plot_data <- plot_data[match(plot_data$Var1, var_levels) >= match(plot_data$Var2, var_levels), ]

# Plot: large matrix, smaller text
ggplot(plot_data, aes(x = Var2, y = Var1, fill = cor)) +
  geom_tile(color = "white", width = 1.0, height = 1.0) +  # Bigger boxes
  geom_text(aes(label = label), color = "black", size = 2.6) +  # Smaller cell text
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7),  # Smaller x labels
    axis.text.y = element_text(size = 7),  # Smaller y labels
    panel.grid = element_blank()
  ) +
  scale_x_discrete(expand = c(0, 0), limits = var_levels) +
  scale_y_discrete(expand = c(0, 0), limits = rev(var_levels))
