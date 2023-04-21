library(ggplot2)



bc <- read.csv("https://raw.githubusercontent.com/AndrewPiasecki/Fake_Bills/main/data.csv")



# Step 2: Remove non-numeric columns
data <- bc[,3:32]

# Step 3: Standardize the numeric columns
data_scaled <- scale(data)


# Step 4: Perform PCA on the standardized data
pca <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

# Step 5: Extract the principal components
summary(pca)

plot(pca)

PC1 <- pca$x[, 1]
PC2 <- pca$x[, 2]

scatter_plot <- ggplot(data, aes(x = PC1, y = PC2, color = bc$diagnosis)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "PC1 vs PC2", x = "PC1", y = "PC2")
scatter_plot






get_eigenvalue(pca)
fviz_eig(pca)

# Scree Plot shows how much variation of PCA captures from the data

fviz_pca_biplot(pca)



svmPCA <- svm(PC2 ~ PC1, data = bc, kernel = "linear", cost = 10, scale = FALSE)
summary(svmPCA)



