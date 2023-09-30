# Install necessary libraries if not already installed
## install.packages("ggplot2")
## install.packages("ggrepel")
## install.packages("BiocManager")
## BiocManager::install("multtest")
## install.packages("ggpubr")

# Load necessary libraries
library(ggplot2)
library(ggrepel)
library(multtest)
library(ggpubr)

# Load the Golub et al. ALL/AML dataset
data(golub)

## Prep data
gene_values <- golub[1042,]
plot_data <- data.frame(Patients = 1:length(gene_values), Expression = gene_values)

## Basic Scatter plot dark
basic_dark_plot <- ggplot(plot_data, aes(x = Patients, y = Expression)) +
  geom_point(color = "lightskyblue") +
  geom_smooth(method="lm", se=FALSE, color="cornflowerblue", linetype="dashed") +
  geom_text_repel(aes(label = Expression), vjust = -0.5, color = "white", size = 2.5) +
  xlab("Patients") +
  ylab("Expression values") +
  ## facet_wrap(~c("ALL", "AML")) +
  labs(title = "Plot of gene expression values of CCND3 cyclin D3 gene") + 
  theme(
    panel.background = element_rect(fill = "grey2"),
    panel.grid.major = element_line(color = "lightskyblue1", linetype = "dotted"),
    panel.grid.minor = element_line(color = "steelblue2", linetype = "dotted"),
    axis.title.x = element_text(color = "steelblue4", size = 12, face = "bold"),
    axis.title.y = element_text(color = "steelblue4", size = 12, face = "bold"),
    plot.title = element_text(color = "royalblue3", size = 14, face = "bold")
  )

## Basic Scatter plot light
basic_light_plot <- ggplot(plot_data, aes(x = Patients, y = Expression)) +
  geom_point(color = "#6BAED6") +
 # geom_smooth(method = "lm", se = FALSE, color = "#2171B5", linetype = "dashed") +
  geom_text_repel(aes(label = Expression), vjust = -1, color = "gray40", size = 2.5) +
  xlab("Patients") +
  ylab("Expression values") +
  labs(title = "Plot of gene expression values of CCND3 cyclin D3 gene") +
  facet_wrap(~c("ALL", "AML")) +
  theme(
    panel.background = element_rect(fill = "#F0F0F0"),
    panel.grid.major = element_line(color = "#D9D9D9", linetype = "dotted"),
    panel.grid.minor = element_line(color = "#E5E5E5", linetype = "dotted"),
    axis.title.x = element_text(color = "#333333", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#333333", size = 12, face = "bold"),
    plot.title = element_text(color = "#333333", size = 14, face = "bold")
  )

## Publication ready scatter plot with labels, axis, legends
gol.fac <- factor(golub.cl, levels = 0:1, labels = c("ALL", "AML"))

publication_plot <- ggplot(plot_data, aes(x = Patients, y = Expression, 
                                          color = gol.fac)) +
  geom_point(size = 2, shape=1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.5) +
  geom_text_repel(aes(label = Expression), vjust = -2, color = "black", size = 2) +
  xlab("Patients") +
  ylab("Expression values") +

  labs(
    title = "Gene Expression Analysis of CCND3 Cyclin D3 Gene",
    subtitle = "Comparison of gene expression values between ALL and AML samples",
    caption = "Data source: Golub et al. ALL/AML dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#333333", size = 16, face = "bold"),
    plot.subtitle = element_text(color = "#666666", size = 12),
    plot.caption = element_text(color = "#666666", size = 10),
    axis.title.x = element_text(color = "#333333", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#333333", size = 12, face = "bold"),
    axis.text = element_text(color = "#666666", size = 10),
    legend.title = element_text(color = "#333333", size = 12, face = "bold"),
    legend.text = element_text(color = "#666666", size = 10),
    legend.position = "top",
    legend.background = element_rect(fill = "white"),
    legend.spacing.x = unit(0.5, "cm")
  ) +
  scale_color_manual(values = c("#6BAED6", "#F8766D"), name = "Sample Type")

## ggarrange
ggarrange(basic_dark_plot, publication_plot, ncol=1, nrow=2, heights=c(1,1))


## Prep data
gol.fac <- factor(golub.cl, levels = 0:1, labels = c("ALL", "AML"))
golub_fac_table <- table(gol.fac)
golub_fac_df <- data.frame(Type = names(golub_fac_table), Count = as.numeric(golub_fac_table))

## Boxplot
bplot <- ggplot(expression_data_box, aes(x = SampleType, y = Expression, fill = SampleType)) +
  geom_boxplot() +
  xlab("Types of Leukemia") +
  ylab("Expression values") +
  labs(title = "Boxplot of ALL and AML expression values of CCND3") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#333333", size = 16, face = "bold"),
    axis.title.x = element_text(color = "#333333", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#333333", size = 12, face = "bold"),
    axis.text = element_text(color = "#666666", size = 10),
    legend.position = "none"
  )

## Violin plot
vplot <- ggplot(expression_data_box, aes(x = SampleType, y = Expression, fill = SampleType)) +
  geom_violin() +
  xlab("Types of Leukemia") +
  ylab("Expression values") +
  labs(title = "Violin Plot of ALL and AML expression values of CCND3") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#333333", size = 16, face = "bold"),
    axis.title.x = element_text(color = "#333333", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#333333", size = 12, face = "bold"),
    axis.text = element_text(color = "#666666", size = 10),
    legend.position = "none"
  )

withoutStat <- ggarrange(bplot, vplot, nrow=1, ncol=2)

bplot_stat <- ggplot(expression_data_box, aes(x = SampleType, y = Expression, fill = SampleType)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar", width = 0.5, position = position_dodge(width = 0.75)) +
  xlab("Types of Leukemia") +
  ylab("Expression values") +
  labs(title = "Boxplot of ALL and AML expression values of CCND3") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#333333", size = 16, face = "bold"),
    axis.title.x = element_text(color = "#333333", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#333333", size = 12, face = "bold"),
    axis.text = element_text(color = "#666666", size = 10),
    legend.position = "none"
  )

vplot_stat <- ggplot(expression_data_box, aes(x = SampleType, y = Expression, fill = SampleType)) +
  geom_violin() +
  stat_boxplot(geom = "errorbar", width = 0.5, position = position_dodge(width = 0.75)) +
  xlab("Types of Leukemia") +
  ylab("Expression values") +
  labs(title = "Violin Plot of ALL and AML expression values of CCND3") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#333333", size = 16, face = "bold"),
    axis.title.x = element_text(color = "#333333", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#333333", size = 12, face = "bold"),
    axis.text = element_text(color = "#666666", size = 10),
    legend.position = "none"
  )

withStat <- ggarrange(bplot_stat, vplot_stat, nrow=1, ncol=2)

## nested ggarrange
ggarrange(withoutStat, withStat, nrow=2, heights=c(1, 1))





################################################################################

install.packages("Rtsne")
install.packages("vegan")
install.packages("cluster")
# Load necessary libraries
library(ggplot2)
library(Rtsne)
library(vegan)
library(cluster)

# Load the Golub et al. ALL/AML dataset
data(golub)

# Select a subset of the Golub dataset for analysis (e.g., first 100 genes and 30 patients)
subset_golub <- golub[1:100, 1:30]

# Perform PCA
pca_result <- prcomp(subset_golub, scale = TRUE)

# Create a data frame for PCA results
pca_df <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2])

# Create PCA scatter plot using ggplot2
ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(x = "PC1", y = "PC2", title = "PCA Plot")


# Perform t-SNE
tsne_result <- Rtsne(subset_golub)

# Create a data frame for t-SNE results
tsne_df <- data.frame(Dimension1 = tsne_result$Y[, 1], Dimension2 = tsne_result$Y[, 2])

# Create t-SNE scatter plot using ggplot2
ggplot(tsne_df, aes(x = Dimension1, y = Dimension2)) +
  geom_point() +
  labs(x = "Dimension 1", y = "Dimension 2", title = "t-SNE Plot")


# Perform K-means clustering (specify the number of clusters)
kmeans_result <- kmeans(t(subset_golub), centers = 3)

# Create a data frame for PCA results
pca_df <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2])

# Add cluster assignments to the PCA data frame
# Extend cluster labels to match the number of rows in pca_df
cluster_labels <- rep(kmeans_result$cluster, length.out = nrow(pca_df))
pca_df$Cluster <- as.factor(cluster_labels)

# Create K-means scatter plot using ggplot2
library(ggplot2)
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(x = "PC1", y = "PC2", title = "K-means Clustering") +
  scale_color_manual(values = c("red", "blue", "green"))





# Perform NMDS
nmds_result <- metaMDS(subset_golub)

# Create a data frame for NMDS results
nmds_df <- data.frame(Dimension1 = nmds_result$points[, 1], Dimension2 = nmds_result$points[, 2])

# Create NMDS scatter plot using ggplot2
ggplot(nmds_df, aes(x = Dimension1, y = Dimension2)) +
  geom_point(color = "blue") +
  labs(x = "Dimension 1", y = "Dimension 2", title = "NMDS Plot")




################################################################################

## Prep data

# Sample patients (you can adjust the number of patients)
set.seed(123)  # For reproducibility
num_patients <- 5

# Create a vector of patient names (you can replace these with actual patient names)
patients <- paste("Patient", 1:num_patients)


## Chord Diagram
#install.packages("circlize")
library(circlize)

# Generate synthetic relationship data (random values between 0 and 1)
relationship_data <- matrix(runif(num_patients^2), nrow = num_patients, ncol = num_patients)
rownames(relationship_data) <- colnames(relationship_data) <- patients

# Create the chord diagram
chordDiagram(relationship_data)


## Arc diagram
#install.packages("ggraph")
#install.packages("igraph")

library(ggraph)
library(igraph)


# Generate synthetic relationship data (random connections)
edges <- expand.grid(from = patients, to = patients)
edges <- edges[edges$from != edges$to, ]

# Create a graph from the synthetic edges
graph <- graph_from_data_frame(edges)

# Create an arc diagram
ggraph(graph, layout = "linear") +
  geom_edge_arc()


## Dendogram
num_genes <- 500  # Adjust this based on the number of genes in your dataset
selected_genes <- sample(1:nrow(golub), num_genes)

# Create a subset of the Golub dataset with random genes
subset_data <- golub[selected_genes, ]

# Perform hierarchical clustering
hierarchical_result <- hclust(dist(t(subset_data)))

# Create dendrogram plot
plot(hierarchical_result)




num_patients <- 38
rownames(golub) <- paste("Patient", 1:38)

patients <- rownames(golub)  # Get patient names from the golub dataset

patients

# Create a random assignment of nationalities to patients (for illustration)
nationalities <- sample(c("USA", "Canada", "UK", "France", "Germany", "Japan"), num_patients, replace = TRUE)

# Create a data frame with patient information (including nationality)
patient_data <- data.frame(Patient = patients, Nationality = nationalities)

# Create a basic map
library(ggplot2)
ggplot(patient_data, aes(x = Nationality, y = 1)) +
  geom_point() +
  labs(title = "Geographic Visualization (Fictional)")




