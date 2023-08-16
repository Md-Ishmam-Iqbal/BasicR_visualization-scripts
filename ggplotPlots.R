# Install necessary libraries if not already installed
# install.packages("ggplot2")
# install.packages("ggrepel")
# install.packages("BiocManager")
# BiocManager::install("multtest")


# Load necessary libraries
library(ggplot2)
library(ggrepel)
library(multtest)


# Load the Golub et al. ALL/AML dataset
data(golub)

gol.fac <- factor(golub.cl, levels = 0:1, labels = c("ALL", "AML"))


# Plot gene expression values using ggplot2

## Prep data
gene_index <- 1042
gene_values <- golub[gene_index,]

plot_data <- data.frame(Patients = 1:length(gene_values), Expression = gene_values)

## Basic Scatter plot
ggplot(plot_data, aes(x = Patients, y = Expression)) +
  geom_point(color = "lightskyblue") +
  geom_smooth(method="lm", se=FALSE, color="cornflowerblue", linetype="dashed") +
  geom_text_repel(aes(label = Expression), vjust = -0.5, color = "white", size = 2.5) +
  xlab("Patients") +
  ylab("Expression values") +
  labs(title = "Plot of gene expression values of CCND3 cyclin D3 gene") + 
  theme(
    panel.background = element_rect(fill = "grey2"),
    panel.grid.major = element_line(color = "lightskyblue1", linetype = "dotted"),
    panel.grid.minor = element_line(color = "steelblue2", linetype = "dotted"),
    axis.title.x = element_text(color = "steelblue4", size = 12, face = "bold"),
    axis.title.y = element_text(color = "steelblue4", size = 12, face = "bold"),
    plot.title = element_text(color = "royalblue3", size = 14, face = "bold")
  )

## Publication ready scatter plot
ggplot(plot_data, aes(x = Patients, y = Expression)) +
  geom_point(color = "#6BAED6") +
  geom_smooth(method = "lm", se = FALSE, color = "#2171B5", linetype = "dashed") +
  geom_text_repel(aes(label = Expression), vjust = -1, color = "gray40", size = 2.5) +
  xlab("Patients") +
  ylab("Expression values") +
  labs(title = "Plot of gene expression values of CCND3 cyclin D3 gene") +
  theme(
    panel.background = element_rect(fill = "#F0F0F0"),
    panel.grid.major = element_line(color = "#D9D9D9", linetype = "dotted"),
    panel.grid.minor = element_line(color = "#E5E5E5", linetype = "dotted"),
    axis.title.x = element_text(color = "#333333", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#333333", size = 12, face = "bold"),
    plot.title = element_text(color = "#333333", size = 14, face = "bold")
  )


## Publication ready scatter plot with labels, axis, legends

ggplot(plot_data, aes(x = Patients, y = Expression, color = gol.fac)) +
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
    legend.position = "top",  # Adjust the legend position
    legend.background = element_rect(fill = "white"),  # Add a white background to the legend
    legend.spacing.x = unit(0.5, "cm")  # Adjust horizontal spacing between legend items
  ) +
  scale_color_manual(values = c("#6BAED6", "#F8766D"), name = "Sample Type",
                     labels = c("ALL", "AML"))



# Stripchart

## Create a data frame from the golub matrix
golub_df <- data.frame(Expression = golub[1042,], SampleType = as.factor(gol.fac))

## Stripchart
ggplot(golub_df, aes(x = SampleType, y = Expression, color = SampleType)) +
  geom_jitter(width = 0.1, height = 0) +
  xlab("Types of patients") +
  ylab("Expression values") +
  labs(title = "Strip chart of gene expression values of CCND3") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#333333", size = 16, face = "bold"),
    axis.title.x = element_text(color = "#333333", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#333333", size = 12, face = "bold"),
    axis.text = element_text(color = "#666666", size = 10)
  )

# Barplot
## Prepare data
golub_fac_table <- table(gol.fac)
golub_fac_df <- data.frame(Type = names(golub_fac_table), Count = as.numeric(golub_fac_table))

## Barplot
ggplot(golub_fac_df, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity", color = "black") +
  xlab("Types of leukemia") +
  ylab("Number of patients") +
  labs(title = "Barplot of patients by types of leukemia") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#333333", size = 16, face = "bold"),
    axis.title.x = element_text(color = "#333333", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#333333", size = 12, face = "bold"),
    axis.text = element_text(color = "#666666", size = 10),
    legend.position = "none"
  )

# Histogram
## Prepare data
expression_data <- data.frame(Expression = golub[1042,])

## Histogram
ggplot(expression_data, aes(x = Expression)) +
  geom_histogram(binwidth = 1, fill = "powderblue", color = "black") +
  xlab("Expression values of ALL") +
  ylab("Frequency") +
  labs(title = "Histogram of ALL expression values of CCND3 cyclin D3") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#333333", size = 16, face = "bold"),
    axis.title.x = element_text(color = "#333333", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#333333", size = 12, face = "bold"),
    axis.text = element_text(color = "#666666", size = 10)
  )


# Density plot
## Create a data frame from the CCND3 expression values for "ALL"
expression_data_all <- data.frame(Expression = golub[1042, gol.fac == "ALL"])

## Density plot
ggplot(expression_data_all, aes(x = Expression, fill = "red")) +
  geom_density(alpha = 0.5, color = "red") +
  xlab("Expression values of ALL") +
  ylab("Density") +
  labs(title = "Density curve of ALL expression values of CCND3 cyclin D3 gene") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#333333", size = 16, face = "bold"),
    axis.title.x = element_text(color = "#333333", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#333333", size = 12, face = "bold"),
    axis.text = element_text(color = "#666666", size = 10)
  )

# Box plot

## Prepare dataset
expression_data_box <- data.frame(Expression = golub[1042,], SampleType = as.factor(gol.fac))

## Boxplot
ggplot(expression_data_box, aes(x = SampleType, y = Expression, fill = SampleType)) +
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


# Pie chart

ceval <- ifelse(golub[1042,] < 1.5, "A", ifelse(golub[1042,] <= 2, "B", "C"))

# Table of ceval
ftab <- table(ceval)

# Labels for pie chart
lbls <- c("A", "B", "C")

# Pie chart
ggplot(data.frame(ftab), aes(x = "", y = Freq, fill = ceval)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Pie chart of expression values of CCND3 Cyclin D3")


# Line plot

## Create a data frame from the CCND3 expression values
expression_data_line <- data.frame(Patients = seq_along(golub[1042,]), Expression = golub[1042,])

## Line plot
ggplot(expression_data_line, aes(x = Patients, y = Expression)) +
  geom_line() +
  xlab("Patients") +
  ylab("Expression value") +
  labs(title = "Line plot of expression values of Gene CCND3") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#333333", size = 16, face = "bold"),
    axis.title.x = element_text(color = "#333333", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#333333", size = 12, face = "bold"),
    axis.text = element_text(color = "#666666", size = 10)
  )