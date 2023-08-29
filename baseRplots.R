# Load necessary libraries
library(ggplot2)
library(ggrepel)
library(multtest)


# Load the Golub et al. ALL/AML dataset
data(golub)

gol.fac <- factor(golub.cl, levels = 0:1, labels = c("ALL", "AML"))

plot(golub[1042, ],
     col = "blue", xlab = "Patients", ylab = "Expression values",
     main = "Plot of gene expression values of CCND3 cyclin D3 gene"
)

barplot(table(gol.fac),
     col = "lightblue", xlab = "Types of leukemia",
     ylab = "Number of patients", main = "Barplot of patients by types of leukemia"
)

boxplot(golub[1042, ] ~ gol.fac, col = "lightgreen", main = "Boxplot of ALL and AML expression
        values of CCND3")
title(xlab = "Types of Leukemia", ylab = "Expression values")
