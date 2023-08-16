# Load necessary libraries
library(ggplot2)
library(ggrepel)
library(multtest)


# Load the Golub et al. ALL/AML dataset
data(golub)

gol.fac <- factor(golub.cl, levels = 0:1, labels = c("ALL", "AML"))

plot(golub[1042,], col="blue", xlab="Patients", ylab="Expression values",
    main="Plot of gene expression values of CCND3 cyclin D3 gene")

stripchart(golub[1042,]~gol.fac, method="jitter", vertical=TRUE, 
           col=c("red","darkgreen"), xlab="Types of patients",
           ylab="Expression values", main="Strip chart of gene expression values \n of CCND3 for AML and ALL patients")

barplot(table(gol.fac), col="lightblue", xlab="Types of leukemia", 
        ylab="Number of patients", main="Barplot of patients by types of leukemia")

hist(golub[1042, gol.fac=="ALL"], col="lightblue", xlab = "Expression values of ALL",
     ylab="Frequency", main="Histogram of ALL expression values of CCND3 cyclin D3")

plot(density(golub[1042, gol.fac=="ALL"]), col="red", xlab="Expression values of ALL",
     ylab="Density", main="Density curve of ALL expression values of CCND3 cyclin D3 gene")

boxplot(golub[1042,]~gol.fac, col="lightgreen", main="Boxplot of ALL and AML expression
        values of CCND3")
title(xlab="Types of Leukemia", ylab="Expression values")


# pie chart

ceval = NULL
ceval[golub[1042,]<1.5] <- 1
ceval[golub[1042,]>=1.5 & golub[1042,]<=2] <- 2
ceval[golub[1042,]>2] <- 3
ftab <- table(ceval)
lbls <- c("A", "B", "C")
pie(ftab, 
    labels=lbls,
    main="Pie chart of expression values of CCND3 Cyclin D3")


dotchart(golub[1:10,1],
         labels=golub.gnames[1:10,1],
         xlab="Expression values",
         ylab="Gene number",
         main="Expression values of 10 genes of first patient")


# Line plot
plot(golub[1042,],
     type="l",
     ylab="Expression value", xlab="Patients",
     main="Line plot of expression values of Gene CCND3")

