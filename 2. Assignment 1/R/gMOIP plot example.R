library(gMOIP)
library(tidyverse)

A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
obj <- c(7.75, 10)  # coefficients c


plotPolytope(
  A,
  b,
  obj,
  type = rep("c", ncol(A)),
  crit = "max",
  faces = rep("c", ncol(A)),
  plotFaces = TRUE,
  plotFeasible = TRUE,
  plotOptimum = TRUE,
  labels = "coord"
) + ggplot2::xlab("x") + ggplot2::ylab("y") + ggplot2::ggtitle("Solution (max) with other axis labels")

## MILP model
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
obj <- c(7.75, 10)
# Second coordinate integer
plotPolytope(
  A,
  b,
  obj,
  type = c("c", "i"),
  crit = "max",
  faces = c("c", "i"),
  plotFaces = FALSE,
  plotFeasible = TRUE,
  plotOptimum = TRUE,
  labels = "coord"
)
# First coordinate integer and with LP faces:
plotPolytope(
  A,
  b,
  obj,
  type = c("i", "c"),
  crit = "max",
  faces = c("c", "c"),
  plotFaces = TRUE,
  plotFeasible = TRUE,
  plotOptimum = TRUE,
  labels = "coord"
)
# First coordinate integer and with LP faces:
plotPolytope(
  A,
  b,
  obj,
  type = c("i", "c"),
  crit = "max",
  faces = c("i", "c"),
  plotFaces = TRUE,
  plotFeasible = TRUE,
  plotOptimum = TRUE,
  labels = "coord"
)