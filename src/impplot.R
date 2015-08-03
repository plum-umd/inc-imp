#!/usr/bin/Rscript

# Assumes the following libraries have been installed:
library(reshape2)
library(ggplot2)
library(ggmap)

# Usage: ./impplot.R file barFactor xFactor gridFacet
# Where barFactor, xFactor, and gridFactet are one of
# { "memoType", "eval", "sto", "minDepth" } (with
# no duplicates).
args <- commandArgs(trailingOnly = TRUE)

file = args[1]
barFactor = args[2]
gridFacet = args[3]

# Rearrange
df = read.csv(file)
df.long <- melt(df, id.vars = c("memoType", "eval", "sto", "minDepth"))

# Display
X11()
ggplot(df.long, aes(variable, value, fill=as.factor(df.long[,barFactor]))) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(as.formula(paste("~", gridFacet, sep='')) ,nrow=2)
gglocator(1)

