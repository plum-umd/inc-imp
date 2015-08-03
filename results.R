#!/usr/bin/env Rscript

library(dplyr,    quietly=TRUE, warn.conflicts = FALSE)
library(reshape2, quietly=TRUE, warn.conflicts = FALSE)

get.value <- function (df, artlib, col) {
  return(as.numeric(df[df$artlib == artlib,][col]))
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
    stop("Usage: ./results.R results/some-test-name.csv")
}
path <- args[1]
results <- read.csv(path)

# This groups the tests by configuration, takes the median time2 of
# each configuration's repetitions, and takes the best configuration
# for each artlib by time2 (then time1, eval1, and finally eval2).
results <- results %>%
             # Get median statistics of each configuration's tests.
             group_by(artlib, eval_strat, store_type, min_depth) %>%
             summarize( time1   = median(time1)
                      , dirty1  = median(dirty1)
                      , clean1  = median(clean1)
                      , create1 = median(create1)
                      , eval1   = median(eval1)
                      , time2   = median(time2)
                      , dirty2  = median(dirty2)
                      , clean2  = median(clean2)
                      , create2 = median(create2)
                      , eval2   = median(eval2) ) %>%
             # Get best configuration of each artlib.
             ungroup() %>%
             group_by(artlib) %>%
             top_n(1, -time2) %>%
             top_n(1, -time1) %>%
             top_n(1, -eval1) %>%
             top_n(1, -eval2)

results$time2 <- results$time2 * 1000

# Add result column.
results <- results %>%
    mutate(Variable = ifelse(artlib == "fromscratch", time2,
                             get.value(results, 'fromscratch', 'time2') /
                               get.value(results, artlib, 'time2'))) %>%
    ungroup()

results <- results[c("artlib", "Variable")]

# Rename rows.
results$artlib <- as.character(results$artlib)
results$artlib[results$artlib == "fromscratch"] <- "From Scratch (ms)"
results$artlib[results$artlib == "nominal"]     <- "Nominal Adapton (X)"
results$artlib[results$artlib == "structural"]  <- "Structural Adapton (X)"

# Long format --> short format
results <- mutate(results, id = 1)
results <- dcast(results, id ~ artlib, value.var="Variable")
results <- results[,!(names(results) %in% c("id"))]

options(width=160)
print.data.frame(results)
