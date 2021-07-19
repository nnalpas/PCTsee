


rm(list = ls())

source("C:/Users/kxmna01/Documents/GitHub/PCTsee/R/helpers.R")

#
library(magrittr)
library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggpubr)

my_f <- choose.files(caption = "Select a PCTsee RDS file!", multi = FALSE)

my_data <- readRDS(my_f)

add_cols <- c(
    "Protein IDs", "Majority protein IDs", "Protein names", "Gene names",
    "Sequence coverage [%]", "Q-value", "Score", "Only identified by site")

my_id <- unique(my_data$crossmap[
    my_data$crossmap$value == my_data$default$`Gene names / Protein IDs`, ][["id"]])

default_plot <- duration_plot(
    target = my_id,
    df = my_data$protein,
    x = my_data$default$`X-axis`,
    y = my_data$default$`Y-axis`,
    colour = my_data$default$`Colour per`,
    shape = my_data$default$`Point shape per`,
    add_cols = add_cols,
    add_ref_value = FALSE)
default_plot$plot