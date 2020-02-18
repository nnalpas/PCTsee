


#
library(magrittr)
library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(lubridate)
library(stringr)

# 
my_pg_path <- "S:/processing/Fabio/HipA turnover triplicates/combined/txt/proteinGroups.txt"

#
my_pg <- data.table::fread(
    input = my_pg_path, sep = "\t", quote = "", header = TRUE,
    stringsAsFactors = FALSE, integer64 = "double",
    data.table = FALSE, na.strings = "NaN")

# 
my_pheno_path <- "C:/Users/kxmna01/Dropbox/Home_work_sync/Work/Colleagues shared work/Fabio/HipA-HipB/HipA/Phenodata.txt"

# 
my_pheno <- data.table::fread(
    input = my_pheno_path, sep = "\t", quote = "", header = TRUE,
    stringsAsFactors = FALSE, integer64 = "double",
    data.table = FALSE, na.strings = "NaN")

#
my_pheno_filt <- my_pheno %>%
    dplyr::select(., -Name) %>%
    unique(.)

#
if (any(grepl("Time", colnames(my_pheno_filt)))) {
    my_pheno_filt %<>%
        tidyr::separate(
            data = ., col = Time, into = c("Time value", "Time unit"),
            sep = "(?<=[0-9])(?=[A-Za-z])", remove = FALSE, convert = TRUE) %>%
        dplyr::rowwise(.) %>%
        dplyr::mutate(
            .,
            Duration = as.numeric(
                duration(num = `Time value`, units = `Time unit`),
                "hours"))
}

# 
my_pg_format <- my_pg %>%
    tidyr::gather(
        data = ., key = "key", value = "value", -id,
        na.rm = FALSE, convert = FALSE)

# Extract the experiment name from affected columns
all_exps <- my_pheno_filt$Experiment
patt_exps <- paste0("\\b", paste(all_exps, collapse = "\\b|\\b"), "\\b")
my_pg_exp <- my_pg_format %>%
    dplyr::mutate(., Experiment = as.character(str_match(
        string = key, pattern = patt_exps))) %>%
    dplyr::mutate(., key_no_exp = str_replace(
        string = key,
        pattern = paste0(" ", Experiment),
        replacement = ""))

# Extract the labelling from affected columns
all_labels <- grep("Intensity ", my_pg_exp$key_no_exp, value = TRUE) %>%
    unique(.) %>%
    sub("Intensity ", "", .)
all_ratios <- expand.grid(
    first = all_labels, second = all_labels, stringsAsFactors = FALSE) %>%
    dplyr::filter(., first != second) %>%
    dplyr::mutate(., ratio = paste(first, second, sep = "/"))
my_pg_label <- my_pg_exp %>%
    dplyr::mutate(
        ., Label = dplyr::case_when(
            grepl("Ratio ", key_no_exp) ~ as.character(str_match(
                string = key_no_exp,
                pattern = paste(all_ratios$ratio, collapse = "|"))),
            grepl("Intensity |iBAQ ", key_no_exp) ~ as.character(str_match(
                string = key_no_exp,
                pattern = paste(all_labels, collapse = "|"))),
            TRUE ~ paste(all_labels, collapse = "+")),
        key_no_lab = str_replace(
            string = key_no_exp,
            pattern = paste0(" ", Label),
            replacement = ""))

my_pg_final <- my_pg_label %>%
    dplyr::select(., -key_no_exp, -key) %>%
    dplyr::left_join(x = ., y = my_pheno_filt, by = "Experiment")

my_cross_map <- my_pg_final %>%
    dplyr::filter(., key_no_lab %in% c("Protein IDs", "Gene names")) %>%
    dplyr::select(., id, key_no_lab, value) %>%
    tidyr::separate_rows(
        data = ., value, sep = ";", convert = TRUE) %>%
    dplyr::filter(., !is.na(value) & value != "")

saveRDS(
    object = list(protein = my_pg_final, crossmap = my_cross_map),
    file = "C:/Users/kxmna01/Documents/GitHub/PCTshowoff/inst/extdata/HipA.RDS",
    compress = "gzip")


