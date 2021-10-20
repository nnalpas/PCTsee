


library(magrittr)

my_pg_path <- "D:/MQ_Data/Srimosus_20210920/G7/combined/txt/proteinGroups.txt"
my_crossmap_path <- "H:/data/Srim_6frame_2nd_analysis/CrossMap/Cross-mapping.txt"
my_fasta_path <- "D:/MQ_Data/Srimosus_20210920/CP048261_CP048262_prot_sequence.fasta"

my_pg <- data.table::fread(
    input = my_pg_path, sep = "\t", quote = "", header = TRUE,
    stringsAsFactors = FALSE, colClasses = "character")

my_crossmap <- data.table::fread(
    input = my_crossmap_path, sep = "\t", quote = "", header = TRUE,
    stringsAsFactors = FALSE, colClasses = "character")

my_fasta <- seqinr::read.fasta(
    file = my_fasta_path, seqtype = "AA",
    as.string = TRUE, whole.header = TRUE)

all_ids <- data.frame(Header = names(my_fasta), stringsAsFactors = FALSE) %>%
    tidyr::separate(
        data = ., col = Header, into = c("Name", "ID", "Description"),
        sep = " ", convert = FALSE, extra = "merge")

all_ids_final <- my_crossmap %>%
    dplyr::select(., Name = qseqid, OldName = sseqid) %>%
    dplyr::left_join(x = all_ids, y = ., by = "Name") %>%
    dplyr::select(., -Description)

my_pg_id_map <- my_pg %>%
    dplyr::select(., id, `Protein IDs`) %>%
    tidyr::separate_rows(
        data = ., `Protein IDs`, sep = ";", convert = FALSE) %>%
    dplyr::left_join(
        x = ., y = all_ids_final, by = c("Protein IDs" = "ID")) %>%
    tidyr::replace_na(data = ., replace = list(Name = "", OldName = "")) %>%
    dplyr::group_by(., id) %>%
    dplyr::summarise_all(~paste0(., collapse = ";")) %>%
        dplyr::select(., id, OtherName1 = Name, OtherName2 = OldName)

my_pg_final <- dplyr::left_join(x = my_pg, y = my_pg_id_map, by = "id")

data.table::fwrite(
    x = my_pg_final, file = sub("\\.txt", "_crossmap.txt", my_pg_path),
    append = FALSE, quote = FALSE, sep = "\t",
    row.names = FALSE, col.names = TRUE)


