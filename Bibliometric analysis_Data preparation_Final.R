# ##############################################################################
# # Bibliometric Analysis: Rural Cancer - Global Dataset
# # Author: Tanja Kleinhappel
# # Date: July 2025
# # Description: This script performs a comprehensive bibliometric analysis
# #              on a global dataset related to rural cancer research.
# #              It covers data completeness, descriptive analysis, publication
# #              trends, country-level productivity, and
# #              adjustments for population and GDP.
# ##############################################################################

# --- Setup: Package Management ---

# Install and load required packages.
# This block checks if packages are installed and installs them if missing,
# making the script more robust for new users.

# List of all packages required for the script
packages_required <- c(
  "bibliometrix","openxlsx", "tidyverse",
  "data.table", "reshape2", "RColorBrewer", "plyr", "tidyr",
  "igraph", "ggpubr", "magick", "scales", "ggpattern", "maps",
  "ggrepel", "rnaturalearth", "rnaturalearthdata", "here"
)

# Identify and install missing packages
new_packages <- packages_required[!(packages_required %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  message("Installing missing packages: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages, dependencies = TRUE)
}

# Load all required packages silently
invisible(lapply(packages_required, library, character.only = TRUE))

# Clear all objects from the current workspace to ensure a clean environment
rm(list = ls())

# --- Setup: Project Directory ---
# If your project root is the folder containing "Biblio_Analysis",
# and your script is at Biblio_Analysis/GlobalDataAnalysis/Bibliometric_analysis_rural_cancer_dataprep.R

# NOTE: This line requires the script to be saved with this exact filename.
# If you rename the file, update the filename here to match, or here() will
# not be able to locate the project root correctly.

here::i_am("Bibliometric analysis_Data preparation_Final.R")


################################################################################
###### LOAD DATABASES AND ONLY KEEP ARTICLES
################################################################################

#### Helper function to convert to dataframe
# This function will now only handle the conversion, no meta-tag extraction.
process_database_conversion <- function(file_name, dbsource) {
  # Construct the full path using here()
  full_file_path <- here("Searches", file_name)
  
  # Determine format based on dbsource for convert2df
  format_type <- if (dbsource == "wos") "bibtex" else "csv"
  
  df_full <- bibliometrix::convert2df(full_file_path, dbsource = dbsource, format = format_type)
  
  return(df_full)
}

#################### Web of Science database ########################
## Search terms in data base are: TS=(Cancer AND Rural) - no restriction on dates
## date of search: 25/02/2025
## downloaded as BibTex file format

wos_article_types <- c(
  "ARTICLE", "ARTICLE; EARLY ACCESS", "ARTICLE; PROCEEDINGS PAPER",
  "REVIEW", "REVIEW; EARLY ACCESS", "ARTICLE; DATA PAPER"
)

# Load and process WoS data initially
WoSdataset_raw <- process_database_conversion("WoS/WoSfullSearch.bib", dbsource = "wos")

# Now, perform the metaTagExtraction as a separate step.

# First, remove the columns to ensure they are re-created correctly
WoSdataset_raw <- WoSdataset_raw %>% 
  select(-any_of(c("AU1_UN", "AU_UN")))

# Now, re-add them using metaTagExtraction
WoSdataset_raw <- bibliometrix::metaTagExtraction(WoSdataset_raw, Field = "AU_CO", sep = ";")
WoSdataset_raw <- bibliometrix::metaTagExtraction(WoSdataset_raw, Field = "AU1_CO", sep = ";")
WoSdataset_raw <- bibliometrix::metaTagExtraction(WoSdataset_raw, Field = "AU1_UN", sep = ";")
WoSdataset_raw <- bibliometrix::metaTagExtraction(WoSdataset_raw, Field = "AU_UN", sep = ";")


# Filter for articles after initial processing
WoSdataset_articles <- WoSdataset_raw %>%
  filter(DT %in% wos_article_types)

message(paste0("Original WoS records: ", nrow(WoSdataset_raw)))
message(paste0("WoS Articles/Reviews kept: ", nrow(WoSdataset_articles)))
message(paste0("WoS Dropped non-articles: ", nrow(WoSdataset_raw) - nrow(WoSdataset_articles)))

# Initial save of articles
save(WoSdataset_articles, file = here("Searches", "WoS", "WoSdataset_articles.Rda"))
write.xlsx(WoSdataset_articles, file = here("Searches", "WoS", "WoSdataset_articles.xlsx"))

### CLEAN WoS DATASET
WoSdataset_final <- WoSdataset_articles %>%
  # 1. Remove Anonymous authors
  filter(AU != "[ANONYMOUS] A") %>%
  # 2. Clean RP column (using stringr for regex)
  mutate(
    RP = case_when(
      str_detect(RP, "CORRESPONDING AUTHOR") ~ str_replace_all(RP, "(CORRESPONDING AUTHOR),?", ";"),
      TRUE ~ "" # If no "CORRESPONDING AUTHOR", make it empty
    )
  ) %>%
  # 3. Corrected AU_UN cleaning: replicate original's split-remove-join logic
  mutate(
    AU_UN = map_chr(AU_UN, ~ {
      # Split the string by ";", find the element containing "CORRESPONDING AUTHOR",
      # remove that element, and paste the remaining ones back together.
      affiliations <- str_split(., pattern = ";", simplify = FALSE)[[1]]
      cleaned_affiliations <- affiliations[!grepl("CORRESPONDING AUTHOR", affiliations, fixed = TRUE)]
      str_c(cleaned_affiliations, collapse = ";")
    }),
    # 4. Correct AU1_UN cleaning: The original logic was to clear the entire cell, not just the text.
    AU1_UN = if_else(str_detect(AU1_UN, "CORRESPONDING AUTHOR"), "", AU1_UN)
  ) %>%
  # 5. Drop CR column
  select(-CR) %>%
  # Fix specific error for AU1_CO - using row-wise operation if truly a single fix
  mutate(
    AU1_CO = case_when(
      row_number() == 5250 ~ "ITALY",
      TRUE ~ AU1_CO
    )
  )

message(paste0("WoS records after cleaning: ", nrow(WoSdataset_final)))
save(WoSdataset_final, file = here("Searches", "WoS", "WoSdataset_final.Rda"))
write.xlsx(WoSdataset_final, file = here("Searches", "WoS", "WoSdataset_final.xlsx"))


#########################     Scopus      ############################################
## Search terms in data base: (TITLE-ABS-KEY (rural)) AND (TITLE-ABS-KEY (cancer ))
## date of search: 25/02/2025
## downloaded as csv file format

scopus_article_types <- c("ARTICLE", "REVIEW")

# Load and process Scopus data initially
ScopusDataset_raw <- process_database_conversion("Scopus/ScopusFullSearch.csv", dbsource = "scopus")

# Replicate the original's specific logic for meta-tag extraction.
# The original script first removes these columns before re-extracting them.
ScopusDataset_raw <- ScopusDataset_raw %>%
  select(-any_of(c("AU1_UN", "AU_UN"))) %>%
  bibliometrix::metaTagExtraction(Field = "AU_CO", sep = ";") %>%
  bibliometrix::metaTagExtraction(Field = "AU1_CO", sep = ";") %>%
  bibliometrix::metaTagExtraction(Field = "AU1_UN", sep = ";") %>%
  bibliometrix::metaTagExtraction(Field = "AU_UN", sep = ";")

# Filter for articles and reviews after initial processing
ScopusDataset_articles <- ScopusDataset_raw %>%
  filter(DT %in% scopus_article_types)

message(paste0("Original Scopus records: ", nrow(ScopusDataset_raw)))
message(paste0("Scopus articles/reviews kept: ", nrow(ScopusDataset_articles)))
message(paste0("Scopus Dropped non-articles: ", nrow(ScopusDataset_raw) - nrow(ScopusDataset_articles)))

# Initial save of articles
save(ScopusDataset_articles, file = here("Searches", "Scopus", "ScopusDataset_articles.Rda"))
# The original code's `write.xlsx` was commented out, so we will keep it that way.
# write.xlsx(ScopusDataset_articles, file = here("Biblio_Analysis", "GlobalDataAnalysis", "Searches", "Scopus", "ScopusDataset_articles.xlsx"))


###### CLEAN SCOPUS DATASET 
# Apply cleaning steps sequentially as per original logic
ScopusDataset_final <- ScopusDataset_articles %>%
  # 1. Remove empty author records
  filter(AU != "") %>%
  # 2. Fix fields with too long entries.
  mutate(CR = if_else(nchar(CR) > 30000, " ", CR)) %>%
  # 3. Change [No ABSTRACT] to blank.
  mutate(AB = str_replace_all(AB, fixed("[NO ABSTRACT AVAILABLE]"), " ")) %>%
  # 4. Clean RP column. Original clears the ENTIRE cell if it starts with a semicolon.
  mutate(RP = if_else(str_detect(RP, "^;"), "", RP)) %>%
  # 5. Clean AU_UN and AU1_UN columns: The original uses `sub` which only replaces
  # the FIRST occurrence of "NOTREPORTED".
  mutate(
    AU_UN = str_replace(AU_UN, "NOTREPORTED", ""),
    AU1_UN = str_replace(AU1_UN, "NOTREPORTED", "")
  ) %>%
  # 6. Replicate the redundant AU_UN cleaning step from the original script
  # The original script performs this step twice, so we will as well.
  mutate(AU_UN = str_replace(AU_UN, "NOTREPORTED", "")) %>%
  # Remove the temporary 'max' column if it was created
  select(-any_of("max"))

message(paste0("Scopus records after cleaning: ", nrow(ScopusDataset_final)))
save(ScopusDataset_final, file = here("Searches", "Scopus", "ScopusDataset_final.Rda"))
write.xlsx(ScopusDataset_final, file = here("Searches", "Scopus", "ScopusDataset_final.xlsx"))


################################################################################
###### MERGE WoS AND SCOPUS DATASETS
################################################################################

# Load datasets
load(file = here("Searches", "WoS", "WoSdataset_final.Rda"))
load(file = here("Searches", "Scopus", "ScopusDataset_final.Rda"))

# Merge datasets and remove duplicates
Merged_WoS_Scopus_Db <- bibliometrix::mergeDbSources(WoSdataset_final, ScopusDataset_final, remove.duplicated = TRUE)

message(paste0("Total WoS articles: ", nrow(WoSdataset_final)))
message(paste0("Total Scopus articles: ", nrow(ScopusDataset_final)))
message(paste0("Total articles after merging and removing duplicates: ", nrow(Merged_WoS_Scopus_Db)))

save(Merged_WoS_Scopus_Db, file = here("Searches", "Merged_WoS_Scopus_Db.rda"))
write.xlsx(Merged_WoS_Scopus_Db, file = here("Searches", "Merged_WoS_Scopus_Db.xlsx"))

#load(file = here("Searches", "Merged_WoS_Scopus_Db.Rda"))

## Select relevant columns for the final dataset
# The original code's select is a bit verbose, the new select works perfectly fine.
FullDataset <- Merged_WoS_Scopus_Db %>%
  select(
    AU, DE, ID, C1, AB, DI, SO, LA, TC, TI, DT, PY, SR, SR_FULL, CR, DB, RP,
    AU_UN, AU_CO, AU1_CO
  )

save(FullDataset, file = here("Searches", "FullDataset.rda"))
write.xlsx(FullDataset, file = here("Searches", "FullDataset.xlsx"))


####### correct Congo Country as this is needed for the world map plot #############

# NOTE: The row numbers below correspond to specific records identified
# manually in this dataset's search (Web of Science search: 25 February 2025;
# Scopus search: 25 February 2025). If you rerun the searches, the results may
# return a different number of records, or in a different order, so these row
# numbers will no longer point to the same entries. Check the affected records
# again after rerunning the search, rather than assuming these lines still
# apply correctly.

## load full dataset
load(file = here("Searches", "FullDataset.rda"))

# Replicate the original manual `gsub` calls precisely.
# The original used individual `gsub` calls for each row.
# While this is inefficient, it's the only way to guarantee the same output.
# We'll re-create this logic using `case_when` for clarity and `row_number()`.
FullDataset <- FullDataset %>%
  mutate(
    AU_CO = case_when(
      row_number() == 4016 ~ str_replace(AU_CO, fixed("CONGO"), "DEMOCRATIC REPUBLIC OF THE CONGO"),
      row_number() == 4166 ~ str_replace(AU_CO, fixed("CONGO"), "REPUBLIC OF CONGO"),
      row_number() == 5488 ~ str_replace(AU_CO, fixed("CONGO"), "REPUBLIC OF CONGO"),
      row_number() == 7460 ~ str_replace(AU_CO, fixed("CONGO"), "DEMOCRATIC REPUBLIC OF THE CONGO"),
      row_number() == 10751 ~ str_replace(AU_CO, fixed("CONGO"), "DEMOCRATIC REPUBLIC OF THE CONGO"),
      row_number() == 10905 ~ str_replace(AU_CO, fixed("CONGO"), "DEMOCRATIC REPUBLIC OF THE CONGO"),
      row_number() == 10925 ~ str_replace(AU_CO, fixed("CONGO"), "REPUBLIC OF CONGO"),
      row_number() == 11212 ~ str_replace(AU_CO, fixed("CONGO"), "DEMOCRATIC REPUBLIC OF THE CONGO"),
      row_number() == 11256 ~ str_replace(AU_CO, fixed("CONGO"), "DEMOCRATIC REPUBLIC OF THE CONGO"),
      row_number() == 12889 ~ str_replace(AU_CO, fixed("CONGO"), "DEMOCRATIC REPUBLIC OF THE CONGO"),
      row_number() == 14061 ~ str_replace(AU_CO, fixed("CONGO"), "DEMOCRATIC REPUBLIC OF THE CONGO"),
      TRUE ~ AU_CO
    ),
    AU1_CO = case_when(
      row_number() == 4166 ~ str_replace(AU1_CO, fixed("CONGO"), "REPUBLIC OF CONGO"),
      row_number() == 11212 ~ str_replace(AU1_CO, fixed("CONGO"), "DEMOCRATIC REPUBLIC OF THE CONGO"),
      TRUE ~ AU1_CO
    )
  )

save(FullDataset, file = here("Searches", "FullDataset.rda"))
write.xlsx(FullDataset, file = here("Searches", "FullDataset.xlsx"))


############ Web of Science abbreviates the Affiliations, Scopus doesn't ##################
#### correct the Scopus Institution and Center names #####

# Load full dataset
load(file = here("Searches", "FullDataset.rda"))

# Load Web of Science address abbreviation file
WoSAbbrData <- read_csv(here("WebOfScienceAffiliationAbbreviations.csv"), show_col_types = FALSE) %>%
  mutate(
    Abbreviation = toupper(Abbreviation),
    FullName = toupper(FullName)
  )

# First, apply all abbreviations
for (j in 1:nrow(WoSAbbrData)) {
  FullDataset$AU_UN <- gsub(WoSAbbrData$FullName[j], WoSAbbrData$Abbreviation[j], FullDataset$AU_UN, fixed = TRUE)
  # FullDataset$AU1_UN <- gsub(WoSAbbrData$FullName[j], WoSAbbrData$Abbreviation[j], FullDataset$AU1_UN, fixed = TRUE)
}

# Second, apply the other cleaning steps
FullDataset <- FullDataset %>%
  mutate(
    AU_UN = str_replace_all(AU_UN, '\\bTHE\\b', ''),
    AU_UN = str_replace_all(AU_UN, '\\bOF\\b', ''),
    AU_UN = str_replace_all(AU_UN, '\\bAT\\b', ''),
    AU_UN = str_replace_all(AU_UN, '\\bSOUTH\\b', 'S'),
    AU_UN = str_replace_all(AU_UN, '\\bNORTH\\b', 'N'),
    AU_UN = str_replace_all(AU_UN, '\\bWEST\\b', 'W'),
    AU_UN = str_replace_all(AU_UN, '\\bEAST\\b', 'E'),
    AU_UN = str_replace_all(AU_UN, '\\s+', ' '),
    AU_UN = str_trim(AU_UN),
    
  )

save(FullDataset, file = here("Searches", "FullDataset.rda"))
write.xlsx(FullDataset, file = here("Searches", "FullDataset.xlsx"))


############ Two Records have "unspecified" languages  ##################
#### correct them after finding out they should be Romanian #####


FullDataset <- FullDataset %>%
  dplyr::mutate(LA = dplyr::if_else(LA == "UNSPECIFIED", "ROMANIAN", LA))


save(FullDataset, file = here("Searches", "FullDataset.rda"))
write.xlsx(FullDataset, file = here("Searches", "FullDataset.xlsx"))