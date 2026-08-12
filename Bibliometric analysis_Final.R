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
  "bibliometrix", "shiny", "openxlsx", "tidyverse", "ggplot2",
  "data.table", "reshape2", "RColorBrewer", "plyr", "tidyr",
  "igraph", "ggpubr", "magick", "scales", "ggpattern", "maps",
  "ggrepel", "rnaturalearth", "rnaturalearthdata", "here"
)

# Identify and install missing packages
new_packages <- packages_required[!(packages_required %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  message("Installing missing packages: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages, dependencies = TRUE)
}

# Load all required packages silently
invisible(lapply(packages_required, library, character.only = TRUE))

# Clear all objects from the current workspace to ensure a clean environment
rm(list = ls())


# --- Data Loading and Initial Inspection ---

# Load full dataset using here::here() for robust path management.
message("Attempting to load FullDataset.rda...")
tryCatch({
  load(here::here("Searches", "FullDataset.rda"))
  message("FullDataset.rda loaded successfully.")
}, error = function(e) {
  stop("Error loading FullDataset.rda. Please ensure the file is in 'GlobalDataAnalysis/Searches/' relative to your project root. Details: ", e$message)
})

# Display a quick overview of the loaded data
message("\n--- Initial Data Inspection ---")
print(head(FullDataset))
message("\nDimensions of FullDataset:")
print(dim(FullDataset))


# --- Data Completeness Assessment ---

message("\n--- Data Completeness ---")
data_completeness <- bibliometrix::missingData(FullDataset)
print(data_completeness)


# --- Descriptive Analysis: Main Bibliometric Measures ---

message("\n--- Descriptive Analysis: Main Bibliometric Measures ---")
# Perform bibliometric analysis of the dataset
results_global <- bibliometrix::biblioAnalysis(FullDataset, sep = ";")

# Obtain summary of the results
summary_global <- summary(object = results_global, k = 20, pause = FALSE)


# Get frequency and percentage for languages
message("\n--- Language Summary ---")
language_summary <- FullDataset %>%
  dplyr::group_by(LA) %>% # LA is the Language field
  dplyr::summarise(Frequency = n(), .groups = 'drop') %>% # '.groups = 'drop'' prevents "ungrouping" messages
  dplyr::mutate(Percentage = (Frequency / sum(Frequency)) * 100) %>%
  dplyr::arrange(desc(Frequency))
print(language_summary)


# --- Document Age Statistics ---

message("\n--- Document Age Statistics ---")
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Calculate the age of each document, converting publication year robustly
document_ages <- current_year - as.numeric(FullDataset$PY)
document_ages <- na.omit(document_ages) # Remove NAs if publication year is missing

# Calculate and print document age statistics
if (length(document_ages) > 0) {
  age_summary_stats <- quantile(document_ages, probs = c(0.25, 0.5, 0.75))
  
  cat("Q1:", age_summary_stats["25%"], "\n")
  cat("Median:", age_summary_stats["50%"], "\n")
  cat("Q3:", age_summary_stats["75%"], "\n")
  cat("IQR (Q3-Q1):", IQR(document_ages), "\n") # Using direct IQR function
  cat("Mean:", mean(document_ages), "\n")
  cat("Standard Deviation:", sd(document_ages), "\n\n")
} else {
  warning("No valid document ages found to calculate statistics.")
}


# --- Citations per Document Statistics ---

message("\n--- Citations per Document Statistics ---")
# Extract citations and handle NAs, converting robustly
citations_per_document <- as.numeric(FullDataset$TC) # TC is Times Cited
citations_per_document <- na.omit(citations_per_document)

# Calculate and print citation statistics
if (length(citations_per_document) > 0) {
  citations_summary_stats <- quantile(citations_per_document, probs = c(0.25, 0.5, 0.75))
  
  cat("Q1:", citations_summary_stats["25%"], "\n")
  cat("Median:", citations_summary_stats["50%"], "\n")
  cat("Q3:", citations_summary_stats["75%"], "\n")
  cat("IQR (Q3-Q1):", IQR(citations_per_document), "\n")
  cat("Mean:", mean(citations_per_document), "\n")
  cat("Standard Deviation:", sd(citations_per_document), "\n\n")
} else {
  warning("No valid citation data found to calculate statistics.")
}


# --- Citations per Year per Document ---

message("\n--- Citations per Year per Document ---")
# Select relevant columns and convert to numeric, then calculate citations per year
document_data_citations <- FullDataset %>%
  dplyr::select(PY, TC) %>%
  dplyr::mutate(
    PY = as.numeric(PY),
    TC = as.numeric(TC)
  ) %>%
  na.omit() %>% # Remove rows with NA in PY or TC
  dplyr::mutate(
    # Add 1 to age to ensure a minimum age of 1, preventing division by zero
    # and properly accounting for the publication year itself.
    Age = current_year - PY + 1,
    Age = pmax(Age, 1), # Ensure age is at least 1
    CitationsPerYear = TC / Age
  )

if (nrow(document_data_citations) > 0) {
  mean_citations_per_year <- mean(document_data_citations$CitationsPerYear)
  sd_citations_per_year <- sd(document_data_citations$CitationsPerYear)
  
  cat("Mean Citations per Year per Document:", round(mean_citations_per_year, 2), "\n")
  cat("SD Citations per Year per Document:", round(sd_citations_per_year, 2), "\n")
} else {
  warning("Not enough data to calculate Citations per Year per Document.")
}


# --- Additional Analysis: Growth Rate of Papers ---

message("\n--- Annual Publication Growth Rate ---")

# Access the AnnualProduction table from bibliometrix summary
annual_production_data <- summary_global$AnnualProduction

# Ensure the 'Year' column name is cleaned immediately after assignment
names(annual_production_data)[names(annual_production_data) == "Year   "] <- "Year" # Remove extra spaces in column name

# Prepare data for growth rate calculation: ensure Year is numeric and filter out 0 articles
growth_data_filtered <- annual_production_data %>%
  dplyr::mutate(Year = as.numeric(as.character(Year))) %>%
  dplyr::filter(Articles > 0)

if (nrow(growth_data_filtered) >= 2) { # Need at least two data points for growth calculation
  # Linear regression for exponential growth (log-linear model)
  growth_data_filtered <- growth_data_filtered %>%
    dplyr::mutate(
      Time = Year - min(Year), # Time variable starting from 0 for regression
      Log_N_Publications = log(Articles)
    )
  
  model_growth <- lm(Log_N_Publications ~ Time, data = growth_data_filtered)
  
  message("\nRegression Model Summary (Log-linear growth):")
  print(summary(model_growth))
  
  # Manual Compound Annual Growth Rate (CAGR) Calculation
  first_year_data <- min(growth_data_filtered$Year)
  last_year_data <- max(growth_data_filtered$Year)
  
  beginning_value <- growth_data_filtered$Articles[growth_data_filtered$Year == first_year_data]
  ending_value <- growth_data_filtered$Articles[growth_data_filtered$Year == last_year_data]
  
  number_of_periods <- last_year_data - first_year_data
  
  # Handle edge cases for CAGR calculation (e.g., beginning_value being zero)
  if (beginning_value == 0) {
    cagr_manual <- NA
    warning("Beginning value is 0. CAGR cannot be calculated.")
  } else if (number_of_periods <= 0) {
    cagr_manual <- NA
    warning("Not enough periods to calculate CAGR (need at least two years).")
  } else {
    cagr_manual <- (ending_value / beginning_value)^(1 / number_of_periods) - 1
  }
  
  cagr_percentage <- cagr_manual * 100
  
  cat("\n--- Manual Compound Annual Growth Rate (CAGR) ---\n")
  cat("Period:", first_year_data, "to", last_year_data, "\n")
  cat("Publications in First Year:", beginning_value, "\n")
  cat("Publications in Last Year:", ending_value, "\n")
  cat("Number of Periods (Years):", number_of_periods, "\n")
  cat("Calculated CAGR:", round(cagr_percentage, 2), "%\n")
  
} else {
  warning("Not enough data (less than 2 years with publications) to calculate meaningful growth rates.")
}





# ##############################################################################
# #               Publication Trends
# ##############################################################################

# --- Annual Scientific Production ---

message("\n--- Annual Scientific Production ---")

# Load global cancer research publications from Scopus
# Use here::here() for robust path management
message("Attempting to load ScopusCancerArticles.csv...")
tryCatch({
  CancerArticles <- readr::read_csv(here::here("ScopusCancerArticles.csv"), show_col_types = FALSE)
  message("ScopusCancerArticles.csv loaded successfully.")
}, error = function(e) {
  stop("Error loading ScopusCancerArticles.csv. Please ensure the file is in 'GlobalDataAnalysis/' relative to your project root. Details: ", e$message)
})
print(head(CancerArticles))


# Extract annual production data from summary and clean column name
AnnualProduction <- summary_global$AnnualProduction
names(AnnualProduction)[names(AnnualProduction) == "Year   "] <- "Year" # Remove extra spaces in column name
print(head(AnnualProduction))

# Merge with all cancer data, filling NA values with 0 where no publications exist
AnnualProduction <- merge(CancerArticles, AnnualProduction, by = "Year", all = TRUE)
AnnualProduction[is.na(AnnualProduction)] <- 0
print(head(AnnualProduction))

# Convert year to date format for plotting
AnnualProduction$YearAsDate <- as.Date(ISOdate(AnnualProduction$Year, 1, 1))
print(head(AnnualProduction))

# Define plot variables for x-axis limits and breaks
min_date <- as.Date("1904-01-01")
max_date <- as.Date("2025-01-01")
date_breaks <- as.Date(c("1904-01-01", "1936-01-01", "1946-01-01", "1956-01-01", "1966-01-01", "1976-01-01",
                         "1986-01-01", "1996-01-01", "2006-01-01", "2016-01-01", "2025-01-01"))

# Plot combined production trends (rural cancer vs. total cancer)
ProdAndCancerPlot <- ggplot(data = AnnualProduction) +
  geom_line(aes(x = YearAsDate, y = CancerArticles * max(Articles) / max(CancerArticles), group = 1),
            color = "snow3", linewidth = 0.6) +
  geom_point(aes(x = YearAsDate, y = Articles), size = 1.0, color = "black",
             alpha = ifelse(AnnualProduction$Articles == 0, 0, 1)) +
  geom_line(aes(x = YearAsDate, y = Articles, group = 1), color = "black", linewidth = 0.6) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 12) +
  labs(x = "Year") +
  scale_x_date(limits = c(min_date, max_date), guide = guide_axis(angle = 45),
               breaks = date_breaks, date_labels = "%Y") +
  scale_y_continuous(name = "Total Articles",
                     limits = c(0, 1400), breaks = seq(0, 1400, by = 200),
                     sec.axis = sec_axis(~ . * max(AnnualProduction$CancerArticles) / max(AnnualProduction$Articles),
                                         name = "Total Cancer Articles")) +
  # annotate("text", x = -170, y = 80, label = "(a)", size = 6, hjust = 0, vjust = 1) + # Adjust x, y for position
  theme(axis.text.y  = element_text(color = 'black'),
        axis.title.y = element_text(color = 'black'),
        axis.text.y.right =  element_text(color = 'snow3'),
        axis.title.y.right = element_text(color = 'snow3'),
        axis.line.y.right = element_line(color = 'snow3'),
        axis.ticks.y.right = element_line(color = 'snow3'))


# # save the plot
# ggsave(here::here("PlotsGlobal", "AnnualProductionCancerPlot.png"),
#        plot = AnnualProductionPlot_Cancer,
#        width = 7.5,
#        height = 4,
#        dpi = 300)


# --- Average citations per year ---

message("\n--- Average Citations per Year ---")

# Extract years and total citations per year data
PubYear <- FullDataset$PY
TCperYear <- results_global$TCperYear # Corrected variable name

# Calculate the mean of total citations per year for each year
MeanAnnualCitations <- aggregate(TCperYear, list(PubYear), FUN = mean)
names(MeanAnnualCitations)[names(MeanAnnualCitations) == "Group.1"] <- "Year"
names(MeanAnnualCitations)[names(MeanAnnualCitations) == "x"] <- "MeanCitations"
MeanAnnualCitations$YearAsDate <- as.Date(ISOdate(MeanAnnualCitations$Year, 1, 1))
print(head(MeanAnnualCitations))

# Plot average annual citations
AvAnnualCitationsPlot <- ggplot(data = MeanAnnualCitations, aes(x = YearAsDate, y = MeanCitations, group = 1)) +
  geom_line(color = "black", linewidth = 0.6) +
  geom_point(size = 1.0) +
  theme_classic(base_size = 12) +
  labs(x = "Year", y = "Average citations per year") +
  scale_x_date(limits = c(min_date, max_date), guide = guide_axis(angle = 45),
               breaks = date_breaks, date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1))

# Save the plot using here::here()
ggsave(here::here("PlotsGlobal", "AvAnnualCitationsPlot.png"),
       plot = ProdAndCancerPlot,
       width = 7.5,
       height = 4,
       dpi = 300)


# --- Combine graphs into a single publication trends plot ---

message("\n--- Combining Publication Trend Plots ---")

# Combine the two trend plots (Annual Scientific Production and Average Citations)
PublicationTrendsPlot <- ggpubr::ggarrange(
  ProdAndCancerPlot + ggpubr::rremove("x.text"), # Remove x-axis text from top plot
  AvAnnualCitationsPlot,
  nrow = 2,
  align = "v"
)

# Save the combined plot using here::here()
ggsave(here::here("PlotsGlobal", "PublicationTrendsPlot.eps"),
       plot = PublicationTrendsPlot,
       width = 7.5,
       height = 6,
       dpi = 300)




# ##############################################################################
# #               Who is publishing? (Country Analysis)
# ##############################################################################

# --- Most Productive Countries ---

message("\n--- Most Productive Countries ---")

# Extract most productive countries from Summary data
MostProdCountries <- summary_global$MostProdCountries
print(head(MostProdCountries))

# Ensure correct data types for plotting
MostProdCountries <- data.frame(
  Country = as.character(MostProdCountries$Country),
  Articles = as.numeric(MostProdCountries$Articles),
  SCP = as.numeric(MostProdCountries$SCP), # Corrected: SCP should be numeric
  MCP = as.numeric(MostProdCountries$MCP), # Multiple Country Publications
  MCP_Ratio = as.numeric(MostProdCountries$MCP_Ratio)
)

# Reshape data from wide to long format for ggplot
longMostProdCountries <- reshape2::melt(data.table::setDT(MostProdCountries),
                                        id.vars = c("Articles", "Country", "MCP_Ratio"),
                                        variable.name = "Type")
print(head(longMostProdCountries))

# Plot data as a bar chart (histogram)
MostProdCountriesPlot <- ggplot(longMostProdCountries, aes(x = value, y = reorder(Country, +value))) +
  geom_col(aes(fill = Type), width = 0.7) +
  scale_fill_manual(values = c("darkorange2", "darkcyan")) +
  theme(axis.text.y = element_text(hjust = 1.2)) +
  labs(x = "Total Articles", y = "Country") +
  scale_x_continuous(limits = c(0, 5100), breaks = seq(0, 5100, by = 500)) +
  theme_classic(base_size = 16)

# Save plot using here::here()
ggsave(here::here("PlotsGlobal", "MostProdCountriesPlot.eps"),
       plot = MostProdCountriesPlot,
       width = 10,
       height = 5,
       dpi = 300)




# --- Country Scientific Production for All Authors (Cumulative & World Map) ---

message("\n--- Country Scientific Production (All Authors) ---")

# Process FullDataset to extract countries and associate with years
CountriesOverTime_Initial <- FullDataset %>%
  mutate(
    # Ensure PY (Years) is numeric at the source
    Years = as.numeric(PY),
    # Split AU_CO (AllCountries string) into a list of country names for each row
    listCountries = str_split(AU_CO, ";")
  ) %>%
  # Add a unique row ID for later processing
  rowid_to_column(var = "record_identifier") %>%
  # Select only the columns needed for the next step: unique ID, year, and country list
  select(ID, Years, listCountries) %>%
  # Unnest the listCountries, creating a new row for each country in the list.
  # This efficiently transforms the data to a long format early.
  unnest(listCountries) %>%
  # Rename the unnested column to 'Countries'
  dplyr::rename(Countries = listCountries) %>%
  # Remove any rows where Countries is empty or NA (from empty strings after split)
  filter(!is.na(Countries) & Countries != "") %>%
  # Trim whitespace from country names if any
  mutate(Countries = str_trim(Countries))


# --- Aggregate and Calculate Cumulative Sum ---

# Group by Years & Countries and count occurrences
# Using `dplyr::summarise` explicitly to prevent masking issues
CountriesOverTime_Final <- CountriesOverTime_Initial %>%
  group_by(Years, Countries) %>%
  dplyr::summarise(total_count = n(), .groups = "drop")

# Ensure the full sequence of years for each country for consistent plotting
# Get min and max years from the aggregated data to define the sequence
min_year_data <- min(CountriesOverTime_Final$Years, na.rm = TRUE)
max_year_data <- max(CountriesOverTime_Final$Years, na.rm = TRUE)

# The original code specified 1904:2024 for `full_seq`.
# Using the specified range, or dynamically from data if preferred.
# For consistency with your original, I'll use 1904:2024.
CountriesOverTime_Final <- CountriesOverTime_Final %>%
  group_by(Countries) %>%
  # Use `complete` to fill in missing years for each country, setting total_count to 0
  complete(Years = full_seq(1904:2024, 1), fill = list(total_count = 0)) %>%
  ungroup() %>% # Ungroup after completion
  # Calculate cumulative count of articles for each country over the years
  group_by(Countries) %>%
  mutate(cumulative_sum = cumsum(total_count)) %>%
  ungroup() # Final ungroup

# --- Identify Top Countries ---

# Find the maximum cumulative articles for each country to identify top performers
CountriesArticles <- CountriesOverTime_Final %>%
  group_by(Countries) %>%
  dplyr::summarise(Articles = max(cumulative_sum, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Articles)) # Arrange in descending order of articles

# # Identify the top 6 Countries
# top6Countries <- head(CountriesArticles$Countries, 6)
# 
# # Create a tibble containing data for only the top 6 countries for plotting or further analysis
# Top6Countries <- CountriesOverTime_Final %>%
#   filter(Countries %in% top6Countries)
# 
# # --- Display Results ---
# 
# # You can now use 'Top6Countries' for plotting or further analysis
# print(head(Top6Countries))
# print(paste("Top 6 Countries:", paste(top6Countries, collapse = ", ")))






### Plot World Map with Total Country Production (All Authors) ###

message("\n--- World Map: Total Country Production (All Authors) ---")

# --- 1. Load Map Data and Initial Article Data Cleaning ---

# Load map data for ggplot
world <- map_data("world")

# Initial data preparation:
# 1. Start with CountriesArticles
# 2. Rename the 'Countries' column to 'Country' for consistency with downstream operations
# 3. Apply country name standardization and title casing
CountriesArticles <- CountriesArticles %>%
  dplyr::rename(Country = Countries) %>% # Renaming 'Countries' column to 'Country'
  # Change upper case letters to only be capitalised first letters for consistency
  mutate(Country = str_to_title(Country)) %>%
  # Standardize country names to match map data using case_when for cleaner conditional replacements
  mutate(
    Country = case_when(
      Country == "Usa" ~ "USA",
      Country == "United Kingdom" ~ "UK",
      Country == "Democratic Republic Of The Congo" ~ "Democratic Republic of the Congo",
      Country == "Republic Of Congo" ~ "Republic of Congo",
      Country == "U Arab Emirates" ~ "United Arab Emirates",
      Country == "Bosnia" ~ "Bosnia and Herzegovina",
      Country == "Trinidad" ~ "Trinidad And Tobago",
      Country == "Faroe Islands" ~ "Faroe Islands",
      Country == "Korea" ~ "South Korea",
      Country == "Cote D'ivoire" ~ "Ivory Coast",
      TRUE ~ Country # Keep all other country names as they are
    )
  )

# --- 2. Load and Merge Adjustment Data ---

# Load population size data using here()
TotalPopulation <- read.csv(here("TotalPopulation.csv"), header = TRUE)

# Load GDP per capita data using here()
GDPperCapita <- read.csv(here("GDP per Capita.csv"), header = TRUE)

# Load rural population data using here()
RuralPop <- read.csv(here("RuralPop.csv"), header = TRUE)

# Merge all three adjustment datasets using tidyverse's full_join
Df_adjustments <- TotalPopulation %>%
  full_join(GDPperCapita, by = "Country") %>%
  full_join(RuralPop, by = "Country")

# Clean country names in adjustment data to match map data
# Using case_when for cleaner conditional logic
Df_adjustments <- Df_adjustments %>%
  filter(!Country %in% c(
    "Channel Islands", "Hong Kong SAR, China", "Macao SAR, China",
    "St. Martin (French part)", "Sint Maarten (Dutch part)", "Tuvalu",
    "West Bank and Gaza", "Gibraltar", "Virgin Islands (U.S.)",
    "British Virgin Islands"
  )) %>%
  mutate(
    Country = case_when(
      Country == "Antigua and Barbuda" ~ "Antigua",
      Country == "Bahamas, The" ~ "Bahamas",
      Country == "Brunei Darussalam" ~ "Brunei",
      Country == "Cabo Verde" ~ "Cape Verde",
      Country == "Cote d'Ivoire" ~ "Ivory Coast",
      Country == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
      Country == "Congo, Rep." ~ "Republic of Congo",
      Country == "Czechia" ~ "Czech Republic",
      Country == "Egypt, Arab Rep." ~ "Egypt",
      Country == "Korea, Rep." ~ "South Korea",
      Country == "St. Lucia" ~ "Saint Lucia",
      Country == "Gambia, The" ~ "Gambia",
      Country == "Korea, Dem. People's Rep." ~ "North Korea",
      Country == "Lao PDR" ~ "Laos",
      Country == "Syrian Arab Republic" ~ "Syria",
      Country == "Venezuela, RB" ~ "Venezuela",
      Country == "Gibraltar" ~ "Saint Lucia",
      Country == "Iran, Islamic Rep." ~ "Iran",
      Country == "Russian Federation" ~ "Russia",
      Country == "St. Vincent and the Grenadines" ~ "Saint Vincent",
      Country == "Turkiye" ~ "Turkey",
      Country == "Viet Nam" ~ "Vietnam",
      Country == "Kyrgyz Republic" ~ "Kyrgyzstan",
      Country == "Eswatini" ~ "Swaziland",
      Country == "Micronesia, Fed. Sts." ~ "Micronesia",
      Country == "Trinidad and Tobago" ~ "Tobago",
      Country == "Yemen, Rep." ~ "Yemen",
      Country == "Slovak Republic" ~ "Slovakia",
      Country == "St. Kitts and Nevis" ~ "Saint Kitts",
      Country == "United Kingdom" ~ "UK",
      Country == "United States" ~ "USA",
      TRUE ~ Country # Keep all other country names as they are
    )
  )

# Merge article data with adjustment data, updating CountriesArticles directly
CountriesArticles <- CountriesArticles %>%
  full_join(Df_adjustments, by = "Country")

# Handle RuralPop conversion and NA for 0 values
CountriesArticles <- CountriesArticles %>%
  mutate(
    RuralPop = as.numeric(as.character(RuralPop)), # Ensure numeric
    RuralPopNAs = ifelse(RuralPop == 0, NA, RuralPop) # Set 0 to NA
  )

# --- 3. Calculate Adjusted and Log-Transformed Variables ---

# Total Articles Log-transformed
CountriesArticles <- CountriesArticles %>%
  mutate(ArticlesLog = log(Articles))

# Articles adjusted for Total Population (per million people)
CountriesArticles <- CountriesArticles %>%
  mutate(adjArticlesPop = (Articles / TotalPopulation) * 1000000) %>%
  mutate(ArticlesPopLog = log(adjArticlesPop))

# Articles adjusted for Rural Population (per 100,000 rural people)
CountriesArticles <- CountriesArticles %>%
  mutate(adjArticlesRuralPopNAs = (Articles / RuralPopNAs) * 100000) %>%
  mutate(ArticlesRuralLog = log(adjArticlesRuralPopNAs))

# Articles adjusted for GDP per Capita (per $100 of GDP per Capita)
CountriesArticles <- CountriesArticles %>%
  mutate(adjArticlesGDPcapita = (Articles / GDPcapita) * 100) %>%
  mutate(ArticlesGDPLog = log(adjArticlesGDPcapita))



# --- 4. Plotting Functions (Helper for consistent themes) ---

# Define a common plotting theme function to reduce repetition
plot_theme <- function() {
  theme(
    panel.background = element_blank(),
    legend.position = c(0.1, 0.25), # Set legend position to inside plot
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
  )
}

# Define the common color palette for log-transformed maps
log_palette_colors <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")


# --- 5. Generate Log-Transformed World Plots ---

## 5.1. Log-transformed Total Article Frequency (logmap1)
# Calculate breaks and labels for logmap1
min_log <- min(CountriesArticles$ArticlesLog, na.rm = TRUE)
max_log <- max(CountriesArticles$ArticlesLog, na.rm = TRUE)
num_breaks <- 5
log_breaks <- seq(min_log, max_log, length.out = num_breaks)
rounded_labels <- c(
  round(exp(log_breaks[1]), 0),
  round(exp(log_breaks[2]), 0),
  round(exp(log_breaks[3]), 0),
  round(exp(log_breaks[4]), 0),
  round(exp(max_log), 0)
)
original_labels <- c(
  paste0(rounded_labels[1]),
  paste0(rounded_labels[2]),
  paste0(rounded_labels[3]),
  paste0(rounded_labels[4]),
  paste0(rounded_labels[5])
)

logmap1 <- world %>%
  merge(CountriesArticles, by.x = "region", by.y = "Country", all.x = TRUE) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = ArticlesLog)) +
  geom_polygon() +
  annotate("text", x = -170, y = 80, label = "A", size = 6, hjust = 0, vjust = 1) +
  scale_fill_gradientn(
    colors = log_palette_colors,
    breaks = log_breaks,
    labels = original_labels,
    name = "Article Frequency",
    limits = c(min_log, max_log)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0.5)) +
  plot_theme()

ggsave(here("PlotsGlobal", "logmap1.png"),
       width = 11, height = 5, dpi = 300)


## 5.2. Log-transformed Articles per Million People (logmap2)
# Calculate breaks and labels for logmap2
min_log_pop <- min(CountriesArticles$ArticlesPopLog, na.rm = TRUE)
max_log_pop <- max(CountriesArticles$ArticlesPopLog, na.rm = TRUE)
num_breaks <- 5
log_breaks_pop <- seq(min_log_pop, max_log_pop, length.out = num_breaks)
rounded_labels_pop <- c(
  round(exp(log_breaks_pop[1]), 2),
  round(exp(log_breaks_pop[2]), 1),
  round(exp(log_breaks_pop[3]), 0),
  round(exp(log_breaks_pop[4]), 0),
  round(exp(max_log_pop), 0)
)
original_labels_pop <- c(
  paste0("~", rounded_labels_pop[1]),
  paste0("~", rounded_labels_pop[2]),
  paste0("~", rounded_labels_pop[3]),
  paste0("~", rounded_labels_pop[4]),
  paste0(">", rounded_labels_pop[5])
)

logmap2 <- world %>%
  merge(CountriesArticles, by.x = "region", by.y = "Country", all.x = TRUE) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = ArticlesPopLog)) +
  geom_polygon() +
  annotate("text", x = -170, y = 80, label = "B", size = 6, hjust = 0, vjust = 1) +
  scale_fill_gradientn(
    colors = log_palette_colors,
    breaks = log_breaks_pop,
    labels = original_labels_pop,
    name = "Articles per\nmillion people",
    limits = c(min_log_pop, max_log_pop)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0.5)) +
  plot_theme()

ggsave(here("PlotsGlobal", "logmap2.png"),
       width = 11, height = 5, dpi = 300)


## 5.3. Log-transformed Articles per 100,000 Rural People (logmap3)
# Calculate breaks and labels for logmap3
min_log_rural <- min(CountriesArticles$ArticlesRuralLog, na.rm = TRUE)
max_log_rural <- max(CountriesArticles$ArticlesRuralLog, na.rm = TRUE)
num_breaks <- 5
log_breaks_rural <- seq(min_log_rural, max_log_rural, length.out = num_breaks)
rounded_labels_rural <- c(
  round(exp(log_breaks_rural[1]), 2),
  round(exp(log_breaks_rural[2]), 2),
  round(exp(log_breaks_rural[3]), 1),
  round(exp(log_breaks_rural[4]), 0),
  round(exp(max_log_rural), 0)
)
original_labels_rural <- c(
  paste0("~", rounded_labels_rural[1]),
  paste0("~", rounded_labels_rural[2]),
  paste0("~", rounded_labels_rural[3]),
  paste0("~", rounded_labels_rural[4]),
  paste0(">", rounded_labels_rural[5])
)

logmap3 <- world %>%
  merge(CountriesArticles, by.x = "region", by.y = "Country", all.x = TRUE) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = ArticlesRuralLog)) +
  geom_polygon() +
  annotate("text", x = -170, y = 80, label = "C", size = 6, hjust = 0, vjust = 1) +
  scale_fill_gradientn(
    colors = log_palette_colors,
    breaks = log_breaks_rural,
    labels = original_labels_rural,
    name = "Articles per 100,000\nrural people",
    limits = c(min_log_rural, max_log_rural)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0.5)) +
  plot_theme()

ggsave(here("PlotsGlobal", "logmap3.png"),
       width = 11, height = 5, dpi = 300)


## 5.4. Log-transformed Articles per $100 of GDP per Capita (logmap4)
# Calculate breaks and labels for logmap4
min_log_gdp <- min(CountriesArticles$ArticlesGDPLog, na.rm = TRUE)
max_log_gdp <- max(CountriesArticles$ArticlesGDPLog, na.rm = TRUE)
num_breaks <- 5
log_breaks_gdp <- seq(min_log_gdp, max_log_gdp, length.out = num_breaks)
rounded_labels_gdp <- c(
  round(exp(log_breaks_gdp[1]), 3),
  round(exp(log_breaks_gdp[2]), 2),
  round(exp(log_breaks_gdp[3]), 1),
  round(exp(log_breaks_gdp[4]), 0),
  round(exp(max_log_gdp), 0)
)
original_labels_gdp <- c(
  paste0("~", rounded_labels_gdp[1]),
  paste0("~", rounded_labels_gdp[2]),
  paste0("~", rounded_labels_gdp[3]),
  paste0("~", rounded_labels_gdp[4]),
  paste0(">", rounded_labels_gdp[5])
)

logmap4 <- world %>%
  merge(CountriesArticles, by.x = "region", by.y = "Country", all.x = TRUE) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = ArticlesGDPLog)) +
  geom_polygon() +
  annotate("text", x = -170, y = 80, label = "D", size = 6, hjust = 0, vjust = 1) +
  scale_fill_gradientn(
    colors = log_palette_colors,
    breaks = log_breaks_gdp,
    labels = original_labels_gdp,
    name = "Articles per $100\nof GDP per Capita",
    limits = c(min_log_gdp, max_log_gdp)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0.5)) +
  plot_theme()

ggsave(here("PlotsGlobal", "logmap4.png"),
       width = 11, height = 5, dpi = 300)


# --- 6. Combine Log-Transformed Graphs ---

# Load the saved log-transformed images using here()
image1_log <- image_read(here("PlotsGlobal", "logmap1.png"))
image2_log <- image_read(here("PlotsGlobal", "logmap2.png"))
image3_log <- image_read(here("PlotsGlobal", "logmap3.png"))
image4_log <- image_read(here("PlotsGlobal", "logmap4.png"))

# Combine images as specified
combined_top_row_log <- image_append(c(image1_log, image2_log), stack = FALSE) # Horizontally
combined_bottom_row_log <- image_append(c(image3_log, image4_log), stack = FALSE) # Horizontally
final_combined_log_image <- image_append(c(combined_top_row_log, combined_bottom_row_log), stack = TRUE) # Vertically

# Save the final combined log-transformed image using here()
image_write(final_combined_log_image, here("PlotsGlobal", "combined_log_maps.png"))



# ##############################################################################
# #               Affiliations
# ##############################################################################

# Data preparation for Most Relevant Affiliations
MostRelevantAffiliations_cleaned <- results_global$Affiliations %>%
  as.data.frame() %>% # Convert to a data.frame if it's a table or similar frequency object
  dplyr::rename(Affiliations = AFF, Articles = Freq) %>% # Rename the frequency column to 'Articles'
  dplyr::filter(!is.na(Affiliations) & Affiliations != "NA") %>% # Remove both actual NAs and the string "NA"
  dplyr::mutate(
    Affiliations = as.character(Affiliations) %>% stringr::str_trim(), # Ensure 'Affiliations' is character type
    Articles = as.numeric(Articles) # Ensure 'Articles' (frequency) is numeric type
  ) %>%
  dplyr::arrange(desc(Articles)) %>% # Sort by 'Articles' in descending order (highest first)
  dplyr::mutate(Affiliations = stringr::str_to_title(Affiliations)) %>%
  dplyr::slice_head(n = 20) # Select the top 20 affiliations

# Plotting the Most Relevant Affiliations
MostRelevantAffiliationsPlot <- ggplot(MostRelevantAffiliations_cleaned, aes(x = Articles, y = reorder(Affiliations, Articles))) +
  geom_col(fill = "black") + # Create column chart with black bars
  geom_text(aes(label = Articles), color = "white", vjust = 0.5, hjust = 1.5) + # Add text labels inside/next to bars
  labs(x = "Articles", y = "Affiliations") + # Set axis labels
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 50)) + # Define x-axis limits and breaks
  theme_classic(base_size = 16) + # Use a classic theme with a base font size
  theme(axis.text.y = element_text(hjust = 1.2)) # Adjust y-axis text justification

# Save the plot
ggsave(
  here("PlotsGlobal", "MostRelevantAffiliationsPlot.eps"),
  plot = MostRelevantAffiliationsPlot, # Specify the plot object to save
  width = 10,
  height = 5,
  dpi = 300
)




# ##############################################################################
# #               Citations
# ##############################################################################

message("\n--- Citations ---")


# Define the current year and fraction of the year
current_year <- 2025
fraction_of_year <- 2/12 # Approximately end of February

# --- 1. Calculate years since publication for each publication ---
# This step also filters out publications with invalid years or zero/negative age.
FullDataset_with_age <- FullDataset %>%
  # Ensure PY is numeric for calculations
  mutate(PY = as.numeric(PY)) %>%
  mutate(
    years_since_publication = case_when(
      PY < current_year ~ current_year - PY,
      PY == current_year ~ fraction_of_year,
      TRUE ~ NA_real_ # Handle any future years or non-numeric PY if present
    )
  ) %>%
  # Filter out publications where years_since_publication is NA or non-positive
  filter(!is.na(years_since_publication) & years_since_publication > 0)

# --- 2. Calculate citations per year for each individual publication ---
Individual_Citations_Per_Year <- FullDataset_with_age %>%
  # Ensure TC is numeric for calculations
  mutate(TC = as.numeric(TC)) %>%
  mutate(citations_per_year = TC / years_since_publication)

# --- 3. Summarize total citations and SD per country (using FullDataset) ---
# This summary uses the total citations (TC) directly from the original dataset.
Average_Citations_SD_Per_Country <- FullDataset %>%
  # Ensure TC is numeric and AU1_CO is character for consistent grouping/filtering
  mutate(
    TC = as.numeric(TC),
    AU1_CO = as.character(AU1_CO)
  ) %>%
  group_by(AU1_CO) %>%
  # Filter out actual NA values and the string "NA" from AU1_CO
  filter(!is.na(AU1_CO) & AU1_CO != "NA") %>%
  dplyr::summarise( # Use dplyr::summarise to avoid masking issues
    total_citations = sum(TC, na.rm = TRUE), # Use na.rm=TRUE for sum
    average_total_citations = mean(TC, na.rm = TRUE), # Use na.rm=TRUE for mean
    sd_total_citations = sd(TC, na.rm = TRUE), # Use na.rm=TRUE for sd
    .groups = "drop" # Drop grouping after summarization
  ) %>%
  dplyr::arrange(desc(total_citations)) # Sort by total citations in descending order

# --- 4. Summarize citations per year by country (using Individual_Citations_Per_Year) ---
# This summary uses the calculated citations_per_year.
Average_Citations_Per_Year_By_Country <- Individual_Citations_Per_Year %>%
  # Ensure AU1_CO is character for consistent grouping/filtering
  mutate(AU1_CO = as.character(AU1_CO)) %>%
  group_by(AU1_CO) %>%
  # Filter out actual NA values and the string "NA" from AU1_CO
  filter(!is.na(AU1_CO) & AU1_CO != "NA") %>%
  dplyr::summarise( # Use dplyr::summarise to avoid masking issues
    number_of_articles = n(),
    total_citations_per_year_sum = sum(citations_per_year, na.rm = TRUE), # Sum of citations per year
    mean_citations_per_year = mean(citations_per_year, na.rm = TRUE),
    median_citations_per_year = median(citations_per_year, na.rm = TRUE),
    iqr_citations_per_year = IQR(citations_per_year, na.rm = TRUE),
    q1_citations_per_year = quantile(citations_per_year, 0.25, na.rm = TRUE),
    q3_citations_per_year = quantile(citations_per_year, 0.75, na.rm = TRUE),
    sd_citations_per_year = sd(citations_per_year, na.rm = TRUE),
    .groups = "drop" # Drop grouping after summarization
  ) %>%
  dplyr::arrange(desc(total_citations_per_year_sum)) %>% # Sort by sum of citations per year
  dplyr::slice_max(order_by = total_citations_per_year_sum, n = 20) # Select the top 20 countries based on sum of citations per year

# --- 5. Join the two summary tables ---
# Join by the country column (AU1_CO)
Final_Country_Summary <- Average_Citations_Per_Year_By_Country %>%
  dplyr::left_join(
    Average_Citations_SD_Per_Country %>%
      # Select only the columns needed from Average_Citations_SD_Per_Country to avoid redundancy
      dplyr::select(AU1_CO, total_citations, average_total_citations, sd_total_citations),
    by = "AU1_CO"
  ) %>%
  # Sort by total citations (from the first summary, which is total_citations_per_year_sum here)
  dplyr::arrange(desc(total_citations_per_year_sum))

# --- 6. Print and Save the resulting summary ---
print(Final_Country_Summary)




# ##############################################################################
# #               Country Collaboration 
# ##############################################################################

message("\n--- Country Collaboration ---")

# 1. Create the collaboration network matrix (top 50 countries)
# Removed default arguments for cleaner code
CountryNetwork <- biblioNetwork(
  FullDataset,
  analysis = "collaboration",
  network = "countries",
  n = 50, # Number of countries to include in the network
  sep = ";" # Separator for multiple country entries (if applicable)
)

# 2. Standardize country names for the network matrix
# This ensures consistency for labels within the network structure
country_labels_cleaned <- colnames(CountryNetwork) %>%
  str_to_title() %>% # Convert to Title Case (e.g., "united states" -> "United States")
  str_replace_all("Usa", "USA") # Correct specific instances like "Usa" to "USA"

colnames(CountryNetwork) <- country_labels_cleaned
rownames(CountryNetwork) <- country_labels_cleaned

# 3. Calculate comprehensive network statistics
network_stats <- networkStat(CountryNetwork, stat = "all")

# 4. Generate the network plot using bibliometrix's networkPlot
# Only essential and non-default parameters are kept for clarity.
CountryNetworkPlot <- networkPlot(
  CountryNetwork,
  normalize = "association", # Normalization method for edge weights
  n = 50,                    # Number of nodes (countries) to display in the plot
  label = TRUE,              # Display node labels
  labelsize = 1.2,           # Size of the node labels
  label.color = TRUE,        # Color labels by their community
  cluster = "walktrap",      # Community detection algorithm
  community.repulsion = 0.1, # Repulsion force between communities
  size = 8,                  # Base size of nodes
  size.cex = TRUE,           # Scale node size based on degree
  curved = FALSE,            # Edges are straight lines
  noloops = TRUE,            # Do not draw self-loops
  remove.multiple = TRUE,    # Remove multiple edges between the same two nodes
  weighted = TRUE,           # Use edge weights (collaboration strength)
  edgesize = 10,             # Base size of edges
  edges.min = 0,             # Minimum edge weight to display (0 shows all edges)
  alpha = 0.9,               # Transparency of network elements
  verbose = FALSE            # Suppress verbose output from networkPlot
)

# 5. Extract the igraph object for further custom visualization
networkGraph <- CountryNetworkPlot$graph

# 6. Apply cleaned labels directly to the igraph object's vertices
# This ensures the labels displayed on the plot are the cleaned names.
# V(networkGraph)$name should already contain the cleaned labels from step 2,
# but this line ensures V(networkGraph)$label is also set consistently.
V(networkGraph)$label <- V(networkGraph)$name

# 7. Customize node and edge colors based on community detection
# Get the communities detected by networkPlot
communities <- V(networkGraph)$community

# Define a color palette based on the number of communities.
# Max of "Paired" and "Set3" palettes is 12 colors. If more communities, consider other strategies.
num_communities <- max(communities)
v.colours <- brewer.pal(name = "Paired", n = max(3, min(12, num_communities))) # Ensure at least 3 colors, max 12

V(networkGraph)$color <- v.colours[communities] # Assign colors to nodes based on their community

# Color edges: within-community edges get community color, between-community edges get grey
E(networkGraph)$color <- apply(as.data.frame(as_edgelist(networkGraph, names = FALSE)), 1,
                               function(x) {
                                 node1_comm <- communities[x[1]]
                                 node2_comm <- communities[x[2]]
                                 if (node1_comm == node2_comm) {
                                   v.colours[node1_comm] # Same community color
                                 } else {
                                   'grey90' # Grey for between-community edges
                                 }
                               })

# Scale edge width based on collaboration strength (weight)
E(networkGraph)$width <- E(networkGraph)$weight / max(E(networkGraph)$weight) * 5 # Scale width (e.g., to a max of 5 units)

# 8. Plot the customized igraph network
# This uses the base R plot function for igraph objects.
plot(
  networkGraph,
  vertex.color = V(networkGraph)$color,
  edge.color = E(networkGraph)$color,
  vertex.label.color = "black", # Ensure labels are clearly visible
  vertex.label.cex = CountryNetworkPlot$labelsize # Use the label size set in networkPlot
)

# 9. Export network data for VOSviewer
# Use here() to construct the path to your VOSviewer.jar directory.
# This assumes VOSviewer_1.6.20_jar is a folder containing the VOSviewer.jar file.
net2VOSviewer(
  CountryNetworkPlot,
  vos.path = here("VOSviewer_1.6.20_jar")
)

# 10. Summary of network statistics (e.g., centrality measures)
# This provides insights into the network structure.
summary(network_stats, k = 50) # Summarize top 50 stats



# ##############################################################################
# #               Sources
# ##############################################################################

message("\n--- Sources ---")

# 1. Extract and clean "Most relevant sources" data
MostRelevantSources_cleaned <- summary_global$MostRelSources %>%
  as_tibble() %>% # Convert the data to a tibble
  # Rename the column with trailing spaces to a clean 'Sources'
  dplyr::rename(Sources = `Sources       `) %>% # Use backticks for names with spaces
  dplyr::mutate(
    # Trim leading/trailing spaces from the 'Sources' values and convert to character
    Sources = as.character(Sources) %>% stringr::str_trim(),
    Articles = as.numeric(Articles) # Ensure 'Articles' column is numeric
  ) %>%
  dplyr::mutate(Sources = stringr::str_to_title(Sources)) %>% # Apply title casing
  dplyr::arrange(desc(Articles)) # Arrange by 'Articles' in descending order


# 2. Plot the results using ggplot2
MostRelevantSourcesPlot <- ggplot(
  MostRelevantSources_cleaned,
  aes(x = Articles, y = reorder(Sources, Articles)) # Reorder sources by article count for the plot
) +
  geom_col(fill = "black") + # Create a column chart with black bars
  # Add text labels for 'Articles' count on the bars.
  # 'color="white"' makes them visible on black bars, 'hjust=1' right-aligns them inside the bar.
  geom_text(aes(label = Articles), color = "white", hjust = 1) +
  # Adjust Y-axis text justification for better alignment if needed.
  theme(axis.text.y = element_text(hjust = 1.2)) +
  labs(x = "Articles", y = "Sources") + # Set x and y axis labels
  # Define x-axis limits and breaks for consistent scaling
  scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, by = 50)) +
  theme_classic(base_size = 16) # Apply a classic theme with a base font size for overall readability

# 3. Save the plot
ggsave(
  here("PlotsGlobal", "MostRelevantSourcesPlot.eps"),
  plot = MostRelevantSourcesPlot, # Specify the ggplot object to save
  width = 13, # Set plot width
  height = 8, # Set plot height
  dpi = 300   # Set resolution for print quality
)





# ##############################################################################
# #               Conceptual Structure 
# ##############################################################################


# --- 1. Co-word Analysis (Network Plot) ---

message("\n--- Co-word Analysis ---")

KeywordNetwork <- biblioNetwork(FullDataset,
                                analysis = "co-occurrences",
                                network = "author_keywords",
                                n = 50,
                                sep = ";",
                                short = FALSE,
                                shortlabel = TRUE,
                                remove.terms = NULL,
                                synonyms = NULL)

networkPlot(KeywordNetwork,
            normalize = "association",
            n = 50,
            degree = NULL,
            Title = NULL,
            type = "auto",
            label = TRUE,
            labelsize = 1.2,
            label.cex = FALSE,
            label.color = TRUE,
            label.n = NULL,
            halo = FALSE,
            cluster = "walktrap",
            community.repulsion = 0.1,
            vos.path = NULL,
            size = 8,
            size.cex = TRUE,
            curved = FALSE,
            noloops = TRUE,
            remove.multiple = TRUE,
            remove.isolates = FALSE,
            weighted = TRUE,
            edgesize = 10,
            edges.min = 0,
            alpha = 0.5,
            verbose = FALSE)

# --- 2. Multiple Correspondence Analysis (MCA) ---

message("\n--- Multiple Correspondence Analysis (MCA) ---")

suppressWarnings(
  CS <- conceptualStructure(
    FullDataset,
    field = "DE",
    ngrams = 1,
    method = "MCA",
    quali.supp = NULL,
    quanti.supp = NULL,
    minDegree = 100,
    clust = 4,
    k.max = 5,
    stemming = FALSE,
    labelsize = 10,
    documents = 2,
    graph = TRUE,
    remove.terms = NULL,
    synonyms = NULL
  )
)

# Save and Load: Keep if CS object creation is very time-consuming.
# Using here::here() for the file path.
saveRDS(CS, file = here("Searches", "CS.RData"))
CS <- readRDS(file = here("Searches", "CS.RData"))


# --- Reusable Function for Plot Styling and Logo Removal ---
modify_bibliometrix_plot <- function(plot_obj, type = c("MCA", "ThematicMap")) {
  type <- match.arg(type)
  
  if (!inherits(plot_obj, "ggplot") || is.null(plot_obj$layers)) {
    warning(paste("Input is not a valid ggplot object or has no layers for type:", type))
    return(plot_obj)
  }
  
  modified_plot <- plot_obj +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 16, face = "plain"),
      axis.title.y = ggplot2::element_text(size = 16, face = "plain"),
      plot.margin = ggplot2::unit(c(0.5, 1, 0.5, 0.5), "cm")
    )
  
  if (type == "MCA") {
    if (length(modified_plot$layers) >= 6) {
      modified_plot$layers[[6]] <- NULL
    } else {
      message("Warning: Layer 6 (potential logo) not found or plot has fewer than 6 layers for MCA plot.")
    }
    
    if (length(modified_plot$layers) >= 3 && inherits(modified_plot$layers[[3]]$geom, "GeomTextRepel")) {
      modified_plot$layers[[3]]$aes_params$size <- 5.5
      message("MCA: Adjusted geom_text_repel size.")
    } else {
      message("MCA: GeomTextRepel not found at layer 3. Label size not modified.")
    }
    
  } else if (type == "ThematicMap") {
    modified_plot <- modified_plot +
      ggplot2::labs(
        x = "Relevance degree (Centrality)",
        y = "Development degree (Density)"
      )
    
    if (length(modified_plot$layers) >= 6) {
      modified_plot$layers[[6]] <- NULL
    } else {
      message("Warning: Layer 6 (potential logo) not found or plot has fewer than 6 layers for Thematic Map plot.")
    }
    
    if (length(modified_plot$layers) >= 2 && inherits(modified_plot$layers[[2]]$geom, "GeomLabelRepel")) {
      modified_plot$layers[[2]]$aes_params$fill <- NA
      modified_plot$layers[[2]]$aes_params$size <- 5.5
      modified_plot$layers[[2]]$aes_params$min.segment.length <- 0.3
      modified_plot$layers[[2]]$aes_params$force <- 1
      modified_plot$layers[[2]]$aes_params$box.padding <- 0.5
      modified_plot$layers[[2]]$aes_params$point.padding <- 0.5
      message("Thematic Map: Adjusted GeomLabelRepel parameters.")
    } else {
      message("Thematic Map: GeomLabelRepel not found at layer 2. Label parameters not modified.")
    }
    
    for (i in seq_along(modified_plot$layers)) {
      current_layer_geom <- class(modified_plot$layers[[i]]$geom)[1]
      if (current_layer_geom == "GeomVline" || current_layer_geom == "GeomHline") {
        modified_plot$layers[[i]]$aes_params$colour <- "grey80"
        message(paste("Thematic Map: Modified color of layer", i, "to a paler shade."))
      }
    }
  }
  return(modified_plot)
}


# Apply modifications to MCA plot
if ("graph_terms" %in% names(CS)) {
  MCAplot_final <- modify_bibliometrix_plot(CS$graph_terms, type = "MCA")
  print(MCAplot_final)
} else {
  warning("CS$graph_terms not found. MCA plot could not be generated or modified.")
  MCAplot_final <- NULL
}

# Save MCA plot
if (!is.null(MCAplot_final)) {
  ggsave(here("PlotsGlobal", "MCAplot_no_logo.png"),
         plot = MCAplot_final,
         width = 8.5,
         height = 8.5,
         dpi = 300)
}


# --- 3. Thematic Analysis ---

message("\n--- Thematic Analysis ---")

# change keyword as both ways are used in the dataset
FullDataset$DE <- gsub(pattern = "POLYCYCLIC AROMATIC HYDROCARBONS",
                       replacement = "PAHS",
                       x = FullDataset$DE)

Map <- thematicMap(
  FullDataset,
  field = "DE",
  n = 250,
  minfreq = 3,
  ngrams = 1,
  stemming = FALSE,
  size = 0.5,
  n.labels = 7,
  community.repulsion = 0.1,
  repel = TRUE,
  remove.terms = NULL,
  synonyms = NULL,
  cluster = "walktrap",
  subgraphs = TRUE
)

# Apply modifications to Thematic Map plot
if ("map" %in% names(Map)) {
  ThematicMapPlot_final <- modify_bibliometrix_plot(Map$map, type = "ThematicMap")
  print(ThematicMapPlot_final)
} else {
  warning("Map$map not found. Thematic Map plot could not be generated or modified.")
  ThematicMapPlot_final <- NULL
}

# Save Thematic Map plot (PNG and SVG for Illustrator)
if (!is.null(ThematicMapPlot_final)) {
  ggsave(here("PlotsGlobal", "ThematicMapPlot_no_logo.png"),,
         plot = ThematicMapPlot_final,
         width = 8.5,
         height = 8.5,
         dpi = 300)
  
  ggsave(here("PlotsGlobal", "ThematicMapPlot_no_logo.svg"),
         plot = ThematicMapPlot_final,
         width = 8.5,
         height = 8.5,
         units = "in",
         dpi = 300,
         device = "svg")
}

# --- 4. Combining Plots with patchwork ---

mca_plot_filename <- here("PlotsGlobal", "MCAplot_no_logo.png")
thematic_plot_png_filename <- here("PlotsGlobal", "ThematicMapPlot_no_logo.png")


# Only proceed if both individual PNG files were successfully created
if (file.exists(mca_plot_filename) && file.exists(thematic_plot_png_filename)) {
  message("Combining pre-saved images with magick...")
  
  image1 <- image_read(mca_plot_filename) # Use the variable holding the full path
  image2 <- image_read(thematic_plot_png_filename) # Use the variable holding the full path
  
  # Combine horizontally (stack = FALSE)
  combined_MCA_thematic <- image_append(c(image1, image2), stack = FALSE)
  
  # Save the combined image
  combined_image_filename <- here("PlotsGlobal", "combined_MCA_thematic.png")
  image_write(combined_MCA_thematic, combined_image_filename)
  message(paste("Combined plot saved to:", combined_image_filename))
  
} else {
  warning("Skipping plot combination with magick as one or both required PNG files were not found.")
}