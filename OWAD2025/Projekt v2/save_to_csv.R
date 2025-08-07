# -------------------------------------------------------------------------------------------------
# Skrypt: Przetwarzanie danych COVID-19 i eksport do CSV
# -------------------------------------------------------------------------------------------------

# 1. Ładowanie bibliotek
library(tidyverse)
library(openxlsx)

# 2. Wczytanie wszystkich CSV z folderu 'datasets/'
path <- "datasets/"
for (file in list.files(path)) {
  actual_dataset <- read.csv2(paste0(path, file), sep = ",", dec = ".", header = TRUE)
  assign(gsub(".csv", "", file), actual_dataset)
}

# 3. Tworzenie base_df
base_df <- full_grouped %>%
  mutate(Date = as.Date(Date)) %>%
  left_join(
    worldometer_data %>%
      select(Country.Region, Continent, Population) %>%
      mutate(Population = as.numeric(gsub(",", ".", Population))),
    by = "Country.Region"
  ) %>%
  pivot_longer(
    cols = c(Confirmed:Active),
    names_to = "Case",
    values_to = "Value"
  ) %>%
  select(
    Country = Country.Region,
    Continent,
    Population,
    Date,
    Case,
    Value
  ) %>%
  mutate(
    Continent = case_when(
      Country %in% c("Brunei", "Burma", "China", "South Korea", "Taiwan", "United Arab Emirates", "West Bank and Gaza", "Taiwan*") ~ "Asia",
      Country %in% c("Central African Republic", "Congo (Brazzaville)", "Congo (Kinshasa)", "Cote d'Ivoire") ~ "Africa",
      Country %in% c("Holy See", "Kosovo", "United Kingdom") ~ "Europe",
      Country %in% c("Saint Vincent and the Grenadines", "US") ~ "North America",
      .default = Continent
    ),
    Population = case_when(
      is.na(Population) ~ 0,
      .default = Population
    )
  )

# 4. Tworzenie folderu wyjściowego, jeśli nie istnieje
if (!dir.exists("output")) {
  dir.create("output")
}

# 5. Tworzenie nazwy pliku z datą
plik_wyjsciowy <- paste0("output/base_df_combined_", Sys.Date(), ".csv")

# 6. Zapis do pliku CSV
readr::write_csv(base_df, plik_wyjsciowy)

# 7. Informacja zwrotna
cat("✅ Plik zapisany pomyślnie jako:", plik_wyjsciowy, "\n")
