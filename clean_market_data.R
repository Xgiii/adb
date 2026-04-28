cat("\n=== Uruchamianie systemu czyszczenia danych rynkowych ===\n")

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

input_file <- file.path(getwd(), "market_data_raw.csv")
output_file <- file.path(getwd(), "market_data_clean.csv")

if (!file.exists(input_file)) {
    stop("Brak pliku wejściowego: ", input_file)
}

cat("Wczytywanie danych...\n")
df <- read.csv(input_file, stringsAsFactors = FALSE, encoding = "UTF-8")

cat(paste0("Początkowa liczba wierszy: ", nrow(df), "\n\n"))

cat("Rozpoczynanie czyszczenia danych...\n")

df_clean <- df %>%
  # 1. Usunięcie wierszy bez tytułu lub z pustym tytułem
  filter(!is.na(title), str_trim(title) != "") %>%
  
  # 2. Standaryzacja ułożenia / wielkości liter dla gatunków i języków
  mutate(
    genre = str_to_lower(str_trim(genre)),
    language = str_to_lower(str_trim(language)),
    # upraszczanie kodów językowych by zachować jednolity format
    language = case_when(
      language %in% c("eng", "english") ~ "en",
      language %in% c("pol", "polish") ~ "pl",
      language %in% c("fre", "fra", "french") ~ "fr",
      language %in% c("ger", "deu", "german") ~ "de",
      language %in% c("spa", "spanish") ~ "es",
      TRUE ~ language
    )
  ) %>%
  
  # 3. Filtrowanie nielogicznych wartości liczbowych
  mutate(
    price_usd = if_else(!is.na(price_usd) & (price_usd < 0 | price_usd > 500), NA_real_, as.numeric(price_usd)),
    rating = if_else(!is.na(rating) & (rating < 0 | rating > 5), NA_real_, as.numeric(rating)),
    ratings_count = if_else(!is.na(ratings_count) & (ratings_count < 0), NA_integer_, as.integer(ratings_count)),
    year = if_else(!is.na(year) & (year < 1000 | year > as.numeric(format(Sys.Date(), "%Y")) + 1), NA_integer_, as.integer(year))
  ) %>%
  
  # 4. Zastąpienie tekstów 'NA' prawdziwymi wartościami NA
  mutate(
    author = if_else(str_trim(str_to_lower(author)) == "na", NA_character_, author),
    language = if_else(str_trim(str_to_lower(language)) == "na", NA_character_, language)
  ) %>%
  
  # 5. Identyfikacja i usuwanie duplikatów 
  mutate(
    z_title_lower = str_to_lower(str_replace_all(title, "[[:punct:]]", "")),
    z_author_lower = str_to_lower(str_replace_all(author, "[[:punct:]]", ""))
  ) %>%
  distinct(z_title_lower, z_author_lower, .keep_all = TRUE) %>%
  select(-z_title_lower, -z_author_lower)

cat(paste0("Liczba wierszy po czyszczeniu (usunięte duplikaty i zepsute wiersze): ", nrow(df_clean), "\n"))
cat(paste0("Odrzucono: ", nrow(df) - nrow(df_clean), " wierszy (", round(100*(nrow(df) - nrow(df_clean))/nrow(df), 2), "%).\n\n"))

cat("Zapisywanie czystych danych do pliku CSV...\n")
write.csv(df_clean, output_file, row.names = FALSE, fileEncoding = "UTF-8")

cat("===================================================\n")
cat("Czyszczenie danych zakończone sukcesem!\n")
cat("Zapisano do: ", output_file, "\n")
