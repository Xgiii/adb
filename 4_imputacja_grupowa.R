# =========================================================================
# Opcja 4: Skomplikowana Imputacja po podgrupach dla WSZYSTKICH kolumn liczbowych
# =========================================================================
suppressPackageStartupMessages(library(dplyr))

# Definicja funkcji
opcja_4_imputuj_grupowo_wszystkie <- function(df, kolumna_grupujaca, metoda = "mediana") {
  df %>%
    group_by(across(all_of(kolumna_grupujaca))) %>%
    mutate(
      # across dynamicznie celuje w absolutnie wszystkie wektory z liczbami (prócz samego grupującego)
      across(
        where(is.numeric),
        ~ if (metoda == "mediana") {
             coalesce(.x, suppressWarnings(median(.x, na.rm = TRUE)))
          } else {
             coalesce(.x, suppressWarnings(mean(.x, na.rm = TRUE)))
          }
      )
    ) %>%
    ungroup() %>% 
    as.data.frame()
}

# Blok wykonawczy
input_file <- "market_data_clean.csv"
output_file <- "market_data_4_inteligentna_mediana_grupowa.csv"

if (file.exists(input_file)) {
  cat("Wczytywanie danych...\n")
  dane <- read.csv(input_file, stringsAsFactors = FALSE)
  
  cat("Trwa zaawansowane łatanie absolutnie wszystkich cech numerycznych (price_usd, rating, rok etc.) \nwedle zgrupowania po gatunku (genre)...\n")
  dane_wynik <- opcja_4_imputuj_grupowo_wszystkie(dane, kolumna_grupujaca = "genre", metoda = "mediana")
  
  write.csv(dane_wynik, output_file, row.names = FALSE)
  cat("Sukces! Zapisano wynik do pliku:", output_file, "\n")
} else {
  cat("Błąd: Nie znaleziono pliku", input_file, "\n")
}
