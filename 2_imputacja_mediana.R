# =========================================================================
# Opcja 2: Imputacja globalna - Mediana lub Średnia dla wszystkich kolumn numerycznych
# =========================================================================
suppressPackageStartupMessages(library(dplyr))

# Definicja funkcji
opcja_2_imputuj_numeryczne <- function(df, metoda = "mediana") {
  # Funkcja dynamicznie wyłapuje wszystkie kolumny stricte liczbowe 
  # i wstawia dla nich medianę lub średnią ze wszystkich wektorów.
  df %>%
    mutate(
      across(where(is.numeric), 
             ~ if (metoda == "mediana") {
                 coalesce(.x, suppressWarnings(median(.x, na.rm = TRUE)))
               } else {
                 coalesce(.x, suppressWarnings(mean(.x, na.rm = TRUE)))
               }
      )
    )
}

# Blok wykonawczy
input_file <- "market_data_clean.csv"
output_file <- "market_data_2_globalna_mediana.csv"

if (file.exists(input_file)) {
  cat("Wczytywanie danych...\n")
  dane <- read.csv(input_file, stringsAsFactors = FALSE)
  
  cat("Trwa zastępowanie pustych wpisów we wszystkich dostępnych kolumnach numerycznych (np. price_usd, rating, rok)...\n")
  dane_wynik <- opcja_2_imputuj_numeryczne(dane, metoda = "mediana")
  
  write.csv(dane_wynik, output_file, row.names = FALSE)
  cat("Sukces! Zapisano wynik do pliku:", output_file, "\n")
} else {
  cat("Błąd: Nie znaleziono pliku", input_file, "\n")
}
