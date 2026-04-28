# =========================================================================
# Opcja 3: Imputacja stałą wartością numeryczną lub tekstową - Wszystkie kolumny
# =========================================================================
suppressPackageStartupMessages(library(dplyr))

# Definicja funkcji
opcja_3_imputuj_stala_wszystkie <- function(df, stala_numeryczna = 0, stala_tekstowa = "Brak Informacji") {
  # Automatyczne załatanie całego dataframe'u używając dwóch rodzajów zastępstw 
  # dla zachowania bezkolizyjności (0 dla liczb, Tekst dla stringow).
  df %>%
    mutate(
      across(where(is.numeric), ~ coalesce(.x, as.numeric(stala_numeryczna))),
      across(where(is.character), ~ coalesce(.x, as.character(stala_tekstowa)))
    )
}

# Blok wykonawczy
input_file <- "market_data_clean.csv"
output_file <- "market_data_3_wartosc_stala.csv"

if (file.exists(input_file)) {
  cat("Wczytywanie danych...\n")
  dane <- read.csv(input_file, stringsAsFactors = FALSE)
  
  cat("Trwa automatyczne łatanie całego zbioru dla wszystkich kolumn. Liczby braki=0, Tekst braki='Brak Informacji'...\n")
  dane_wynik <- opcja_3_imputuj_stala_wszystkie(dane, stala_numeryczna = 0, stala_tekstowa = "Brak Informacji")
  
  write.csv(dane_wynik, output_file, row.names = FALSE)
  cat("Sukces! Zapisano wynik do pliku:", output_file, "\n")
} else {
  cat("Błąd: Nie znaleziono pliku", input_file, "\n")
}
