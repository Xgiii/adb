# =========================================================================
# Opcja 1: CCA (Complete Case Analysis) - Usunięcie wierszy z NA dla wszystkich kolumn
# =========================================================================
suppressPackageStartupMessages(library(dplyr))

# Definicja funkcji
opcja_1_usun_braki_wszystkie <- function(df) {
  # na.omit domyślnie skanuje wszystkie kolumny i usuwa wiersz, jeżeli w którejkolwiek znajduje się punkt NA.
  return(na.omit(df))
}

# Blok wykonawczy
input_file <- "market_data_clean.csv"
output_file <- "market_data_1_cca.csv"

if (file.exists(input_file)) {
  cat("Wczytywanie danych...\n")
  dane <- read.csv(input_file, stringsAsFactors = FALSE)
  
  cat("Trwa usuwanie braków (badanie wszystkich kolumn na istnienie NA)...\n")
  dane_wynik <- opcja_1_usun_braki_wszystkie(dane)
  
  write.csv(dane_wynik, output_file, row.names = FALSE)
  cat("Sukces! Zapisano wynik do pliku:", output_file, "\n")
  cat("Liczba wierszy spadła z", nrow(dane), "do", nrow(dane_wynik), "\n")
} else {
  cat("Błąd: Nie znaleziono pliku", input_file, "\n")
}
