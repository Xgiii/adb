# =========================================================================
# Opcja 5: Algorytmiczna imputacja uczeniem maszynowym MICE
# =========================================================================
suppressPackageStartupMessages(library(dplyr))

# Definicja funkcji
opcja_5_zastosuj_mice <- function(df) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Ta funkcja wymaga pakietu MICE: install.packages('mice')")
  }
  
  # Przygotowanie do regresji - bierzemy tylko numery i kategorie do zgadywania
  dane_do_modelu <- df %>% 
    select(genre, price_usd, rating, ratings_count, year) %>% 
    mutate(genre = as.factor(genre))
  
  # Jedna iteracja symulacyjna Predictive Mean Matching
  model_mice <- mice::mice(dane_do_modelu, m = 1, method = "pmm", maxit = 3, printFlag = FALSE)
  kompletne_dane <- mice::complete(model_mice)
  
  # Przepisanie załatanych kolumn bazując na prawdopodobieństwie z innych cech
  df_wynik <- df
  df_wynik$price_usd <- kompletne_dane$price_usd
  df_wynik$rating <- kompletne_dane$rating
  df_wynik$ratings_count <- kompletne_dane$ratings_count
  df_wynik$year <- kompletne_dane$year
  
  return(df_wynik)
}

# Blok wykonawczy
input_file <- "market_data_clean.csv"
output_file <- "market_data_5_imputacja_mice.csv"

if (file.exists(input_file)) {
  cat("Wczytywanie danych...\n")
  dane <- read.csv(input_file, stringsAsFactors = FALSE)
  
  if (requireNamespace("mice", quietly = TRUE)) {
    cat("Trwa wyliczanie symulacji regresyjnej MICE (Ostrzeżenie: to zajmie dłużej)...\n")
    dane_wynik <- opcja_5_zastosuj_mice(dane)
    
    write.csv(dane_wynik, output_file, row.names = FALSE)
    cat("Sukces! Zapisano maszynowo odgadnięte dane do:", output_file, "\n")
  } else {
    cat("Pakiet 'mice' nie jest zainstalowany. Zainstaluj przy użyciu opcji: install.packages('mice') aby uruchomić ten blok. Brak możliwości wygenerowania pliku CSV.\n")
  }
} else {
  cat("Błąd: Nie znaleziono pliku", input_file, "\n")
}
