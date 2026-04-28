# =========================================================================
# Skrypt 6: Symulator zbierania danych w przedziale czasowym
# =========================================================================
# Symuluje wielokrotne uruchamianie scrapera poprzez podział istniejących
# danych na N snapshotów z różnymi znacznikami czasu.
# Wynikiem jest plik market_data_timeseries.csv gotowy do analizy.
# =========================================================================

cat("\n=== Symulator szeregu czasowego ===\n")
cat("Źródło danych: market_data_raw.csv\n\n")

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

# ---- Parametry symulacji ------------------------------------------------
N_SNAPSHOTS      <- 6        # Liczba symulowanych uruchomień scrapera
DAYS_SPAN        <- 30       # Zakres dni między pierwszym a ostatnim snapshotem
START_DATE       <- as.POSIXct("2025-11-01 09:00:00")  # Data pierwszego uruchomienia
INPUT_FILE       <- "market_data_raw.csv"
OUTPUT_FILE      <- "market_data_timeseries.csv"
SEED             <- 42       # Powtarzalność wyników

set.seed(SEED)

# ---- Wczytanie danych ---------------------------------------------------
if (!file.exists(INPUT_FILE)) {
  stop("Brak pliku: ", INPUT_FILE,
       "\nUruchom najpierw scraper_multi_source.R aby wygenerować dane.")
}

cat("Wczytywanie danych z:", INPUT_FILE, "\n")
df_all <- read.csv(INPUT_FILE, stringsAsFactors = FALSE, encoding = "UTF-8")
cat(paste0("Załadowano ", nrow(df_all), " rekordów z ", ncol(df_all), " kolumnami.\n\n"))

# ---- Generowanie timestampów snapshotów ---------------------------------
# Snapshoty rozłożone nierównomiernie (realistyczne — scraper nie chodzi co stały interwał)
set.seed(SEED)
interval_days <- sort(c(0, cumsum(runif(N_SNAPSHOTS - 1, 1, DAYS_SPAN / (N_SNAPSHOTS - 1) * 1.8))))
interval_days <- interval_days / max(interval_days) * DAYS_SPAN  # normalizacja do DAYS_SPAN

snapshot_times <- START_DATE + as.difftime(interval_days, units = "days")

cat("=== Plan symulowanych uruchomień scrapera ===\n")
for (i in seq_along(snapshot_times)) {
  cat(sprintf("  Snapshot %d/%d: %s\n", i, N_SNAPSHOTS,
              format(snapshot_times[i], "%Y-%m-%d %H:%M")))
}
cat("\n")

# ---- Podział danych na snapshoty ----------------------------------------
# Każdy snapshot = losowy podzbiór danych (z nakładaniem się — realistyczne)
# Dodatkowo: każdy snapshot może mieć trochę innych danych (nowe/znikające tytuły)

n_total <- nrow(df_all)

timeseries_parts <- list()

for (i in seq_len(N_SNAPSHOTS)) {
  cat(sprintf("Generowanie snapshotu %d/%d (%s)... ",
              i, N_SNAPSHOTS, format(snapshot_times[i], "%Y-%m-%d")))
  
  # Bazowy zbiór: ~75-90% wszystkich rekordów (losowo)
  base_frac  <- runif(1, 0.75, 0.90)
  base_idx   <- sample(seq_len(n_total), size = floor(n_total * base_frac))
  df_snap    <- df_all[base_idx, ]
  
  # Symulacja dryfu danych: lekka modyfikacja ocen i cen (±5%) — 
  # odzwierciedla zmiany ocen na platformach między sesjami
  noise_rating <- runif(nrow(df_snap), 0.97, 1.03)
  noise_price  <- runif(nrow(df_snap), 0.95, 1.05)
  
  df_snap$rating    <- suppressWarnings(
    round(df_snap$rating * noise_rating, 2))
  df_snap$rating    <- ifelse(df_snap$rating > 5, 5, df_snap$rating)
  df_snap$price_usd <- suppressWarnings(
    round(df_snap$price_usd * noise_price, 2))
  
  # Dodanie znacznika czasowego
  df_snap$scraped_at <- format(snapshot_times[i], "%Y-%m-%d %H:%M:%S")
  df_snap$snapshot_id <- i
  
  timeseries_parts[[i]] <- df_snap
  cat(paste0(nrow(df_snap), " rekordów\n"))
}

# ---- Scalenie i zapis ---------------------------------------------------
df_timeseries <- bind_rows(timeseries_parts)
df_timeseries$scraped_at <- as.POSIXct(df_timeseries$scraped_at,
                                        format = "%Y-%m-%d %H:%M:%S")

cat(paste0("\nŁącznie w szeregu czasowym: ", nrow(df_timeseries), " rekordów\n"))
cat(paste0("(", N_SNAPSHOTS, " snapshotów x śr. ",
           round(nrow(df_timeseries) / N_SNAPSHOTS), " rekordów)\n\n"))

write.csv(df_timeseries, OUTPUT_FILE, row.names = FALSE, fileEncoding = "UTF-8")

cat("=== Podsumowanie snapshotów ===\n")
summary_tbl <- df_timeseries %>%
  group_by(snapshot_id, scraped_at) %>%
  summarise(
    rekordy       = n(),
    sr_ocena      = round(mean(rating, na.rm = TRUE), 3),
    sr_cena       = round(mean(price_usd, na.rm = TRUE), 2),
    braki_ocena   = sum(is.na(rating)),
    .groups       = "drop"
  )
print(as.data.frame(summary_tbl))

cat(paste0("\n>>> Plik szeregu czasowego zapisany do: ", OUTPUT_FILE, "\n"))
cat(">>> Gotowy do: 7_wizualizacja_czas.R\n")
