# =========================================================================
# Skrypt 8: Kompleksowa analiza eksploracyjna danych (EDA) z wizualizacjami
# =========================================================================
# Wczytuje market_data_5_imputacja_mice.csv i generuje pełen zestaw wykresów PNG
# analizujących rozkłady, korelacje, braki danych i porównania źródeł.
# =========================================================================

cat("\n=== Analiza eksploracyjna danych (EDA) ===\n\n")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
  library(stringr)
  library(forcats)
})

# ---- Parametry ----------------------------------------------------------
INPUT_FILE <- "market_data_5_imputacja_mice.csv"
OUTPUT_DIR <- "wykresy_eda"

if (!file.exists(INPUT_FILE)) {
  stop("Brak pliku: ", INPUT_FILE,
       "\nUruchom najpierw 5_imputacja_mice.R")
}
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

# ---- Wczytanie danych ---------------------------------------------------
cat("Wczytywanie danych...\n")
df <- read.csv(INPUT_FILE, stringsAsFactors = FALSE, encoding = "UTF-8")
cat(paste0("Załadowano ", nrow(df), " rekordów | ",
           ncol(df), " kolumn | ",
           length(unique(df$source)), " źródła\n\n"))

# ---- Motyw graficzny ----------------------------------------------------
theme_eda <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, hjust = 0.5,
                                    margin = margin(b = 8)),
    plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "grey40",
                                    margin = margin(b = 14)),
    plot.caption     = element_text(size = 9, color = "grey55", hjust = 1),
    axis.title       = element_text(size = 11, color = "grey25"),
    axis.text        = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 10, face = "bold"),
    strip.text       = element_text(face = "bold", size = 11),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

PALETTE_SRC <- c("Open Library" = "#2196F3",
                 "iTunes Store" = "#FF5722",
                 "Google Books" = "#4CAF50")

save_plot <- function(p, filename, w = 10, h = 6) {
  path <- file.path(OUTPUT_DIR, filename)
  ggsave(path, plot = p, width = w, height = h, dpi = 150, bg = "white")
  cat(paste0("  [OK] ", path, "\n"))
}

cat("Generowanie wykresów EDA...\n\n")

# ==========================================================================
# 1. Rozkład cen
# ==========================================================================
df_cena <- df %>% filter(!is.na(price_usd), price_usd > 0, price_usd <= 80)
med_cena <- median(df_cena$price_usd, na.rm = TRUE)

p1 <- ggplot(df_cena, aes(x = price_usd)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2,
                 fill = "#42A5F5", color = "white", alpha = 0.75) +
  geom_density(color = "#1565C0", linewidth = 1.2) +
  geom_vline(xintercept = med_cena, linetype = "dashed",
             color = "#E53935", linewidth = 1) +
  annotate("text", x = med_cena + 1, y = Inf, vjust = 1.5,
           label = paste0("Mediana: $", round(med_cena, 2)),
           color = "#E53935", size = 3.8, hjust = 0) +
  facet_wrap(~source, scales = "free_y") +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title    = "Rozkład cen e-booków",
    subtitle = paste0("Tylko rekordy z ceną (", nrow(df_cena), " z ", nrow(df), ")"),
    x = "Cena (USD)",
    y = "Gęstość",
    caption = "Źródło: market_data_5_imputacja_mice.csv | Wykluczone ceny > $80"
  ) +
  theme_eda

save_plot(p1, "01_rozklad_cen.png", w = 12, h = 6)

# ==========================================================================
# 2. Rozkład ocen
# ==========================================================================
df_ocena <- df %>% filter(!is.na(rating))
med_ocena <- median(df_ocena$rating, na.rm = TRUE)

p2 <- ggplot(df_ocena, aes(x = rating, fill = source)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.25,
                 position = "identity", alpha = 0.55, color = NA) +
  geom_density(aes(color = source), linewidth = 1.1, fill = NA) +
  geom_vline(xintercept = med_ocena, linetype = "dashed",
             color = "grey20", linewidth = 1) +
  annotate("text", x = med_ocena + 0.05, y = Inf, vjust = 1.5,
           label = paste0("Mediana: ", round(med_ocena, 2)),
           color = "grey20", size = 3.8, hjust = 0) +
  scale_fill_manual(values  = PALETTE_SRC, name = "Źródło") +
  scale_color_manual(values = PALETTE_SRC, name = "Źródło") +
  scale_x_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(
    title    = "Rozkład ocen książek wg źródła",
    subtitle = paste0("n = ", nrow(df_ocena), " rekordów z oceną"),
    x = "Ocena (0–5)",
    y = "Gęstość",
    caption = "Źródło: market_data_5_imputacja_mice.csv"
  ) +
  theme_eda

save_plot(p2, "02_rozklad_ocen.png")

# ==========================================================================
# 3. Ocena vs cena (scatter)
# ==========================================================================
df_sc <- df %>% filter(!is.na(rating), !is.na(price_usd),
                        price_usd > 0, price_usd <= 80)

p3 <- ggplot(df_sc, aes(x = price_usd, y = rating, color = source)) +
  geom_jitter(alpha = 0.35, size = 1.5, width = 0.3, height = 0.05) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.15) +
  scale_color_manual(values = PALETTE_SRC, name = "Źródło") +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(
    title    = "Ocena a cena e-booków",
    subtitle = "Scatter plot z linią regresji liniowej per źródło",
    x = "Cena (USD)",
    y = "Ocena (0–5)",
    caption = paste0("n = ", nrow(df_sc), " | Źródło: market_data_5_imputacja_mice.csv")
  ) +
  theme_eda

save_plot(p3, "03_ocena_vs_cena.png")

# ==========================================================================
# 4. Top 15 gatunków wg liczby tytułów
# ==========================================================================
top_genres <- df %>%
  count(genre, sort = TRUE) %>%
  slice_head(n = 15) %>%
  mutate(genre = str_wrap(genre, 20),
         genre = fct_reorder(genre, n))

p4 <- ggplot(top_genres, aes(x = n, y = genre, fill = n)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = comma(n)), hjust = -0.15, size = 3.6, fontface = "bold") +
  scale_fill_gradient(low = "#90CAF9", high = "#1565C0") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = comma_format()) +
  labs(
    title    = "Top 15 gatunków wg liczby tytułów",
    subtitle = "Całkowita liczba rekordów per gatunek (wszystkie źródła)",
    x = "Liczba tytułów",
    y = NULL,
    caption = "Źródło: market_data_5_imputacja_mice.csv"
  ) +
  theme_eda +
  theme(panel.grid.major.y = element_blank())

save_plot(p4, "04_top_gatunki.png", h = 7)

# ==========================================================================
# 5. Boxplot ocen per gatunek (top 12)
# ==========================================================================
top12g <- df %>%
  filter(!is.na(rating)) %>%
  count(genre, sort = TRUE) %>%
  slice_head(n = 12) %>%
  pull(genre)

df_g <- df %>%
  filter(genre %in% top12g, !is.na(rating)) %>%
  mutate(genre = factor(str_wrap(genre, 15),
                         levels = str_wrap(top12g, 15)))

p5 <- ggplot(df_g, aes(x = genre, y = rating, fill = genre)) +
  geom_violin(alpha = 0.4, color = NA, scale = "width") +
  geom_boxplot(width = 0.2, outlier.size = 0.6, outlier.alpha = 0.3,
               color = "grey30", fill = "white") +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(
    title    = "Rozkład ocen wg gatunku (Top 12)",
    subtitle = "Violin + Boxplot | skala = szerokość proporcjonalna do liczby rekordów",
    x = NULL,
    y = "Ocena (0–5)",
    caption = "Źródło: market_data_5_imputacja_mice.csv"
  ) +
  theme_eda +
  theme(axis.text.x = element_text(size = 9, angle = 20, hjust = 1))

save_plot(p5, "05_ocena_per_gatunek.png", w = 13, h = 6)

# ==========================================================================
# 6. Rozkład roku publikacji
# ==========================================================================
df_rok <- df %>% filter(!is.na(year), year >= 1900, year <= 2026)

p6 <- ggplot(df_rok, aes(x = year, fill = source)) +
  geom_histogram(binwidth = 5, position = "stack", alpha = 0.85, color = "white") +
  scale_fill_manual(values = PALETTE_SRC, name = "Źródło") +
  scale_x_continuous(breaks = seq(1900, 2030, 10)) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title    = "Rozkład roku pierwszej publikacji",
    subtitle = paste0("n = ", nrow(df_rok), " rekordów z rokiem | przedziały 5-letnie"),
    x = "Rok publikacji",
    y = "Liczba tytułów",
    caption = "Źródło: market_data_5_imputacja_mice.csv"
  ) +
  theme_eda +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p6, "06_rozklad_lat.png")

# ==========================================================================
# 7. Violin plot cen per gatunek (top 10 z cenami)
# ==========================================================================
top10c <- df %>%
  filter(!is.na(price_usd), price_usd > 0) %>%
  count(genre, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(genre)

df_cv <- df %>%
  filter(genre %in% top10c, !is.na(price_usd), price_usd > 0, price_usd <= 60) %>%
  mutate(genre = factor(str_wrap(genre, 15),
                         levels = str_wrap(top10c, 15)))

p7 <- ggplot(df_cv, aes(x = genre, y = price_usd, fill = genre)) +
  geom_violin(alpha = 0.5, color = NA, scale = "width") +
  geom_boxplot(width = 0.2, outlier.size = 0.5, outlier.alpha = 0.3,
               color = "grey30", fill = "white") +
  scale_fill_viridis_d(option = "magma", begin = 0.2, guide = "none") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title    = "Rozkład cen wg gatunku (Top 10)",
    subtitle = "Violin + Boxplot | Tylko e-booki z ceną (max $60)",
    x = NULL,
    y = "Cena (USD)",
    caption = "Źródło: market_data_5_imputacja_mice.csv"
  ) +
  theme_eda +
  theme(axis.text.x = element_text(size = 9, angle = 20, hjust = 1))

save_plot(p7, "07_cena_per_gatunek.png", w = 13, h = 6)

# ==========================================================================
# 8. Heatmapa braków danych per kolumna i źródło
# ==========================================================================
miss_heat <- df %>%
  group_by(source) %>%
  summarise(
    Tytuł       = mean(is.na(title) | title == "") * 100,
    Autor       = mean(is.na(author) | author == "") * 100,
    Gatunek     = mean(is.na(genre) | genre == "") * 100,
    `Cena (USD)`= mean(is.na(price_usd)) * 100,
    Ocena       = mean(is.na(rating)) * 100,
    `Liczba ocen` = mean(is.na(ratings_count)) * 100,
    Rok         = mean(is.na(year)) * 100,
    Język       = mean(is.na(language) | language == "") * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(-source, names_to = "Kolumna", values_to = "pct_brak")

p8 <- ggplot(miss_heat, aes(x = Kolumna, y = source, fill = pct_brak)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(pct_brak, 1), "%"),
                color = pct_brak > 50), size = 3.8, fontface = "bold") +
  scale_fill_gradient2(low = "#E8F5E9", mid = "#FFF9C4",
                       high = "#E53935", midpoint = 50,
                       name = "% braków",
                       labels = percent_format(scale = 1)) +
  scale_color_manual(values = c("FALSE" = "grey20", "TRUE" = "white"),
                     guide = "none") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "Heatmapa braków danych",
    subtitle = "Procent brakujących wartości per źródło i kolumna",
    x = NULL,
    y = NULL,
    caption = "Źródło: market_data_5_imputacja_mice.csv"
  ) +
  theme_eda +
  theme(
    axis.text.x    = element_text(angle = 25, hjust = 1),
    panel.grid      = element_blank()
  )

save_plot(p8, "08_braki_danych.png", w = 11, h = 5)

# ==========================================================================
# 9. Macierz korelacji
# ==========================================================================
num_cols <- df %>%
  select(price_usd, rating, ratings_count, year) %>%
  rename(`Cena (USD)` = price_usd, Ocena = rating,
         `Liczba ocen` = ratings_count, Rok = year)

cor_mat <- cor(num_cols, use = "pairwise.complete.obs")

cor_df <- as.data.frame(as.table(cor_mat)) %>%
  rename(X = Var1, Y = Var2, corr = Freq)

p9 <- ggplot(cor_df, aes(x = X, y = Y, fill = corr)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(corr, 3),
                color = abs(corr) > 0.5), size = 4.2, fontface = "bold") +
  scale_fill_gradient2(low = "#1565C0", mid = "white", high = "#C62828",
                       midpoint = 0, limits = c(-1, 1),
                       name = "Korelacja Pearsona") +
  scale_color_manual(values = c("FALSE" = "grey30", "TRUE" = "white"),
                     guide = "none") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "Macierz korelacji zmiennych numerycznych",
    subtitle = "Korelacja Pearsona | Metoda: pairwise complete obs.",
    x = NULL,
    y = NULL,
    caption = "Źródło: market_data_5_imputacja_mice.csv"
  ) +
  theme_eda +
  theme(
    axis.text.x   = element_text(angle = 20, hjust = 1),
    panel.grid    = element_blank()
  )

save_plot(p9, "09_korelacje.png", w = 8, h = 6)

# ==========================================================================
# 10. Porównanie źródeł: kluczowe statystyki
# ==========================================================================
src_stats <- df %>%
  group_by(source) %>%
  summarise(
    n_rekordow    = n(),
    sr_ocena      = mean(rating, na.rm = TRUE),
    sr_cena       = mean(price_usd[price_usd > 0], na.rm = TRUE),
    pct_z_cena    = mean(!is.na(price_usd) & price_usd > 0) * 100,
    pct_z_ocena   = mean(!is.na(rating)) * 100,
    sr_rok        = mean(year, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Podsumowanie statystyczne per źródło ===\n")
print(as.data.frame(src_stats))

# Wykres: 4-panelowe porównanie źródeł
src_long <- src_stats %>%
  select(source, sr_ocena, sr_cena, pct_z_cena, pct_z_ocena) %>%
  pivot_longer(-source, names_to = "metryka", values_to = "wartosc") %>%
  mutate(metryka = recode(metryka,
    "sr_ocena"    = "Śr. ocena",
    "sr_cena"     = "Śr. cena (USD)",
    "pct_z_cena"  = "% rekordów z ceną",
    "pct_z_ocena" = "% rekordów z oceną"
  ))

p10 <- ggplot(src_long, aes(x = source, y = wartosc, fill = source)) +
  geom_col(show.legend = FALSE, alpha = 0.85, width = 0.6) +
  geom_text(aes(label = round(wartosc, 1)), vjust = -0.5,
            size = 3.8, fontface = "bold") +
  facet_wrap(~metryka, scales = "free_y") +
  scale_fill_manual(values = PALETTE_SRC) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Porównanie źródeł danych",
    subtitle = "Kluczowe metryki: oceny, ceny i pokrycie zmiennych",
    x = NULL,
    y = NULL,
    caption = "Źródło: market_data_5_imputacja_mice.csv"
  ) +
  theme_eda +
  theme(
    axis.text.x         = element_text(angle = 15, hjust = 1),
    panel.grid.major.x  = element_blank()
  )

save_plot(p10, "10_porownanie_zrodel.png", w = 12, h = 8)

# ==========================================================================
# Wydruk podsumowania
# ==========================================================================
cat(paste0("\n=== Raport braków danych (ogółem) ===\n"))
miss_summary <- data.frame(
  Kolumna     = c("title", "author", "genre", "price_usd",
                  "rating", "ratings_count", "year", "language"),
  Brakuje_n   = sapply(df[c("title","author","genre","price_usd",
                             "rating","ratings_count","year","language")],
                        function(x) sum(is.na(x) | x == "")),
  Brakuje_pct = round(sapply(df[c("title","author","genre","price_usd",
                                   "rating","ratings_count","year","language")],
                              function(x) mean(is.na(x) | x == "") * 100), 2)
)
print(miss_summary, row.names = FALSE)

cat(paste0("\n=== Wszystkie wykresy EDA zapisane do folderu: ", OUTPUT_DIR, "/ ===\n"))
cat(">>> Analiza zakończona pomyślnie.\n")
