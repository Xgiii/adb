# =========================================================================
# Skrypt 7: Wizualizacja zmian danych w przedziale czasowym
# =========================================================================
# Wczytuje market_data_timeseries.csv i generuje wykresy PNG pokazujące
# jak dane ewoluowały między kolejnymi sesjami zbierania danych.
# =========================================================================

cat("\n=== Wizualizacja szeregu czasowego ===\n\n")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(tidyr)
})

# ---- Parametry ----------------------------------------------------------
INPUT_FILE  <- "market_data_timeseries.csv"
OUTPUT_DIR  <- "wykresy_czas"

if (!file.exists(INPUT_FILE)) {
  stop("Brak pliku: ", INPUT_FILE,
       "\nUruchom najpierw 6_scraper_scheduler.R")
}
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

# ---- Wczytanie danych ---------------------------------------------------
cat("Wczytywanie danych...\n")
df <- read.csv(INPUT_FILE, stringsAsFactors = FALSE, encoding = "UTF-8")
df$scraped_at <- as.POSIXct(df$scraped_at, format = "%Y-%m-%d %H:%M:%S")

if (!"snapshot_id" %in% names(df)) {
  df$snapshot_id <- as.integer(as.factor(df$scraped_at))
}

n_snaps <- length(unique(df$snapshot_id))
cat(paste0("Załadowano ", nrow(df), " rekordów z ", n_snaps, " snapshotów.\n\n"))

# ---- Motyw graficzny (spójny dla wszystkich wykresów) -------------------
theme_proj <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, hjust = 0.5,
                                    margin = margin(b = 8)),
    plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "grey40",
                                    margin = margin(b = 14)),
    plot.caption     = element_text(size = 9, color = "grey55", hjust = 1),
    axis.title       = element_text(size = 11, color = "grey25"),
    axis.text        = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 10, face = "bold"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

PALETTE_SRC <- c("Open Library" = "#2196F3",
                 "iTunes Store" = "#FF5722",
                 "Google Books" = "#4CAF50")

save_plot <- function(p, filename, w = 10, h = 6) {
  path <- file.path(OUTPUT_DIR, filename)
  ggsave(path, plot = p, width = w, height = h, dpi = 150, bg = "white")
  cat(paste0("  [OK] Zapisano: ", path, "\n"))
}

# ---- Agregaty per snapshot ----------------------------------------------
snap_agg <- df %>%
  group_by(snapshot_id, scraped_at) %>%
  summarise(
    rekordy        = n(),
    sr_ocena       = mean(rating, na.rm = TRUE),
    sr_cena        = mean(price_usd, na.rm = TRUE),
    braki_ocena_pct = mean(is.na(rating)) * 100,
    braki_cena_pct  = mean(is.na(price_usd)) * 100,
    braki_autor_pct = mean(is.na(author) | author == "") * 100,
    .groups = "drop"
  ) %>%
  mutate(label = format(scraped_at, "%d %b"))

cat("Generowanie wykresów czasowych...\n")

# ==========================================================================
# Wykres 1: Liczba rekordów per snapshot
# ==========================================================================
p1 <- ggplot(snap_agg, aes(x = scraped_at, y = rekordy)) +
  geom_area(fill = "#2196F3", alpha = 0.15) +
  geom_line(color = "#2196F3", linewidth = 1.2) +
  geom_point(color = "#2196F3", size = 4, shape = 21,
             fill = "white", stroke = 2) +
  geom_text(aes(label = format(rekordy, big.mark = " ")),
            vjust = -1.1, size = 3.5, color = "#1565C0", fontface = "bold") +
  scale_x_datetime(date_labels = "%d %b %Y", date_breaks = "1 week") +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title    = "Liczba zebranych rekordów per sesja scrapera",
    subtitle = paste0("Zakres: ", format(min(snap_agg$scraped_at), "%d %b %Y"),
                      " — ", format(max(snap_agg$scraped_at), "%d %b %Y")),
    x = "Data sesji zbierania danych",
    y = "Liczba rekordów",
    caption = "Źródło: market_data_timeseries.csv"
  ) +
  theme_proj

save_plot(p1, "01_liczba_rekordow_w_czasie.png")

# ==========================================================================
# Wykres 2: Średnia ocena w czasie (ogółem + per źródło)
# ==========================================================================
ocena_src <- df %>%
  group_by(snapshot_id, scraped_at, source) %>%
  summarise(sr_ocena = mean(rating, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(ocena_src, aes(x = scraped_at, y = sr_ocena, color = source)) +
  geom_line(linewidth = 1.0, alpha = 0.8) +
  geom_point(size = 3.5, shape = 21, aes(fill = source), color = "white", stroke = 1.5) +
  geom_line(data = snap_agg, aes(x = scraped_at, y = sr_ocena),
            color = "grey30", linewidth = 1.4, linetype = "dashed", inherit.aes = FALSE) +
  scale_color_manual(values = PALETTE_SRC, name = "Źródło") +
  scale_fill_manual(values  = PALETTE_SRC, guide  = "none") +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "1 week") +
  scale_y_continuous(limits = c(NA, 5), breaks = seq(0, 5, 0.5)) +
  labs(
    title    = "Średnia ocena książek w czasie",
    subtitle = "Linia przerywana = ogółem; kolory = poszczególne źródła",
    x = "Data sesji",
    y = "Średnia ocena (0–5)",
    caption = "Źródło: market_data_timeseries.csv"
  ) +
  theme_proj

save_plot(p2, "02_srednia_ocena_w_czasie.png")

# ==========================================================================
# Wykres 3: Średnia cena w czasie (tylko rekordy z ceną)
# ==========================================================================
cena_src <- df %>%
  filter(!is.na(price_usd), price_usd > 0) %>%
  group_by(snapshot_id, scraped_at, source) %>%
  summarise(sr_cena = mean(price_usd, na.rm = TRUE), .groups = "drop")

snap_cena <- df %>%
  filter(!is.na(price_usd), price_usd > 0) %>%
  group_by(snapshot_id, scraped_at) %>%
  summarise(sr_cena = mean(price_usd, na.rm = TRUE), .groups = "drop")

p3 <- ggplot(cena_src, aes(x = scraped_at, y = sr_cena, color = source)) +
  geom_ribbon(data = snap_cena,
              aes(x = scraped_at,
                  ymin = sr_cena - sd(snap_cena$sr_cena),
                  ymax = sr_cena + sd(snap_cena$sr_cena)),
              fill = "grey80", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(linewidth = 1.0, alpha = 0.85) +
  geom_point(size = 3.5, shape = 21, aes(fill = source), color = "white", stroke = 1.5) +
  geom_line(data = snap_cena, aes(x = scraped_at, y = sr_cena),
            color = "grey30", linewidth = 1.4, linetype = "dashed", inherit.aes = FALSE) +
  scale_color_manual(values = PALETTE_SRC, name = "Źródło") +
  scale_fill_manual(values  = PALETTE_SRC, guide  = "none") +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "1 week") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title    = "Średnia cena e-booków w czasie",
    subtitle = "Obszar szary = ±1 odchylenie standardowe (ogółem)",
    x = "Data sesji",
    y = "Średnia cena (USD)",
    caption = "Źródło: market_data_timeseries.csv | Wykluczone: brakujące i zerowe ceny"
  ) +
  theme_proj

save_plot(p3, "03_srednia_cena_w_czasie.png")

# ==========================================================================
# Wykres 4: Pokrycie zmiennych (% braków) w czasie
# ==========================================================================
missingness <- snap_agg %>%
  select(scraped_at, braki_ocena_pct, braki_cena_pct, braki_autor_pct) %>%
  pivot_longer(cols = -scraped_at, names_to = "zmienna", values_to = "pct_brak") %>%
  mutate(zmienna = recode(zmienna,
    "braki_ocena_pct"  = "Ocena",
    "braki_cena_pct"   = "Cena (USD)",
    "braki_autor_pct"  = "Autor"
  ))

PALETTE_MISS <- c("Ocena" = "#E91E63", "Cena (USD)" = "#FF9800", "Autor" = "#9C27B0")

p4 <- ggplot(missingness, aes(x = scraped_at, y = pct_brak, color = zmienna, group = zmienna)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.5, shape = 21, aes(fill = zmienna), color = "white", stroke = 1.5) +
  scale_color_manual(values = PALETTE_MISS, name = "Zmienna") +
  scale_fill_manual(values  = PALETTE_MISS, guide  = "none") +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "1 week") +
  scale_y_continuous(labels = percent_format(scale = 1),
                     limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Poziom braków danych w czasie (missingness)",
    subtitle = "Procent brakujących wartości per sesja zbierania",
    x = "Data sesji",
    y = "% brakujących wartości",
    caption = "Źródło: market_data_timeseries.csv"
  ) +
  theme_proj

save_plot(p4, "04_pokrycie_zmiennych_w_czasie.png")

# ==========================================================================
# Wykres 5: Nowe unikalne tytuły odkryte w każdym snapshotu
# ==========================================================================
seen_titles <- character(0)
new_titles_per_snap <- integer(n_snaps)

for (sid in sort(unique(df$snapshot_id))) {
  tytuly_snap     <- unique(tolower(df$title[df$snapshot_id == sid]))
  nowe            <- setdiff(tytuly_snap, seen_titles)
  new_titles_per_snap[sid] <- length(nowe)
  seen_titles     <- union(seen_titles, tytuly_snap)
}

snap_nowe <- snap_agg %>%
  mutate(nowe_tytuly = new_titles_per_snap[snapshot_id],
         skumulowane  = cumsum(new_titles_per_snap)[snapshot_id])

p5 <- ggplot(snap_nowe, aes(x = scraped_at)) +
  geom_col(aes(y = nowe_tytuly), fill = "#00BCD4", alpha = 0.75, width = 86400 * 3) +
  geom_line(aes(y = skumulowane / 10), color = "#006064", linewidth = 1.3, linetype = "solid") +
  geom_point(aes(y = skumulowane / 10), color = "#006064", size = 3.5) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "1 week") +
  scale_y_continuous(
    name = "Nowe tytuły (słupki)",
    labels = comma_format(),
    sec.axis = sec_axis(~ . * 10, name = "Łącznie unikalne tytuły (linia)",
                        labels = comma_format())
  ) +
  labs(
    title    = "Nowe unikalne tytuły odkryte w kolejnych sesjach",
    subtitle = "Słupki = nowe w danej sesji | Linia = skumulowana liczba unikalnych tytułów",
    x = "Data sesji",
    caption = "Źródło: market_data_timeseries.csv"
  ) +
  theme_proj +
  theme(axis.title.y.right = element_text(color = "#006064"),
        axis.text.y.right  = element_text(color = "#006064"))

save_plot(p5, "05_nowe_tytuly_w_czasie.png")

# ==========================================================================
# Wykres 6: Rozkład ocen (boxplot) per snapshot
# ==========================================================================
df_oceny <- df %>% filter(!is.na(rating))
df_oceny$snap_label <- format(df_oceny$scraped_at, "%d %b")
df_oceny$snap_label <- factor(df_oceny$snap_label,
                               levels = format(sort(unique(df$scraped_at)), "%d %b"))

p6 <- ggplot(df_oceny, aes(x = snap_label, y = rating, fill = snap_label)) +
  geom_violin(alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.25, outlier.size = 0.8, outlier.alpha = 0.3,
               color = "grey30", fill = "white", alpha = 0.8) +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(
    title    = "Rozkład ocen książek w kolejnych sesjach",
    subtitle = "Violin + Boxplot per snapshot czasowy",
    x = "Data sesji",
    y = "Ocena (0–5)",
    caption = "Źródło: market_data_timeseries.csv"
  ) +
  theme_proj

save_plot(p6, "06_rozklad_ocen_per_snapshot.png")

cat(paste0("\n=== Wszystkie wykresy zapisane do folderu: ", OUTPUT_DIR, "/ ===\n"))
cat(">>> Gotowy do: 8_analiza_eda.R\n")
