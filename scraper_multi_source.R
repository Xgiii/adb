cat("\n=== Uruchamianie systemu zbierania danych rynkowych ===\n")
cat("Źródła: Open Library API | iTunes Store API | Google Books API\n\n")

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(stringr)
})

# Timestamp bieżącego uruchomienia — będzie identyczny dla wszystkich rekordów z tej sesji
run_timestamp <- Sys.time()
cat(paste0("Timestamp uruchomienia: ", format(run_timestamp, "%Y-%m-%d %H:%M:%S"), "\n\n"))

# Wspólna ramka danych — ujednolicony schemat dla wszystkich 3 źródeł
# source, title, author, genre, price_usd, rating, ratings_count, year, language, scraped_at
combined_data <- data.frame(
  source        = character(),
  title         = character(),
  author        = character(),
  genre         = character(),
  price_usd     = numeric(),
  rating        = numeric(),
  ratings_count = integer(),
  year          = integer(),
  language      = character(),
  scraped_at    = as.POSIXct(character()),
  stringsAsFactors = FALSE
)

# Operator %||% nie jest wbudowany — definiujemy go, użyjemy go w wielu miejscach
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (is.na(x[1])) return(y)
  x
}

# =========================================================================
# === ŹRÓDŁO 1: Open Library API ==========================================
# =========================================================================
cat("--- [1/3] Pobieranie z Open Library API ---\n")

# Lista gatunków do przeszukania (większy zakres = więcej danych)
genres <- c("fiction", "science_fiction", "mystery", "romance", "fantasy",
            "thriller", "historical_fiction", "horror", "biography", "self_help",
            "poetry", "history", "philosophy", "business", "cookbook",
            "children", "art", "psychology", "sports", "travel")

for (genre in genres) {
  cat(paste0(" >> Gatunek: ", genre, "... "))
  
  url <- paste0("https://openlibrary.org/search.json?subject=", genre,
                "&limit=100&fields=title,author_name,ratings_average,ratings_count,first_publish_year,language")
  
  resp <- tryCatch(GET(url, timeout(15)), error = function(e) NULL)
  
  if (is.null(resp) || status_code(resp) != 200) {
    cat("BŁĄD\n"); next
  }
  
  data_json <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
  docs <- data_json$docs
  
  if (is.null(docs) || nrow(docs) == 0) {
    cat("Brak wyników\n"); next
  }
  
  # Bezpieczna ekstrakcja zapobiegająca problemom z pustymi wektorami (np character(0))
  get_v <- function(col_name, row_idx) {
    if (!col_name %in% names(docs)) return(NA)
    val <- docs[[col_name]]
    if (is.null(val)) return(NA)
    
    item <- if (is.list(val)) val[[row_idx]] else val[row_idx]
    if (is.null(item) || length(item) == 0 || is.na(item[1])) return(NA)
    return(item[1])
  }
  
  for (i in seq_len(nrow(docs))) {
    title_v   <- get_v("title", i)
    author_v  <- get_v("author_name", i)
    rating_v  <- get_v("ratings_average", i)
    rcount_v  <- get_v("ratings_count", i)
    year_v    <- get_v("first_publish_year", i)
    lang_v    <- get_v("language", i)
    
    combined_data <- rbind(combined_data, data.frame(
      source        = "Open Library",
      title         = as.character(title_v %||% NA),
      author        = as.character(author_v %||% NA),
      genre         = genre,
      price_usd     = NA_real_,  # Open Library nie podaje ceny
      rating        = as.numeric(rating_v %||% NA),
      ratings_count = as.integer(rcount_v %||% NA),
      year          = as.integer(year_v %||% NA),
      language      = as.character(lang_v %||% NA),
      scraped_at    = run_timestamp,
      stringsAsFactors = FALSE
    ))
  }
  cat(paste0(nrow(docs), " rekordów\n"))
  Sys.sleep(0.5) # Szanujemy serwer — pauza między żądaniami
}

cat(paste0(" >> Open Library: pobrano ", sum(combined_data$source == "Open Library"), " rekordów łącznie.\n\n"))

# =========================================================================
# === ŹRÓDŁO 2: iTunes Store Search API ===================================
# =========================================================================
cat("--- [2/3] Pobieranie z iTunes Store API (Apple) ---\n")

itunes_genres <- c("mystery thriller", "science fiction", "fantasy", 
                   "romance novel", "horror fiction", "biography memoir",
                   "historical fiction", "self help", "true crime", "adventure",
                   "poetry", "world history", "philosophy", "business finance", "cooking",
                   "childrens fiction", "art design", "psychology", "sports outdoors", "travel")

for (genre in itunes_genres) {
  cat(paste0(" >> Gatunek: ", genre, "... "))
  
  url <- paste0("https://itunes.apple.com/search?",
                "term=", URLencode(genre),
                "&entity=ebook",
                "&limit=100",
                "&country=us")
  
  resp <- tryCatch(GET(url, timeout(15)), error = function(e) NULL)
  
  if (is.null(resp) || status_code(resp) != 200) {
    cat("BŁĄD\n"); next
  }
  
  data_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  results   <- data_json$results
  
  if (is.null(results) || length(results) == 0 || nrow(results) == 0) {
    cat("Brak wyników\n"); next
  }
  
  for (i in seq_len(nrow(results))) {
    r <- results[i, ]
    combined_data <- rbind(combined_data, data.frame(
      source        = "iTunes Store",
      title         = as.character(r$trackName %||% NA),
      author        = as.character(r$artistName %||% NA),
      genre         = genre,
      price_usd     = as.numeric(r$price %||% NA),
      rating        = as.numeric(r$averageUserRating %||% NA),
      ratings_count = as.integer(r$userRatingCount %||% NA),
      year          = NA_integer_,
      language      = as.character(r$language %||% NA),
      scraped_at    = run_timestamp,
      stringsAsFactors = FALSE
    ))
  }
  cat(paste0(nrow(results), " rekordów\n"))
  Sys.sleep(0.5)
}

cat(paste0(" >> iTunes Store: pobrano ", sum(combined_data$source == "iTunes Store"), " rekordów łącznie.\n\n"))

# =========================================================================
# === ŹRÓDŁO 3: Google Books API ==========================================
# =========================================================================
cat("--- [3/3] Pobieranie z Google Books API ---\n")

google_genres <- c("fiction+novel", "science+fiction", "mystery+detective",
                   "fantasy+epic", "romance+novel", "horror+supernatural",
                   "historical+fiction", "self+help+motivation", "thriller+suspense", "biography",
                   "poetry", "history", "philosophy", "business+economics", "cooking+recipes",
                   "childrens+books", "art+architecture", "psychology", "sports", "travel")

for (genre in google_genres) {
  cat(paste0(" >> Gatunek: ", genre, "... "))
  
  url <- paste0("https://www.googleapis.com/books/v1/volumes?",
                "q=subject:", URLencode(genre),
                "&maxResults=40",
                "&printType=books",
                "&orderBy=relevance")
  
  resp <- tryCatch(GET(url, timeout(15)), error = function(e) NULL)
  
  if (is.null(resp) || status_code(resp) != 200) {
    cat("BŁĄD\n"); next
  }
  
  data_json <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
  items     <- data_json$items
  
  if (is.null(items) || nrow(items) == 0) {
    cat("Brak wyników\n"); next
  }
  
  fetched <- 0
  for (i in seq_len(nrow(items))) {
    vi <- items[i, ]
    
    # helper — bezpieczna ekstrakcja skalara z potencjalnie NULL/długości 0
    safe_char <- function(x) {
      val <- tryCatch(x, error = function(e) NULL)
      if (is.null(val) || length(val) == 0 || all(is.na(val))) return(NA_character_)
      as.character(val[[1]])
    }
    safe_num <- function(x) {
      val <- tryCatch(x, error = function(e) NULL)
      if (is.null(val) || length(val) == 0 || all(is.na(val))) return(NA_real_)
      suppressWarnings(as.numeric(val[[1]]))
    }
    safe_int <- function(x) {
      val <- tryCatch(x, error = function(e) NULL)
      if (is.null(val) || length(val) == 0 || all(is.na(val))) return(NA_integer_)
      suppressWarnings(as.integer(val[[1]]))
    }
    
    # Autor — lista wektorów
    authors_raw <- tryCatch(vi$volumeInfo.authors[[1]], error = function(e) NULL)
    author_str  <- if (!is.null(authors_raw) && length(authors_raw) > 0)
                     paste(authors_raw, collapse = ", ") else NA_character_
    
    # Gatunek/kategoria — lista wektorów
    cats_raw <- tryCatch(vi$volumeInfo.categories[[1]], error = function(e) NULL)
    cat_str  <- if (!is.null(cats_raw) && length(cats_raw) > 0) cats_raw[1] else genre
    
    # Rok z publishedDate (format "YYYY" lub "YYYY-MM-DD")
    pub_date <- safe_char(vi$volumeInfo.publishedDate)
    year_v   <- if (!is.na(pub_date)) suppressWarnings(as.integer(substr(pub_date, 1, 4))) else NA_integer_
    
    # Cena z zagnieżdżonej listy saleInfo.listPrice
    price_v <- tryCatch({
      p <- items$saleInfo.listPrice[[i]]
      if (!is.null(p) && "amount" %in% names(p)) as.numeric(p$amount) else NA_real_
    }, error = function(e) NA_real_)
    
    combined_data <- rbind(combined_data, data.frame(
      source        = "Google Books",
      title         = safe_char(vi$volumeInfo.title),
      author        = author_str,
      genre         = as.character(cat_str),
      price_usd     = price_v,
      rating        = safe_num(vi$volumeInfo.averageRating),
      ratings_count = safe_int(vi$volumeInfo.ratingsCount),
      year          = year_v,
      language      = safe_char(vi$volumeInfo.language),
      scraped_at    = run_timestamp,
      stringsAsFactors = FALSE
    ))
    fetched <- fetched + 1
  }
  cat(paste0(fetched, " rekordów\n"))
  Sys.sleep(1.0) # Google Books wymaga nieco spokojniejszego tempa zapytań
}

cat(paste0(" >> Google Books: pobrano ", sum(combined_data$source == "Google Books"), " rekordów łącznie.\n\n"))

# =========================================================================
# === PODSUMOWANIE I ZAPIS ================================================
# =========================================================================

cat("=== Podsumowanie zebranych danych ===\n")
cat(paste0("Łączna liczba rekordów: ", nrow(combined_data), "\n"))
print(table(combined_data$source))
cat("\n")

# Podgląd kilku rekordów z każdego źródła
for (src in unique(combined_data$source)) {
  cat(paste0("\n--- Przykład: ", src, " ---\n"))
  print(head(combined_data[combined_data$source == src, c("title","author","genre","price_usd","rating","ratings_count")], 3))
}

# ---- Zapis 1: plik bieżący (nadpisanie) — zachowanie kompatybilności z etapami 2-5
output_file <- file.path(getwd(), "market_data_raw.csv")
write.csv(combined_data, file = output_file, row.names = FALSE, fileEncoding = "UTF-8")
cat(paste0(">>> Dane bieżącej sesji zapisano do: ", output_file, "\n"))

# ---- Zapis 2: plik szeregu czasowego (tryb append — doklejanie kolejnych sesji)
timeseries_file <- file.path(getwd(), "market_data_timeseries.csv")
first_write     <- !file.exists(timeseries_file)
write.table(
  combined_data,
  file      = timeseries_file,
  sep       = ",",
  row.names = FALSE,
  col.names = first_write,   # nagłówek tylko przy pierwszym zapisie
  append    = !first_write,
  fileEncoding = "UTF-8"
)

cat(paste0(">>> Dane dołączono do szeregu czasowego: ", timeseries_file, "\n"))
cat(paste0("    (tryb: ", ifelse(first_write, "NOWY PLIK", "DOŁĄCZONO DO ISTNIEJĄCEGO"), ")\n"))
cat("\n>>> ZAPIS ZAKOŃCZONY <<<\n")
cat(">>> Plik gotowy do Etapu 3: czyszczenie i strukturyzacja danych.\n")
