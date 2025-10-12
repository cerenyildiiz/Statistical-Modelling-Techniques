# ------------------------------------------------------------
# HW-2 | Raw -> Analysis-ready (Basit Akış)
# Veri: UCI Air Quality (AirQualityUCI.csv)
# ------------------------------------------------------------

# 0) Tekrarlanabilirlik için (rastgele işlem ekleyeceksek)
set.seed(123)

# 1) Paketler
 install.packages(c("readr","dplyr","lubridate","ggplot2"))
library(readr); library(dplyr); library(lubridate); library(ggplot2)

# 2) Ham veriyi indir & oku
zip_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00360/AirQualityUCI.zip"
zipfile  <- tempfile(fileext = ".zip")
download.file(zip_url, zipfile, mode = "wb")
csv_path <- unzip(zipfile, files = "AirQualityUCI.csv", exdir = tempdir())

# Noktalı-virgüllü CSV, ondalık ayıracı virgül (UCI dosyası böyle)
df_raw <- read_delim(csv_path, delim = ";",
                     locale = locale(decimal_mark = ","),
                     show_col_types = FALSE)

# 3) Ham veri: "tamamen boş" (tümü NA) veya otomatik "...16" vb. sütunları temizle
to_drop <- grep("^\\.\\.\\.", names(df_raw), value = TRUE) # ...16, ...17 gibi
df_raw  <- df_raw |>
  select(-any_of(to_drop)) |>
  select(where(~ !all(is.na(.))))  # yine de tümü NA olan varsa at

# (İsteğe bağlı kısa kontrol: ham yapı)
cat("HAM boyut:", nrow(df_raw), "x", ncol(df_raw), "\n")
# head(df_raw, 3); str(df_raw)

# 4) Özel eksik kodu (-200) -> NA  (yalnızca sayısal kolonlarda!)
df <- df_raw |>
  mutate(across(where(is.numeric), ~ na_if(., -200)))

# 5) Tek zaman ekseni (Datetime) üret
df <- df |>
  mutate(Time2    = gsub("\\.", ":", Time),
         Datetime = dmy_hms(paste(Date, Time2), quiet = TRUE)) |>
  select(-Time2)

# Doğrulama (rapora not düşebilirsin)
cat("Datetime NA oranı:", mean(is.na(df$Datetime)), "\n")
# range(df$Datetime, na.rm = TRUE)

# 6) Günlük ortalama (analize-hazır en basit özet)
daily <- df |>
  group_by(gun = as.Date(Datetime)) |>
  summarise(
    CO_daily  = mean(`CO(GT)`,  na.rm = TRUE),
    NO2_daily = mean(`NO2(GT)`, na.rm = TRUE),
    .groups = "drop"
  )

# 7) Basit grafik (CO günlük ortalama)
ggplot(daily, aes(gun, CO_daily)) +
  geom_line() +
  labs(title = "CO(GT) – Günlük Ortalama", x = "Tarih", y = "CO (mg/m³)")

# 8) Analize-hazır çıktı (CSV)
write.csv(daily, "air_quality_daily.csv", row.names = FALSE)
cat("Kaydedildi: air_quality_daily.csv\n")
