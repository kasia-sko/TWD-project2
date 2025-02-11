library(dplyr)
library(tidyr)

# 1 Aplikacje

# Mając rozdzielony plik nasz_dzien.xlsx na 3 pliki: "nasz_dzien(Zuzia).csv", "nasz_dzien(Kasia).csv", "nasz_dzien(Milosz).csv"
# Przy zapisywaniu poszczególnych sheets użyłem formatu CSV (Comma delimited), Zuzia miała ze średnikiem.  

zuzia_apps_df <- read.csv("nasz_dzienZuzia.csv",sep=";")
kasia_apps_df <- read.csv("nasz_dzienKasia.csv",sep=";")
milosz_apps_df <- read.csv("nasz_dzienMilosz.csv",sep=";")

process_daily_data <- function(data, user) {
  data <- data %>%
    filter(X!="Unlocked" & X!="Notifications"  & X!="Total time")%>% 
    pivot_longer(cols = -1, names_to = "date", values_to = "time_spent") %>%
    rename(application = 1) %>%
    mutate(
      date = as.Date(sub("^X", "", date), format = "%Y.%m.%d"),  
      time_spent = round(as.numeric(as.difftime(time_spent, format="%H:%M:%S", units="mins"))),
      user = user
    )
  return(data)
}

kasia_apps_df <- process_daily_data(kasia_apps_df, "Kasia")
zuzia_apps_df <- process_daily_data(zuzia_apps_df, "Zuzia")
milosz_apps_df <- process_daily_data(milosz_apps_df, "Milosz")

# Ostateczna ramka z dokładnością do minut
apps_df <- bind_rows(kasia_apps_df, zuzia_apps_df, milosz_apps_df)


# 2 Czynności w ciągu dnia

kasia_activities_df <- read.csv("kasia.csv")
zuzia_activities_df <- read.csv("zuzia.csv")
milosz_activities_df <- read.csv("milosz.csv")

kasia_activities_df$name <- "Kasia"
zuzia_activities_df$name <- "Zuzia"
milosz_activities_df$name <- "Milosz"

activities_df <- bind_rows(kasia_activities_df, zuzia_activities_df, milosz_activities_df)

# Konwersja kolumn daty-czasu do formatu POSIXct
activities_df <- activities_df %>%
  mutate(
    time.started = as.POSIXct(time.started, format = "%Y-%m-%d %H:%M:%S"),
    time.ended = as.POSIXct(time.ended, format = "%Y-%m-%d %H:%M:%S")
  )

