library(dplyr) # Wrangling data
library(inspectdf) # add on wrangling data
library(caret) # classification & regression training
library(scorecard) # credit risk scorecard
library(rsample) # resampling data
library(gtools) # programming tools
library(readxl)
library(GGally) # plot heatmap correlation
library(knitr) # reporting in R
library(scales) # manipulasi skala data untuk visualisasi
library(car) # check multicollinearity
library(DT)
library(DBI)
library(writexl)
library(ggplot2)
library(plotly)
library(lubridate)
library(ggthemes)
library(rsconnect)



theme_algoritma <- theme(
  legend.key = element_rect(fill = "black"),
  legend.background = element_rect(color = "white", fill =
                                     "#263238"),
  plot.subtitle = element_text(size = 6, color =
                                 "white"),
  panel.background = element_rect(fill = "#dddddd"),
  panel.border = element_rect(fill = NA),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "darkgrey", linetype =
                                      2),
  panel.grid.minor.y = element_blank(),
  plot.background = element_rect(fill = "#263238"),
  text = element_text(color = "white"),
  axis.text = element_text(color = "white")
)



################################################################################

# dashboard

# read data

scorecard_all <- rbind(read_xlsx("scorecard_n.xlsx"), 
                       read_xlsx("scorecard.xlsx"))

# wrangling data

scorecard_all <- scorecard_all %>%
  mutate(
    sex = ifelse(sex == 1, "Male", "Female"),
    sex = as.factor(sex),
    education = case_when(
      education == 1 ~ "Pascasarjana",
      education == 2 ~ "Sarjana",
      education == 3 ~ "SMA",
      education == 4 ~ "Lainnya"
    ),
    education = as.factor(education),
    marriage = case_when(
      marriage == 1 ~ "Menikah",
      marriage == 2 ~ "Lajang",
      marriage == 3 ~ "Lainnya"
    ),
    marriage = as.factor(marriage),
    recommendation = as.factor(recommendation),
    Waktu_Input =  ymd_hms(Waktu_Input),
    Tanggal_Input = as_date(Waktu_Input) # ekstrak tanggal
  )


################################################################################

# import data

# read data

data <- read_xlsx("credit_taiwan.xlsx")
data

data_clean <- data %>%
  select(-c(id, gb_flag))

################################################################################

# predict

# load model

model <- readRDS("model.RDS")

################################################################################
