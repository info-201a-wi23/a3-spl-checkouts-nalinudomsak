library("ggplot2")
library("maps")
library("dplyr")
library("plotly")
library("stringr")
library("readr")
library("tidyverse")
library("scales")
spl_df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_df <- spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

# Excluding books, audiobooks, ebooks, music
df_material <- spl_df %>% 
  group_by(MaterialType) %>% 
  summarize(Checkouts = sum(Checkouts))

df_material_sum <- df_material[c(2,3,7,41,50),]

bar_chart <- ggplot(df_material_sum)+
  geom_col(aes(x = MaterialType, y = Checkouts, fill= MaterialType))+
  scale_y_continuous(name="Checkouts", labels = scales::comma)+
  labs(x = "Material Type", y = "Checkouts", title = "A Bar Chart showing the number of checkouts by the most popular Material Types")
