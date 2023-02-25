library("ggplot2")
library("maps")
library("dplyr")
library("plotly")
library("stringr")
library("readr")
library("tidyverse")

spl_df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_df <- spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

romance <- spl_df %>% 
  filter(str_detect(Subjects, "Romance"))

thriller <- spl_df %>% 
  filter(str_detect(Subjects, "Thriller"))

mystery <- spl_df %>% 
  filter(str_detect(Subjects, "Mystery"))

checkouts_per_month_rom <- romance %>% 
  group_by(date) %>%
  summarize(total_checkouts_rom = sum(Checkouts, na.rm = TRUE))

checkouts_per_month_thril <- thriller %>% 
  group_by(date) %>%
  summarize(total_checkouts_thril = sum(Checkouts, na.rm = TRUE))

checkouts_per_month_myst <- mystery %>% 
  group_by(date) %>%
  summarize(total_checkouts_myst = sum(Checkouts, na.rm = TRUE))

checkouts_per_month <- left_join(checkouts_per_month_rom, checkouts_per_month_thril)
checkouts_per_month <- left_join(checkouts_per_month, checkouts_per_month_myst)

checkouts_per_month_long <- checkouts_per_month %>%
  pivot_longer(cols=c('total_checkouts_rom', 'total_checkouts_thril', 'total_checkouts_myst'),
               names_to='genre',
               values_to='total_checkouts')

genre_graph <- ggplot(checkouts_per_month_long) +
  geom_line(aes(x = date, y=total_checkouts, group=genre, color=genre))+
  scale_y_continuous(limits = c(0, 80000)) +
  labs(title = "Total Checkouts by Popular Genres in 2022", 
       x = "Month", 
       y = "Total Checkouts")
