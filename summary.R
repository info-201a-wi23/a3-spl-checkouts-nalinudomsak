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

author_dfch <- spl_df %>% 
  filter(str_detect(Creator, "Colleen")) %>% 
  filter(str_detect(Creator, "Hoover")) 

author_dftjr <- spl_df %>% 
  filter(str_detect(Creator, "Taylor")) %>% 
  filter(str_detect(Creator, "Jenkins")) %>% 
  filter(str_detect(Creator, "Reid")) 

checkouts_per_month_ch <- author_dfch %>% 
  group_by(date) %>%
  summarize(total_checkouts_ch = sum(Checkouts, na.rm = TRUE))

checkouts_per_month_tjr <- author_dftjr %>%
  group_by(date) %>%
  summarize(total_checkouts_tjr = sum(Checkouts, na.rm = TRUE))

checkouts_per_month <- left_join(checkouts_per_month_ch, checkouts_per_month_tjr)

#Average checkouts per Month 
mean_ch <- mean(checkouts_per_month$total_checkouts_ch)
#mean_ch = 1517
mean_tjr <- mean(checkouts_per_month$total_checkouts_tjr)
#mean_tjr = 1651

romance <- spl_df %>% 
  filter(str_detect(Subjects, "Romance")) %>% 
  filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% 
  pull(Title)
# Title "Tomorrow, and Tomorrow, and Tomorrow"
thriller <- spl_df %>% 
  filter(str_detect(Subjects, "Thriller")) %>% 
  filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% 
  pull(Title)
# Title "Tomorrow, and Tomorrow, and Tomorrow"

mystery <- spl_df %>% 
  filter(str_detect(Subjects, "Mystery")) %>% 
  filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% 
  pull(Title)
# Title "Back to the garden"

df_book <- spl_df %>% 
  group_by(MaterialType) %>% 
  filter(MaterialType=="BOOK") %>% 
  filter(Checkouts == max(Checkouts, na.rm=TRUE)) 
# Prince Harry's Biography with 485 Checkouts 

df_audiobook <- spl_df %>% 
  group_by(MaterialType) %>% 
  filter(MaterialType=="AUDIOBOOK") %>% 
  filter(Checkouts == max(Checkouts, na.rm=TRUE)) 
# The House of Broken Angels by Luis Alberto Urrea with 851 Checkouts 

df_ebook <- spl_df %>% 
  group_by(MaterialType) %>% 
  filter(MaterialType=="EBOOK") %>% 
  filter(Checkouts == max(Checkouts, na.rm=TRUE)) 
# The House of Broken Angels by Luis Alberto Urrea with 2951 Checkouts 
