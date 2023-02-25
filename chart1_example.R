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
checkouts_per_month_long <- checkouts_per_month %>%
  pivot_longer(cols=c('total_checkouts_ch', 'total_checkouts_tjr'),
                    names_to='author',
                    values_to='total_checkouts')

ch_tjr_graph <- ggplot(checkouts_per_month_long) +
  geom_line(aes(x = date, y=total_checkouts, group=author, color=author))+
  scale_y_continuous(limits = c(0, 3000)) +
  labs(title = "Total Checkouts for Colleen Hoover and Taylor Jenkins Reid in in 2022", 
       x = "Month", 
       y = "Total Checkouts")

  