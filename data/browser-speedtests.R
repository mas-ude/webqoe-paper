library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)
library(viridis)

setwd("~/git/webqoe-paper/data/")

df.ff <- read.csv(file = "Firefox/combinedDataBoxPlot.csv")
df.cr <- read.csv(file = "Chrome/combinedDataBoxPlot.csv")


df.ff$browser <- "Firefox 57"
df.cr$browser <- "Chrome 63"

df <- rbind(df.ff, df.cr)

# properly read the date/time
df$DateTime <- ymd_hms(df$DateTime, tz = "CET")

# set the category factor labels
df$Category <- factor(df$Category, labels = c("video", "text", "shop", "search engine", "forum"))

# extract domain name from url as shorthand
# regex: look-before with http(s):// at the start of a line,
# then match anything until the first '/' or EOL
df$domain <- str_extract(df$URL, "(?<=^http(s?)://).+?(?=(/|$))")

# remove and reformat stuff
df <- df %>% 
  select(-Testcounter, -Browserversion, -NAME) %>%
  rename(mtime = DateTime, category = Category)

# gather all measured value into a key-value format
df <- df %>%
  gather(key = metric, value = measurement, SI, PSI)
  

ggplot(df, aes(x = domain, y = measurement, color = metric)) + 
  geom_boxplot() + 
  coord_flip() +
  facet_wrap( ~ category, scales = "free_y", nrow = 5, shrink = TRUE) +
  scale_color_viridis(discrete = TRUE, end = 0.9) + theme_bw()
ggsave("categorized-SI-PSI-comparison.pdf")


# plot SI/PSI for every combination
ggplot(df, aes(x = domain, y = measurement, color = metric)) + 
  geom_boxplot() + 
  coord_flip() +
  facet_grid(category ~ browser, scales = "free_y", shrink = TRUE) +
  scale_color_viridis(discrete = TRUE, end = 0.9) + theme_bw()
ggsave("categorized-SI-PSI-browser-comparison.pdf")

ggplot(df, aes(x = domain, y = measurement, color = browser)) + 
  geom_boxplot() + 
  coord_flip() +
  facet_grid(category ~ metric, scales = "free_y", shrink = TRUE) +
  scale_color_viridis(discrete = TRUE, end = 0.9) + theme_bw()
ggsave("categorized-browser-SI-PSI-comparison.pdf")


ggplot(df, aes(x = domain, y = measurement, color = metric)) + 
  geom_violin() + 
  coord_flip() +
  facet_grid(category ~ browser, scales = "free_y", shrink = TRUE) +
  scale_color_viridis(discrete = TRUE, end = 0.9) + theme_bw()
ggsave("categorized-SI-PSI-browser-comparison.pdf")

  
## check for temporal correlations and variations
ggplot(filter(df, browser == "Firefox 57"), aes(x = mtime, y = measurement, color = metric)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap( ~ URL, scales = "free_x", shrink = TRUE)
ggsave("firefox-timeseries.pdf")

ggplot(filter(df, browser == "Chrome 63"), aes(x = mtime, y = measurement, color = metric)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap( ~ URL, scales = "free_x", shrink = TRUE)
ggsave("chrome-timeseries.pdf")
