library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)

files <- list.files(path=file.path("data/output/exports/"),recursive=T,include.dirs=T)
files <- paste("data/output/exports/", files, sep="")

# #test loop sequence
# filename = files[[1]]
# df <- read.csv(filename, header=FALSE)
# testcol <- df[1,1]

#create empty dataframe
df_total = data.frame()

for (i in seq_along(files)){
  filename = files[[i]]
  df <- read.csv(filename, header=FALSE)
  testcol <- df[1,1]
  if(is.na(testcol)){
    dates <- seq(as.Date("2020-01-01"), as.Date("2020-08-18"), "days")
    df <- data.frame(matrix(ncol=0, nrow=length(dates)))
    df$date <-  dates
    df$hits <- NA
    df$translation <- NA
    df$language <- NA
    df$filename <- filename
  } else if(!is.null(testcol)){
    df <- df[,-3]
    translation <- df[1,2]
    df <- df[-1,]
    colnames(df) <- c("date", "hits", "language")
    df$translation <- translation
    df$filename <- filename
  }
  df_total <- rbind(df_total,df)
}
rachits <- df_total

#get two character geo code
rachits$geo <- substr(df_total$filename, 21, 22)
#format column names
colnames(rachits) <- c("date", "hits","language", "translation", "filename", "geo")
#create country-keyword identifier
rachits$lword <- paste(rachits$geo, rachits$language, sep="-")

#remove rows where translation is NA; i.e. no data for country-keyword pairing
rachits$translation[is.na(rachits$translation)] <- "999"
rachits <- subset(rachits, translation != "999")
#change hits to integer from character
rachits$hits <- as.integer(rachits$hits)
#format date column and generate week column
rachits$date <- as.Date(rachits$date)
rachits$week <- week(rachits$date)

#get cumulative sum of zeroes (0s) for every country-keyword pair
# subset to those w/ >= 212 days of zeros; i.e. at least twenty days with data
rachits$zero <- ifelse(rachits$hits==0,1,0)
rachitscnts <- rachits %>%
  group_by(lword) %>%
  summarise(cnts = sum(zero)) %>%
  filter(cnts<212)

zeroes <- rachitscnts$lword
#filter out country-keyword combinations with 0 hits
rachits <- subset(rachits, lword %in% zeroes)

#reformat language column
rachits$language[rachits$language=="\"ar - Arabic\""] <- "Arabic"
rachits$language[rachits$language=="\"pt-PT - Portuguese\""  ] <- "Portuguese"
rachits$language[rachits$language=="\"es-419 - Español (Latinoamérica)\""] <- "Spanish"
rachits$language[rachits$language=="\"de - German\""] <- "German"
rachits$language[rachits$language=="\"en - English\""] <- "English"
rachits$language[rachits$language=="\"fr - French\"" ] <- "French"
rachits$language[rachits$language=="\"nl - Dutch\""] <- "Dutch"
rachits$language[rachits$language=="\"bg - Bulgarian\""] <- "Bulgarian"
rachits$language[rachits$language=="\"pt-BR - Portuguese (Brasil)\""] <- "Portuguese"
rachits$language[rachits$language=="\"ru - Russian\""] <- "Russian"
rachits$language[rachits$language=="\"cs - Czech\""] <- "Czech"
rachits$language[rachits$language=="\"da - Danish\""] <- "Danish"
rachits$language[rachits$language=="\"ca - Catalan; Valencian\""] <- "Catalan"
rachits$language[rachits$language=="\"es - Spanish; Castilian\""] <- "Spanish"
rachits$language[rachits$language=="\"fi - Finnish\""] <- "Finnish"
rachits$language[rachits$language== "\"sv - Swedish\""] <- "Swedish"
rachits$language[rachits$language=="\"el - Greek, Modern\""] <- "Greek"
rachits$language[rachits$language=="\"hr - Croatian\""] <- "Croatian"
rachits$language[rachits$language=="\"hu - Hungarian\""] <- "Hungarian"
rachits$language[rachits$language=="\"jw - Basa Jawa\""] <- "Javanese"
rachits$language[rachits$language=="\"id - Indonesian\""] <- "Indonesian"
rachits$language[rachits$language=="\"iw - Hebrew\""] <- "Hebrew"
rachits$language[rachits$language=="\"hi - Hindi\""] <- "Hindi"
rachits$language[rachits$language=="\"ja - Japanese\""] <- "Japanese"
rachits$language[rachits$language=="\"ky - Kyrgyz\""] <- "Kyrgyzs"
rachits$language[rachits$language=="\"lv - Latvian\""] <- "Latvian"
rachits$language[rachits$language=="\"ms - Malay\""] <- "Malay"
rachits$language[rachits$language=="\"no - Norwegian\""] <- "Norwegian"
rachits$language[rachits$language=="\"mi - Māori\""] <- "Maori"
rachits$language[rachits$language=="\"pl - Polish\""] <- "Polish"
rachits$language[rachits$language=="\"ro - Romanian, Moldavian, Moldovan\""] <- "Romanian"
rachits$language[rachits$language=="\"sl - Slovene\""] <- "Slovene"
rachits$language[rachits$language=="\"sk - Slovak\""] <- "Slovak"
rachits$language[rachits$language=="\"tr - Turkish\""] <- "Turkish"
rachits$language[rachits$language=="\"uk - Ukrainian\""] <- "Ukrainian"
rachits$language[rachits$language=="\"uk - Ukrainian\""] <- "Ukrainian"
rachits$language[rachits$language=="\"it - Italian\""] <- "Italian"

rachits$lword <- paste(rachits$geo, rachits$language, sep=": ")

#Remove Ukraine Russian combination as returns same data as Ukraine Ukrainian
rachits <- subset(rachits, lword!="UA: Russian")

save(rachits, file = "data/output/rachits1.RData")

#get country names for Latex

fullcountcodes <- read.csv("data/output/fullcountcodes.csv")
cnts <- unique(rachits$geo)
fullcountcodes <- fullcountcodes[,-1]
colnames(fullcountcodes) <- c("country", "geo")
final_cnts <- subset(fullcountcodes, geo %in% cnts)

final_cnts <- final_cnts[!duplicated(final_cnts$country), ]
final_cnts$country_code <- paste(final_cnts$geo, final_cnts$country, sep= ": ")
country_codes <- final_cnts$country_code
country_codes <- sort(country_codes)

ccodes <- as.character()
for (i in 1:length(country_codes)) {
  cntp <- country_codes[i]
  sep <- ifelse(is_empty(ccodes), "","; ")
  ccodes <- paste(ccodes, cntp, sep=sep)
}
#file for latex inclusion
write.table(ccodes, "data/output/ccode_countries.txt", col.names = F, row.names = F,
            quote=F)
#file for viz. inclusion
save(ccodes, file = "data/output/ccodes.RData")
