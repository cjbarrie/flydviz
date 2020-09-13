library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(Hmisc)
library(gtrendsR)

google_domain_langs <- read_excel("data/raw_google_sheet_trans.xlsx")
colnames(google_domain_langs) <- c("TLD", "extension", "country", "language_a", "language_b", 
                                   "lang_code_a", "lang_code_b", "trans_a", "trans_b", "trans_c", 
                                   "lang_code_c", "language_c")

#remove extraneous columns
cnt_langs <- google_domain_langs[,-c(1,2)]

#replace "#N/A" with NA
cnt_langs$trans_a[cnt_langs$trans_a=="#N/A"] <- NA
cnt_langs$trans_b[cnt_langs$trans_b=="#N/A"] <- NA
cnt_langs$trans_c[cnt_langs$trans_c=="#N/A"] <- NA

cnt_langs$trans_a[cnt_langs$trans_a=="#VALUE!"] <- NA
cnt_langs$trans_b[cnt_langs$trans_b=="#VALUE!"] <- NA
cnt_langs$trans_c[cnt_langs$trans_c=="#VALUE!"] <- NA

#get country names alongside country codes from gtrendsR package
counts <- countries
#get only country not sub_codes
counts$sub_code[counts$sub_code==""] <- NA
counts$sub_code <- as.character(counts$sub_code)
countsh <- counts[is.na(counts$sub_code),]
countsh <- countsh[,c(1,3)]
#convert to proper noun format from all caps
countsh$name <- str_to_title(countsh$name)
colnames(countsh) <- c("country_code", "country")

#get countries alongside translations in long format
cnt_langs_long <- gather(cnt_langs, trans_AtoC, translation, 
                         trans_a:trans_c, factor_key=TRUE)

#generate variable for trans. number and language
cnt_langs_long$lcode[cnt_langs_long$trans_AtoC=="trans_a"] <- cnt_langs_long$lang_code_a
cnt_langs_long$lcode[cnt_langs_long$trans_AtoC=="trans_b"] <- cnt_langs_long$lang_code_b
cnt_langs_long$lcode[cnt_langs_long$trans_AtoC=="trans_c"] <- cnt_langs_long$lang_code_c

cnt_langs_long$language[cnt_langs_long$trans_AtoC=="trans_a"] <- cnt_langs_long$language_a
cnt_langs_long$language[cnt_langs_long$trans_AtoC=="trans_b"] <- cnt_langs_long$language_b
cnt_langs_long$language[cnt_langs_long$trans_AtoC=="trans_c"] <- cnt_langs_long$language_c

#merge with Google country names and country codes
final <- merge(cnt_langs_long, countsh, by="country")

#get rid of NAs
final <- final[!is.na(final$translation),]

#note country code has to be stored as character for below to work
final$country_code <- as.character(final$country_code)

#replace عنصرية with العنصرية
final$translation[final$translation=="عنصرية"] <- "العنصرية"

#replace es-419 with es in lcode column
final$lcode[final$lcode=="es-419"] <- "es"
#correct lcode for Hebrew
final$lcode[final$lcode=="iw"] <- "he"
#correct lcode for Chinese
final$lcode[final$lcode=="zh-cn"] <- "zh-CN"

#store full country names and codes for later use
fullcountcodes <- final[,c(1,12)]
write.csv(fullcountcodes, "data/output/fullcountcodes.csv")

#get ccodes and translation and language for pytrends
pyccodes <- final$country_code
write.table(pyccodes, "data/output/country_codes.txt", row.names = F, col.names = F, quote=F)

pytrans <- final$translation
write.table(pytrans, "data/output/translations.txt", row.names = F, col.names = F, quote=F)

pylangs <- final$language
write.table(pylangs, "data/output/languages.txt", row.names = F, col.names = F)