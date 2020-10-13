library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(gtools)
library(zoo)
library(ggpubr)

load("data/output/rachits1.Rdata")

#average over hits values by week for country-keyword combinations
rachitsbd <- rachits %>%
  group_by(lword)

rachitsbd$rhits <- round(rachitsbd$hits)

rachitsbd$hitfac <- cut(rachitsbd$rhits, 
                        breaks=c(-Inf, 10, 20,30,40,50,60,70,80,90, Inf), 
                        labels=c("0-10","10-20","20-30", 
                                 "30-40", "40-50", "50-60",
                                 "60-70", "70-80", "80-90", "90-100"))

load("data/output/ccodes.Rdata")
#get country codes split at every nth element
ccodesn <- gsub('(.{1,200})(\\s|$)', '\\1\n', ccodes)

hmp <- ggplot(rachitsbd,aes(date,lword,fill=hitfac)) +
  geom_tile(colour="#faf8f3",size=0.6) +
  geom_segment(x=as.Date("2020-05-25"), xend=as.Date("2020-05-25"), y=-10, 
               yend=150, size=1, col="red4") +
  guides(fill=guide_legend(title=paste0("Search interest\n for ",'"',"racism",'"'),
                           nrow=1)) +
  labs(x="Month | Week\n ",y="Country: Language\n ",title="") + #add line break on y-axis so red vline lines up across top and bottom panels
  scale_x_date(date_labels="%b \n", date_breaks="month",
               date_minor_breaks = "week") +
  scale_fill_grey(start = 1, end=0) + 
  theme_tufte(base_size=10, base_family = "GillSans") +
  theme(legend.position="bottom",
        legend.direction="horizontal",
        legend.key.height=unit(.75,"cm"),
        legend.key.width=unit(.75,"cm"),
        axis.text.x=element_text(size=25),
        axis.text.y=element_text(vjust=0.2, size=15),
        axis.title=element_text(size=30, face="bold"),
        legend.text=element_text(size=30),
        legend.title=element_text(size=30),
        axis.ticks=element_line(size=0.4),
        plot.background = element_rect(fill = "#faf8f3", color = NA), 
        panel.background = element_rect(fill = "#faf8f3", color = NA), 
        legend.background = element_rect(fill = "#faf8f3", color = NA),
        plot.margin=margin(0.7,0.4,.1,0.2,"cm"),
        plot.caption = element_text(size=20, hjust=0,
                                    margin=margin(t=50,0,0,0))) +
  labs(caption = ccodesn) +
  annotate("rect", fill = "#b80000", alpha = 0, #creates blank space at top for red line label
           xmin= min(date1), xmax= max(date1), 
           ymin = 120, ymax = 124) +
  annotate("text", x = as.Date("2020-05-12"), 
           y = 122, label = "George Floyd killed \n May 25, 2020", 
           size=7, fontface="bold") +
  annotate("rect", fill = "#b80000", alpha = 0, #creates blank space at bottom for week labels
           xmin= min(date1), xmax= max(date1), 
           ymin = -1, ymax = 0)

#add weekly minor ticks with small hackaround
dates <- seq(as.Date("2020/01/01"), by = "week", length.out = 33)
dates

for(date in seq_along(dates)) {
  hmp <- hmp + geom_segment(x=as.Date(dates[date]), xend=as.Date(dates[date]), y=-1, 
                            yend=0, size=1, col="black")
}
hmp

cntincs <- rachitsbd %>%
  group_by(lword) %>%
  mutate(sdpcnt = sd(hits),
         mpcnt = mean(hits))
cntincs$mostd <- (cntincs$hits - cntincs$mpcnt)/(cntincs$sdpcnt)

#2020-06-30 is first date with negative global mean standardized search interest
getdates <- cntincs %>%
  group_by(date) %>%
  summarise(avgmstd = mean(mostd))
date1 <- getdates$date[getdates$avgmstd>0 & getdates$date>="2020-05-26" & 
                         getdates$date<"2020-06-30"]
date2 <- getdates$date[getdates$avgmstd>1 & getdates$date>="2020-05-26" & 
                         getdates$date<"2020-06-30"]
date3 <- getdates$date[getdates$avgmstd>2 & getdates$date>="2020-05-26" & 
                         getdates$date<"2020-06-30"]

sdp <- cntincs %>% 
  group_by(date) %>%
  mutate(avgmstd = mean(mostd)) %>%
  ggplot() +
  geom_line(aes(date, avgmstd), size=3) +
  geom_point(aes(date, mostd, group = lword),  color = "grey50", 
             show.legend = F, alpha = 0.3, size = 1) +
  scale_fill_identity() +
  scale_x_date(date_breaks="month") +
  geom_vline(aes(xintercept = as.integer(as.Date("2020-05-25"))),
             size=1, col="red4") +
  geom_segment(aes(x=as.Date("2020-01-05"),xend=as.Date("2020-08-18"), #2SDs
                   y=2,yend=2),size=.8, col="black", linetype="dashed") +
  geom_segment(aes(x=as.Date("2020-01-05"),xend=as.Date("2020-08-18"), #1SD
                   y=1,yend=1),size=.8, col="black", linetype="dashed") +
  geom_segment(aes(x=as.Date("2020-01-05"),xend=as.Date("2020-08-18"), #mean
                   y=0,yend=0),size=.8, col="black", linetype="dashed") +
  ylab("Standardized search interest \n ") +
  theme_tufte(base_size=10, base_family = "GillSans") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_text(size=30, face="bold"),
        legend.position = "right",
        plot.background = element_rect(fill = "#faf8f3", color = NA), 
        panel.background = element_rect(fill = "#faf8f3", color = NA), 
        legend.background = element_rect(fill = "#faf8f3", color = NA)) +
  annotate("rect",fill = "#b80000", alpha = .2,
           xmin = min(date1), xmax = max(date1), 
           ymin = -0.5, ymax = 10) +
  annotate("rect",fill = "#900000", alpha = .2,
           xmin = min(date2), xmax = max(date2), 
           ymin = -0.5, ymax = 10) +
  annotate("rect",fill = "#740000", alpha = .2,
           xmin = min(date3), xmax = max(date3), 
           ymin = -0.5, ymax = 10) + 
  annotate("text", x = as.Date("2020-06-18"), y = 4, label = "0-1SDs", size=7) +
  annotate("text", x = as.Date("2020-06-09"), y = 7, label = "1-2SDs", size=7) +
  annotate("text", x = as.Date("2020-06-03"), y = 10.5, label = ">2SDs", size=7) +
  annotate("text", x = as.Date("2020-01-01"), y = 0.3, label = "Mean", size=7, fontface="bold") +
  annotate("text", x = as.Date("2020-01-01"), y = 1.3, label = "1SD", size=7, fontface="bold") +
  annotate("text", x = as.Date("2020-01-01"), y = 2.3, label = "2SDs", size=7, fontface="bold")

gg <- ggarrange(sdp,hmp,
                ncol = 1, nrow = 2,  align = "v", 
                widths = c(1, 1), heights = c(1,5))

ggsave("data/output/plots/plot2gg_v2.png", width=800, height = 900, dpi=100, units="mm")
