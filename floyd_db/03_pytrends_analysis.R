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

hmp <- ggplot(rachitsbd,aes(date,lword,fill=hitfac)) +
  geom_tile(colour="#faf8f3",size=0.6) +
  geom_segment(x=as.Date("2020-05-25"), xend=as.Date("2020-05-25"), y=-10, 
               yend=150, size=1, col="red4") +
  guides(fill=guide_legend(title="Search interest",
                           nrow=1)) +
  labs(x="Date",y="Country: Language",title="") +
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
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"))

cntincs <- rachitsbd %>%
  group_by(lword) %>%
  mutate(sdpcnt = sd(hits),
         mpcnt = mean(hits))
cntincs$mostd <- (cntincs$hits - cntincs$mpcnt)/(cntincs$sdpcnt)

#2020-06-30 is first dat with negative global mean standardized search interest
test <- cntincs %>%
  group_by(date) %>%
  summarise(avgmstd = mean(mostd))
date1 <- test$date[test$avgmstd>0 & test$date>="2020-05-26" & test$date<"2020-06-30"]
date2 <- test$date[test$avgmstd>1 & test$date>="2020-05-26" & test$date<"2020-06-30"]
date3 <- test$date[test$avgmstd>2 & test$date>="2020-05-26" & test$date<"2020-06-30"]

sdp <- cntincs %>% 
  group_by(date) %>%
  mutate(avgmstd = mean(mostd)) %>%
  ggplot() +
  geom_line(aes(date, avgmstd), size=3) +
  geom_point(aes(date, mostd, group = lword),  color = "grey50", 
             show.legend = F, alpha = 0.3, size = 1) +
  scale_fill_identity() +
  geom_vline(aes(xintercept = as.integer(as.Date("2020-05-25"))),
             size=1, col="red4") +
  geom_segment(aes(x=as.Date("2020-01-05"),xend=as.Date("2020-08-18"), #2SDs
                   y=2,yend=2),size=.8, col="black", linetype="dashed") +
  geom_segment(aes(x=as.Date("2020-01-05"),xend=as.Date("2020-08-18"), #1SD
                   y=1,yend=1),size=.8, col="black", linetype="dashed") +
  geom_segment(aes(x=as.Date("2020-01-05"),xend=as.Date("2020-08-18"), #mean
                   y=0,yend=0),size=.8, col="black", linetype="dashed") +
  ylab("Mean standardized hits") +
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

ggsave("data/output/plots/plot2gg.png", width=800, height = 900, dpi=300, units="mm")
