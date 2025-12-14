

rm(list=ls())

library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(ggrepel)
library(lubridate)
library(scales)
library(ggpubr)
library(patchwork)
library(ggplot2)


source("D:/all_project/Italy_project/0_data_process/input/data_uesd.R")
###Vaccine data processing
startDate = ymd("2020-12-27")
endDate = ymd("2022-03-05")
index_date = ymd("2020-01-31")

vaccine_data_raw <- read.csv("C:/Users/USER01/Documents/Zhao-personal/important_literature/vacinee_imunity/The effect of COVID-19 vaccination in Italy and/covid19-opendata-vaccini-master/dati/somministrazioni-vaccini-latest.csv")

#names(vaccine_data_raw)

vaccine_data_Italy_doses <- vaccine_data_raw %>% 
  mutate(data = as.Date(data)) %>% 
  filter(forn != "Novavax" & forn !="Sanofi") %>% 
  mutate(vacc_type = if_else(forn == "Janssen" | forn == "Vaxzevria (AstraZeneca)","Viral_vector_vacc" , "mRNA_vacc")) %>% 
  group_by(data) %>% 
  summarise(d1 = sum(d1), d2 = sum(d2),
            dpi = sum(dpi),db1 = sum(db1),
            db2 = sum(db2),db3 = sum(db3))

vaccine_data_Italy_doses_tot <- rbind(data.frame(data = seq(as.Date("2020-02-21"), as.Date("2020-12-26"), by = 1),
                                                 d1 = rep(0, length(seq(as.Date("2020-02-21"), as.Date("2020-12-26"), by = 1))),
                                                 d2 = rep(0, length(seq(as.Date("2020-02-21"), as.Date("2020-12-26"), by = 1))),
                                                 dpi = rep(0, length(seq(as.Date("2020-02-21"), as.Date("2020-12-26"), by = 1))),
                                                 db1 = rep(0, length(seq(as.Date("2020-02-21"), as.Date("2020-12-26"), by = 1))),
                                                 db2 = rep(0, length(seq(as.Date("2020-02-21"), as.Date("2020-12-26"), by = 1))),
                                                 db3 = rep(NA, length(seq(as.Date("2020-02-21"), as.Date("2020-12-26"), by = 1))) ), vaccine_data_Italy_doses)
####A vaccine coverage

startDate = ymd("2020-02-15")
endDate = ymd("2022-04-01")
figure1A <- ggplot(vaccine_data_Italy_doses_tot, aes(x = data)) + 
  geom_line( aes( y = 100*cumsum(d1)/59500579 , color = "d1"), linewidth = 0.7 ) + 
  geom_line( aes( y = 100*cumsum(d2)/59500579 , color = "d2") , linewidth = 0.7) + 
  geom_line( aes( y = 100*cumsum(db1)/59500579, color = "db1") , linewidth = 0.7) +
  scale_x_date(date_breaks = "2 month",expand = c(0.02, 0.02), limits=c(startDate,endDate)) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,20)) +   theme_bw() + ylab("Cumulative vaccine\ncoverage (%)") + xlab("")+
  xlab("Date")+
  geom_vline(xintercept = ymd("2020-12-15"), lty=5,colour="grey", linewidth = 0.8)+
  geom_vline(xintercept = ymd("2021-05-15"), lty=5,colour="grey", linewidth = 0.8)+
  geom_vline(xintercept = ymd("2021-12-02"), lty=5,colour="grey", linewidth = 0.8)+
  annotate("text", x=ymd("2020-07-01"), y=94, label= "Non-VOCs",col="orchid4", size=4.5)+
  annotate("text", x=ymd("2021-03-01"), y=94, label= "Alpha",col="orchid4", size=4.5)+
  annotate("text", x=ymd("2021-08-20"), y=94, label= "Delta",col="orchid4", size=4.5)+
  annotate("text", x=ymd("2022-02-01"), y=94, label= "Omicron",col="orchid4", size=4.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5,size=14),
        axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 17),axis.title.y = element_text(size = 17) ) +
  theme(legend.text=element_text(size=10), legend.position = c(0.2,0.4), 
        legend.key.size = unit(0.8, 'cm'),
        legend.background = element_rect(fill = "transparent"),legend.spacing.y = unit(0.1, 'pt'),
        legend.title=element_text(size=11))+
  theme( panel.grid = element_blank()) +
  scale_colour_manual(name = "Dose", values = c("d1" = "#3C5487", "d2" =  "#00A088","db1"="#E64B34" ),
                      labels = c('1 dose', '2 doses', '3 doses') )

####B mobility index


mob_data <- data.frame(date = merged_mob$date, mobility_index = merged_mob_ave)

figure1B <- ggplot(mob_data, aes(x = date)) + 
  geom_line( aes( y = mobility_index*100), color = "#6A8B21", linewidth = 0.7 ) + 
  scale_x_date(date_breaks = "2 month",expand = c(0.02, 0.02), limits=c(startDate,endDate)) +
  scale_y_continuous(limits=c(0,200), breaks = seq(0,200,40)) +   
  geom_hline(yintercept = 100, lty=5, colour="grey", linewidth = 0.6)+
  theme_bw() + ylab("Daily mobility\nindex (%)") + xlab("")+
  geom_vline(xintercept = ymd("2020-12-15"), lty=5,colour="grey", linewidth = 0.8)+
  geom_vline(xintercept = ymd("2021-05-15"), lty=5,colour="grey", linewidth = 0.8)+
  geom_vline(xintercept = ymd("2021-12-02"), lty=5,colour="grey", linewidth = 0.8)+
  annotate("text", x=ymd("2020-07-01"), y=188, label= "Non-VOCs",col="orchid4", size=4.5)+
  annotate("text", x=ymd("2021-03-01"), y=188, label= "Alpha",col="orchid4", size=4.5)+
  annotate("text", x=ymd("2021-08-20"), y=188, label= "Delta",col="orchid4", size=4.5)+
  annotate("text", x=ymd("2022-02-01"), y=188, label= "Omicron",col="orchid4", size=4.5)+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 17) ) +
  theme(legend.text=element_text(size=13), legend.position = c(0.1,0.8),
        plot.title=element_text(size=18, face="bold",hjust = 0.5))+
  theme( panel.grid = element_blank())


Italy_reported_cases$new_cases[120] <- mean(Italy_reported_cases$new_cases[c(117:119,121:123)])
Italy_reported_cases$Italy_cu_sum_cases <- cumsum(Italy_reported_cases$new_cases)
#Italy_reported_cases
#write.csv(Italy_reported_cases, "D:/all_project/Italy_project/manuscript/Italy_reported_cases.csv")
####C daily reported cases
Italy_reported_cases$log_Italy_cu_sum_cases <- log10(Italy_reported_cases$Italy_cu_sum_cases)

figure1C <- ggplot(Italy_reported_cases) +
  #geom_point(aes(x = date, y = new_cases), size = 3, alpha = 0.5, color = "#6666CC99") +
  geom_line(aes(x = date, y = new_cases), linewidth = 0.4, color="#E64B35FF")+
  scale_x_date(date_breaks = "2 month",expand = c(0.02, 0.02), limits=c(startDate,endDate)) + 
  scale_y_continuous(transform = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x) (c( 10, 100, 1000,1e4,1e5,1e6, 1e7, 1e8)) ,
                     labels = trans_format("log10", math_format(10^.x) ), limits=c(10, 1e8) ,
                     sec.axis = sec_axis(~.,
                                         name = 'Cumulative Cases\n(log10 scale)', 
                                         breaks = trans_breaks("log10", function(x) 10^x)(c(10, 100, 1000, 1e4, 1e5, 1e6, 1e7, 1e8)),
                                         labels = trans_format("log10", math_format(10^.x)) )
  ) + 
  geom_line(aes(x = date, y = Italy_cu_sum_cases ),linewidth = 0.7, color = "#E8BF80") +
  theme_bw() +
  geom_vline(xintercept = ymd("2020-12-15"), lty=5,colour="grey", linewidth = 0.8)+
  geom_vline(xintercept = ymd("2021-05-15"), lty=5,colour="grey", linewidth = 0.8)+
  geom_vline(xintercept = ymd("2021-12-02"), lty=5,colour="grey", linewidth = 0.8)+
  labs(x = "", y = "Daily incidence\n(log10 scale)",width=1)+
  annotate("text", x=ymd("2020-07-01"), y=10^7.6, label= "Non-VOCs",col="orchid4", size=4.5)+
  annotate("text", x=ymd("2021-03-01"), y=10^7.6, label= "Alpha",col="orchid4", size=4.5)+
  annotate("text", x=ymd("2021-08-20"), y=10^7.6, label= "Delta",col="orchid4", size=4.5)+
  annotate("text", x=ymd("2022-02-01"), y=10^7.6, label= "Omicron",col="orchid4", size=4.5)+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, color="#E64B35FF")) +
  theme(axis.title.y = element_text(size = 17, color="#E64B35FF"),axis.text.y.right = element_text(color = "#E8BF80"),
        axis.title.y.right = element_text(size = 17,color = "#E8BF80")) +
  theme(legend.text=element_text(size=13), legend.position =c(0.2,0.3),
        plot.title=element_text(size=18, face="bold",hjust = 0.5))+
  theme( panel.grid = element_blank() ) 

# break_vec <- c(10, 100, 1000, 1e4, 1e5, 1e6, 1e7, 1e8)
# 
# figure1C <- ggplot(Italy_reported_cases) +
#   geom_line(aes(x = date, y = new_cases), size = 0.4, color = "#E64B35FF") +
#   geom_line(aes(x = date, y = Italy_cu_sum_cases), size = 0.7, color = "#E8BF80") +
#   scale_x_date(date_breaks = "2 months",
#                expand = c(0.02, 0.02),
#                limits = c(startDate, endDate)) +
#   scale_y_continuous(
#     trans  = log10_trans(),
#     breaks = trans_breaks("log10", function(x) 10^x)(break_vec),
#     labels = trans_format("log10", math_format(10^.x)),
#     limits = c(10, 1e8),
#     sec.axis = sec_axis(~ .,
#                         name   = "Cumulative Cases\n(log10 scale)",
#                         breaks = trans_breaks("log10", function(x) 10^x)(break_vec),
#                         labels = trans_format("log10", math_format(10^.x))
#     )
#   ) +
#   theme_bw() +
#   geom_vline(xintercept = ymd("2020-12-15"), lty = 5, colour = "grey", size = 0.8) +
#   geom_vline(xintercept = ymd("2021-05-15"), lty = 5, colour = "grey", size = 0.8) +
#   geom_vline(xintercept = ymd("2021-12-02"), lty = 5, colour = "grey", size = 0.8) +
#   labs(x = "", y = "Daily incidence\n(log10 scale)") +
#   annotate("text", x = ymd("2020-07-01"), y = 10^7.6, label = "Non-VOCs", col = "orchid4", size = 4.5) +
#   annotate("text", x = ymd("2021-03-01"), y = 10^7.6, label = "Alpha",    col = "orchid4", size = 4.5) +
#   annotate("text", x = ymd("2021-08-20"), y = 10^7.6, label = "Delta",    col = "orchid4", size = 4.5) +
#   annotate("text", x = ymd("2022-02-01"), y = 10^7.6, label = "Omicron",  col = "orchid4", size = 4.5) +
#   theme(
#     axis.text.x = element_blank(),
#     axis.text.y = element_text(size = 12, color = "#E64B35FF"),
#     axis.title.y = element_text(size = 17, color = "#E64B35FF"),
#     axis.text.y.right = element_text(color = "#E8BF80"),
#     axis.title.y.right = element_text(size = 17, color = "#E8BF80"),
#     legend.text = element_text(size = 13),
#     legend.position = c(0.2, 0.3),
#     plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
#     panel.grid = element_blank()
#   )

# 
# ####D vaccine type
# library(RColorBrewer)
# library(dplyr)
# library(graphics)
# 
# vaccine_total_num_by_each_type <- vaccine_data_raw %>%
#   filter(data <= as.Date("2022-03-05")) %>% 
#   group_by(forn) %>%
#   summarise(d1 = sum(d1), d2 = sum(d2),
#             dpi = sum(dpi),db1 = sum(db1),
#             db2 = sum(db2),db3 = sum(db3)) %>% 
#   mutate(total_num = d1 + d2 + dpi + db1 + db2 + db3) 
# 
# # Calculate percentages
# pct <- round(100*vaccine_total_num_by_each_type$total_num/sum(vaccine_total_num_by_each_type$total_num),2)
# # Draw oie chart
# par(mar =c(0,0,2,2))
# myPalette <- brewer.pal(7, "Pastel1") 
# vaccine_total_num_by_each_type$forn[6] <- "Vaxzevria"
# #vaccine_total_num_by_each_type <-arrange(vaccine_total_num_by_each_type,desc(total_num))
# pie(vaccine_total_num_by_each_type$total_num,
#     labels = paste(vaccine_total_num_by_each_type$forn," (" , sep = "", pct, "%",")" ), clockwise = T,init.angle=90,
#     col = rev(brewer.pal(length(pct), "Oranges")), cex = 1.05)


layout <- 
  "A
B
C"

figure_2A_2B_2C <- figure1C+figure1B+figure1A+
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 21, hjust = -0.1, vjust = 0.1,face = "bold"),plot.tag.position = c(0, 0.99))

ggsave(figure_2A_2B_2C, file='D:/all_project/Italy_project/manuscript/iScience/Figure2_updated/figure_2A_2B_2C.pdf', width=8, height=11.5)
