

# 假设 sample_table 是截图一的数据框，mutation_table 是截图二的数据框
# mutation_table 有列 mutation 和 delta_bind
rm(list=ls())
# 拆分 label_mut 并累加 delta_bind
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(slider)
library(dplyr)
library(lubridate)
library(ggtrendline)
library(data.table)
library(patchwork)

sample_table <- read.csv("D:/all_project/lineage_accession_ID/count_variant.csv")
RBD_deep_scan_data_updated <- read.table('D:/all_project/Italy_project/manuscript/iScience/Delta_binding_cal/final_variant_scores_omicron.txt',sep = ",",header = T)

mut_tab_wild_v1 <- RBD_deep_scan_data_updated %>% filter(target == "Wuhan-Hu-1_v1")
mut_tab_wild_v2 <- RBD_deep_scan_data_updated %>% filter(target == "Wuhan-Hu-1_v2")
mut_tab_Alpha <- RBD_deep_scan_data_updated %>% filter(target == "Alpha")
mut_tab_Beta <- RBD_deep_scan_data_updated %>% filter(target == "Beta")
mut_tab_Delta <- RBD_deep_scan_data_updated %>% filter(target == "Delta")
mut_tab_Eta <- RBD_deep_scan_data_updated %>% filter(target == "Eta")
mut_tab_Omicron_BA1 <- RBD_deep_scan_data_updated %>% filter(target == "Omicron_BA1")
mut_tab_Omicron_BA2 <- RBD_deep_scan_data_updated %>% filter(target == "Omicron_BA2")
# 假设 mutation_table 有列 mutation, delta_bind
# 确保 mutation_table$mutation 格式如 L452R, T478K, G482S 等
# 1. 拆 label_mut 为突变向量
sample_table$label_mut_split <- str_split(sample_table$label_mut, "-", simplify = FALSE)

# 2. 定义函数计算 sum_delta_bind
sum_delta_bind <- function(mut_vec, mutation_table) {
  # 找到 mutation_table 里与 mut_vec 匹配的 delta_bind，忽略NA
  delta_values <- mutation_table$delta_bind[match(mut_vec, mutation_table$mutation)]
  sum(delta_values, na.rm = TRUE)
}

# 3. 应用到每一行，生成新列 delta_bind_sum
sample_table$bind_Alpha_bg <- map_dbl(sample_table$label_mut_split, sum_delta_bind, mut_tab_Alpha)
sample_table$bind_Delta_bg <- map_dbl(sample_table$label_mut_split, sum_delta_bind, mut_tab_Delta)
sample_table$bind_BA1_bg <- map_dbl(sample_table$label_mut_split, sum_delta_bind, mut_tab_Omicron_BA1)
sample_table$bind_BA2_bg <- map_dbl(sample_table$label_mut_split, sum_delta_bind, mut_tab_Omicron_BA2)

Alpha <- sample_table %>% filter(VOCs == "Alpha") %>% rename(VOC = VOCs)
Beta <- sample_table %>% filter(VOCs == "Beta") %>% rename(VOC = VOCs)
Delta <- sample_table %>% filter(VOCs == "Delta") %>% rename(VOC = VOCs)
Gamma <- sample_table %>% filter(VOCs == "Gamma") %>% rename(VOC = VOCs)
Omicron <- sample_table %>% filter(VOCs == "Omicron") %>% rename(VOC = VOCs)



library(purrr)
file_list <- list.files(path = "D:/all_project/Italy_project/manuscript/figures/updated_figure2/Omicron_accession_ID",
                        pattern = "*.csv")  # 获取当前目录下所有CSV文件的文件名
path_list <- paste0("D:/all_project/Italy_project/manuscript/figures/updated_figure2/Omicron_accession_ID/",file_list)
data_list <- map(path_list, read.csv,header = F)   # 使用map()函数逐个读取CSV文件并存储为数据框列表

# 使用 sub 函数去掉 .csv 后缀
filenames_no_csv <- sub("\\.csv$", "", file_list)
names(data_list) <- filenames_no_csv

# 使用 lapply 将每个data.frame中的name添加为一列
my_list_with_name <- lapply(names(data_list), function(name) {
  df <- data_list[[name]]
  df$name <- name  # 添加name列
  names(df)[1] <- "Accession_ID"
  return(df)
})

# 将list转换为单个data.frame
omicron_id_lineage <- do.call(rbind, my_list_with_name)
#table(omicron_id_lineage$name)
# omicron_id_lineage <- omicron_id_lineage %>%
#   mutate(VOCs = paste0("Omicron(", gsub("^(BA\\.[12345])(\\..*)?$", "\\1*", omicron_id_lineage$name), ")" ) ) %>%
#   select(-name)

omicron_id_lineage <- omicron_id_lineage %>%
  mutate(VOCs = name ) %>%
  select(-name)

Omicron1 <- Omicron %>%
  left_join(omicron_id_lineage, by = "Accession_ID")  %>%
  select(-VOC) %>%
  rename(VOC = VOCs) %>% #drop_na(VOCs) %>%
  #mutate(VOC = gsub("Omicron\\(BA\\.[345]\\*[,]?\\*?\\)", "Omicron(BA.3-5*)", VOC) ) %>%
  mutate(VOC = replace_na(VOC, "Others"))



data.plot <- rbind(Alpha, Beta, Delta, Gamma, Omicron1)
data.plot$date <- as.Date(data.plot$date)

data.plot1_1 <- data.plot %>%
  filter(binding_score > -4 & escape_score < 1) %>%
  filter(VOC != "Beta" & VOC !="Gamma")

# data.plot1_1 %>%
#   filter(!VOC %in% c("Alpha", "Delta", "Others"))%>%
#   count(VOC) %>%
#   mutate(prop = n / sum(n)) %>%
#   arrange(desc(prop))
#table(data.plot1$VOC)
#“Alpha”, “Delta” ,“BA.2”,“BA.1.1”,“BA.1.17.2”,“BA.2.9”,“BA.1”,“BA.2.3.15”,“BA.1.1.1”,“BA.1.17”,”BA.1.15“
voc_list <- c("Alpha", "Delta","B.1.1.529", "BA.2", "BA.1.1", "BA.1.17.2", "BA.2.9", 
              "BA.1", "BA.2.3.15", "BA.1.1.1", "BA.1.17", "BA.1.15")
subset_data <- data.plot1_1 %>%
  filter(VOC %in% voc_list)

data.plot1.1 <- rbind(Alpha, Beta, Delta, Gamma, Omicron)
data.plot1.1$date <- as.Date(data.plot1.1$date)

data.plot1.1 <- data.plot1.1 %>%
  filter(binding_score > -4 & escape_score < 1) %>%
  filter(VOC != "Beta" & VOC !="Gamma")
#calculate weekly mean values of each voc binding and immune escape
data.plot1.1$week <- floor_date(data.plot1.1$date, "week")
data.plot1.1 %>% group_by(VOC) %>% summarise(mean_bind = mean(binding_score,na.rm = T), sd = sd(binding_score,na.rm = T))
data.plot1_1 %>% group_by(VOC) %>% summarise(mean_bind = mean(binding_score,na.rm = T), sd = sd(binding_score,na.rm = T))

week_avg <- data.plot1.1 %>%
  group_by(week, VOC) %>%
  summarise(binding_score_week_avg = mean(binding_score, na.rm = TRUE),
            escape_score_week_avg = mean(escape_score, na.rm = TRUE))

week_avg_voc_Alpha <- week_avg %>% filter(VOC == 'Alpha' ) %>%
  filter(week >= ymd("2020-11-01") & week <= ymd("2021-09-01"))

week_avg_voc_Delta <- week_avg %>% filter(VOC == 'Delta' ) %>%
  filter(week >= ymd("2021-01-01") & week <= ymd("2022-03-01"))

week_avg_voc_Omicorn <- week_avg %>% filter( VOC == "Omicron" )
week_avg_voc_total <- rbind(week_avg_voc_Alpha, week_avg_voc_Delta, week_avg_voc_Omicorn)


startDate = ymd("2020-02-15")
endDate = ymd("2022-04-01")

data.plot1_1 <- data.plot1_1 %>% filter(VOC != "Others")

# c( "Delta","B.1.1.529", "BA.1.1", "BA.1.17.2", 
#    "BA.1",  "BA.1.1.1", "BA.1.17", "BA.1.15")

data.label_mutipl_bg <- subset_data  %>%
  mutate(
    use_bind = case_when(
      VOC %in% c("Alpha") ~ bind_Alpha_bg + 9.95503 - 8.77161,
      VOC %in% c("Delta") ~ bind_Delta_bg + 9.03525 - 8.77161,
      VOC %in% c( "B.1.1.529", "BA.1","BA.1.1","BA.1.1.1", "BA.1.17", "BA.1.15" ,"BA.1.17.2") ~ bind_BA1_bg + 9.06247 - 8.77161,
      VOC %in% c( "BA.2", "BA.2.9", "BA.2.3.15") ~ bind_BA2_bg + 9.09572- 8.77161,
      TRUE ~ NA_real_  # 其他未定义情况，赋NA
    )
  )


shape_values <- c(
  'Alpha' = 0, 'Delta'=1, 'B.1.1.529'=2,'BA.1'=3, 'BA.1.1'=4, 
  'BA.1.1.1'=5,'BA.1.15'=5,'BA.1.17'=5,'BA.1.17.2'=5,   
  'BA.2'=6, 'BA.2.9'=7, 'BA.2.3.15'=7
)

# 先去除NA行
plotdata <- data.label_mutipl_bg %>% filter(!is.na(use_bind))
voc_levels <- c(
  'Alpha', 'Delta', 'B.1.1.529','BA.1', 'BA.1.1', 
  'BA.1.1.1','BA.1.15','BA.1.17','BA.1.17.2',   
  'BA.2', 'BA.2.9', 'BA.2.3.15'
)

plotdata$VOC <- factor(plotdata$VOC, levels = voc_levels)

plotdata <- plotdata %>%
  mutate(
    VOC_group = case_when(
      VOC == "Alpha" ~ "Alpha",
      VOC == "Delta" ~ "Delta",
      VOC == "BA.1" ~ "BA.1",
      VOC == "BA.1.1" ~ "BA.1.1",
      VOC %in% c("BA.1.1.1", "BA.1.15", "BA.1.17", "BA.1.17.2") ~ "BA.1# (Except BA.1 and BA.1.1)",
      VOC == "BA.2" ~ "BA.2",
      VOC %in% c("BA.2.9", "BA.2.3.15") ~ "BA.2# (Except BA.2)",
      VOC == "B.1.1.529" ~ "B.1.1.529",
      TRUE ~ "Other"
    )
  ) %>% filter(VOC != "B.1.1.529")

voc_group_levels <- c("Alpha", "Delta","B.1.1.529", "BA.1", "BA.1.1", 
                      "BA.1# (Except BA.1 and BA.1.1)", 
                      "BA.2", "BA.2# (Except BA.2)",  "Other")
plotdata$VOC_group <- factor(plotdata$VOC_group, levels = voc_group_levels)

group_color_values <- c(
  "Alpha" = "#F8766D",
  "Delta" = "#00BA38",
  "BA.1" = "#64B5F6FF",
  "BA.1.1" = "#64B5F6FF",
  "BA.1# (Except BA.1 and BA.1.1)" = "#64B5F6FF",
  "BA.2" = "#1565C0FF",
  "BA.2# (Except BA.2)" = "#1565C0FF",
  "B.1.1.529" = "black",
  "Other" = "grey"
)
group_shape_values <- c(
  "Alpha" = 0, "Delta" = 1, "BA.1" = 3, "BA.1.1" = 4, 
  "BA.1# (Except BA.1 and BA.1.1)" = 5, "BA.2" = 6, 
  "BA.2# (Except BA.2)" = 7, "B.1.1.529" = 2, "Other" = 8
)

labels_voc <- c(
  "Alpha (0.236)",
  "Delta (0.0366)",
  "BA.1 (-2.17)",
  "BA.1.1 (0.136)",
  "BA.1# (Except BA.1 and BA.1.1) (0.0212)",
  "BA.2 (-0.831)",
  "BA.2# (Except BA.2) (-0.838)"
)

figure3B <- ggplot(plotdata) +
  geom_point(aes(x = date, y = use_bind, shape = VOC_group, color = VOC_group), size = 2, alpha = 0.3) +
  scale_shape_manual(values = group_shape_values, labels = labels_voc) +
  scale_color_manual(values = group_color_values, labels = labels_voc) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 months",
               limits = c(startDate, endDate), expand = c(0.024, 0.018)) +
  scale_y_continuous(limits = c(-4, 1), breaks = seq(1, -4)) +
  theme_bw() + 
  ylab("ACE2 Addative\n binding score") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 16),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 11), 
        legend.position = c(0.2, 0.35),
        legend.background = element_rect(fill = NA),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        panel.grid = element_blank()
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 2, alpha = 1), title = "VOC"), 
    shape = guide_legend(override.aes = list(size = 2), title = "VOC")
  )

plotdata[68447, 4]
plotdata[68448, 4]
plotdata[56723, 4]
#ggsave(figure3B, file='D:/all_project/Italy_project/manuscript/iScience/Figure3_updated/figure3B.png', width=8, height=6.5, dpi = 400)

########################################Figure 3C #######################################
#rm(list=ls())

##lodad data

Alpha <- read.csv("D:/all_project/Italy_project/manuscript/iScience/immune_escape_cal/Alpha_Italy_bind_esc_socre.csv")
Beta <- read.csv("D:/all_project/Italy_project/manuscript/iScience/immune_escape_cal/Beta_Italy_bind_esc_socre.csv")
Delta <- read.csv("D:/all_project/Italy_project/manuscript/iScience/immune_escape_cal/Delta_Italy_bind_esc_socre.csv")
Gamma <- read.csv("D:/all_project/Italy_project/manuscript/iScience/immune_escape_cal/Gamma_Italy_bind_esc_socre.csv")
Omicron <- read.csv("D:/all_project/Italy_project/manuscript/iScience/immune_escape_cal/Omicron_Italy_bind_esc_socre.csv")


Alpha$VOC <- "Alpha"
Beta$VOC <- "Beta"
Delta$VOC<- "Delta"
Gamma$VOC <- "Gamma"
Omicron$VOC <- "Omicron"


library(purrr)
file_list <- list.files(path = "D:/all_project/Italy_project/manuscript/figures/updated_figure2/Omicron_accession_ID",
                        pattern = "*.csv")  # 获取当前目录下所有CSV文件的文件名
path_list <- paste0("D:/all_project/Italy_project/manuscript/figures/updated_figure2/Omicron_accession_ID/",file_list)
data_list <- map(path_list, read.csv,header = F)   # 使用map()函数逐个读取CSV文件并存储为数据框列表

# 使用 sub 函数去掉 .csv 后缀
filenames_no_csv <- sub("\\.csv$", "", file_list)
names(data_list) <- filenames_no_csv

# 使用 lapply 将每个data.frame中的name添加为一列
my_list_with_name <- lapply(names(data_list), function(name) {
  df <- data_list[[name]]
  df$name <- name  # 添加name列
  names(df)[1] <- "Accession_ID"
  return(df)
})

# 将list转换为单个data.frame
omicron_id_lineage <- do.call(rbind, my_list_with_name)
omicron_id_lineage <- omicron_id_lineage %>%
  mutate(VOCs = paste0("Omicron(", gsub("^(BA\\.[12345])(\\..*)?$", "\\1*", omicron_id_lineage$name), ")" ) ) %>%
  select(-name)

Omicron1 <- Omicron %>%
  left_join(omicron_id_lineage, by = "Accession_ID")  %>%
  select(-VOC) %>%
  rename(VOC = VOCs) %>% #drop_na(VOCs) %>%
  mutate(VOC = gsub("Omicron\\(BA\\.[345]\\*[,]?\\*?\\)", "Omicron(BA.3-5*)", VOC) ) %>%
  mutate(VOC = replace_na(VOC, "Others"))

data.plot <- rbind(Alpha, Beta, Delta, Gamma, Omicron1)
data.plot$date <- as.Date(data.plot$date)

data.plot1_1 <- data.plot %>%
  filter(binding_score > -4 & escape_score < 1) %>%
  filter(VOC != "Beta" & VOC !="Gamma")


#table(data.plot1$VOC)
data.plot1.1 <- rbind(Alpha, Beta, Delta, Gamma, Omicron)
data.plot1.1$date <- as.Date(data.plot1.1$date)

data.plot1.1 <- data.plot1.1 %>%
  filter(binding_score > -4 & escape_score < 1) %>%
  filter(VOC != "Beta" & VOC !="Gamma")
#calculate weekly mean values of each voc binding and immune escape
data.plot1.1$week <- floor_date(data.plot1.1$date, "week")
data.plot1.1 %>% group_by(VOC) %>% summarise(mean_bind = mean(binding_score,na.rm = T), sd = sd(binding_score,na.rm = T))
data.plot1_1 %>% group_by(VOC) %>% summarise(mean_bind = mean(binding_score,na.rm = T), sd = sd(binding_score,na.rm = T))

week_avg <- data.plot1.1 %>%
  group_by(week, VOC) %>%
  summarise(binding_score_week_avg = mean(binding_score, na.rm = TRUE),
            escape_score_week_avg = mean(escape_score, na.rm = TRUE))

week_avg_voc_Alpha <- week_avg %>% filter(VOC == 'Alpha' ) %>%
  filter(week >= ymd("2020-11-01") & week <= ymd("2021-09-01"))

week_avg_voc_Delta <- week_avg %>% filter(VOC == 'Delta' ) %>%
  filter(week >= ymd("2021-01-01") & week <= ymd("2022-03-01"))

week_avg_voc_Omicorn <- week_avg %>% filter( VOC == "Omicron" )
week_avg_voc_total <- rbind(week_avg_voc_Alpha, week_avg_voc_Delta, week_avg_voc_Omicorn)

####################################################################################################################
###########################################################Figure 3C plot immune escape#############################
####################################################################################################################
data.plot1_1 <- data.plot1_1 %>% filter(VOC != "Others" & VOC != "Omicron(B.1.1.529)")

figure3C <- ggplot(data.plot1_1) +
  geom_point(aes(x = date, y = escape_score, shape = VOC, color = VOC), size = 2, alpha = 0.3) +
  scale_shape_discrete(solid = F)+
  #geom_line(data = week_avg_voc_total, aes(x = week, y = escape_score_week_avg, group = VOC), linewidth = 0.9, color = "gray40") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 months",
               limits = c(startDate,endDate), expand = c(0.024, 0.018)) +
  scale_y_continuous(limits=c(0,1),breaks = seq(0,1,0.2)) +
  theme_bw() + ylab("Immune escape") +xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5,size=16),
        axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16) ) +
  theme(legend.text=element_text(size=11),
        plot.title=element_text(size=16, face="bold",hjust = 0.5))+
  #ggtitle("Daily Immune escape of VOCs in Italy") +
  theme( panel.grid = element_blank()) +
  scale_color_manual(values=c('Omicron(BA.2*)' = "#1565C0FF", 'Omicron(BA.1*)'= "#64B5F6FF",
                              'Omicron(B.1.1.529)' =  "black", 'Omicron(BA.3-5*)'= "#984EA3",
                              'Others' = "#787878FF",
                              'Gamma' = "#00BFC4",
                              'Alpha' = "#F8766D", 'Delta' = "#00BA38",
                              'Beta' = "#B79F00", 'Non-VOCs' = "#F564E3"))
plotdata1 <- plotdata %>% select(-label_mut_split)
#ggsave(figure3C, file='D:/all_project/Italy_project/manuscript/iScience/Figure3_updated/figure3C.png', width=8, height=6.5, dpi = 400)
# write.csv(plotdata1, 'D:/all_project/Italy_project/manuscript/iScience/Figure3_updated/bind_use.csv')
# write.csv(data.plot1_1, 'D:/all_project/Italy_project/manuscript/iScience/Figure3_updated/escape_use.csv')
####################################################################################################################
###########################################################Figure 3A plot VOC PREVALENCE#############################
####################################################################################################################
Alpha_Italy_bind_esc_socre <- read.csv("D:/all_project/Italy_project/manuscript/figures/updated_figure2/updated/Alpha_Italy_bind_esc_socre1.csv")
Beta_Italy_bind_esc_socre <- read.csv("D:/all_project/Italy_project/manuscript/figures/updated_figure2/updated/Beta_Italy_bind_esc_socre1.csv")
Delta_Italy_bind_esc_socre <- read.csv("D:/all_project/Italy_project/manuscript/figures/updated_figure2/updated/Delta_Italy_bind_esc_socre1.csv")
Gamma_Italy_bind_esc_socre <- read.csv("D:/all_project/Italy_project/manuscript/figures/updated_figure2/updated/Gamma_Italy_bind_esc_socre1.csv")
Omicron_Italy_bind_esc_socre <- read.csv("D:/all_project/Italy_project/manuscript/figures/updated_figure2/updated/Omicron_Italy_bind_esc_socre1.csv")
Non_voc_Italy_bind_esc_socre <- read.csv("D:/all_project/Italy_project/manuscript/figures/updated_figure2/updated/Non_voc_Italy_bind_esc_socre1.csv")

Alpha_Italy_bind_esc_socre$VOCs <- "Alpha"
Beta_Italy_bind_esc_socre$VOCs <- "Beta"
Delta_Italy_bind_esc_socre$VOCs <- "Delta"
Gamma_Italy_bind_esc_socre$VOCs <- "Gamma"
Omicron_Italy_bind_esc_socre$VOCs <- "Omicron"

Non_voc_Italy_bind_esc_socre$VOCs <- ifelse( Non_voc_Italy_bind_esc_socre$date < as.Date("2020-09-01"),"Non-VOCs",
                                             ifelse(Non_voc_Italy_bind_esc_socre$date > as.Date("2020-09-01") & Non_voc_Italy_bind_esc_socre$binding_score < 0.01,"Non-VOCs","Alpha"  ) )
# count_variant <- rbind(Alpha_Italy_bind_esc_socre, Beta_Italy_bind_esc_socre,
#                        Delta_Italy_bind_esc_socre,Gamma_Italy_bind_esc_socre,
#                        Omicron_Italy_bind_esc_socre, Non_voc_Italy_bind_esc_socre ) %>%
#   mutate(date = ymd(date)) %>%
#   mutate(month_index = floor_date(date,"1 month")) %>%
#   select(month_index, VOCs) %>% group_by(month_index, VOCs) %>%
#   summarise(N = n())

Omicron_Italy_bind_esc_socre_new <- Omicron_Italy_bind_esc_socre %>%
  left_join(omicron_id_lineage, by = "Accession_ID")  %>%
  select(-VOCs.x) %>%
  rename(VOCs = VOCs.y) %>% #drop_na(VOCs) %>%
  mutate(VOCs = gsub("Omicron\\(BA\\.[345]\\*[,]?\\*?\\)", "Omicron(BA.3-5*)", VOCs) ) %>%
  mutate(VOCs = replace_na(VOCs, "Others"))

data.plot2 <- rbind(Alpha_Italy_bind_esc_socre, Beta_Italy_bind_esc_socre,
                    Delta_Italy_bind_esc_socre,Gamma_Italy_bind_esc_socre,
                    Omicron_Italy_bind_esc_socre_new) %>% filter(date > as.Date("2021-02-01")) %>%
  rbind(Non_voc_Italy_bind_esc_socre) %>% mutate(date = ymd(date))

data.plot2$date <- as.Date(data.plot2$date)

data_count_voc <- data.plot2 %>%
  #filter(date <= as.Date("2022-10-01")) %>%
  mutate(month_index = floor_date(date,"1 month")) %>%
  group_by(month_index,VOCs) %>%
  summarize(Count = n()) %>% filter(VOCs != "Omicron(BA.3-5*)" & VOCs != "Omicron(B.1.1.529)")

startDate = ymd("2020-01-31")
endDate = ymd("2022-03-30")

figure3A <- ggplot(data = data_count_voc, aes(x=month_index, y = Count, fill=VOCs)) +
  #annotate(geom = "rect",xmin=startDate,xmax=endDate,ymin=0,ymax=Inf,fill="#F39B7F99",alpha=0.1)+
  #geom_col(position = position_stack(reverse = F)) +
  geom_bar(stat = "identity", position = "fill")+
  labs(y = "% of VOCs") + xlab("")+
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 months",
               limits = as.Date(c(startDate,endDate)), expand = c(0.01,0.01))+
  # scale_fill_discrete(labels = c("Dose 1", "Dose 2"))+
  #scale_y_continuous(limits=c(0,3000), breaks = seq(0,3000,500))+
  scale_y_continuous(labels = scales::percent_format(),expand = c(0,0) )+
  # scale_fill_discrete(breaks=c('Non-VOCs', 'Gamma','Alpha', 'Delta', 'Beta', 'Omicron(B.1.1.529)', 'Omicron(BA.1*)',
  #                              'Omicron(BA.2*)', 'Omicron(BA.3*/4*/5*)', 'Others')) +
  scale_fill_manual(values=c('Omicron(BA.2*)' = "#1565C0FF", 'Omicron(BA.1*)'= "#64B5F6FF",
                             'Omicron(B.1.1.529)' =  "black", 'Omicron(BA.3-5*)'= "#984EA3",
                             'Others' = "#787878FF",
                             'Gamma' = "#00BFC4",
                             'Alpha' = "#F8766D", 'Delta' = "#00BA38",
                             'Beta' = "#B79F00", 'Non-VOCs' = "#F564E3"),
                    breaks=c('Non-VOCs', 'Gamma','Alpha', 'Delta', 'Beta', 'Omicron(B.1.1.529)', 'Omicron(BA.1*)',
                             'Omicron(BA.2*)', 'Omicron(BA.3-5*)', 'Others')) +
  #scale_fill_manual(values=c('dose 1' = "#E64B35FF","dose 2" = "#F39B7F99", "1 dose (with previous infection)" = "#4DBBD5FF",
  #"dose 3" = "#00A087FF", "dose 4" = "#3C5488FF"))+, margin = margin(0,1,0,0,'line')
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5,size=16),
        axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16) ) +
  theme(legend.text=element_text(size=9), legend.position = "top",legend.key.size = unit(0.25, 'cm'),
        plot.title=element_text(size=14, face="bold",hjust = 0.5),panel.grid=element_blank())



f1 <-figure3A
f2 <-figure3B +  theme(axis.text.x = element_blank(),
                       axis.text.y = element_text(size = 14),
                       axis.title.y = element_text(size = 16),
                       legend.text = element_text(size = 11), 
                       legend.position = c(0.3, 0.35),
                       legend.background = element_rect(fill = NA),
                       plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                       panel.grid = element_blank()
)
f3 <-figure3C + theme(legend.position="none") 


# layout <- "ABC"
# 
# figure_3A_3B_3C <- f1+f2+f3+
#   plot_layout(design = layout) +
#   plot_annotation(tag_levels = 'A') &
#   theme(plot.tag = element_text(size = 21, hjust = -0.1, vjust = 0.1,face = "bold"),plot.tag.position = c(0, 0.99))
# 
# ggsave(figure_3A_3B_3C, file='D:/all_project/Italy_project/manuscript/iScience/Figure3_updated/figure_3A_3B_3C.png', width=8, height=11.5, dpi = 400)


###updated 2025-09-09##
# layout <- 
# "A
#  B
#  C"
# 
# figure_3A_3B_3C <- f1 + f2+ f3 +
#   plot_layout(design = layout) +
#   plot_annotation(tag_levels = 'A') &
#   theme(plot.tag = element_text(size = 21, hjust = -0.1, vjust = 0.1,face = "bold"),plot.tag.position = c(0, 0.99))
# 
# ggsave(figure_3A_3B_3C, file='D:/all_project/Italy_project/manuscript/iScience/Figure3_updated/figure_3A_3B_3C4.png', width=9, height=14, dpi = 400)

######################################################################################################
plotdata <- plotdata %>% select(-escape_score) %>% 
  left_join(
    data.plot1_1 %>% select(Accession_ID, escape_score),
    by = "Accession_ID"
  )

group_color_values <- c(
  "Alpha" = "#F8766D",
  "Delta" = "#00BA38",
  "BA.1" = scales::alpha("#BBDEFB", 0.5),
  "BA.1.1" = "#64B5F6FF",
  "BA.1# (Except BA.1 and BA.1.1)" = "#64B5F6FF",
  "BA.2" = "#1565C0FF",
  "BA.2# (Except BA.2)" = "#1565C0FF",
  "B.1.1.529" = "black",
  "Other" = "grey"
)
plotdata %>% group_by(VOC_group) %>% summarise(mean_bind = mean(use_bind,na.rm = T), sd = sd(use_bind,na.rm = T))

labels_voc_binding <- c(
  "Alpha (1.170)",
  "Delta (0.261)",
  "BA.1 (0.277)",
  "BA.1.1 (0.427)",
  "BA.1# (Except BA.1 and BA.1.1) (0.312)",
  "BA.2 (0.323)",
  "BA.2# (Except BA.2) (0.324)"
)

plotdata %>% group_by(VOC_group) %>% summarise(mean_escape = mean(escape_score,na.rm = T), sd = sd(escape_score,na.rm = T))

labels_voc_escape <- c(
  "Alpha (0.0414)",
  "Delta (0.208)",
  "BA.1 (0.773)",
  "BA.1.1 (0.846)",
  "BA.1# (Except BA.1 and BA.1.1) (0.802)",
  "BA.2 (0.834)",
  "BA.2# (Except BA.2) (0.833)"
)

plotdata1 <- plotdata %>% select(-label_mut_split)
#ggsave(figure3C, file='D:/all_project/Italy_project/manuscript/iScience/Figure3_updated/figure3C.png', width=8, height=6.5, dpi = 400)
write.csv(plotdata1, 'D:/all_project/Italy_project/manuscript/iScience/Figure3_updated/bind_use.csv')
# write.csv(data.plot1_1, 'D:/all_project/Italy_project/manuscript/iScience/Figure3_updated/escape_use.csv')

figure3B <- ggplot(plotdata %>% filter(use_bind >-1.5 )) +
  geom_point(aes(x = date, y = use_bind, shape = VOC_group, color = VOC_group), size = 2, alpha = 0.3) +
  scale_shape_manual(values = group_shape_values, labels = labels_voc_binding) +
  scale_color_manual(values = group_color_values, labels = labels_voc_binding) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 months",
               limits = c(startDate, endDate), expand = c(0.017, 0.017)) +
  scale_y_continuous(limits = c(-1.5, 1.5), breaks = rev(seq(-1.5,1.5, by = 0.5) ) ) +
  theme_bw() + 
  ylab("ACE2 Addative\n binding score") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 16),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        legend.text = element_text(size = 11), 
        legend.position = c(0.2, 0.35),
        legend.background = element_rect(fill = NA),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        panel.grid = element_blank()
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 2, alpha = 1), title = "VOC"), 
    shape = guide_legend(override.aes = list(size = 2), title = "VOC")
  )


figure3C <- ggplot(plotdata) +
  geom_point(aes(x = date, y = escape_score, shape = VOC_group, color = VOC_group), size = 2, alpha = 0.3) +
  scale_shape_manual(values = group_shape_values, labels = labels_voc_escape) +
  scale_color_manual(values = group_color_values, labels = labels_voc_escape) +
  #geom_line(data = week_avg_voc_total, aes(x = week, y = escape_score_week_avg, group = VOC), linewidth = 0.9, color = "gray40") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 months",
               limits = c(startDate,endDate), expand = c(0.017, 0.017)) +
  scale_y_continuous(limits=c(0,1),breaks = seq(0,1,0.2)) +
  theme_bw() + ylab("Immune escape") +xlab("Date")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5,size=16),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 18) ) +
  theme(legend.text=element_text(size=11),
        legend.position = c(0.3, 0.5),
        legend.background = element_rect(fill = NA),
        panel.grid = element_blank(),
        plot.title=element_text(size=16, face="bold",hjust = 0.5))+
  guides(
    colour = guide_legend(override.aes = list(size = 2, alpha = 1), title = "VOC"), 
    shape = guide_legend(override.aes = list(size = 2), title = "VOC")
  )
# scale_color_manual(values=c('Omicron(BA.2*)' = "#1565C0FF", 'Omicron(BA.1*)'= "#64B5F6FF",
#                             'Omicron(B.1.1.529)' =  "black", 'Omicron(BA.3-5*)'= "#984EA3",
#                             'Others' = "#787878FF",
#                             'Gamma' = "#00BFC4",
#                             'Alpha' = "#F8766D", 'Delta' = "#00BA38",
#                             'Beta' = "#B79F00", 'Non-VOCs' = "#F564E3"))


figure3A <- ggplot(data = data_count_voc, aes(x=month_index, y = Count, fill=VOCs)) +
  #annotate(geom = "rect",xmin=startDate,xmax=endDate,ymin=0,ymax=Inf,fill="#F39B7F99",alpha=0.1)+
  #geom_col(position = position_stack(reverse = F)) +
  geom_bar(stat = "identity", position = "fill")+
  labs(y = "% of VOCs") + xlab("")+
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 months",
               limits = as.Date(c(startDate,endDate)), expand = c(0.01,0.01))+
  # scale_fill_discrete(labels = c("Dose 1", "Dose 2"))+
  #scale_y_continuous(limits=c(0,3000), breaks = seq(0,3000,500))+
  scale_y_continuous(labels = scales::percent_format(),expand = c(0,0) )+
  # scale_fill_discrete(breaks=c('Non-VOCs', 'Gamma','Alpha', 'Delta', 'Beta', 'Omicron(B.1.1.529)', 'Omicron(BA.1*)',
  #                              'Omicron(BA.2*)', 'Omicron(BA.3*/4*/5*)', 'Others')) +
  scale_fill_manual(values=c('Omicron(BA.2*)' = "#1565C0FF", 'Omicron(BA.1*)'= "#64B5F6FF",
                             'Omicron(B.1.1.529)' =  "black", 'Omicron(BA.3-5*)'= "#984EA3",
                             'Others' = "#787878FF",
                             'Gamma' = "#00BFC4",
                             'Alpha' = "#F8766D", 'Delta' = "#00BA38",
                             'Beta' = "#B79F00", 'Non-VOCs' = "#F564E3"),
                    breaks=c('Non-VOCs', 'Gamma','Alpha', 'Delta', 'Beta', 'Omicron(B.1.1.529)', 'Omicron(BA.1*)',
                             'Omicron(BA.2*)', 'Omicron(BA.3-5*)', 'Others')) +
  #scale_fill_manual(values=c('dose 1' = "#E64B35FF","dose 2" = "#F39B7F99", "1 dose (with previous infection)" = "#4DBBD5FF",
  #"dose 3" = "#00A087FF", "dose 4" = "#3C5488FF"))+, margin = margin(0,1,0,0,'line')
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5,size=16),
    axis.text.x  = element_blank(),
    axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 18) ) +
  theme(legend.text=element_text(size=10), legend.position = "top",legend.key.size = unit(0.6, 'cm'),
        plot.title=element_text(size=14, face="bold",hjust = 0.5),panel.grid=element_blank())



f1 <-figure3A
f2 <-figure3B +  theme(axis.text.x = element_blank(),
                       axis.text.y = element_text(size = 14),
                       axis.title.y = element_text(size = 16),
                       legend.text = element_text(size = 11), 
                       legend.position = c(0.3, 0.5),
                       legend.background = element_rect(fill = NA),
                       plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                       panel.grid = element_blank()
)
f3 <-figure3C 


# layout <- "ABC"
# 
# figure_3A_3B_3C <- f1+f2+f3+
#   plot_layout(design = layout) +
#   plot_annotation(tag_levels = 'A') &
#   theme(plot.tag = element_text(size = 21, hjust = -0.1, vjust = 0.1,face = "bold"),plot.tag.position = c(0, 0.99))
# 
# ggsave(figure_3A_3B_3C, file='D:/all_project/Italy_project/manuscript/iScience/Figure3_updated/figure_3A_3B_3C.png', width=8, height=11.5, dpi = 400)


###updated 2025-09-09##
layout <- 
  "A
 B
 C"

figure_3A_3B_3C <- f1 + f2+ f3 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 21, hjust = -0.1, vjust = 0.1,face = "bold"),plot.tag.position = c(0, 0.99))

ggsave(figure_3A_3B_3C, file='D:/all_project/Italy_project/manuscript/iScience/Figure3_updated/figure_3A_3B_3C8.png', width=9, height=14, dpi = 400)

ggsave(figure_3A_3B_3C, file='D:/all_project/Italy_project/manuscript/iScience/Figure3_updated/figure_3A_3B_3C8.pdf', width=9, height=14)
