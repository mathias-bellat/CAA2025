####################################################################
# This script is for creating and computing the graph of the paper #
# "Human are you tehre?" Bellat, Hatton, Orellana-Figueroa 2025    #
#                                                                  #
# Author: Mathias Bellat                                           #
# Affiliation : Tübingen University                                #
# Creation date : 12/08/2025                                       #
# E-mail: mathias.bellat@uni-tuebingen.de                          #
####################################################################


# 00 Preparation ###############################################################

# 0.1 Folder check =============================================================
getwd()

# Clean up workspace
rm(list = ls(all.names = TRUE))

# 0.2 Load packages ============================================================
install.packages("pacman")
library(pacman) # Easier way of loading packages
pacman::p_load(dplyr, readr, stringr, tidyr, treemap, knitr, kableExtra,
               ggplot2, readxl,patchwork,lubridate) # Specify required packages and download it if needed

# 0.3 Show session infos =======================================================
sessionInfo()

# 01 Figure 2, year of publication of the papers ###############################

# 1.1 Prepare the data =========================================================
asd <- read_excel("../data/raw_data/Statistics_papers.xlsx", sheet = "Final_results_ASD")
apm <- read_excel("../data/raw_data/Statistics_papers.xlsx", sheet = "Final_results_APMs")

# Remove Theory (Reason 1) and not ML (Reason 2) papers
asd <- asd[!asd$Results %in%  c("Reason 1", "Reason 2"), ]

# Remove the duplicate study case from Agapiou et al. 2021 and li et al. 2023
asd.single <- asd[!asd$Name %in%  "Agapiou et al. 2021-2", ]
apm.single <- apm[!apm$Name %in%  "Li et al. 2023a-2", ]

# 1.2 Format the data ==========================================
asd_table <- table(asd.single$Year)
apm_table <- table(apm.single$Year)

asd_df <- as.data.frame(asd_table)
apm_df <- as.data.frame(apm_table)

colnames(asd_df) <- c("year", "Freq")
colnames(apm_df) <- c("year", "Freq")

asd_df$year <- as.numeric(as.character(asd_df$year))
apm_df$year <- as.numeric(as.character(apm_df$year))

asd_df$type <- "ASD"
apm_df$type <- "APM"

combined_df <- rbind(asd_df, apm_df)

# Create a full sequence
all_years <- min(combined_df$year):max(combined_df$year)
complete_df <- data.frame(
  year = rep(all_years, 2),
  type = rep(c("ASD", "APM"), each = length(all_years))
)

# Join both df
df_final <- left_join(complete_df, combined_df, by = c("year", "type"))

# Remove NA by 0
df_final[is.na(df_final$Freq), "Freq"] <- 0

# Filter the APMs data before 2015
df_final_filtered <- df_final
df_final_filtered[df_final_filtered$type == "APM" & df_final_filtered$year < 2015, "Freq"] <- NA

# 1.3 Create the plot ==========================================================
plot <- ggplot(df_final_filtered, aes(x = year, y = Freq, color = type)) +
  geom_rect(xmin = 2021, xmax = 2024, ymin = -Inf, ymax = Inf,
            fill = "lightblue", alpha = 0.03, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(min(df_final$year), max(df_final$year) + 1),
                  ylim = c(0, 30)) +
  geom_line(data = subset(df_final_filtered, type == "ASD"),  linewidth = 1.5) +
  geom_line(data = subset(df_final_filtered, type == "APM"),  linewidth = 0.75) +
  geom_point(data = subset(df_final_filtered, type == "ASD" & Freq > 0),
             shape = 21, color = "#69b3a2", fill = "white", size = 4.5, stroke = 1) +
  geom_point(data = subset(df_final_filtered, type == "APM" & Freq > 0),
             shape = 19, color = "#ff7f0e", size = 3) +
  geom_text(aes(label = ifelse(Freq > 0 & type == "ASD" &
                                 year != 2024, round(Freq, 2), "")),
            vjust = -1.2, hjust = 1, color = "black") +
  geom_text(aes(label = ifelse(Freq > 0 & type == "ASD" & year == 2024, round(Freq, 2), "")),
            vjust = -0.95, hjust = -0.2, color = "black") +
  geom_text(aes(label = ifelse(Freq > 0 & type == "APM" & year != 2015, round(Freq, 2), "")),
            vjust = 1.75, hjust = 0, color = "black") +
  geom_text(aes(label = ifelse(Freq > 0 & type == "APM" & year == 2015, round(Freq, 2), "")),
            vjust = -0.95, hjust = 1, color = "black") +
  labs(x = "Years",
       y = paste0("Number of publications (ASD: n = ", sum(asd_df$Freq),
                  ", APM: n = ", sum(apm_df$Freq), ")"),
       color = "Type") +
  scale_x_continuous(breaks = seq(min(df_final$year), max(df_final$year), by = 2)) +
  scale_color_manual(values = c("ASD" = "#69b3a2", "APM" = "#ff7f0e")) +
  theme_classic() +
  theme(legend.position = "top")

# Check the plot
plot

ggsave("../figures/Fig.2/Fig.2.png", plot = plot, width = 7, height = 6, units = "in", dpi = 600)
ggsave("../figures/Fig.2/Fig.2.pdf", plot = plot, width = 7, height = 6, units = "in")

# 02 Figure 3, tree map ########################################################

# 2.1 Import the data ==========================================================
df <- read_excel("data/Statistics_papers.xlsx", sheet = "Models statistics")
df <- df[-109,-c(6:10)]
df <- df[df[3] !=0,] # Remove models with no occurences

# 2.2 Format the data ==========================================================
df_counted <- df[,c(1:5)]
names(df_counted) <- c("description", "model", "value", "value.best", "family")
df_counted$family[df_counted$family == "N/A"] <- "Statistics"

# 2.2 Create and export the graph  =============================================
png("../figures/Fig.3/Fig.3.png", width = 1200, height = 1000)
pdf("../figures/Fig.3/Fig.3.pdf", width = 12, height = 10)
par(mar = c(0, 0, 0, 0))
tm <- treemap(df_counted,
              index = c("family", "model"),
              vSize = "value",
              type = "index",
              fontsize.labels = c(16, 15),  # Masquer les labels de famille
              fontcolor.labels = c("white", "black"),  # Noir sur Pastel1
              bg.labels = 0,
              align.labels = list(c("left", "top"), c("center", "center")),
              title = "",
              palette = "Pastel1",
              border.col = c("black", "white"),
              border.lwds = c(2, 0))
dev.off()

# 03 Table 2, models family and occurrence #####################################

# 3.1 Prepare the data =========================================================
df_counted$approach <- "Machine learning"
df_counted$approach[df_counted$family == "Statistics"] <- "Statistics"
df_counted$family[df_counted$model == "LR"] <- "Linear regression"
df_counted$family[df_counted$model == "k-MC"] <- "Dimensionality reduction"

# 3.2 Format the data ==========================================================
df_counted <- select(df_counted, approach, family, description, model, value, value.best)
df_counted <- df_counted %>%  arrange(approach, family, desc(value))

# 3.3 Plot the table ===========================================================
# use the html format for the html quarto file and the latex format for the word and pdf

df_counted %>%
  kable("pipe", caption = "List of algorithms used in the papers under review organized by the approach and family of analysis,
        along with their abbreviations and number of use. In the case the model was compared to others, we highlighted the number of time he
        performed as the best model.", col.names = c("Approach", "Family", "Description", "Model abreviation", "Number of uses",
                                                     "Number of time the model performed the best")) %>%
  kable_styling(full_width = FALSE) %>%
  collapse_rows(columns = c(1,2), valign = "top")


# 4 Figure 4, family per year of publication ###################################
# 4.1 Prepare the data =========================================================
apm_long <- apm %>%
  separate_rows(Family, sep = ";")

asd_long <- asd %>%
  separate_rows(Family, sep = ";")

combined <- rbind(apm_long[,c(3,6)], asd_long[,c(3,6)])
table <- table(combined)
freq_df <- as.data.frame(table)
colnames(freq_df) <- c("year", "family", "Freq")

# Remove absence of data
freq_df <- freq_df[freq_df$Freq > 0,]
freq_df <- freq_df[freq_df$family != "N/A",]

# Convert the date to number
freq_df$year <- as.numeric(as.character(freq_df$year))

# 4.2 Prepare the plot =========================================================

# Blindfold colors
color <- c('#9F0162', '#009F81', '#FF5AAF', '#00FCCF', '#8400CD', '#008DF9', '#00C2F9', '#FFB2FD')

freq_df <- freq_df %>%
  arrange(year, family) %>%
  mutate(
    cumsum_freq = ave(Freq, year, FUN = cumsum),
    pos = cumsum_freq - 0.5 * Freq)

print(freq_df)

# 4.3 The plot =================================================================
plot<- ggplot(freq_df, aes(x = year, y = Freq, fill = reorder(family, -as.numeric(family)))) +
  geom_bar(stat = "identity", colour = "white", width = 0.9) +
  geom_text(aes(y = pos, label = ifelse(Freq > 1, Freq, "")),
            color = "white",
            size = 4) +
  scale_fill_manual(values = color,
                    labels = c(
                      "Unsupervised Learning and Clustering"  = "Unsupervised Learning \nand Clustering",
                      "Nearest Neighbour Classifier"  = "Nearest Neighbour \nClassifier",
                      "Artificial Neural Network"  = "Artificial Neural \nNetwork",
                      "Decision Trees and Rule Induction" = "Decision Trees \nand Rule Induction")
  )+
  labs(
    x = "Year",
    y = paste0("Number of observations (n = ", sum(freq_df$Freq), ")"),
    fill = "Architectures categories"
  ) +
  coord_cartesian(xlim = c(2006, 2024), ylim = c(0, 36)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(size = 11, face = "bold"))

# Plot
plot

ggsave("../figures/Fig.4/Fig.4.png", plot = plot, width = 9, height = 6, units = "in", dpi = 600)
ggsave("../figures/Fig.4/Fig.4.pdf", plot = plot, width = 9, height = 6, units = "in")

# 05 Figure 5, results and metrics #############################################

# 5.1 Import the data ==========================================================
metrics <- read_excel("data/Statistics_papers.xlsx", sheet = "Metrics_full")

# 5.2 Prepare the data =========================================================
metrics <- as.data.frame(metrics)
metrics_df <- data.frame(
  eval_metrics = character(),
  study_id    = character(),
  score       = numeric(),
  type        = character(),
  stringsAsFactors = FALSE
)
metrics_df[1,] <- c("","",0,"")

for (i in 1:nrow(metrics)) {
  metric <- metrics[i,c(5:23)]
  metric <- metric[, colSums(is.na(metric)) == 0, drop = FALSE]
  metric <- round(metric, digit = 2)
  if (ncol(metric) == 0) {
    NA
  } else if (ncol(metric) == 1) {
    x <- as.data.frame(c(colnames(metric), metrics[i, 2], metric, metrics[i, 4]))
    colnames(x) <- colnames(metrics_df)
    metrics_df <- rbind(metrics_df, x)
  } else {
    for (j in 1:ncol(metric)) {
      x <- c(colnames(metric[j]), metrics[i, 2], metric[,j], metrics[i, 4])
      metrics_df <- rbind(metrics_df, x)
    }
  }
}

metrics_df <- metrics_df[-1,]

# 5.3 Prepare the plot =========================================================
metric_counts <- table(metrics_df$eval_metric)
sorted_metrics <- names(sort(metric_counts, decreasing = FALSE))

study_counts <- table(metrics_df$study_id)
sorted_studies <- names(sort(study_counts, decreasing = TRUE))

metrics_df$present <- 1

all_combinations <- expand.grid(
    eval_metrics = sorted_metrics,
    study_id = sorted_studies,
    stringsAsFactors = FALSE)


plot_data <- merge(all_combinations, metrics_df,
                     by = c("eval_metrics", "study_id"), all.x = TRUE)

plot_data$present[is.na(plot_data$present)] <- 0

study_counts_df <- data.frame(
    study_id = names(study_counts),
    metric_count = as.numeric(study_counts))

plot_data <- merge(plot_data, study_counts_df, by = "study_id")

plot_data <- plot_data %>%
  select(-type) %>%
  left_join(metrics %>% select(Name, Task) %>% distinct(),
            by = c("study_id" = "Name"))

plot_data$eval_metrics <- factor(plot_data$eval_metrics, levels = sorted_metrics)
plot_data$study_id <- factor(plot_data$study_id, levels = sorted_studies)
plot_data<- plot_data[order(plot_data$metric_count, decreasing = TRUE), ]


# 5.4 Create the plot ==========================================================
plot <- ggplot(plot_data, aes(x = study_id, y = eval_metrics))+
    geom_tile(aes(fill = ifelse(present == 1, metric_count, NA)),
              color = "gray", size = 0.3) +
  geom_text(aes(
    label = ifelse(
      present == 1 & !is.na(score) & metric_count <= 3,
      score, "")),
    color = "black", size = 2, fontface = "bold") +
  geom_text(aes(
    label = ifelse(
      present == 1 & !is.na(score) & metric_count > 3,
      score, "")),
    color = "white", size = 2, fontface = "bold") +
    scale_fill_gradient(low = "#DEEBF7", high = "#08519C",
                        name = "Metric\nCount",
                        na.value = "transparent",
                        breaks = c(2, 3, 4, 5, 6),
                        labels = c("2", "3", "4", "5", "6+")) +
    labs(
      x = "Case Studies",
      y = "Evaluation Metrics"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y = element_text(size = 6),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8),
      panel.grid = element_blank()
)


plot

ggsave("../figures/Fig.5/Fig.5_raw.png", plot = plot, width = 20, height = 6, units = "in", dpi = 600)
ggsave("../figures/Fig.5/Fig.5_raw.pdf", plot = plot, width = 20, height = 6, units = "in")


# 5.5 Basic stats ==============================================================
metrics_list <- unlist(strsplit(paste(metrics_df$eval_metrics, collapse = ","), ","))
metrics_clean <- trimws(metrics_list)
cat("Number of metrics:", length(unique(metrics_clean)), "\n")
print(sort(table(metrics_clean), decreasing = TRUE))

# 06 Figure 5, scatterplot  ####################################################

# 6.1 Prepare the data =========================================================

scatter_df <- metrics[metrics$Task == "Automatic Structure Detection",]
scatter_df <- scatter_df[!is.na(scatter_df$`F1 Score`),]
scatter_df$ID <- gsub("ID0", "", scatter_df$ID)
scatter_df$ID <- gsub("ID", "", scatter_df$ID)

# 6.2 Plot the data ============================================================

plot <- ggplot(scatter_df , aes(x = Recall, y = Precision, label = ID)) +
  geom_point(aes(size = `F1 Score`, color = Accuracy)) +
  geom_text(size = 2.5, vjust = -2, hjust = 1) +
  scale_size_continuous(range = c(3, 8)) +
  scale_color_viridis_c(option = "viridis", end = 0.99) +
  xlim(0, 1) + ylim(0, 1) +
  labs(
    x = "Recall",
    y = "Precision",
    color = "Accuracy",
    size  = "F1-score"
  ) +
  theme_minimal()

plot

ggsave("../figures/Fig.6/Fig.6_raw.png", plot = plot, width = 7, height = 6, units = "in", dpi = 600)
ggsave("../figures/Fig.6/Fig.6_raw.pdf", plot = plot, width = 7, height = 6, units = "in")

# 07 Figure 7, Remote sensing publication ######################################

# 7.1 Prepare the data for dimension ===========================================

# For the Dimension website (visited on the 20/08/2025)
# Inclusion criteria "remote sensing" keyword; 43 (History, Heritage and Archaeology) category
# Years 2000 to 2024

number <- c(176,192,188,181,205,213,246,322,345,375,431,
            585,770,854,926,850,1109,1092,1204,1180,1480,1597,1758,1804,1949)
years <- seq(2000, 2024, by = 1)
df_dimension <- data.frame(year = years, Freq = number)

# 7.2 Prepare the data for web of science ======================================
# For the Web of Science website (visited on the 20/08/2025)
# Inclusion criteria "remote sensing" keyword; Arcaheology Web of Science category
# Years 2000 to 2024

number <- c(6,4,2,5,2,9,10,14,13,32,18,61,22,22,25,21,61,64,147,243,88,73,128,753,78)
years <- seq(2000, 2024, by = 1)
df_web <- data.frame(year = years, Freq = number)

# 7.3 Prepare the data for UCS Satellite Database ==============================
# Visited on the 20/08/2025 at https://test.ucsaction.org/resources/satellite-database
sat <- read_excel("data/UCS-Satellite-Database 5-1-2023.xlsx")

# Select only satellites for earh observation non military
sat <- sat[sat$Purpose == "Earth Observation",]
sat <- sat[sat$Users != "Military",]
number <- sat$`Date of Launch`
number <- na.omit(number)
number <- as.numeric(number)
number <-  as.Date(number, origin = "1899-12-30")
year <- year(number)

# Select only satellites lunched after 2000
year <- year[year > 1999]
df_sat <- as.data.frame(table(year))
df_sat$year <- as.character(df_sat$year)
df_sat$year <- as.numeric(df_sat$year)

# 7.4 Plot the data ============================================================

plot1 <- ggplot(df_dimension, aes(x = year, y = Freq)) +
  coord_cartesian(xlim = c(min(years), max(years) + 1),
                  ylim = c(0, 2000)) +
  geom_line(color = "#69b3a2", linewidth = 1) +
  geom_point(shape = 19, color = "#69b3a2", size = 3) +
  annotate("text", x = 2000, y = 2000, label = "A", fontface = "bold", size = 11, hjust = 0.5, vjust = 1.1) +
  labs(y = "Number of records in Dimension",
       x = "Year") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

plot1

plot2 <- ggplot(df_web, aes(x = year, y = Freq)) +
  coord_cartesian(xlim = c(min(years), max(years) + 1),
                  ylim = c(0, 800)) +
  annotate("text", x = 2000, y = 800, label = "B", fontface = "bold", size = 11, hjust = 0.5, vjust = 1.1) +
  geom_line(color = "#69b3a2", linewidth = 1) +
  geom_point(shape = 19, color = "#69b3a2", size = 3) +
  labs(y = "Number of records Web of Science",
       x = "Year") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

plot2

plot3 <- ggplot(df_sat, aes(x = year, y = Freq)) +
  coord_cartesian(xlim = c(min(years), max(years) + 1),
                  ylim = c(0, 150)) +
  annotate("text", x = 2000, y = 150, label = "C", fontface = "bold", size = 11, hjust = 0.5, vjust = 1.3) +
  geom_line(color = "#69b3a2", linewidth = 1) +
  geom_point(shape = 19, color = "#69b3a2", size = 3) +
  labs(y = "Number of earth observation satellites \nlunched (UCS Satellite Database)",
       x = "Year") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

plot3

plot <- (plot1 | plot2) / plot3
plot

ggsave("../figures/Fig.7/Fig.7.png", plot = plot, width = 10, height = 8, units = "in", dpi = 600)
ggsave("../figures/Fig.7/Fig.7.pdf", plot = plot, width = 10, height = 8, units = "in")

# 08 Figure 8, covariates selection ############################################

# 8.1 Prepare the data =========================================================

cov <- data.frame(name = apm$Name, nb.cov = apm$`Nb. Cov.`, cov.sel = apm$`Cov. Selec.`)
cov <- na.omit(cov)
cov$nb.cov
cov$cov.sel
cov$cov.sel <- c(8,2,8,21,10,25,6,65,8,16,16,7,12)
cov$cov.type <- c("PCA", NA, NA,"Human", "PCA", NA, "Human",
              NA, "VIF/auto-correl", NA, NA, NA, NA)

cov_long <- cov %>%
  pivot_longer(cols = c("nb.cov", "cov.sel"),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = case_when(
    metric == "nb.cov" ~ "Nb Coverage",
    metric == "cov.sel" ~ "Coverage Selected",
    TRUE ~ metric
  ))


# 8.2 Plot the data ============================================================

plot <- ggplot(cov, aes(x = name)) +
  geom_col(aes(y = nb.cov),
           fill = "lightpink", width = 0.8) +
  geom_col(aes(y = cov.sel, fill = cov.type),
           width = 0.8) +
  labs(
    y = "Number of covariates",
    x = "",
    fill = "Covariates selection \ntype (if any)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_fill_brewer(type = "qual", palette = "Pastel2")

plot
ggsave("../figures/Fig.8/Fig.8_raw.png", plot = plot, width = 8, height = 6, units = "in", dpi = 600)
ggsave("../figures/Fig.8/Fig.8_raw.pdf", plot = plot, width = 8, height = 6, units = "in")
