# LEGENDplex Cytokine Panel Bar Charts (by treatment group)

# load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

# import data from Excel file
# data was exported from LEGENDplex analysis software and replicates for each sample were averaged on sheet titled "plotting". Treatment groups added as column B. 
file_path <- "/Users/caseymeili/Downloads/Pioglitazone_2eport_2024-08-12.xlsx"
data <- read_excel(file_path, sheet = "Plotting")

# ensure treatment is a factor
data$Treatment <- factor(data$Treatment, levels = c("Drug", "Vehicle"))

# select the relevant columns: treatment and cytokine of interest
data_infg <- data %>%
  select(Treatment, `CXCL10 (IP-10) (B3)`)

# convert cytokine column to numeric, handling non-numeric values
data_infg$`CXCL10 (IP-10) (B3)` <- as.numeric(gsub("<", "", data_infg$`CXCL10 (IP-10) (B3)`))

# remove rows with NA values in cytokine column
data_infg <- data_infg %>%
  filter(!is.na(`CXCL10 (IP-10) (B3)`))

# remove any remaining rows where treatment is NA (removes PBS controls)
data_infg <- data_infg %>%
  filter(!is.na(Treatment))

# rename drug as Pioglitazone
data_infg$Treatment <- recode(data_infg$Treatment, "Drug" = "Pioglitazone")

# create bar chart for cytokines by treatment group (includes t-test and significance)
ggplot(data_infg, aes(x = Treatment, y = `CXCL10 (IP-10) (B3)`, fill = Treatment)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "summary", fun = "mean", color = "black") +  # add colored bars
  geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = .9), color = "black") +  # add individual data points on bars
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2, color = "black") +  # add error bars representing standard deviations
  labs(title = "CXCL10 Levels (plasma)", # set title and axis labels
       x = "Treatment",
       y = "CXCL10 Level") +
  scale_fill_manual(values = c("#DC267F", "#FE6100")) + # set bar colors with hex codes
  theme_minimal() + # make plot background transparent
  theme(plot.title = element_text(hjust = 0.5), # center plot title, remove legend, and set size of axis labels
        legend.position = "none",
        axis.text = element_text(size = 11)) + 
  stat_compare_means(label = "p.signif", # run t-test, plot significance, and set location on y axis
                     method = "t.test", 
                     comparisons = list(c("Pioglitazone", "Vehicle")),
                     label.y = 1300) 
# save plot
output_path <- "/Users/caseymeili/Downloads/CXCL10.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)
