# plotting of rodent lung 16S and ITS data 
# stacked bar charts for genera abundance by sample
# bar charts for Good's Coverage Index and number of high quality sequences per sample

library(tidyr)
library(ggplot2)
library(pals)
library(readxl)
library(dplyr)

# 16S PLOTTING
# import data
# data formatted as mothur taxonomy file with everything except taxa and counts per sample removed
# low abundance genera summed and grouped as "other genera"
bact.data <- read_excel("~/Desktop/Amplicon-Data/mothur/plotting.xlsx", sheet="16S") 

# reshape the data for ggplot
df_long_bac <- gather(bact.data, key = "Sample", value = "Value", -taxon, -total)

# manually set the order of taxon and corresponding colors
taxon_order_bac <- unique(df_long_bac$taxon)
df_long_bac$taxon <- factor(df_long_bac$taxon, levels = taxon_order_bac)

# create the stacked bar chart
ggplot(df_long_bac, aes(x = Sample, y = Value, fill = taxon)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values=as.vector(stepped3(16)), name="Genus") +
  labs(title = "16S",
       x = "Sample Number",
       y = NULL) +
  scale_x_discrete(breaks = c("Rodent-1", "Rodent-2", "Rodent-3", "Rodent-4"),
                   labels = c("1", "2", "3", "4")) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -3), 
        axis.text.y = element_blank(),        
        axis.ticks.y = element_blank()) 

# save plot
output_path <- "/Users/caseymeili/Downloads/rodentlungs-16S.pdf"
ggsave(output_path, plot=last_plot())

# ggsave("updatedfinalmap.jpg", plot=last_plot(), height=720, width=1280, units="px")


######################################################

# ITS PLOTTING
# import data
# data formatted as mothur taxonomy file with everything except taxa and counts per sample removed
# low abundance genera summed and grouped as "other genera"
fungi.data <- read_excel("~/Desktop/Amplicon-Data/mothur/plotting.xlsx", sheet="ITS")

 # reshape the data for ggplot
df_long_fung <- gather(fungi.data, key = "Sample", value = "Value", -taxon, -total)

# manually set the order of taxon and corresponding colors
taxon_order_fung <- unique(df_long_fung$taxon)
df_long_fung$taxon <- factor(df_long_fung$taxon, levels = taxon_order_fung)

# create the stacked bar chart
ITS_plot <- ggplot(df_long_fung, aes(x = Sample, y = Value, fill = taxon)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values=as.vector(stepped3(16)), name="Genus") +
  labs(title = "ITS",
       x = "Sample Number",
       y = NULL) +
  scale_x_discrete(breaks = c("Rodent-1", "Rodent-2", "Rodent-3", "Rodent-4"),
                   labels = c("1", "2", "3", "4")) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -3), 
        axis.text.y = element_blank(),        
        axis.ticks.y = element_blank()) 
ITS_plot
# save plot
output_path <- "/Users/caseymeili/Downloads/rodentlungs-ITS.pdf"
ggsave(output_path, plot=last_plot())


######################################################

# COVERAGE PLOTTING
# import data
# data is good's coverage index value for each sample computed using mother/1.48.0 command summary.single
coverage.data <- read_excel("~/Desktop/Amplicon-Data/mothur/plotting.xlsx", sheet="Coverage")

# reshape the data for ggplot
coverage_long <- coverage.data %>%
  gather(key = "Sample", value = "Value", -Type)

# Filter out non-numeric rows
coverage_long_numeric <- coverage_long %>%
  filter(grepl("^\\d+\\.?\\d*$", Value))
coverage_long_numeric$Value <- as.numeric(as.character(coverage_long_numeric$Value))

# create bar chart
ggplot(coverage_long_numeric, aes(x = Sample, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values=c("#4880B8","#7AACD2"), name="Amplicon") +
labs(title = "Coverage",
       x = "Sample Number",
       y = "Good's Coverage Index") +
  scale_x_discrete(breaks = c("Rodent-1", "Rodent-2", "Rodent-3", "Rodent-4"),
                   labels = c("1", "2", "3", "4")) +  
  scale_y_continuous(limits = c(0, 1)) +
  geom_text(aes(label = round(Value, 4)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 2.5, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0), 
        axis.ticks.y = element_blank()) 

output_path <- "/Users/caseymeili/Downloads/rodentlungs-coverage.pdf"
ggsave(output_path, plot=last_plot())

######################################################

# HIGH QUALITY SEQUENCE NUMBER PLOTTING
# import data
# data is the number of sequences following every processing step, computed using mother/1.48.0 command count.groups
coverage.data <- read_excel("~/Desktop/Amplicon-Data/mothur/plotting.xlsx", sheet="HQ")

# reshape the data for ggplot
coverage_long <- coverage.data %>%
  gather(key = "Sample", value = "Value", -Type)

# Filter out non-numeric rows
coverage_long_numeric <- coverage_long %>%
  filter(grepl("^\\d+\\.?\\d*$", Value))
coverage_long_numeric$Value <- as.numeric(as.character(coverage_long_numeric$Value))

# create bar chart
ggplot(coverage_long_numeric, aes(x = Sample, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values=c("#D55F2B", "#EE934F"), name="Amplicon") +
  labs(title = "Number of High Quality Sequences",
       x = "Sample Number",
       y = "Number of High Quality Sequences") +
  scale_x_discrete(breaks = c("Rodent-1", "Rodent-2", "Rodent-3", "Rodent-4"),
                   labels = c("1", "2", "3", "4")) +  
  #scale_y_continuous(limits = c(0, 1)) +
  geom_text(aes(label = round(Value, 4)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 2.5, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0), 
        axis.ticks.y = element_blank()) 

output_path <- "/Users/caseymeili/Downloads/rodentlungs-hq.pdf"
ggsave(output_path, plot=last_plot())


######################################################
#########################################################################################################################
######################################################


# plotting of air filter 16S and ITS data 
# stacked bar charts for genera abundance by sample
# bar charts for Good's Coverage Index and number of high quality sequences per sample

library(tidyr)
library(ggplot2)
library(pals)
library(readxl)
library(dplyr)

# 16S PLOTTING
# import data
# data formatted as mothur taxonomy file with everything except taxa and counts per sample removed
# low abundance genera summed and grouped as "other genera"
bact.data <- read_excel("~/Desktop/Amplicon-Data/mothur/plotting-dust.xlsx", sheet="16S") 

# reshape the data for ggplot
df_long_bac <- gather(bact.data, key = "Sample", value = "Value", -taxon, -total)

# manually set the order of taxon and corresponding colors
taxon_order_bac <- unique(df_long_bac$taxon)
df_long_bac$taxon <- factor(df_long_bac$taxon, levels = taxon_order_bac)

# create the stacked bar chart
ggplot(df_long_bac, aes(x = Sample, y = Value, fill = taxon)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values=as.vector(stepped3(20)), name="Genus") +
  labs(title = "16S",
       x = "\nSample Site and Date\nPM10 Mass (ng/m^3)",
       y = NULL) +
  scale_x_discrete(breaks = c("Filter-1", "Filter-2", "Filter-3", "Filter-4"),
                   labels = c("Lindon 08-31-2019\n24211.82", "Hawthorne 09-09-2019\n26917.48", "Hawthorne 05-18-2020\n36321.78", "Hawthorne 06-05-2020\n40591.13")) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -3), 
        axis.text.y = element_blank(),        
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

# save plot
output_path <- "/Users/caseymeili/Downloads/filter-16S.pdf"
ggsave(output_path, plot=last_plot(), width = 10, height = 7)


######################################################

# ITS PLOTTING
# import data
# data formatted as mothur taxonomy file with everything except taxa and counts per sample removed
# low abundance genera summed and grouped as "other genera"

fungi.data <- read_excel("~/Desktop/Amplicon-Data/mothur/plotting-dust.xlsx", sheet="ITS")

# reshape the data for ggplot
df_long_fung <- gather(fungi.data, key = "Sample", value = "Value", -taxon, -total)

# manually set the order of taxon and corresponding colors
taxon_order_fung <- unique(df_long_fung$taxon)
df_long_fung$taxon <- factor(df_long_fung$taxon, levels = taxon_order_fung)

# create the stacked bar chart
ggplot(df_long_fung, aes(x = Sample, y = Value, fill = taxon)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values=as.vector(stepped3(20)), name="Genus") +
  labs(title = "ITS",
       x = "\nSample Site and Date\nPM10 Mass (ng/m^3)",
       y = NULL) +
  scale_x_discrete(breaks = c("Filter-1", "Filter-2", "Filter-3", "Filter-4"),
                   labels = c("Lindon 08-31-2019\n24211.82", "Hawthorne 09-09-2019\n26917.48", "Hawthorne 05-18-2020\n36321.78", "Hawthorne 06-05-2020\n40591.13")) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -3), 
        axis.text.y = element_blank(),        
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

# save plot
output_path <- "/Users/caseymeili/Downloads/filters-ITS.pdf"
ggsave(output_path, plot=last_plot(), width = 10, height = 7)


######################################################

# COVERAGE PLOTTING
# import data
# data is good's coverage index value for each sample computed using mother/1.48.0 command summary.single
coverage.data <- read_excel("~/Desktop/Amplicon-Data/mothur/plotting-dust.xlsx", sheet="Coverage")

# reshape the data for ggplot
coverage_long <- coverage.data %>%
  gather(key = "Sample", value = "Value", -Type)

# Filter out non-numeric rows
coverage_long_numeric <- coverage_long %>%
  filter(grepl("^\\d+\\.?\\d*$", Value))
coverage_long_numeric$Value <- as.numeric(as.character(coverage_long_numeric$Value))

# create bar chart
ggplot(coverage_long_numeric, aes(x = Sample, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values=c("#4880B8","#7AACD2"), name="Amplicon") +
  labs(title = "Coverage",
       x = "\nSample Site and Date\nPM10 Mass (ng/m^3)",
       y = "Good's Coverage Index") +
  scale_x_discrete(breaks = c("Filter-1", "Filter-2", "Filter-3", "Filter-4"),
                   labels = c("Lindon 08-31-2019\n24211.82", "Hawthorne 09-09-2019\n26917.48", "Hawthorne 05-18-2020\n36321.78", "Hawthorne 06-05-2020\n40591.13")) +  
  scale_y_continuous(limits = c(0, 1.01)) +
  geom_text(aes(label = round(Value, 4)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 2.5, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1.5),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

output_path <- "/Users/caseymeili/Downloads/filter-coverage.pdf"
ggsave(output_path, plot=last_plot())

######################################################

# HIGH QUALITY SEQUENCE NUMBER PLOTTING
# import data
# data is the number of sequences following every processing step, computed using mother/1.48.0 command count.groups
coverage.data <- read_excel("~/Desktop/Amplicon-Data/mothur/plotting-dust.xlsx", sheet="HQ")

# Create a new data frame for labels and numbers
label_data <- data.frame(labels = labels, pm10 = pm10)

# reshape the data for ggplot
coverage_long <- coverage.data %>%
  gather(key = "Sample", value = "Value", -Type)

# Filter out non-numeric rows
coverage_long_numeric <- coverage_long %>%
  filter(grepl("^\\d+\\.?\\d*$", Value))
coverage_long_numeric$Value <- as.numeric(as.character(coverage_long_numeric$Value))

# create bar chart
ggplot(coverage_long_numeric, aes(x = Sample, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values=c("#D55F2B", "#EE934F"), name="Amplicon") +
  labs(title = "Number of High Quality Sequences",
       x = "\nSample Site and Date\nPM10 Mass (ng/m^3)",
       y = "Number of High Quality Sequences") +
  scale_x_discrete(breaks = c("Filter-1", "Filter-2", "Filter-3", "Filter-4"),
                   labels = c("Lindon 08-31-2019\n24211.82", "Hawthorne 09-09-2019\n26917.48", "Hawthorne 05-18-2020\n36321.78", "Hawthorne 06-05-2020\n40591.13")) +  
  scale_y_continuous(limits = c(0, 1000000.01)) +
  geom_text(aes(label = round(Value, 4)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 2.5, color = "black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

output_path <- "/Users/caseymeili/Downloads/filter-hq.pdf"
ggsave(output_path, plot=last_plot())


######################################################
#########################################################################################################################
######################################################


# plotting of ITS data from Neotoma spp. scat (Weinstein et al, 2022, Symbiosis)
# stacked bar charts for genera abundance (whole dataset)

library(tidyr)
library(ggplot2)
library(pals)
library(readxl)
library(dplyr)

# import data
# two columns, genus and number of total hits
# low abundance genera summed and grouped as "other genera"
data <- read_excel("~/Desktop/Amplicon-Data/plotting.scat.xlsx") 

# manually set the order of taxon and corresponding colors
taxon_order <- unique(data$taxon)
data$taxon <- factor(data$taxon, levels = taxon_order)

# create the stacked bar chart
ggplot(data, aes(x = "", y = total, fill = taxon)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values=as.vector(stepped3(20)), name="Genus") +
  geom_col(width = .3) +
  labs(title = "ITS2 - Neotoma spp. Scat ",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = -3), 
        axis.text.y = element_blank(),        
        axis.ticks.y = element_blank()) 

# save plot
output_path <- "/Users/caseymeili/Downloads/scat-ITS.pdf"
ggsave(output_path, plot=last_plot())




