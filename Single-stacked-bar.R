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
