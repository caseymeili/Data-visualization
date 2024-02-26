# Bar graphs for HSPC and CP from FlowJo data
# Graphs contain plotted significance tests (t-test or u-test)

library(readxl)
library(tidyverse)
library(ggpubr)

# set working directory 
setwd("/Users/caseymeili/Desktop/ROUND/PPT")

# import data 
# data contains columns of treatment (C. dub vs. germ-free) and statistic, exported from FlowJo
MPP <- read_excel("/Users/caseymeili/Desktop/ROUND/PPT/CMeili-Flow-Rotation.xls", sheet="MPPs")
CP <- read_excel("/Users/caseymeili/Desktop/ROUND/PPT/CMeili-Flow-Rotation.xls", sheet="CPs")

# MULTI-POTENT PROGENITORS (MPPS)
# subset the data for MPP GF and MPP CD treatments
MPP_subset <- subset(MPP, MPP$Treatment...1 %in% c("MPP GF", "MPP CD"))

# rename the columns
names(MPP_subset)[names(MPP_subset) == "Treatment...1"] <- "Treatment"
names(MPP_subset)[names(MPP_subset) == "Statistic...2"] <- "Statistic"

# check normality 
shapiro.test(MPP_subset$Statistic) # Normally distributed, use T-test

# reorder treatment factor so that GF comes first
MPP_subset$Treatment <- factor(MPP_subset$Treatment, levels = c("MPP GF", "MPP CD"))

# rename the levels of the treatment factor
levels(MPP_subset$Treatment) <- c("Germ Free", "C. dub")

# bar plot of average % MPPs
ggplot(MPP_subset, aes(x = Treatment, y = Statistic, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Multi-potent progenitors (MPPs)",
       x = NULL, y = "Average % MPPs") +
  scale_fill_manual(values = c("#EDBD93", "#E39459")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.text = element_text(size = 11)) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     comparisons = list(c("Germ Free", "C. dub")),
                     label.y = 64)

# save plot
output_path <- "/Users/caseymeili/Downloads/MPP-comp.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)



# MPP3 - MYELOID BIASED
# subset the data for MPP3 GF and MPP3 CD treatments
MPP3_subset <- subset(MPP, MPP$Treatment...4 %in% c("MPP3 GF", "MPP3 CD"))

# rename the columns
names(MPP3_subset)[names(MPP3_subset) == "Treatment...4"] <- "Treatment"
names(MPP3_subset)[names(MPP3_subset) == "Statistic...5"] <- "Statistic"

# check normality 
shapiro.test(MPP3_subset$Statistic) # not normally distributed, use wilcox

# reorder treatment factor so that GF comes first
MPP3_subset$Treatment <- factor(MPP3_subset$Treatment, levels = c("MPP3 GF", "MPP3 CD"))

# rename the levels of the treatment factor
levels(MPP3_subset$Treatment) <- c("Germ Free", "C. dub")

# bar plot of average % MPP3s
ggplot(MPP3_subset, aes(x = Treatment, y = Statistic, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title="MPP3 - Myeloid Biased",
       x=NULL, y="Average % MPP3s") +
  scale_fill_manual(values=c("#98A3CF", "#5D76B5")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.text = element_text(size = 11)) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     comparisons = list(c("Germ Free", "C. dub")),
                     label.y = 26)

# save plot
output_path <- "/Users/caseymeili/Downloads/MPP3-comp.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)



# MPP4 - LYMPHOID BIASED
# subset the data for MPP4 GF and MPP4 CD treatments
MPP4_subset <- subset(MPP, MPP$Treatment...7 %in% c("MPP4 GF", "MPP4 CD"))

# rename the columns
names(MPP4_subset)[names(MPP4_subset) == "Treatment...7"] <- "Treatment"
names(MPP4_subset)[names(MPP4_subset) == "Statistic...8"] <- "Statistic"

# check normality 
shapiro.test(MPP4_subset$Statistic) # Normally distributed, use T-test

# reorder treatment factor so that GF comes first
MPP4_subset$Treatment <- factor(MPP4_subset$Treatment, levels = c("MPP4 GF", "MPP4 CD"))

# rename the levels of the treatment factor
levels(MPP4_subset$Treatment) <- c("Germ Free", "C. dub")

# bar plot of average % MPP4s
ggplot(MPP4_subset, aes(x = Treatment, y = Statistic, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title="MPP4 - Lymphoid Biased",
       x=NULL, y="Average % MPP4s") +
  scale_fill_manual(values=c("#C6DDA8", "#A9CD7C")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.text = element_text(size = 11)) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     comparisons = list(c("Germ Free", "C. dub")),
                     label.y = 63)

# save plot
output_path <- "/Users/caseymeili/Downloads/MPP4-comp.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)



# COMMON PROGENITORS (CPS)
# subset the data for CMP GF and CMP CD treatments
CP_subset <- subset(CP, CP$Treatment...1 %in% c("CMP GF", "CMP CD"))

# rename the columns
names(CP_subset)[names(CP_subset) == "Treatment...1"] <- "Treatment"
names(CP_subset)[names(CP_subset) == "Statistic...2"] <- "Statistic"

# check normality 
shapiro.test(CP_subset$Statistic) # Normally distributed, use T-test

# reorder treatment factor so that GF comes first
CP_subset$Treatment <- factor(CP_subset$Treatment, levels = c("CMP GF", "CMP CD"))

# rename the levels of the treatment factor
levels(CP_subset$Treatment) <- c("Germ Free", "C. dub")

# bar plot of average % CMPs
ggplot(CP_subset, aes(x = Treatment, y = Statistic, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Common Myeloid Progenitor (CMP)",
       x = NULL, y = "Average % CMPs") +
  scale_fill_manual(values = c("#F2C4BF", "#E99B92")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.text = element_text(size = 11)) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     comparisons = list(c("Germ Free", "C. dub")),
                     label.y = 100)

# save plot
output_path <- "/Users/caseymeili/Downloads/CMP-comp.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)



# GMP - GRANULOCYTE-MACROPHAGE PROGENITOR
# subset the data for GMP GF and GMP CD treatments
GMP_subset <- subset(CP, CP$Treatment...4 %in% c("GMP GF", "GMP CD"))

# rename the columns
names(GMP_subset)[names(GMP_subset) == "Treatment...4"] <- "Treatment"
names(GMP_subset)[names(GMP_subset) == "Statistic...5"] <- "Statistic"

# check normality 
shapiro.test(GMP_subset$Statistic) # not normally distributed, use wilcox

# reorder treatment factor so that GF comes first
GMP_subset$Treatment <- factor(GMP_subset$Treatment, levels = c("GMP GF", "GMP CD"))

# rename the levels of the treatment factor
levels(GMP_subset$Treatment) <- c("Germ Free", "C. dub")

# bar plot of average % GMPs
ggplot(GMP_subset, aes(x = Treatment, y = Statistic, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title="Granulocyte-Macrophage Progenitor (GMP)",
       x=NULL, y="Average % GMPs") +
  scale_fill_manual(values=c("#93C8E9", "#60AEDF")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.text = element_text(size = 11)) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     comparisons = list(c("Germ Free", "C. dub")),
                     label.y = 38)

# save plot
output_path <- "/Users/caseymeili/Downloads/GMP-comp.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)



# MEP = MEGAKARYOCYTE-ERYTHOID PROGENITOR (MEP)
# subset the data for MEP GF and MEP CD treatments
MEP_subset <- subset(CP, CP$Treatment...7 %in% c("MEP GF", "MEP CD"))

# rename the columns
names(MEP_subset)[names(MEP_subset) == "Treatment...7"] <- "Treatment"
names(MEP_subset)[names(MEP_subset) == "Statistic...8"] <- "Statistic"

# check normality 
shapiro.test(MEP_subset$Statistic) # Normally distributed, use T-test

# reorder treatment factor so that GF comes first
MEP_subset$Treatment <- factor(MEP_subset$Treatment, levels = c("MEP GF", "MEP CD"))

# rename the levels of the treatment factor
levels(MEP_subset$Treatment) <- c("Germ Free", "C. dub")

# bar plot of average % MEPs
ggplot(MEP_subset, aes(x = Treatment, y = Statistic, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title="Megakeryocyte-Erythoid Progenitor (MEP)",
       x=NULL, y="Average % MEPs") +
  scale_fill_manual(values=c("#AC5D87", "#98396B")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.text = element_text(size = 11)) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     comparisons = list(c("Germ Free", "C. dub")),
                     label.y = 19)

# save plot
output_path <- "/Users/caseymeili/Downloads/MEP-comp.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)


