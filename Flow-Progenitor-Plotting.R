# MPP Flow Plotting

library(readxl)
library(tidyverse)
library(ggpubr)
library(Hmisc)

# set working directory 
setwd("/Users/caseymeili/Desktop/ROUND/PPT")

# import data 
# data contains columns of treatment (C. dub vs. germ-free) and statistic, exported from FlowJo
MPP <- read_excel("/Users/caseymeili/Desktop/ROUND/PPT/CMeili-Flow-Rotation-final.xls", sheet="MPPs")
CP <- read_excel("/Users/caseymeili/Desktop/ROUND/PPT/CMeili-Flow-Rotation-final.xls", sheet="CPs")


# MULTI-POTENT PROGENITORS (MPPS)
# subset the data for MPP GF and MPP CD treatments
MPP_subset <- subset(MPP, MPP$Treatment...1 %in% c("MPP GF", "MPP CD", "MPP SPF"))

# rename the columns
names(MPP_subset)[names(MPP_subset) == "Treatment...1"] <- "Treatment"
names(MPP_subset)[names(MPP_subset) == "Statistic...2"] <- "Statistic"

# check normality 
shapiro.test(MPP_subset$Statistic) # Normally distributed, use T-test

# reorder treatment factor so that GF comes first
MPP_subset$Treatment <- factor(MPP_subset$Treatment, levels = c("MPP SPF", "MPP GF", "MPP CD"))

# rename the levels of the treatment factor
levels(MPP_subset$Treatment) <- c("SPF", "Germ Free", "C. dub")

# bar plot of average % MPPs
ggplot(MPP_subset, aes(x = Treatment, y = Statistic, fill = Treatment)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "summary", fun = "mean", color = "black") +  # Add colored bars
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), color = "black") +  # Add individual data points
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2, color = "black") +  # Add error bars representing standard deviations
  labs(title = "Multi-potent progenitors (MPPs)",
       x = NULL, y = "% MPPs") +
  scale_fill_manual(values = c("#E39459", "#E39459", "#E39459")) +  # Set colors for bars
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.text = element_text(size = 11)) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     comparisons = list(c("Germ Free", "C. dub")),
                     label.y = 89)



# save plot
output_path <- "/Users/caseymeili/Downloads/MPP-comp-final.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)



# MPP3 - MYELOID BIASED
# subset the data for MPP3 GF and MPP3 CD treatments
MPP3_subset <- subset(MPP, MPP$Treatment...4 %in% c("MPP3 GF", "MPP3 CD", "MPP3 SPF"))

# rename the columns
names(MPP3_subset)[names(MPP3_subset) == "Treatment...4"] <- "Treatment"
names(MPP3_subset)[names(MPP3_subset) == "Statistic...5"] <- "Statistic"

# check normality 
shapiro.test(MPP3_subset$Statistic) # not normally distributed, use wilcox

# reorder treatment factor so that GF comes first
MPP3_subset$Treatment <- factor(MPP3_subset$Treatment, levels = c("MPP3 SPF", "MPP3 GF", "MPP3 CD"))

# rename the levels of the treatment factor
levels(MPP3_subset$Treatment) <- c("SPF", "Germ Free", "C. dub")


# bar plot of % MPP3s
ggplot(MPP3_subset, aes(x = Treatment, y = Statistic, fill = Treatment)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "summary", fun = "mean", color = "black") +  # Add colored bars
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), color = "black") +  # Add individual data points
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2, color = "black") +  # Add error bars representing standard deviations
  labs(title = "MPP3 - Myeloid-biased",
       x = NULL, y = "% MPP3s") +
  scale_fill_manual(values = c("#5D76B5", "#5D76B5", "#5D76B5")) +  # Set colors for bars
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.text = element_text(size = 11)) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     comparisons = list(c("Germ Free", "C. dub")),
                     label.y = 34)


# save plot
output_path <- "/Users/caseymeili/Downloads/MPP3-comp-final.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)



# MPP4 - LYMPHOID BIASED
# subset the data for MPP4 GF and MPP4 CD treatments
MPP4_subset <- subset(MPP, MPP$Treatment...7 %in% c("MPP4 GF", "MPP4 CD", "MPP4 SPF"))

# rename the columns
names(MPP4_subset)[names(MPP4_subset) == "Treatment...7"] <- "Treatment"
names(MPP4_subset)[names(MPP4_subset) == "Statistic...8"] <- "Statistic"

# check normality 
shapiro.test(MPP4_subset$Statistic) # Normally distributed, use T-test

# reorder treatment factor so that GF comes first
MPP4_subset$Treatment <- factor(MPP4_subset$Treatment, levels = c("MPP4 SPF", "MPP4 GF", "MPP4 CD"))

# rename the levels of the treatment factor
levels(MPP4_subset$Treatment) <- c("SPF", "Germ Free", "C. dub")

# bar plot of average % MPP4s
ggplot(MPP4_subset, aes(x = Treatment, y = Statistic, fill = Treatment)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "summary", fun = "mean", color = "black") +  # Add colored bars
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), color = "black") +  # Add individual data points
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2, color = "black") +  # Add error bars representing standard deviations
  labs(title = "MPP4 - Lymphoid-biased",
       x = NULL, y = "% MPP4s") +
  scale_fill_manual(values = c("#A9CD7C", "#A9CD7C", "#A9CD7C")) +  # Set colors for bars
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.text = element_text(size = 11)) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     comparisons = list(c("Germ Free", "C. dub")),
                     label.y = 44)


# save plot
output_path <- "/Users/caseymeili/Downloads/MPP4-comp-final.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)



# COMMON PROGENITORS (CPS)
# subset the data for CMP GF and CMP CD treatments
CP_subset <- subset(CP, CP$Treatment...1 %in% c("CMP GF", "CMP CD", "CMP SPF"))

# rename the columns
names(CP_subset)[names(CP_subset) == "Treatment...1"] <- "Treatment"
names(CP_subset)[names(CP_subset) == "Statistic...2"] <- "Statistic"

# check normality 
shapiro.test(CP_subset$Statistic) # Normally distributed, use T-test

# reorder treatment factor so that GF comes first
CP_subset$Treatment <- factor(CP_subset$Treatment, levels = c("CMP SPF", "CMP GF", "CMP CD"))

# rename the levels of the treatment factor
levels(CP_subset$Treatment) <- c("SPF", "Germ Free", "C. dub")

# bar plot of % CMPs

ggplot(CP_subset, aes(x = Treatment, y = Statistic, fill = Treatment)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "summary", fun = "mean", color = "black") +  # Add colored bars
  geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = .9), color = "black") +  # Add individual data points
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2, color = "black") +  # Add error bars representing standard deviations
  labs(title = "Common Myeloid Progenitor (CMP)",
       x = NULL, y = "% CMPs") +
  scale_fill_manual(values = c("#E99B92", "#E99B92", "#E99B92")) +  # Set colors for bars
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.text = element_text(size = 11)) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     comparisons = list(c("Germ Free", "C. dub")),
                     label.y = 21.5)

# save plot
output_path <- "/Users/caseymeili/Downloads/CMP-comp-final.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)



# GMP - GRANULOCYTE-MACROPHAGE PROGENITOR
# subset the data for GMP GF and GMP CD treatments
GMP_subset <- subset(CP, CP$Treatment...4 %in% c("GMP GF", "GMP CD", "GMP SPF"))

# rename the columns
names(GMP_subset)[names(GMP_subset) == "Treatment...4"] <- "Treatment"
names(GMP_subset)[names(GMP_subset) == "Statistic...5"] <- "Statistic"

# check normality 
shapiro.test(GMP_subset$Statistic) # not normally distributed, use wilcox

# reorder treatment factor so that GF comes first
GMP_subset$Treatment <- factor(GMP_subset$Treatment, levels = c("GMP SPF", "GMP GF", "GMP CD"))

# rename the levels of the treatment factor
levels(GMP_subset$Treatment) <- c("SPF", "Germ Free", "C. dub")

# bar plot of % GMPs
ggplot(GMP_subset, aes(x = Treatment, y = Statistic, fill = Treatment)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "summary", fun = "mean", color = "black") +  # Add colored bars
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), color = "black") +  # Add individual data points
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2, color = "black") +  # Add error bars representing standard deviations
  labs(title = "Granulocyte-Macrophage Progenitor (GMP)",
       x = NULL, y = "% GMPs") +
  scale_fill_manual(values = c("#60AEDF", "#60AEDF", "#60AEDF")) +  # Set colors for bars
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.text = element_text(size = 11)) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     comparisons = list(c("Germ Free", "C. dub")),
                     label.y = 12.5)


# save plot
output_path <- "/Users/caseymeili/Downloads/GMP-comp-final.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)


# MEP = MEGAKARYOCYTE-ERYTHOID PROGENITOR (MEP)
# subset the data for MEP GF and MEP CD treatments
MEP_subset <- subset(CP, CP$Treatment...7 %in% c("MEP GF", "MEP CD", "MEP SPF"))

# rename the columns
names(MEP_subset)[names(MEP_subset) == "Treatment...7"] <- "Treatment"
names(MEP_subset)[names(MEP_subset) == "Statistic...8"] <- "Statistic"

# check normality 
shapiro.test(MEP_subset$Statistic) # Normally distributed, use T-test

# reorder treatment factor so that GF comes first
MEP_subset$Treatment <- factor(MEP_subset$Treatment, levels = c("MEP SPF", "MEP GF", "MEP CD"))

# rename the levels of the treatment factor
levels(MEP_subset$Treatment) <- c("SPF", "Germ Free", "C. dub")

# bar plot of % MEPs
ggplot(MEP_subset, aes(x = Treatment, y = Statistic, fill = Treatment)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "summary", fun = "mean", color = "black") +  # Add colored bars
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), color = "black") +  # Add individual data points
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2, color = "black") +  # Add error bars representing standard deviations
  labs(title = "Megakaryocyte-Erythroid Progenitor (MEP)",
       x = NULL, y = "% MEPs") +
  scale_fill_manual(values = c("#98396B", "#98396B", "#98396B")) +  # Set colors for bars
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.text = element_text(size = 11)) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     comparisons = list(c("Germ Free", "C. dub")),
                     label.y = 75)


# save plot
output_path <- "/Users/caseymeili/Downloads/MEP-comp-final.pdf"
ggsave(output_path, plot=last_plot(), height=4, width=4)


################################################################


library(readxl)
library(tidyverse)

# set working directory 
setwd("/Users/caseymeili/Desktop/ROUND")

# import data 
SI <- read_excel("/Users/caseymeili/Desktop/ROUND/colonization-cfus.xlsx", sheet="Small")
colon <- read_excel("/Users/caseymeili/Desktop/ROUND/colonization-cfus.xlsx", sheet="Colon")
panc <- read_excel("/Users/caseymeili/Desktop/ROUND/colonization-cfus.xlsx", sheet="PANC")
BM <- read_excel("/Users/caseymeili/Desktop/ROUND/colonization-cfus.xlsx", sheet="BM")
Exp1 <- read_excel("/Users/caseymeili/Desktop/ROUND/colonization-cfus.xlsx", sheet="Exp1")

# Reorder levels of mouse columns
SI$Mouse <- factor(SI$Mouse, levels = c("P5", "P10", "P15"))
colon$Mouse <- factor(colon$Mouse, levels = c("P5", "P10", "P15", "P20"))
panc$Mouse <- factor(panc$Mouse, levels = c("P5", "P10", "P15"))
BM$Mouse <- factor(panc$Mouse, levels = c("P5", "P10", "P15"))
Exp1$Time <- factor(Exp1$Time, levels = c("1 day", "3 day", "5 day"))

# Create a new variable for x-axis labels with "(n=3)" or "(n=5)" added
SI$Mouse_with_n <- ifelse(SI$Mouse == "P5" | SI$Mouse == "P10", paste(SI$Mouse, "(n=3)"), 
                          ifelse(SI$Mouse == "P15", paste(SI$Mouse, "(n=5)"), NA))

# convert Mouse_with_n to factor
SI$Mouse_with_n <- factor(SI$Mouse_with_n, levels = c("P5 (n=3)", "P10 (n=3)", "P15 (n=5)"))

my.labels <- c("P5\n(n=3)","P10\n(n=3)", "P15\n(n=5)")

# Plot with modified x-axis labels
ggplot(SI, aes(x = Mouse_with_n, y = CFU, color = Sac)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 2) + 
  scale_color_manual(values=c("#5D76B5", "#5D76B5", "#5D76B5")) + 
  labs(title = "Small Intestine",
       x = "Time point",
       y = "CFUs per gram",
       color = "Sac") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(size = 11)) +
  scale_x_discrete(labels= my.labels)

output_path <- "/Users/caseymeili/Downloads/SI-colonization2.pdf"
ggsave(output_path, plot=last_plot(), height=5, width=2.5)

# COLON
colon$Mouse_with_n <- ifelse(colon$Mouse == "P5" | colon$Mouse == "P10", paste(colon$Mouse, "(n=3)"), 
                             ifelse(colon$Mouse == "P15", paste(colon$Mouse, "(n=9)"),
                                    ifelse(colon$Mouse == "P20", paste(colon$Mouse, "(n=5)"), NA)))

# convert Mouse_with_n to factor
colon$Mouse_with_n <- factor(colon$Mouse_with_n, levels = c("P5 (n=3)", "P10 (n=3)", "P15 (n=9)", "P20 (n=5)"))

my.labels <- c("P5\n(n=3)","P10\n(n=3)", "P15\n(n=9)", "P20\n(n=5)")

# plotting colon
ggplot(colon, aes(x = Mouse, y = CFU, color = Sac)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 2.5) + 
  scale_color_manual(values=c("#5D76B5", "#5D76B5", "#5D76B5","#5D76B5")) + 
  labs(title = "Colon",
       x = "Time point",
       y = "CFUs per gram",
       color = "Sac") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(size = 11)) + 
  scale_x_discrete(labels= my.labels)


output_path <- "/Users/caseymeili/Downloads/colon-colonization2.pdf"
ggsave(output_path, plot=last_plot(), height=5, width=2.5)


# PANCREAS
# Create a new variable for x-axis labels with "(n=3)" or "(n=5)" added
panc$Mouse_with_n <- ifelse(panc$Mouse == "P5" | panc$Mouse == "P10", paste(panc$Mouse, "(n=3)"), 
                            ifelse(panc$Mouse == "P15", paste(panc$Mouse, "(n=5)"), NA))

# convert Mouse_with_n to factor
panc$Mouse_with_n <- factor(panc$Mouse_with_n, levels = c("P5 (n=3)", "P10 (n=3)", "P15 (n=5)"))

my.labels <- c("P5\n(n=3)","P10\n(n=3)", "P15\n(n=5)")

# plotting pancreas
ggplot(panc, aes(x = Mouse, y = CFU, color = Sac)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 2.5) + 
  scale_color_manual(values=c("#A5C879", "#A5C879", "#A5C879")) + 
  labs(title = "Pancreas",
       x = "Time point",
       y = "CFUs per gram",
       color = "Sac") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(size = 11)) +
  scale_x_discrete(labels= my.labels)


output_path <- "/Users/caseymeili/Downloads/panc-colonization2.pdf"
ggsave(output_path, plot=last_plot(), height=5, width=2.5)

# BONE MARROW
BM$Mouse_with_n <- ifelse(BM$Mouse == "P5" | BM$Mouse == "P10", paste(BM$Mouse, "(n=3)"), 
                          ifelse(BM$Mouse == "P15", paste(BM$Mouse, "(n=5)"), NA))

# convert Mouse_with_n to factor
BM$Mouse_with_n <- factor(BM$Mouse_with_n, levels = c("P5 (n=3)", "P10 (n=3)", "P15 (n=5)"))

my.labels <- c("P5\n(n=3)","P10\n(n=3)", "P15\n(n=5)")

# plotting BM
ggplot(BM, aes(x = Mouse, y = CFU, color = Sac)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 2.5) + 
  scale_color_manual(values=c("#E39459", "#E39459", "#E39459")) + 
  labs(title = "Bone Marrow",
       x = "Time point",
       y = "CFUs per gram",
       color = "Sac") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(size = 11)) + 
  ylim(0, 600) +
  scale_x_discrete(labels= my.labels)


output_path <- "/Users/caseymeili/Downloads/BM-colonization2.pdf"
ggsave(output_path, plot=last_plot(), height=5, width=2.5)

# plotting experiment 1 
# Create a new variable for x-axis labels with "(n=4)" added
Exp1$Time_with_n <- paste(Exp1$Time, "\n(n=4)")

ggplot(Exp1, aes(x = Time_with_n, y = CFU, color = Time)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 2) + 
  scale_color_manual(values=c("#A5C879", "#A5C879", "#A5C879")) + 
  labs(title = "Pancreas",
       x = "Time point",
       y = "CFUs per gram") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(size = 11)) +
  ylim(0, 600) +
  scale_x_discrete(labels = function(x) gsub("_", " ", x)) 


output_path <- "/Users/caseymeili/Downloads/exp1-panc.pdf"
ggsave(output_path, plot=last_plot(), height=5, width=4)


