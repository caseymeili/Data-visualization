# Box plots for AGF genera abundance in the rumen 
# plots for all sequences, grouped by family and species and individual family

setwd("~/Desktop/R/")
library(ggplot2)
library(vegan)
library(ape)
library(reshape2)
library(readxl)
library(ggpubr)

# plots contain AGF genera above 1% abundance in at least 50% of one animal species
# inputs contain host identity (family, species etc) in column one and a column 
# for each AGF genus and abundance data per sample

# box plots of all sequences
# import data
all<-read_excel("~/Desktop/R/abund_1000.xlsx")
alldata <- melt(all,id.vars='Group', measure.vars=c("Neocallimastix", "Caecomyces", "Orpinomyces", "Cyllamyces", "Piromyces", "Anaeromyces", "Capellomyces", "Feramyces", "Liebetanzomyces", "Khyollomyces", "AL3", "SK3", "RH4", "NY42", "NY9", "NY8", "NY5", "NY11", "NY4", "NY6"))

# plotting
allseq<-ggplot(alldata, aes(x=variable, y=value)) + 
  geom_boxplot(outlier.size = 0.2, fill="#7BC9EF") + 
  xlab(NULL) + ylab("Percent Abundance") +
  scale_color_discrete(name="") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_text(angle=90,hjust=1))
allseq

# save
ggsave("all_seq_box.jpg", plot=last_plot(), height=1280, units="px")


# grouped box plot by host animal family
# import data
all<-read_excel("~/Desktop/R/allfamily.xlsx")
melted <- melt(all,id.vars='Family', measure.vars=c("Neocallimastix", "Caecomyces", "Orpinomyces", "Cyllamyces", "Piromyces", "Anaeromyces", "Capellomyces", "Feramyces", "Liebetanzomyces", "Khyollomyces", "AL3", "SK3", "RH4", "NY42", "NY9", "NY8", "NY5", "NY11", "NY4", "NY6"))

# plotting
allbox<-ggplot(melted, aes(x=variable, y=value, fill=Family)) + 
  geom_boxplot(outlier.size = 0.2) + 
  scale_y_continuous(trans='log10') +
  xlab(NULL) + ylab("Percent Abundance") +
  scale_color_discrete(name="") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("#4E67C8", "#7BC9EF", "#B5E86A", "#7BCBB0")) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 
allbox

#save
ggsave("all_family_box.jpg", plot=last_plot(), height=1280, units="px")


# grouped box plot by host animal species
# import data
all<-read_excel("~/Desktop/R/allspecies.xlsx")
melted <- melt(all,id.vars='Species', measure.vars=c("Neocallimastix", "Caecomyces", "Orpinomyces", "Cyllamyces", "Piromyces", "Anaeromyces", "Capellomyces", "Feramyces", "Liebetanzomyces", "Khyollomyces", "AL3", "SK3", "RH4", "NY42", "NY9", "NY8", "NY5", "NY11", "NY4", "NY6"))

# plotting
allbox<-ggplot(melted, aes(x=variable, y=value, fill=Species)) + 
  geom_boxplot(outlier.size = 0.2) + 
  scale_y_continuous(trans='log10') +
  xlab(NULL) + ylab("Percent Abundance") +
  scale_color_discrete(name="") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("#2C397B", "#418DB8", "#77A737", "#488870")) +
  theme(axis.text.x=element_text(angle=90,hjust=1))
allbox

# save
ggsave("all_species_box.jpg", plot=last_plot(), height=1280, units="px")


# plots by animal family
# Antilocapridae
# import data
allant<-read_excel("~/Desktop/R/allant.xlsx")
allant2 <- melt(all,id.vars='Group', measure.vars=c("Neocallimastix", "Caecomyces", "Orpinomyces", "Cyllamyces", "Piromyces", "Anaeromyces", "Capellomyces", "Feramyces", "Liebetanzomyces", "Khyollomyces", "AL3", "SK3", "RH4", "NewGenus42", "NewGenus9", "NewGenus8", "NewGenus5", "NewGenus11", "NewGenus4", "NewGenus6"))

# plotting
allbox<-ggplot(alldata) + geom_boxplot(aes(x=variable, y=value)) + 
  ggtitle("Antilocapridae") + xlab(NULL) + ylab("Average % Abundance") +
  theme_classic2() + scale_color_discrete(name="") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + expand_limits(y = 100)
allbox

# save 
ggsave("antilocapridae_alpha_box.jpg", plot=last_plot())

#Camelidae
# import data
all<-read_excel("~/Desktop/R/allcam.xlsx")
allcam <- melt(all,id.vars='Group', measure.vars=c("Neocallimastix", "Caecomyces", "Orpinomyces", "Cyllamyces", "Piromyces", "Anaeromyces", "Capellomyces", "Feramyces", "Liebetanzomyces", "Khyollomyces", "AL3", "SK3", "RH4", "NewGenus42", "NewGenus9", "NewGenus8", "NewGenus5", "NewGenus11", "NewGenus4", "NewGenus6"))

# plotting
allbox<-ggplot(alldata) + geom_boxplot(aes(x=variable, y=value)) + 
  ggtitle("Camelidae") + xlab(NULL) + ylab("Average % Abundance") +
  theme_classic2() + scale_color_discrete(name="") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + expand_limits(y = 100)
allbox

# save 
ggsave("camelidae_alpha_box.jpg", plot=last_plot())
