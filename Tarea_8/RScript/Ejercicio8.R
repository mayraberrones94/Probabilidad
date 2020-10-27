library(readr)
library(tidyverse)
library(ggplot2)

file_list = list.files(
  path = '../input', 
  pattern = '*.csv',
  recursive = T,
  full.names = T
)
cat('Pathes of the files: ', file_list)

covid_19_general_MX <- read_csv("covid-19_general_MX.csv")
ENTIDADES <- read_csv("ENTIDADES.csv")
print(dim(covid_19_general_MX ))
covid_19_general_MX  %>% head

counts = table(covid_19_general_MX$ENTIDAD_UM)
barplot(counts, 
        col = c("coral", "cyan4", "brown", 
                "blueviolet", "darkgoldenrod", 
                "darkcyan", "brown2",
                "firebrick3", "darkorange",
                "gold",
                "deepskyblue2", "green4",
                "royalblue", "palevioletred",
                "purple", "orangered",
                "seagreen"))
hist(covid_19_general_MX$ENTIDAD_UM, breaks = 32)

# set up cut-off values 
breaks <- c(0,10,20,30,40,50,60,70,80,90,100)
# specify interval/bin labels
tags <- c("[0-10)","[10-20)", "[20-30)", "[30-40)", "[40-50)",
          "[50-60)","[60-70)", "[70-80)","[80-90)", "[90-100)")
# bucketing values into bins
group_tags <- cut(covid_19_general_MX$EDAD, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
# inspect bins
summary(group_tags)

dev.off()

ggplot(data = as_tibble(group_tags), mapping = aes(x=value)) + 
  geom_bar(fill="bisque",color="white",alpha=0.7) + 
  stat_count(geom="text", aes(label=sprintf("%.4f",..count../length(group_tags))), vjust=-0.5) +
  labs(x='', y = ' ') +
  theme_minimal() 

sector_counts = table(covid_19_general_MX$SECTOR)
options(scipen=999)
barplot(sector_counts,  col = c("coral", "cyan4",  
                                "blueviolet", "darkgoldenrod", 
                                "darkcyan", "brown2",
                                "firebrick3", "darkorange",
                                "gold"))

library(ggplot2)
sector <- read_csv("SECTOR.csv")

da = data.frame(group = c("CRUZ ROJA",  "DIF", "ESTATAL","IMSS","IMSS-BIENESTAR",
                          "ISSSTE" ,"MUNICIPAL","PEMEX", "PRIVADA" ,"SEDENA" ,
                          "SEMAR", "SSA", "UNIVERSITARIO" ,"NO ESPECIFICADO"),
                  value = c(163, 142, 18068, 236968,  33471,852,8377,33845,4848,5286,530880,688,6020, 0))
ggplot(da, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void() # remove background, grid, numeric labels


da2 = data.frame(group = c("Positive", "Negative"),
                value = c(370713, 419350))
ggplot(da2, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void() # remove background, grid, numeric labels


tp = 355144
#tp = 259128

tn=415157
#tn=408866

fp=15570
#fp=111584

fn= 4194
#fn= 10483

loc = 0.00293

acc = (tp + tn) / (tp+tn+fn+fp )
pre = tp / (tp + fp)
sen = tp / (tp + fn)
spe = tn / (tn + fp)