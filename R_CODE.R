# Clear memory
rm(list = ls())

# Libaries
library(ggplot2)
library(plyr)

# Working directory
setwd("D:/NeuAll/R/RestingSites_Arten")
setwd("C:/Users/RenkeLuehken/Google Drive/R/RestingSites_Arten")

# Read a file
data <- read.csv("RS_Daten_Results.csv",
                 header=TRUE, sep=";", na.strings="")

# Sum of all specimens
sum_specimens <- ddply(data, 
                       .(Species.Taxon), 
                       summarize, summe=sum(Sum), .drop = F)

# Sorted sum of all specimens
sum_specimens[order(sum_specimens$summe, decreasing = F),]

# Subset dataset, only species with more than 10 specimens
data_sub <- subset(data,Species.Taxon %in% subset(sum_specimens, sum_specimens[,2] >= 10)[,1])
data_sub2 <- subset(data_sub, !(Position %in% c("NA")))
data_sub3 <- subset(data_sub2, !(Size..l. %in% c("NA")))

# Drop unused levels
data_sub3 <- droplevels(data_sub3)

# Transform dataset (fill gaps)
transform_data <- ddply(data_sub3, 
                        .(Group_Site, Size..l., Position,
                          RS.Tree.Type,
                          KW, Species.Taxon), 
                        summarize, Summe = sum(Sum),
                        .drop = F)

# Order of the levels
transform_data$Size..l. <- factor(transform_data$Size..l., 
                                      levels = c("0.04",
                                                 "76",
                                                 "162"))

# Order of the levels
transform_data$Position <- factor(transform_data$Position, 
                                      levels = c("Ground",
                                                 "Handhight",
                                                 "Elevated"))

# Sum of all mosquito specimens per size, position, tree type and sampling site
transform_data_box <- ddply(transform_data, 
                          .(Group_Site, Size..l., Position,
                            RS.Tree.Type), 
                          summarize, 
                          sum  = sum(Summe, na.rm = T),
                          .drop = F)

# Plot
png(file = "boxplots.jpg",
    width = 5, height=6, units = 'in', res =500)
ggplot(transform_data_box, aes(x = Position, y = sum)) +
  geom_boxplot(aes(fill = as.factor(RS.Tree.Type))) + 
  ylab("specimens") +
  facet_wrap(~ Size..l., scales = "free_y", nrow = 3)+
  scale_fill_manual(guide = guide_legend(title = "tree"),values=c("conifer"="firebrick1","deciduous tree"="steelblue"))+
  theme_bw()
dev.off()

# Number of species
LL <- ddply(data_sub, .(Group_Site, KW, RS.Tree.Type, 
                        Size..l., Position), 
            summarize, summe=length(unique(Species.Taxon)))

LL <- subset(LL, !(RS.Tree.Type %in% c("L Vegeation", "N Vegeation", "Vegetation")))

# Order of the levels
LL$Size..l. <- factor(LL$Size..l., 
                      levels = c("0.04",
                                 "76",
                                 "162"))

# Order of the levels
LL$Position <- factor(LL$Position, 
                      levels = c("Ground",
                                 "Handhight",
                                 "Elevated"))

# Plot
png(file = "species.jpg",
    width = 5, height=6, units = 'in', res =500)
ggplot(LL, aes(x = Position, y = summe)) +
  geom_boxplot(aes(fill = as.factor(RS.Tree.Type))) + 
  ylab("specimens") +
  facet_wrap(~ Size..l., scales = "free_y", nrow = 3)+
  scale_fill_manual(guide = guide_legend(title = "tree"),values=c("conifer"="firebrick1","deciduous tree"="steelblue"))+
  theme_bw()
dev.off()

# Phenology, colour differentiation per size
transform_data_phen_size <- ddply(transform_data, 
                           .(KW, Size..l., Species.Taxon), 
                           summarise, 
                           sum  = mean(Summe, na.rm = T),
                           sd = sd(Summe, na.rm = T))

# Transfrom NA to zero
transform_data_phen_size$sum[is.na(transform_data_phen_size$sum)] <- 0
transform_data_phen_size$sd[is.na(transform_data_phen_size$sd)] <- 0

# Plot
png(file = "phenolog_size.jpg",
    width = 7, height=5.5, units = 'in', res =500)
ggplot(transform_data_phen_size, aes(x = KW, y = sum)) +
  geom_point(aes(group = Size..l., col = Size..l.)) + 
  geom_line(aes(group = Size..l., col = Size..l.)) + 
  #geom_errorbar(aes(ymin = sum-sd, ymax = sum+sd, group = Size..l., col = Size..l.))+
  ylab("specimens") +
  scale_color_manual(guide = guide_legend(title = "size"),values=c("green", "firebrick1","steelblue"))+
  facet_wrap(~ Species.Taxon, scales = "free_y")+
  theme_bw()
dev.off()

# Phenology, colour differentiation per position
transform_data_phen_size <- ddply(transform_data, 
                                  .(KW, Position, Species.Taxon), 
                                  summarise, 
                                  sum  = mean(Summe, na.rm = T),
                                  sd = sd(Summe, na.rm = T))

# Transfrom NA to zero
transform_data_phen_size$sum[is.na(transform_data_phen_size$sum)] <- 0
transform_data_phen_size$sd[is.na(transform_data_phen_size$sd)] <- 0

# Plot
png(file = "phenolog_position.jpg",
    width = 7, height=5.5, units = 'in', res =500)
ggplot(transform_data_phen_size, aes(x = KW, y = sum)) +
  geom_point(aes(group = Position, col = Position)) + 
  geom_line(aes(group = Position, col = Position)) + 
  #geom_errorbar(aes(ymin = sum-sd, ymax = sum+sd, group = Size..l., col = Size..l.))+
  ylab("specimens") +
  scale_color_manual(guide = guide_legend(title = "size"),values=c("green", "firebrick1","steelblue"))+
  facet_wrap(~ Species.Taxon, scales = "free_y")+
  theme_bw()
dev.off()


# Phenology, colour differentiation per RS.Tree.Type
transform_data_phen_size <- ddply(transform_data, 
                                  .(KW, RS.Tree.Type, Species.Taxon), 
                                  summarise, 
                                  sum  = mean(Summe, na.rm = T),
                                  sd = sd(Summe, na.rm = T))

# Transfrom NA to zero
transform_data_phen_size$sum[is.na(transform_data_phen_size$sum)] <- 0
transform_data_phen_size$sd[is.na(transform_data_phen_size$sd)] <- 0

# Plot
png(file = "phenolog_RS.Tree.Type.jpg",
    width = 7, height=5.5, units = 'in', res =500)
ggplot(transform_data_phen_size, aes(x = KW, y = sum)) +
  geom_point(aes(group = RS.Tree.Type, col = RS.Tree.Type)) + 
  geom_line(aes(group = RS.Tree.Type, col = RS.Tree.Type)) + 
  #geom_errorbar(aes(ymin = sum-sd, ymax = sum+sd, group = Size..l., col = Size..l.))+
  ylab("specimens") +
  scale_color_manual(guide = guide_legend(title = "size"),values=c("green", "firebrick1","steelblue"))+
  facet_wrap(~ Species.Taxon, scales = "free_y")+
  theme_bw()
dev.off()

# n species, colour differentiation per size
transform_data2 <- subset(transform_data, Summe > 0)
transform_data_phen_species_size <- ddply(transform_data2, 
                                  .(KW, Size..l.), 
                                  summarise, 
                                  sum  = length(unique(Species.Taxon)),
                                  .drop = F)

# Plot
png(file = "phenolog_n_species.jpg",
    width = 7, height=5.5, units = 'in', res =500)
ggplot(transform_data_phen_species_size, aes(x = KW, y = sum)) +
  geom_point(aes(group = Size..l., col = Size..l.)) + 
  geom_line(aes(group = Size..l., col = Size..l.)) + 
  #geom_errorbar(aes(ymin = sum-sd, ymax = sum+sd, group = Size..l., col = Size..l.))+
  ylab("n species") +
  scale_color_manual(guide = guide_legend(title = "size"),values=c("green", "firebrick1","steelblue"))+
  theme_bw()
dev.off()