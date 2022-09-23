
library(ape)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(ggtree)
library(ggstance)
library(tibble)
library(purrr)
library(RColorBrewer)

id <- read_tsv("plink.dist.id",col_names = c("indname","famname"))
group <- read_tsv("140_pca_all.group",col_names = c("group1","group2","group3"))
distance <- read_tsv("plink.dist",col_names = F)
distance <- rbind(transpose(id[,1]),distance)
names(distance) <- distance %>% slice(1) %>% unlist()
distance <- distance %>% slice(-1)
tre <- nj(as.dist(distance))
tre <- root(tre, out=11)
id <- cbind(id,group)
p1 <- ggtree(tre,layout = "circular",size = 1.5)
id
png("140tree_len.png",width = 2000,height= 2000)
#ggtree(tre,layout = "circular",size = 1.5) %<+% id +
id %>% ggtree(tre,layout = "circular",size = 1.5)+
  geom_tippoint(aes(color = group3,shape = group2),size = 6)+
  geom_tiplab2(aes(color = group3), align = T, linetype = NA, size = 10, offset = 5)+
  theme_tree2(plot.margin = margin(0,0,0,0))+
  scale_colour_manual(name = "Group", values = colorRampPalette(brewer.pal(9,"Set1"))(19))+
  scale_shape_manual(name ="Group", values = c(19,17,15))+
  theme(
  legend.position = "none")
dev.off()
