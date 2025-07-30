library(ggplot2)

#Read in data
caudal_horns <- read.csv("caudal_horns_for_R_plotting_renamed.csv")

head(caudal_horns)

#remove Oki, F1, and BC1 
#remove strains whose sample number is less than 3
caudual_horns_species$strain <- factor(caudual_horns_species$strain,levels=c("c51","e10","f35","g53","k25","n16","o55","o56","p20","p21","p22","p44","p50T","u48","Sakado"))

p1 <- ggplot(caudual_horns_species, aes(x=strain,y=length_cm, color=species))+
	geom_boxplot(outlier.shape=NA) + 
	geom_jitter(size = .5) + 
	labs(x="Strain",y="Length (mm)", color="Species") +
	scale_color_manual(values=c('royalblue', 'orangered'), breaks=c('B. mori', 'B. mandarina'), labels=c(expression(italic('Bombyx mori')), expression(italic('B. mandarina'))))+
	scale_y_continuous(expand=c(0,0), limit=c(0, 5.5))+
	theme_classic() + theme(legend.position=c(0.2, 0.72),legend.background=element_rect(fill="white",color="black"))

pdf("strains_13F1_version_no_Oki.pdf", height=2.5, width=4.2)
p1
dev.off()

