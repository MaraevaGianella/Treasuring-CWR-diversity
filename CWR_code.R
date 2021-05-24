#-------------------------------------------------------------
#DATA PREPARATION AND COUNTS
#-------------------------------------------------------------
library(dplyr)
df<-read.delim(file="TARGET_total.csv",header = TRUE,sep=",")
colnames(df)
#mean % achieved per country, genepool, species, HPS
country<-group_by(df, Country) %>% summarize(achieved = mean(Achieved..))
country_HPS<-group_by(df, Country)%>% filter(Priority=="High")%>% summarize(achieved_HPS= mean(Achieved..))
genepool<-df%>%group_by(Crop.genepool,Genepool.level)%>%summarize(achieved_genepool = mean(Achieved..))
genepool_HPS<-df%>%group_by(Crop.genepool)%>% filter(Priority=="High")%>%summarize(achieved_genepool_HPS = mean(Achieved..))
species<-group_by(df, Accepted) %>% summarize(achieved_species = mean(Achieved..))
species_HPS<-group_by(df, Accepted) %>%filter(Priority=="High")%>% summarize(achieved_species_HPS = mean(Achieved..))

#number of populations collected per country, genepool, species, HPS
country<-group_by(df, Country) %>% summarize(collected = sum(Collected))
country_HPS<-group_by(df, Country)%>% filter(Priority=="High")%>% summarize(collected_HPS= sum(Collected))
genepool<-df%>%group_by(Crop.genepool,Genepool.level)%>%summarize(collected_genepool = sum(Collected))
genepool_HPS<-df%>%group_by(Crop.genepool)%>% filter(Priority=="High")%>%summarize(collected_genepool_HPS = sum(Collected))
species<-group_by(df, Accepted) %>% summarize(collected_species = sum(Collected))
species_HPS<-group_by(df, Accepted) %>%filter(Priority=="High")%>% summarize(collected_species_HPS =sum(Collected))

#other info
genepool<-df%>%group_by(Crop.genepool)%>%summarize(collected_genepool = sum(Collected))
agreedpop<-df%>%group_by(Crop.genepool)%>%summarize(agreed_gp_pop = sum(Agreed.Suggested))
agreedspecies<-df%>%group_by(Crop.genepool)%>%distinct(Accepted)%>%summarize(agreed_gp_species = n())

agreedspeciesHPS<-df%>%group_by(Crop.genepool)%>%distinct(Accepted, .keep_all = TRUE)%>%filter(Priority=="High")%>%summarize(agreed_gp_species = n())
agreedpopHPS<-df%>%group_by(Crop.genepool)%>%filter(Priority=="High")%>%summarize(agreed_gp_pop = sum(Agreed.Suggested))

df1<-read.delim(file="TARGET_total.csv",header = TRUE,sep=",")
agreedspecies<-df1%>%distinct(Accepted)
agreedpop<-df1%>%summarize(sum(Agreed.Suggested))
collectedspecies<-df1%>%group_by(Crop.genepool)%>%distinct(Accepted)%>%summarize(Collected = n())
collectedspeciesHPS<-df1%>%group_by(Crop.genepool)%>%filter(Priority=="High")%>%distinct(Accepted)%>%summarize(Collected = n())
coll_number<-collectedspecies%>%filter(Collected!=0)%>%summarize(collectedspecies = sum(Collected))
coll_number_HPS<-collectedspeciesHPS%>%summarize(collected_gp_species = sum(Collected))

df<-read.delim(file="TARGET_total.csv",header = TRUE,sep=",")
df1<-df%>%filter(Priority=="High")
HPS_pop_agreed<-df1%>%summarize(agreed = sum(Agreed.Suggested))
HPS_pop_coll<-df1%>%summarize(collected= sum(Collected))
HPS_species_agreed<-df1%>%distinct(Accepted)%>%summarize(HPS_species = n())
HPS_species_col<-df1%>%filter(Collected!=0)%>%distinct(Accepted)%>%summarize(HPS_species = n())


#extras
#-----------------------------------------------------------
df<-read.delim(file="extras1.csv", header=TRUE, sep=",")
species_extra<-df1%>%distinct(Accepted)
extra_species<-df%>%distinct(Accepted)%>%summarize(extra_species = n())
extra_crops<-df%>%filter(Crop.genepool=="Extra Crops")%>%summarize(sum(freq))
not_assigned<-df%>%filter(Crop.genepool=="Not assigned")%>%summarize(sum(freq))
not_assigned_species<-df%>%filter(Crop.genepool=="Not assigned")%>%distinct(Accepted)
total<-df%>%summarize(sum(freq))
assigned<-852-62-47
tot_crops_pop<-743+47
species_extra<-df%>%distinct(Accepted)%>%summarize(species = n())
extra_crops_species<-df%>%filter(Crop.genepool=="Extra Crops")%>%distinct(Accepted)
extra_crops_gp<-df%>%filter(Crop.genepool=="Extra Crops")%>%distinct(Crop.extra)
not_assigned<-df%>%filter(Crop.genepool=="Not assigned")%>%filter(Crop.genepool!="Extra Crops")%>%distinct(Accepted)
assigned<-df%>%filter(Crop.genepool!="Not assigned")%>%filter(Crop.genepool!="Extra Crops")%>%distinct(Accepted)

#-----------------------------------------------------------
#GRAPHS
#-----------------------------------------------------------
library(ggplot2)
library(dplyr)

df<-read.delim(file="genepool1.csv",header = TRUE,sep=";")

#barplot
g<-ggplot(data=df, mapping = aes(x=Level, y = sapply(Achieved, FUN=function(x) ifelse(x==0, -2,x) ), fill=Level),rescale = FALSE, interactive = FALSE, addlabel=TRUE, size = 2, colour = "grey")+
  geom_bar(stat='identity', width = 0.5)+theme_light() + theme(text = element_text(size=9, face="bold"),
                                                               legend.position = "right",strip.background =element_rect(fill="white"),
                                                               strip.text = element_text(face="bold", size=9, colour = "black")) 
g+facet_wrap(~Crop, labeller = label_wrap_gen(width=10))+ labs(y="Achieved populations %\n", x="\nGenepool level", fill="Genepool level")+
  scale_fill_discrete(name = "Genepool level", labels = c("Primary", "Secondary", "Tertiary", "Not assigned"))


#Bubble plot

df<-read.delim(file="bubble.csv",header = TRUE,sep=";")

names(df)[1]<-"Crop"
g<-ggplot(df, aes(x=Pop, y=Species, size = Taxa, fill=Crop)) +
  geom_point(alpha=0.5, shape=21, color="black")+
  geom_text(aes(label=Crop), size=3, fontface="bold", check_overlap = T,vjust="inward",hjust="inward")+ scale_fill_discrete(guide=F)+
  scale_size( limits = c(1,35), range=c(1,15), name="Number of taxa\n      per crop")+
  theme_light()+labs(x="\nCollected populations (%)", y="Collected taxa (%)\n") +
  theme(axis.title = element_text(face = "bold", size=15),
        legend.title = element_text(face = "bold", size=9),
        legend.text = element_text(face = "bold", size=9),
        axis.text = element_text(face = "bold", size=9))

#---------------------------------------------------------------
#MAPS
#---------------------------------------------------------------
library(rgdal)
library(dplyr)
library(ggplot2)
library(viridis)
#map achieved %
df<-read.delim(file="country.csv",header = TRUE,sep=";")
df[] <- lapply(df, gsub, pattern='Â', replacement='')
names(df)[1]<-"id"
world_spdf <- readOGR( 
  dsn= paste0("C:/Users/Eva/Documents/CWR Kew/ne_10m_admin_0_countries") , 
  layer="ne_10m_admin_0_countries",
  verbose=FALSE)
world.df <- fortify(world_spdf, region = "NAME")

countries<-world.df%>%distinct(id)
setdiff(df$id, world.df$id)
world1<-left_join(world.df, df, by ="id")
word2<-world1%>%filter(id!="Antarctica")
countriesd<-word2%>%distinct(id)
theme_bare <- theme(
  axis.line = element_blank(), 
  axis.text.x = element_blank(), 
  axis.text.y = element_blank(),
  axis.ticks = element_blank(), 
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(),
  legend.text=element_text(size=7),
  legend.title=element_text(size=8),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

map <- ggplot(data = word2, aes(x = long, y = lat, group = group))
g<-map + 
  geom_polygon(aes(fill = Achieved), color = '#838385', size = 0.1) +
  theme(legend.position =c(0.5,0.05)) +coord_fixed(1.3) +theme_bare+
  scale_fill_viridis_c(
    option = "magma", na.value = "lightgrey",
    direction = -1,
    name = "Achieved %",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(1, units = "mm"),
      barwidth = unit(25, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5,
      label.theme=element_text(face="bold", size=4),
      title.theme = element_text(face="bold", size=4)))  


#map populations
df<-read.delim(file="countrypop.csv",header = TRUE,sep=";")
names(df)[1]<-"id"
world_spdf <- readOGR( 
  dsn= paste0("C:/Users/Eva/Documents/CWR Kew/ne_10m_admin_0_countries") , 
  layer="ne_10m_admin_0_countries",
  verbose=FALSE)
world.df <- fortify(world_spdf, region = "NAME")
world1<-left_join(world.df, df, by ="id")
word2<-world1%>%filter(id!="Antarctica")
map <- ggplot(data = word2, aes(x = long, y = lat, group = group))
g<-map + 
  geom_polygon(aes(fill = collected), color = '#838385', size = 0.1) +
  theme(legend.position =c(0.5,0.05)) +coord_fixed(1.3) +theme_bare+
  scale_fill_viridis_c(
    option = "viridis", na.value = "lightgrey",
    direction = -1,
    name = "Collected populations",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(1, units = "mm"),
      barwidth = unit(25, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5,
      label.theme=element_text(face="bold", size=4),
      title.theme = element_text(face="bold", size=4)))  

#-------------------------------------------------------------------
#GLM
#-------------------------------------------------------------------
library(lsmeans)
df<-read.table(file="glm.csv", header=TRUE, sep=";")
shapiro.test(df$Achieved)
df$GA<-as.factor(df$GA)
df$CropGP<-as.factor(df$CropGP)
df$GPLevel<-as.factor(df$GPLevel)
model<-glm(Collected/Agreed~GPLevel+GA+CropGP, weights = Agreed,
           data=df,family="binomial")
effects<-anova(model, test= "Chisq")
