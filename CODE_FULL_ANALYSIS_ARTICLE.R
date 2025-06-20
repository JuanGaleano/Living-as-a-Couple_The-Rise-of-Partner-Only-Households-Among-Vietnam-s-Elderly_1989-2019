library(tidyverse)
library(haven)

# Read Vietnam full samples from IPUMS I #####
new_vnm <- read_dta("G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\VNM\\ipumsi_00020.dta")

# Create and individual ID by year, household ID and pernum #####
new_vnm<-new_vnm|>mutate(new_id=paste(year,serial,pernum, sep="_"))

# Read IPUMS micro data from GLAD, LAV ######
load("G:/Shared drives/CORESIDENCE/TEAM FOLDERS/Juan Galeano/VNM/VNM_2019_IPUMS.Rda")
load("G:/Shared drives/CORESIDENCE/TEAM FOLDERS/Juan Galeano/VNM/VNM_1989_IPUMS.Rda")

# Cleaning and renaming #######
df <- df |> select(-LA, -LAQ)
VNM_1989_IPUMS <- df
VNM_2019_IPUMS <- VNM_2019_IPUMS |> select(colnames(df))
rm(df)
VNM_2019_IPUMS <- VNM_2019_IPUMS |> mutate(GEOLEVEL1 = if_else(GEOLEVEL1 %in%
                                                                 c(704003, 704014), 704004, GEOLEVEL1))
setdiff(unique(VNM_2019_IPUMS$GEOLEVEL1),
        unique(VNM_1989_IPUMS$GEOLEVEL1))
setdiff(unique(VNM_1989_IPUMS$GEOLEVEL1),
        unique(VNM_2019_IPUMS$GEOLEVEL1))

# Define the 6 Socio-economic regions #######
mkd <- c(704080,
         704082,
         704083,
         704084,
         704087,
         704089,
         704091,
         704092,
         704095)
ser <- c(704070, 
         704075, 
         704072, 
         704079)
chr <- c(704062, 
         704066, 
         704068)
ncr <- c(704038,
         704040,
         704044,
         704045,
         704046,
         704048,
         704051,
         704052,
         704054,
         704056,
         704058,
         704046)
rrd <- c(704001, 
         704035, 
         704034, 
         704030, 
         704031, 
         704022)
nmr <- c(704010, 
         704002, 
         704003, 
         704024, 
         704020, 
         704003, 
         704014, 
         704004)
names(VNM_1989_IPUMS)

# Create and individual ID by year, household ID and pernum for GLAD samples #####
VNM_1989_IPUMS<-VNM_1989_IPUMS|>mutate(new_id=paste(YEAR,SERIAL,PERNUM, sep="_"))
VNM_2019_IPUMS<-VNM_2019_IPUMS|>mutate(new_id=paste(YEAR,SERIAL,PERNUM, sep="_"))

# Join IPUMS full samples and GLAD data #####
VNM_1989_IPUMS<-VNM_1989_IPUMS|>
  left_join(new_vnm, by="new_id")

VNM_2019_IPUMS<-VNM_2019_IPUMS|>
  left_join(new_vnm, by="new_id")

# Drop unnecessary variables ####
VNM_1989_IPUMS<-VNM_1989_IPUMS|>select(-new_id,-country,-year,-sample,
                                       -serial,-hhwt,-gq,-pernum,-perwt)
VNM_2019_IPUMS<-VNM_2019_IPUMS|>select(-new_id,-country,-year,-sample,
                                       -serial,-hhwt,-gq,-pernum,-perwt)

# Create a list with the two samples ######
vnm_list<-list(VNM_1989_IPUMS,VNM_2019_IPUMS)

# Add socioeconomic regions definition to the micro data ######
vnm_list<-lapply(vnm_list, function(df) {

  df<-df|>
    mutate(ser = case_when(
      GEOLEVEL1 %in% mkd ~ "mkd",
      GEOLEVEL1 %in% ser ~ "ser",
      GEOLEVEL1 %in% chr ~ "chr",
      GEOLEVEL1 %in% ncr ~ "ncr",
      GEOLEVEL1 %in% rrd ~ "rrd",
      GEOLEVEL1 %in% nmr ~ "nmr",
      TRUE ~ NA_character_  # Assign NA if not in any category
    ))
})  

# Read classification LAV, LAT, LAI ######
library(readxl)

GELAI_AGRUPACION <- read_excel("G:/Shared drives/CORESIDENCE/TEAM FOLDERS/Juan Galeano/VNM/GELAI_AGRUPACION.xlsx", sheet = "Sheet6")

data<-GELAI_AGRUPACION
data <- data|>
  mutate(LAQ2 = str_replace_all(LAQ, "Extended|extended|composite", ""),
         LAQ2 =str_trim(LAQ2, side = "right"),
         LAQ2 = ifelse(LAQ2=="Alone", "A_Alone",
                       ifelse(LAQ2=="With single parent", "B_With single parent", 
                              ifelse(LAQ2=="With parents", "C_With parents", 
                                     ifelse(LAQ2=="With partner", "D_With partner", 
                                            ifelse(LAQ2=="With partner and children", "E_With partner and children", 
                                                   ifelse(LAQ2=="With children", "F_With children",  
                                                          ifelse(LAQ2=="Others", "G_Others",  
                                                                 ifelse(LAQ2=="Others composite", "H_Others composite",  
                                                                        ifelse(LAQ2=="Non-relative", "I_Non-relative", 0 
                                                                        ))))))))),
         SI = if_else(str_detect(GELAIQ, "Sibling"), 3, 0),
         GC = if_else(str_detect(GELAIQ, "Grandchild"), 4, 0),
         GP = if_else(str_detect(GELAIQ, "Grandaparent"), 5, 0),
         OT = if_else(str_detect(GELAIQ, "Other relative"), 6,0),
         NR = if_else(str_detect(GELAIQ, "Non-relative"), 7, 0),
         FA = if_else(str_detect(GELAIQ, "Father"), 1, 0),
         MO = if_else(str_detect(GELAIQ, "Mother"), 2, 0))

data <- data|>mutate(LA2=as.character(paste(LA,FA,MO,SI,GC,GP,OT,NR,sep="")))
length(unique(data$LA))   

# Add LAT and LAI to the micro data and create age 3 dichotomous  ######
gc()

vnm_list1<-lapply(vnm_list, function(df) {

  df<-df|>
  left_join(data, by="GELAI")|>
  mutate(age3=if_else(AGE>=60 
                      #& AGE<=80
                      , "60-80", "REST"))
})

vnm_list2<-lapply(vnm_list, function(df) {
  
  df<-df|>
    left_join(data, by="GELAI")|>
    mutate(age3=if_else(AGE>=60 
                        & AGE<=80
                        , "60-80", "REST"))
})


# Figure 2 Map and boxplot #######

vnm_list_maps<-lapply(vnm_list1, function(df) {
  
  df_sex<-df|>
    group_by(YEAR, ser,GEOLEVEL1,age3)|>
    summarise(n=n(),
              wpop=round(sum(PWEIGHT),0))|>
    mutate(n_rel = round(wpop / sum(wpop)*100,2))|>
   ungroup()|>
    filter(age3=="60-80")
})

df_maps <- data.table::rbindlist(vnm_list_maps)

df_maps<-df_maps|>mutate(ser=as.factor(ser), 
                     ser=fct_relevel(ser,
                                     "ser",
                                     "mkd",
                                     "nmr",
                                     "chr",
                                     "ncr",
                                     "rrd"),
                     ser2=fct_recode(ser,
                                     "Southeast"="ser",
                                     "Mekong Delta"="mkd",
                                     "Nothern Midland"="nmr",
                                     "Central Higlands"="chr",
                                     "North Central"="ncr",
                                     "Red River Delta"="rrd"),
                     sex2="Total population")

# Get basemap of the world ###### 
library(giscoR)
res <- "03"
target_crs <- 4326   

world <- gisco_get_countries(
  resolution = res, region = NULL,
  epsg = target_crs
)


# Read shapefiles ##### 

library(sf)
sf_vnm <- st_read("G:/Shared drives/CORESIDENCE/TEAM FOLDERS/Juan Galeano/VNM/shapefiles",
                  "vietnam_geolevel_ser")

sf_vnm_ser <- st_read("G:/Shared drives/CORESIDENCE/TEAM FOLDERS/Juan Galeano/VNM/shapefiles",
                  "vietnam_ser")

sf_vnm_ser_c <- st_read("G:/Shared drives/CORESIDENCE/TEAM FOLDERS/Juan Galeano/VNM/shapefiles",
                      "centroids_ser_vietnam")

sf_vnm_ser_c <- sf_vnm_ser_c %>%
  mutate(SER = str_to_lower(SER))

colnames(sf_vnm_ser_c)[6]<-"ser"

pop_ser<-lapply(vnm_list, function(df) {
  df_pop<-df|>
    group_by(YEAR,ser)|>
    summarise(pop=sum(PWEIGHT))|>
    ungroup()
})

df_ser <- data.table::rbindlist(pop_ser)

# Create population intervals for the Bubble layer of maps ######
df_ser<-df_ser|>
  mutate(
  pop_cat=as.factor(
    ifelse(pop<5000000, "2-5M",
           ifelse(pop<10000000, "5-10M",
                  ifelse(pop<15000000, "10-15M",
                         ifelse(pop<20000000, "15-20M",
                                ifelse(pop>=20000000, "≥20M",999)))))),
  pop_cat=fct_relevel(pop_cat,
                           "2-5M",
                           "5-10M",
                           "10-15M",
                           "15-20M",
                           "≥20M"))


mysizes <-rev(c(10,8,4,2,1))
names(mysizes) <- levels(df_ser$pop_cat)

sf_vnm_ser_c <- sf_vnm_ser_c%>%
  left_join(df_ser, by = "ser")

df_maps<-df_maps|>mutate(GEOLEVEL1=as.character(GEOLEVEL1))

sf_vnm <- sf_vnm%>%
  left_join(df_maps, by = "GEOLEVEL1")

# Create intervals for the share of population 60+ by geolevel1 #######
sf_vnm <- sf_vnm |> 
  mutate(
    prop_old_cat = case_when(
      n_rel < 6    ~ "<6%",
      n_rel < 9    ~ "[6-9%)",
      n_rel < 12   ~ "[9-12%)",
      n_rel < 15   ~ "[12-15%)",
      n_rel >= 15  ~ "≥15%"
    ),
    prop_old_cat = factor(prop_old_cat, levels = c("<6%", "[6-9%)", "[9-12%)", "[12-15%)", "≥15%"))
  )

# Base maps as ggplot object ########
mapeu<-ggplot()+  
  geom_sf(data = world,
          colour="black", 
          linewidth=0.2,
          fill="#E6E6E6", alpha = 0.85)


# Define a color palette for the choropleth layer ########
library(RColorBrewer)
myColors <- c(brewer.pal(6, "YlOrRd"))

st_bbox(sf_vnm_ser)
# Multilayer map:  Background map, choropleth map and bubble map #######
map <- mapeu + 
  geom_sf(data = sf_vnm,
          aes(fill = prop_old_cat), 
          color = "black",
          linewidth=.15) +  # Fill provinces with black borders
  geom_sf(data = sf_vnm_ser, 
          fill=NA, 
          color = "black",
          linewidth=.85) +  #
  geom_sf(data=sf_vnm_ser_c,aes(size = pop_cat),
         color="black",
          fill="#bfbfbf",
          alpha=0.15)+
  geom_sf(data=sf_vnm_ser_c,aes(size = pop_cat),
          color="black",
          linewidth=5.75,
          fill=NA,
          alpha=1,
          shape =21)+
  
  scale_fill_manual(values = setNames(myColors, levels(sf_vnm$prop_old_cat)),
                    guide = guide_legend(direction = "horizontal",
                                         title.position = "top",
                                         title.hjust=0.5,
                                         title = "Population (%) 60+ years old\n\n\n",
                                         nrow = 1,
                                         keywidth=5,
                                         label.position = "bottom"))+
  scale_size_manual(values=mysizes*3,name = "Total population",
                    guide = guide_legend(direction = "horizontal",
                                         title.position = "top",
                                         title.hjust=0.5,
                                         nrow = 1,
                                         keywidth=5,
                                         label.position = "bottom"))+
  annotate("text", x = 103.7, y = 8.7, 
           label = "Mekong\nDelta", 
           size=6, #fontface ="bold"
           )+
  annotate("text", x = 100.3, y = 22.5, 
           label = "Northern Midlands\nand Mountains", 
           size=6, #fontface ="bold"
           )+
  annotate("text", x = 108, y = 20, 
           label = "Red River\nDelta", 
           size=6, #fontface ="bold"
           )+
  annotate("text", x = 110.6, y = 16, 
           label = "North Central and\n Central Coast", 
           size=6, #fontface ="bold"
           )+
  annotate("text", x = 106.3, y = 13.6, 
           label = "Central\n Highlands", 
           size=6, #fontface ="bold"
           )+
  annotate("text", x = 105, y = 12.2, 
           label = "Southeast", 
           size=6, #fontface ="bold"
           )+
  coord_sf(
    xlim = c(98, 114),
    ylim = c(8.5, 23)
  ) +
 theme_bw() +
  theme(
    legend.position = "top", 
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0), 
    plot.margin = margin(t = 10, r = 0, b = 0, l = 90),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.title = element_text(face = "bold", colour = "black", size = 24),  
    legend.text = element_text(colour = "black", size = 18),
    strip.text = element_text(face = "bold", colour = "black", size = 25)
  )+
  facet_grid(~YEAR)


# Define a theme #######

theme_bssd<-list( theme(plot.title = element_text(lineheight=1, size=16, face="bold"),
                        plot.subtitle = element_text(vjust=0.5, size=12,colour="black"),
                        plot.caption = element_text(vjust=0.5, size=12,colour="black"),
                        legend.title = element_blank(),
                        legend.text = element_text(colour="black", size = 11),
                        # legend.position="right",
                        # legend.justification=c(1,0), 
                        legend.background = element_rect(fill=NA),
                        legend.key.size = unit(1, 'lines'),
                        strip.text=element_text(angle = 0,vjust=0.5, size=11,colour="black",face="bold"),
                        axis.title.x = element_text(colour="black", size=11),
                        axis.text.x  = element_text(angle = 0,vjust=0.5, size=11,colour="black"),
                        axis.title.y = element_text( colour="black", size=11),
                        axis.text.y  = element_text(vjust=0.5, size=11,colour="black"),
                        panel.grid.major=element_line(colour="#E6E6E6"),
                        plot.background =  element_rect(fill = "white"),
                        panel.background =element_rect(fill ="#FFFFFF", colour = "#FFFFFF")))


# Boxplots share of pop 60+ by geolevel1 #######
plot_old<-ggplot(df_maps, aes(x=as.factor(YEAR), y=n_rel/100, fill=as.factor(YEAR))) + 
  geom_boxplot()+
  scale_y_continuous(labels = scales::percent, limits=c(0,.2))+
  facet_grid(~ser2)+
  theme_bssd+
  labs(title = "Population 60+ years old",
       y="Percentage\n")+
  theme(plot.title = element_text(lineheight=1, size=25, face="bold"),
        legend.position = "none",
        axis.title.y = element_text(colour = "black", size = 20), 
        axis.text.x = element_text(colour = "black", size = 20), 
        axis.text.y = element_text(colour = "black", size = 20), 
        strip.text =  element_text(face="bold",colour = "black", size = 20), 
        panel.background =element_rect(fill ="#FFFFFF",colour = "black"), 
        panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(0, 0, 0, 0),
        axis.title.x = element_blank())


# Arrange map and boxplot #######
library(cowplot)
# a<-ggpubr::ggarrange(map,plot_old, 
#                      ncol = 1, nrow = 2, heights = c(3,1))

a <- plot_grid(
  map, 
  plot_old, 
  ncol = 1,
  rel_heights = c(3, 1),
  align = "v",
  axis = "lr",
  labels = NULL,
  label_size = 0
)

# Save arranged plot ##########
ggsave("FIG1.tiff",
       plot = a,
       scale = 1, 
       dpi = 300,     # resolution of the image
       height = 18,
       width=20.5,
       path ="G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\VNM\\figures")

# Boxplot marital status by geolevel1 #########

vnm_list_marst<-lapply(vnm_list1, function(df) {
  df_ms<-df|>
    filter(age3=="60-80")|>
    mutate(MARST=unclass(MARST))|>
    group_by(YEAR,ser,GEOLEVEL1,SEX,MARST)|>
    summarise(n=n(),
              wpop=sum(PWEIGHT))|>
    mutate(n_rel = round(n / sum(n)*100,2),
           var="MARST")|>
    rename(cat=MARST)|>
    mutate(cat=as.character(cat),
           SEX=as.factor(unclass(SEX)),
           SEX= fct_recode(SEX, Men = "1",
                           Women="2"))
})  

marst <- data.table::rbindlist(vnm_list_marst)

marst<-marst|>mutate(ser=as.factor(ser), 
                     ser=fct_relevel(ser,
                                     "ser",
                                     "mkd",
                                     "nmr",
                                     "chr",
                                     "ncr",
                                     "rrd"),
                     ser2=fct_recode(ser,
                                     "Southeast"="ser",
                                     "Mekong Delta"="mkd",
                                     "Nothern Midland"="nmr",
                                     "Central Higlands"="chr",
                                     "North Central"="ncr",
                                     "Red River Delta"="rrd"))


plot_marst<-ggplot(marst|>filter(cat==4), 
                   aes(x=as.factor(YEAR), 
                       y=n_rel/100, fill=as.factor(SEX))) + 
  geom_boxplot(position = position_dodge(width = 0.75)) +  # <-- this is key
  scale_y_continuous(labels = scales::percent,limits = c(0,1))+
  facet_grid(~ser2)+
  theme_bw()+
  theme_bssd+
  labs(title = "\nPanel 1: Widowed population (%) by sex, year and regions",
       y="Percentage\n")+
  theme(legend.position = "bottom",
        panel.background =element_rect(fill ="#FFFFFF",colour = "black"), 
        axis.title.x = element_blank())

# Boxplot educational attainment by geolevel1 ##########

vnm_list_edattain<-lapply(vnm_list1, function(df) {
  df_ms<-df|>
    filter(age3=="60-80")|>
    mutate(EDATTAIN=unclass(EDATTAIN),
           EDATTAIN=if_else(EDATTAIN>=2,2,EDATTAIN))|>
    group_by(YEAR,ser,GEOLEVEL1,SEX,EDATTAIN)|>
    summarise(n=n(),
              wpop=sum(PWEIGHT))|>
    mutate(n_rel = round(n / sum(n)*100,2),
           var="MARST")|>
    rename(cat=EDATTAIN)|>
    mutate(cat=as.character(cat),
           SEX=as.factor(unclass(SEX)),
           SEX= fct_recode(SEX, Men = "1",
                           Women="2"))
})  

edattain <- data.table::rbindlist(vnm_list_edattain)

edattain<-edattain|>mutate(ser=as.factor(ser), 
                     ser=fct_relevel(ser,
                                     "ser",
                                     "mkd",
                                     "nmr",
                                     "chr",
                                     "ncr",
                                     "rrd"),
                     ser2=fct_recode(ser,
                                     "Southeast"="ser",
                                     "Mekong Delta"="mkd",
                                     "Nothern Midland"="nmr",
                                     "Central Higlands"="chr",
                                     "North Central"="ncr",
                                     "Red River Delta"="rrd"))


plot_edattain<-ggplot(edattain|>filter(cat==2), aes(x=as.factor(YEAR), y=n_rel/100, fill=as.factor(SEX))) + 
  geom_boxplot()+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  facet_grid(SEX~ser2)+
  theme_bw()+
  theme_bssd+
  labs(title = "\nPanel 2: Population with at least primary education (%) by sex, year and regions",
       y="Percentage\n")+
  theme(legend.position = "none",
        panel.background =element_rect(fill ="#FFFFFF",colour = "black"), 
        axis.title.x = element_blank())

# Arrange boxplot marital status and educational attainment ########
library(cowplot)
a<-ggpubr::ggarrange(plot_marst,plot_edattain, 
                     ncol = 1, nrow = 2, heights = c(.5,.5)) 

# Save arranged plot ########
ggsave("FIG2.tiff", 
       plot = a,
       scale = 1,
       height = 8,
       width=12, 
       dpi = 300,
       path ="G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\VNM\\figures")

# Some descriptive full vs sample population  ######
vnm_list_des<-lapply(vnm_list, function(df) {
  
  df_sex<-df|>
    filter(age3=="60-80")|>
    mutate(SEX=unclass(SEX))|>
    group_by(YEAR,SEX)|>
    summarise(n=n(),
              wpop=sum(PWEIGHT))|>
    mutate(n_rel = round(n / sum(n)*100,2),
           var="SEX")|>
    rename(cat=SEX)|>
    mutate(cat=as.character(cat))
  
  df_ms<-df|>
    filter(age3=="60-80")|>
    mutate(MARST=unclass(MARST))|>
    group_by(YEAR,SEX,MARST)|>
    summarise(n=n(),
              wpop=sum(PWEIGHT))|>
    mutate(n_rel = round(n / sum(n)*100,2),
           var="MARST")|>
    rename(cat=MARST)|>
    mutate(cat=as.character(cat))
  
  df_ur<-df|>
    filter(age3=="60-80")|>
    mutate(urban=unclass(urban))|>
    group_by(YEAR,urban)|>
    summarise(n=n(),
              wpop=sum(PWEIGHT))|>
    mutate(n_rel = round(n / sum(n)*100,2),
           var="urban")|>
    rename(cat=urban)|>
    mutate(cat=as.character(cat))
  
  df_se<-df|>
    filter(age3=="60-80")|>
    mutate(ser=unclass(ser))|>
    group_by(YEAR,ser)|>
    summarise(n=n(),
              wpop=sum(PWEIGHT))|>
    mutate(n_rel = round(n / sum(n)*100,2),
           var="ser")|>
    rename(cat=ser)|>
    mutate(cat=as.character(cat))
  
  df_ed<-df|>
    filter(age3=="60-80")|>
    mutate(EDATTAIN=unclass(EDATTAIN))|>
    group_by(YEAR,SEX,EDATTAIN)|>
    summarise(n=n(),
              wpop=sum(PWEIGHT))|>
    mutate(n_rel = round(n / sum(n)*100,2),
           var="EDATTAIN")|>
    rename(cat=EDATTAIN)|>
    mutate(cat=as.character(cat))
  
  
  df_et<-df|>
    filter(age3=="60-80")|>
    mutate(ethnicvn=unclass(ethnicvn),
           ethnicvn=if_else(ethnicvn==1,1,
                            if_else(ethnicvn==2,2,
                                    if_else(ethnicvn==3,3,4))))|>
    group_by(YEAR,ethnicvn)|>
    summarise(n=n(),
              wpop=sum(PWEIGHT))|>
    mutate(n_rel = round(n / sum(n)*100,2),
           var="ethnicvn")|>
    rename(cat=ethnicvn)|>
    mutate(cat=as.character(cat))
   
  
  
  df_em<-df|>
    filter(age3=="60-80")|>
    mutate(empstat=unclass(empstat))|>
    filter(empstat %in% c(1,2,3))|>
    mutate(empstat=if_else(empstat==1,1,2))|>
    group_by(YEAR,empstat)|>
    summarise(n=n(),
              wpop=sum(PWEIGHT))|>
    mutate(n_rel = round(n / sum(n)*100,2),
           var="empstat")|>
    rename(cat=empstat)|>
    mutate(cat=as.character(cat))
  
  df_in<-df|>
    filter(age3=="60-80")|>
    mutate(indgen=unclass(indgen),
           indgen=if_else(indgen==0,0,
                            if_else(indgen==10,1,2)))|>
    group_by(YEAR,indgen)|>
    summarise(n=n(),
              wpop=sum(PWEIGHT))|>
    mutate(n_rel = round(n / sum(n)*100,2),
           var="indgen")|>
    rename(cat=indgen)|>
    mutate(cat=as.character(cat))
  
  
  
    df_desc_full<-bind_rows(df_sex,df_ms,df_ur,df_se,df_ed, df_et,df_em,df_in)
    df_desc_full<-df_desc_full|>mutate(sample2="full")
    
    df_sex<-df|>
      filter(age3=="60-80",
             LA %in% c(40,41,42,50,51,52))|>
      mutate(SEX=unclass(SEX))|>
      group_by(YEAR,SEX)|>
      summarise(n=n(),
                wpop=sum(PWEIGHT))|>
      mutate(n_rel = round(n / sum(n)*100,2),
             var="SEX")|>
      rename(cat=SEX)|>
      mutate(cat=as.character(cat))
    
    df_ms<-df|>
      filter(age3=="60-80",
             LA %in% c(40,41,42,50,51,52))|>
      mutate(MARST=unclass(MARST))|>
      group_by(YEAR,MARST)|>
      summarise(n=n(),
                wpop=sum(PWEIGHT))|>
      mutate(n_rel = round(n / sum(n)*100,2),
             var="MARST")|>
      rename(cat=MARST)|>
      mutate(cat=as.character(cat))
    
    df_ur<-df|>
      filter(age3=="60-80",
             LA %in% c(40,41,42,50,51,52))|>
      mutate(urban=unclass(urban))|>
      group_by(YEAR,urban)|>
      summarise(n=n(),
                wpop=sum(PWEIGHT))|>
      mutate(n_rel = round(n / sum(n)*100,2),
             var="urban")|>
      rename(cat=urban)|>
      mutate(cat=as.character(cat))
    
    df_se<-df|>
      filter(age3=="60-80",
             LA %in% c(40,41,42,50,51,52))|>
      mutate(ser=unclass(ser))|>
      group_by(YEAR,ser)|>
      summarise(n=n(),
                wpop=sum(PWEIGHT))|>
      mutate(n_rel = round(n / sum(n)*100,2),
             var="ser")|>
      rename(cat=ser)|>
      mutate(cat=as.character(cat))
    
    df_ed<-df|>
      filter(age3=="60-80",
             LA %in% c(40,41,42,50,51,52))|>
      mutate(EDATTAIN=unclass(EDATTAIN))|>
      group_by(YEAR,EDATTAIN)|>
      summarise(n=n(),
                wpop=sum(PWEIGHT))|>
      mutate(n_rel = round(n / sum(n)*100,2),
             var="EDATTAIN")|>
      rename(cat=EDATTAIN)|>
      mutate(cat=as.character(cat))
    
    
    df_et<-df|>
      filter(age3=="60-80",
             LA %in% c(40,41,42,50,51,52))|>
      mutate(ethnicvn=unclass(ethnicvn),
             ethnicvn=if_else(ethnicvn==1,1,
                              if_else(ethnicvn==2,2,
                                      if_else(ethnicvn==3,3,4))))|>
      group_by(YEAR,ethnicvn)|>
      summarise(n=n(),
                wpop=sum(PWEIGHT))|>
      mutate(n_rel = round(n / sum(n)*100,2),
             var="ethnicvn")|>
      rename(cat=ethnicvn)|>
      mutate(cat=as.character(cat))
    
    df_em<-df|>
      filter(age3=="60-80",
             LA %in% c(40,41,42,50,51,52))|>
      mutate(empstat=unclass(empstat))|>
      filter(empstat %in% c(1,2,3))|>
      mutate(empstat=if_else(empstat==1,1,2))|>
      group_by(YEAR,empstat)|>
      summarise(n=n(),
                wpop=sum(PWEIGHT))|>
      mutate(n_rel = round(n / sum(n)*100,2),
             var="empstat")|>
      rename(cat=empstat)|>
      mutate(cat=as.character(cat))
    
    df_in<-df|>
      filter(age3=="60-80",
             LA %in% c(40,41,42,50,51,52))|>
      mutate(indgen=unclass(indgen),
             indgen=if_else(indgen==0,0,
                            if_else(indgen==10,1,2)))|>
      group_by(YEAR,indgen)|>
      summarise(n=n(),
                wpop=sum(PWEIGHT))|>
      mutate(n_rel = round(n / sum(n)*100,2),
             var="indgen")|>
      rename(cat=indgen)|>
      mutate(cat=as.character(cat))
    
    
    df_desc_sample<-bind_rows(df_sex,df_ms,df_ur,df_se,df_ed, df_et,df_em,df_in)
    df_desc_sample<-df_desc_sample|>mutate(sample2="sample")
    
    
    desc<-bind_rows(df_desc_full,df_desc_sample)
  
})

desc <- data.table::rbindlist(vnm_list_des)

ggplot(desc|>filter(sample2=="full"), aes(fill=as.factor(YEAR), y=n_rel, x=cat)) + 
  geom_bar(position="dodge", stat="identity")+ 
  facet_wrap(~var, scales = "free")



# Living arrangements plots ######
# Color palette LA ######
library(colorspace)
library(RColorBrewer)

pal<-"Set3"
brewer.pal(8, pal)

my_colors<-c(brewer.pal(8, pal)[1],
             brewer.pal(8, pal)[2], darken(brewer.pal(8, pal)[2], 0.25),#darken(brewer.pal(8, pal)[2], 0.5),
             brewer.pal(8, pal)[3], darken(brewer.pal(8, pal)[3], 0.25),#darken(brewer.pal(8, pal)[3], 0.5),
             brewer.pal(8, pal)[4], darken(brewer.pal(8, pal)[4], 0.25),#darken(brewer.pal(8, pal)[4], 0.5),
             brewer.pal(8, pal)[5], darken(brewer.pal(8, pal)[5], 0.25),#darken(brewer.pal(8, pal)[5], 0.5),
             brewer.pal(8, pal)[6], darken(brewer.pal(8, pal)[6], 0.25),#darken(brewer.pal(8, pal)[6], 0.5),
             brewer.pal(8, pal)[7], darken(brewer.pal(8, pal)[7], 0.25),
             brewer.pal(8, pal)[8])

my_colors2<-c(brewer.pal(8, pal)[1],
              brewer.pal(8, pal)[4], darken(brewer.pal(8, pal)[4], 0.25),#darken(brewer.pal(8, pal)[4], 0.5),
              brewer.pal(8, pal)[5], darken(brewer.pal(8, pal)[5], 0.25),#darken(brewer.pal(8, pal)[5], 0.5),
              brewer.pal(8, pal)[6], darken(brewer.pal(8, pal)[6], 0.25),#darken(brewer.pal(8, pal)[6], 0.5),
              brewer.pal(8, pal)[7],
              darken(brewer.pal(8, pal)[7], 0.25),
              brewer.pal(8, pal)[8],
              brewer.pal(8, pal)[2], darken(brewer.pal(8, pal)[2], 0.25),#darken(brewer.pal(8, pal)[2], 0.5),
              brewer.pal(8, pal)[3], darken(brewer.pal(8, pal)[3], 0.25),#darken(brewer.pal(8, pal)[3], 0.5),
              brewer.pal(8, pal)[4], darken(brewer.pal(8, pal)[4], 0.25)#darken(
      )

# Area plot living arrangements by single ages and sex ######
VNM<-bind_rows(vnm_list1[[1]],vnm_list1[[2]])
VNM<-VNM|>filter(AGE!=999)|>
  mutate(LAQ=case_when(
           LA == 10 ~ "Alone",
           
           LA == 20 ~ "With single parent",
           LA == 21 ~ "With single parent extended",
           LA == 22 ~ "With single parent extended",
           
           LA == 30 ~ "With parents",
           LA == 31 ~ "With parents extended",
           LA == 32 ~ "With parents extended",
           
           LA == 40 ~ "With partner",
           LA == 41 ~ "With partner extended",
           LA == 42 ~ "With partner extended",
           
           LA == 50 ~ "With partner and children",
           LA == 51 ~ "With partner and children extended",
           LA == 52 ~ "With partner and children extended",
           
           LA == 60 ~ "With children",
           LA == 61 ~ "With children extended",
           LA == 62 ~ "With children extended",
           
           LA == 70 ~ "Others",
           LA == 71 ~ "Others composite",
           
           LA== 80 ~ "Non-relative"))

name<-VNM|>
  group_by(SAMPLE,SEX,AGE,LAQ)|>
  summarise(pop=sum(PWEIGHT))|>
  ungroup()

name<-name|>
  mutate(n_rel =pop / sum(pop), .by = c("SEX","AGE"))

name<-name|>
  mutate(SEX=as.factor(unclass(SEX)),
         SEX= fct_recode(SEX, Men = "1",
                         Women="2"),
         type=as.factor(LAQ),
         type=fct_recode(type,
                         "Alone"="Alone",
                          
                         "With single parent"=  "With single parent",
                         "With single parent extended"= "With single parent extended",
                          #"With single parent extended composite",
                          
                         "With parents"="With parents",
                         "With parents extended"=  "With parents extended",
                          #"With parents extended composite",
                          
                         "With partner"=   "With partner",
                          "With partner\nextended"="With partner extended",
                          #"With partner extended composite",
                          
                          "With partner\nand children"="With partner and children",
                          "With partner\nand children extended"= "With partner and children extended",
                          #"With partner and children extended composite",
                          
                         "With children"= "With children",
                          "With children\nextended"="With children extended",
                          #"With children extended composite",
                          
                         "Others"="Others",
                         "Others composite"= "Others composite",
                          
                         "Non-relative"=  "Non-relative"),
         type=fct_relevel(type,
                          "Alone",
                          
                          "With single parent",
                          "With single parent extended",
                          #"With single parent extended composite",
                          
                          "With parents",
                          "With parents extended",
                          #"With parents extended composite",
                          
                          "With partner",
                          "With partner\nextended",
                          #"With partner extended composite",
                          
                          "With partner\nand children",
                          "With partner\nand children extended",
                          #"With partner and children extended composite",
                          
                          "With children",
                          "With children\nextended",
                          #"With children extended composite",
                          
                          "Others",
                          "Others composite",
                          
                          "Non-relative")
         )

filter<-name|>filter(AGE>=60)
unique(filter$type)

name<-name|>mutate(alpha=if_else(AGE %in% c(60:80),"A","B"),
                   YEAR=if_else(SAMPLE=="VNM_1989_IPUMS", "\n1989\n","\n2019\n"))

plot<-ggplot(name|>filter(AGE>=60), aes(x=AGE, y=pop, fill=type, alpha=alpha)) + 
  geom_area(position = "fill", color="black", linewidth=.1)+
  scale_fill_manual(values=my_colors2,
                    breaks = c("Alone",
                               "With partner",
                               "With partner\nextended",
                               "With partner\nand children",
                               "With partner\nand children extended",
                               "With children",
                               "With children\nextended",
                               "Others"
                               
                               ))+
  scale_alpha_manual(values=c(1,.5),guide="none")+
  scale_x_continuous(limits=c(60,80), breaks=c(seq(60,80,5)))+
  scale_y_continuous(labels = scales::percent)+
  labs(x="\nAge",
       y="Percentage\n")+
  # annotate("rect", xmin = 60, xmax = 80, ymin = 0, ymax = 1,
  #          alpha = .2)+
  # annotate("segment", x = 60, xend = 80, y = 0, yend = 0,colour = "black")+
  # annotate("segment", x = 60, xend = 80, y = 1, yend = 1,colour = "black")+
  # annotate("segment", x = 60, xend = 60, y = 0, yend = 1,colour = "black")+
  # annotate("segment", x = 80, xend = 80, y = 0, yend = 1,colour = "black")+
  theme_bssd+
  facet_grid(YEAR~SEX)+
  guides(fill = guide_legend(nrow = 1,
         keywidth=1.5)) +
  labs(title = "Panel 1: Living arrangements by single ages and sex, 1989 and 2019")+
  theme(panel.background =element_rect(fill ="#FFFFFF",colour = "black"), 
        legend.position = "bottom")

plot

# Change in extended #### Revisar ########

name_ext<-VNM|>
  filter(AGE>=60,LAQ=="With children extended")|>
  group_by(SAMPLE,YEAR, SEX,LAQ,LA2)|>
  summarise(pop=sum(PWEIGHT))|>
  ungroup()

name_ext<-name_ext|>
  mutate(n_rel =pop / sum(pop), .by = c("SAMPLE","SEX"))


AVER<-VNM|>filter(LA==61, AGE>60)
# INVESTIGAR SERIAL 10002

# Difference between two points in time #####
VNM1989<-VNM|>filter(SAMPLE=="VNM_1989_IPUMS")
VNM2019<-VNM|>filter(SAMPLE=="VNM_2019_IPUMS")

name1<-VNM1989|>
  group_by(SAMPLE,SEX,AGE,LAQ)|>
  summarise(pop=sum(PWEIGHT))|>
  ungroup()|>
  mutate(n_rel =pop / sum(pop), .by = c("SEX","AGE"))

name2<-VNM2019|>
  group_by(SAMPLE,SEX,AGE,LAQ)|>
  summarise(pop=sum(PWEIGHT))|>
  ungroup()|>
  mutate(n_rel =pop / sum(pop), .by = c("SEX","AGE"))

name1A<-name1|>distinct(SEX,AGE,LAQ)
name2A<-name2|>distinct(SEX,AGE,LAQ)

AVER<-full_join(name2A,name1A,by=c("SEX","AGE", "LAQ"))

f1<-left_join(AVER,name1,by=c("SEX","AGE", "LAQ"))
f2<-left_join(AVER,name2,by=c("SEX","AGE", "LAQ"))

f1<-f1|>
  mutate(SAMPLE=if_else(is.na(SAMPLE), lag(SAMPLE),SAMPLE),
         pop=if_else(is.na(pop), 0,pop),
         n_rel=if_else(is.na(n_rel), 0,n_rel),
         LAQ=as.factor(LAQ))

f2<-f2|>
  mutate(SAMPLE=if_else(is.na(SAMPLE), lag(SAMPLE),SAMPLE),
         pop=if_else(is.na(pop), 0,pop),
         n_rel=if_else(is.na(n_rel), 0,n_rel),
         LAQ=as.factor(LAQ))


f1<-f1|>
  mutate(nrel2000=f2$n_rel, 
         final=nrel2000-n_rel)

f1<-f1|>
  mutate(SEX=as.factor(unclass(SEX)),
         SEX= fct_recode(SEX, Men = "1",
                         Women="2"),
         type2=case_when(LAQ == "Alone" ~ "Alone",
                         LAQ == "With single parent" ~ "With\nsingle\nparent",
                         LAQ == "With single parent extended" ~ "With\nsingle\nparent",
                         LAQ == "With parents" ~ "With\nparents",
                         LAQ == "With parents extended" ~ "With\nparents",
                         LAQ == "With partner" ~ "With\npartner",
                         LAQ == "With partner extended" ~ "With\npartner",
                         LAQ == "With partner and children" ~ "With\npartner &\nchildren",
                         LAQ == "With partner and children extended" ~ "With\npartner &\nchildren",
                         LAQ == "With children" ~ "Whith\nchildren",
                         LAQ == "With children extended" ~ "Whith\nchildren",
                         LAQ == "Others" ~ "Others",
                         LAQ == "Others composite" ~ "Others",
                         LAQ == "Non-relative" ~ "Non\nrelative"),
         type2=as.factor(type2),
         type2=fct_relevel(type2,
                           "Alone",
                           "With\nsingle\nparent",
                           "With\nparents",
                           "With\npartner",
                           "With\npartner &\nchildren",
                           "Whith\nchildren",
                           "Others",
                           "Non\nrelative"),
         LAQ=as.factor(LAQ),
         LAQ=fct_relevel(LAQ,
                         "Alone",
                         
                         "With single parent",
                         "With single parent extended",
                         #"With single parent extended mix",
                         
                         "With parents",
                         "With parents extended",
                         #"With parents extended mix",
                         
                         "With partner",
                         "With partner extended",
                         #"With partner extended mix",
                         
                         "With partner and children",
                         "With partner and children extended",
                         #"With partner and children extended mix",
                         
                         "With children",
                         "With children extended",
                         #"With children extended mix",
                         
                         "Others",
                         "Others composite",
                         
                         "Non-relative"))

plot2<-ggplot(f1|>filter(SAMPLE=="VNM_1989_IPUMS",
                  AGE>=60&AGE<=80,
                  type2 %in% c("Alone",
                               "With\npartner",
                               "With\npartner &\nchildren",
                               "Whith\nchildren",
                               "Others")),
       aes(x=AGE, y=final, fill=LAQ)) + 
  geom_bar(stat="identity", position = "stack",colour="black", linewidth=.1)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=my_colors[c(1,6,7,8,9,10,11,12,13)])+
  labs(y="Change between 1989 and 2019\n", 
       x="\nAge")+
  labs(title = "Panel 2: Living arrangements by single ages and sex, differences 2019-1989")+
  theme_bssd+ facet_grid(type2 ~SEX)+
  theme(panel.background =element_rect(fill ="#FFFFFF",colour = "black"), 
        legend.position = "none")


# boxplot: share couple only by geolevel1 and region #####

vnm_list_LA2<-lapply(vnm_list2, function(df) {
  
  df<-df|>
    group_by(YEAR,
             ser,GEOLEVEL1,
             #  SEX,
             age3,LA2 )|>
    summarise(unweighted_pop=n(),
              weighted_pop=sum(PWEIGHT))|>
    ungroup()|>
    filter(age3=="60-80")|>
    # mutate(LAT2=substr(LA2, 1,2))|>
    # filter(LAT2 %in% c(40,41,42,50,51,52))|>
    mutate(LAT=if_else(substr(LA2, 1,2)==40,1,2))|>
    group_by(YEAR,
             ser,GEOLEVEL1,
             #SEX,
             LAT)|>
    summarise(n=sum(weighted_pop))|>
    mutate(n_rel = round(n / sum(n)*100,2))|>
    ungroup()|>
    filter(LAT==1)
  
})

lat40 <- data.table::rbindlist(vnm_list_LA2)


levels(lat40$ser)
lat40<-lat40|>mutate(ser=as.factor(ser), 
                     ser=fct_relevel(ser,
                                     "ser",
                                     "mkd",
                                     "nmr",
                                     "chr",
                                     "ncr",
                                     "rrd"),
                     ser2=fct_recode(ser,
                                     "Southeast"="ser",
                                     "Mekong Delta"="mkd",
                                     "Northern Midland"="nmr",
                                     "Central Higlands"="chr",
                                     "North Central"="ncr",
                                     "Red River Delta"="rrd"))


plot3<-ggplot(lat40, aes(x=as.factor(YEAR), y=n_rel/100, fill=as.factor(YEAR))) + 
  geom_boxplot()+
  scale_y_continuous(labels = scales::percent)+
  facet_grid(~ser2)+
  #theme_bssd+
  labs(title = "Panel 3: Population 60-80 years old living only with partner by socio-economic regions, 1989 and 2019",
       y="Percentage\n")+
   theme_bssd+
  theme(#plot.title = element_text(lineheight=1, size=16, face="bold"),
        legend.position = "none",
        panel.background =element_rect(fill ="#FFFFFF",colour = "black"), 
        axis.title.x = element_blank())

library(cowplot)
a<-ggpubr::ggarrange(plot,plot2,plot3, 
                     ncol = 1, nrow = 3, heights = c(.45,.4,.15)) 

ggsave("FIG3.tiff", 
       scale = 1,
       height = 18,
       width=12, 
       dpi = 300,
       path ="G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\VNM\\figures")



# REGIONAL VARIABLES #######
# Compute population by region to weight emigration and house ownership rates ######
pop_geolevel<-lapply(vnm_list, function(df) {
  df_pop<-df|>
    group_by(ser, GEOLEVEL1)|>
    summarise(pop=sum(PWEIGHT))|>
    ungroup()
})

# Compute proportion of population 60+ by regions ######
oldratio<-lapply(vnm_list, function(df) {
  df_old<-df|>
    mutate(age4=if_else(AGE>=60, "60+", "REST"))|>
    group_by(GEOLEVEL1,age4)|>
    summarise(pop=sum(PWEIGHT))|>
    ungroup()%>%
    group_by(GEOLEVEL1)|>
    summarize(
      elderly = pop[age4 == "60+"],
      working_age = pop[age4 == "REST"],
      old_ratio = (elderly / working_age)
    )|>
    ungroup()
})

# Compute proportion of of male pop 15-64 working in agriculture  ######

pop_agri<-lapply(vnm_list, function(df) {
  df_agri<-df|>
    mutate(indgen=if_else(indgen%in%c(0,998,999),0,
                          if_else(indgen==10,1,2)))|>
    filter((AGE>=15 & AGE<=64) & SEX==1 & indgen>0)|>
    group_by(GEOLEVEL1,indgen)|>
    summarise(pop=sum(PWEIGHT))|>
    ungroup()%>%
    group_by(GEOLEVEL1)|>
    summarize(
      agr = pop[indgen == 1],
      working_age = pop[indgen == 2],
      agri = (agr / (agr+ working_age))
    )|>
    ungroup()
})

# Data with emigration and house ownership rates ######
library(readxl)
nmr <- read_excel("G:/Shared drives/CORESIDENCE/TEAM FOLDERS/Juan Galeano/VNM/NET_MIGRATION_RATES_2019.xlsx", 
                  sheet = "Sheet2")
names(nmr)
nmr_geo<-nmr|>
  group_by(GEOLEVEL1)|>
  summarise(wer=sum(net_migration_rate/1000*pop_2019)/sum(pop_2019)*1000)|>
  ungroup() # these are emigration rates sorry for the bad labelling


own_geo<-nmr|>
  group_by(GEOLEVEL1)|>
  summarise(whor=sum(homeown_rate/1000*pop_2019)/sum(pop_2019)*1000)|>
  ungroup()

own_geo2<-nmr|>
  group_by(GEOLEVEL1)|>
  summarise(whor2=sum(homeown_rate_real/1000*pop_2019)/sum(pop_2019)*1000)|>
  ungroup()

hdi_geo<-nmr|>
   group_by(GEOLEVEL1)|>
   summarise(hdir=sum(hdi/1000*pop_2019)/sum(pop_2019)*1000)|>
   ungroup()

unique(hdi_geo$GEOLEVEL1)



setdiff(unique(hdi_geo$GEOLEVEL1),
        unique(pop_geolevel[[2]]$GEOLEVEL1))

setdiff(unique(pop_geolevel[[2]]$GEOLEVEL1),
        unique(hdi_geo$GEOLEVEL1))

# Extract base population for the model #######

pop_model<-vnm_list2[[2]]|>
  filter(
    LA %in% c(40,41,42,50,51,52),
    EDATTAIN<=4
  )|>
  filter(age3=="60-80")|>
  mutate(id1=paste(SERIAL, PERNUM,sep="_"),
         id2=paste(SERIAL, SPLOC,sep="_"))

serials<-unique(pop_model$SERIAL)
ids2<-unique(pop_model$id2)
ids1<-unique(pop_model$id1)

`%notin%` <- Negate(`%in%`)

# Find all partners to compute age difference within the couple ####
pop_model2<-vnm_list2[[2]]|>
  filter(SERIAL %in%serials)|>
  mutate(id1=paste(SERIAL, PERNUM,sep="_"))|>
  filter(id1%in% c(ids2))|>
  filter(id1%notin% c(ids1))

pop_model<-pop_model|>select(1:54)

# Put all the partners together ########
pop_model1<-bind_rows(pop_model,pop_model2)
pop_model1<-pop_model1|>arrange(SERIAL, PERNUM)

# Individual variables to include in the model #######
var<-c("GEOLEVEL1","YEAR","SERIAL","PERNUM",
       "HWEIGHT","PWEIGHT","SEX","AGE","AGE2","SPOUSE_AGE","age3",
       "MARST","EDATTAIN","ownership", "urban","ethnicvn","empstat","indgen",
       "ser","LA","LA2","AGE_DIFF","AGE_DIFF_CAT")

# Compute age difference between partners, age difference categories and man older  ######
pop_model1 <- pop_model1|>
  # Join each person with their spouse's data
  left_join(pop_model1|> 
              select(SERIAL, PERNUM, AGE)|>
              rename(SPLOC = PERNUM, SPOUSE_AGE = AGE),
            by = c("SERIAL", "SPLOC"))|>
  # Calculate absolute age difference
  mutate(AGE_DIFF = abs(AGE - SPOUSE_AGE))|>
  mutate(
    MAN_OLDER = case_when(
      SEX == 1 & AGE == SPOUSE_AGE ~ 1,  # Man is older than woman (1 = yes)
      SEX == 2 & AGE == SPOUSE_AGE ~ 1,
      SEX == 1 & AGE > SPOUSE_AGE ~ 1,  # Man is older than woman (1 = yes)
      SEX == 2 & AGE < SPOUSE_AGE ~ 1,  # Woman's record: Her husband is older
      TRUE ~ 2                           # Otherwise, man is not older (0 = no)
    ),
    AGE_DIFF_CAT=if_else(MAN_OLDER==2,1,
                 if_else(AGE_DIFF<5,0,
                 #if_else(AGE_DIFF<5,2,
                 if_else(AGE_DIFF<10,2,3)))
    )

gc()
pop_model1 <- pop_model1|>
  select(all_of(var))

# Final population for model #######
df_model<-pop_model1|>
  mutate(age3=if_else(AGE>=60 
                      & AGE<=80
                      ,"60-80", "REST"))|>
  filter(
    LA %in% c(40,41,42,50,51,52),
    EDATTAIN<=4
  )|>
  filter(age3=="60-80")|>
  mutate(dependent=if_else(LA==40,1,0), 
         ethnicvn=unclass(ethnicvn),
         ethnicvn=if_else(ethnicvn==1,1,
                  if_else(ethnicvn %in% c(2,3,7,8,9,10,38,39,40,43,51,46,
                                          47,50,35),2,
                  if_else(ethnicvn %in% c(6,24,18,15),3,
                                  4))),
         empstat=if_else(empstat==1,1,2),
         indgen=if_else(indgen%in%c(0,998,999),0,
                        if_else(indgen==10,1,2)))|>
  mutate(SEX=unclass(SEX),
         SEX=as.factor(SEX),
         MARST=unclass(MARST),
         MARST=as.factor(MARST),
         urban=unclass(urban),
         urban=as.factor(urban),
         ser=unclass(ser),
         ser=as.factor(ser),
         EDATTAIN=unclass(EDATTAIN),
         EDATTAIN=as.factor(EDATTAIN),
         ethnicvn=unclass(ethnicvn),
         ethnicvn=as.factor(ethnicvn),
         empstat=unclass(empstat),
         empstat=as.factor(empstat),
         indgen=unclass(indgen),
         indgen=as.factor(indgen),
         ownership=unclass(ownership),
         ownership=as.factor(ownership),
         AGE_DIFF_CAT=unclass(AGE_DIFF_CAT),
         AGE_DIFF_CAT=as.factor(AGE_DIFF_CAT)
  )

# Add regional variables ######
#df_model<-df_model |> left_join(pop_agri[[2]], by = "GEOLEVEL1")
df_model<-df_model |> left_join(oldratio[[2]], by = "GEOLEVEL1")
df_model<-df_model|>left_join(nmr_geo, by="GEOLEVEL1")
df_model<-df_model|>left_join(own_geo, by="GEOLEVEL1")
df_model<-df_model|>left_join(own_geo2, by="GEOLEVEL1")
df_model<-df_model|>left_join(hdi_geo, by="GEOLEVEL1")

# Reomve some unnecessary variables ########
head(df_model)
df_model<-df_model|>select(-AGE2,-SPOUSE_AGE,-age3,
                           -AGE_DIFF,-elderly,-working_age,
                           -hdir)

# Modelling  ######
library(lme4)

your_data<-df_model

your_data<-as.data.frame(your_data)

class(your_data$ownership)
levels(your_data$ownership)

sex<-your_data|>
  group_by(SEX)|>
  summarise(n=n())|>
  mutate(n_rel = round(n / sum(n)*100,2))

marst<-your_data|>
  group_by(SEX, MARST)|>
  summarise(n=n())|>
  mutate(n_rel = round(n / sum(n)*100,2))

urban<-your_data|>
  group_by(SEX,urban)|>
  summarise(n=n())|>
  mutate(n_rel = round(n / sum(n)*100,2))

edattain<-your_data|>
  group_by(SEX,EDATTAIN)|>
  summarise(n=n())|>
  mutate(n_rel = round(n / sum(n)*100,2))

living_arr<-your_data|>
  group_by(SEX,LA)|>
  summarise(n=n())|>
  mutate(n_rel = round(n / sum(n)*100,2))


ownership<-your_data|>
group_by(ownership)|>
summarise(n=n())|>
mutate(n_rel = round(n / sum(n)*100,2))

ethnicvn<-your_data|>
  group_by(SEX,ethnicvn)|>
  summarise(n=n())|>
  mutate(n_rel = round(n / sum(n)*100,2))

empstat<-your_data|>
  group_by(SEX,empstat)|>
  summarise(n=n())|>
  mutate(n_rel = round(n / sum(n)*100,2))

your_data<-your_data|>mutate(ownership=as.character(ownership))|>
  filter(ownership %in% c("1","2"))|>
  mutate(ownership=as.factor(ownership))


ownership2<-your_data|>
  group_by(ownership,EDATTAIN)|>
  summarise(n=n())|>
  mutate(n_rel = round(n / sum(n)*100,2))




age_diff<-your_data|>
  group_by(AGE_DIFF_CAT)|>
  summarise(n=n())|>
  mutate(n_rel = round(n / sum(n)*100,2))


# scale variables #####
your_data$AGE3 <-as.numeric(scale(your_data$AGE))
your_data$old_ratio <-as.numeric(scale(your_data$old_ratio))
#your_data$agri <-as.numeric(scale(your_data$agri))
your_data$wer <-as.numeric(scale(your_data$wer))
your_data$whor <-as.numeric(scale(your_data$whor))
your_data$whor2 <-as.numeric(scale(your_data$whor2))
#your_data$AGE_DIFF <-as.numeric(scale(your_data$AGE_DIFF))
#your_data$whdi <-as.numeric(scale(your_data$hdir))
#your_data$PWEIGHT<-round(your_data$PWEIGHT,0)

unique(your_data$ethnicvn)
gc()

# model 0 ######
library(glmmTMB)
library(sjPlot)
model_0 <- glmmTMB(dependent ~(1 | GEOLEVEL1), 
               family = binomial,
               weights=PWEIGHT, 
               data = your_data)

summary(model_0)


# model 1: age and sex ######
model_1 <- glmmTMB(dependent ~ AGE3+SEX+(1 | GEOLEVEL1), 
                 family = binomial,
                 weights=PWEIGHT, 
                 data = your_data)

summary(model_1)
#tab_model(model_1)
# model 2: age + sex + age_diff  ######
model_2 <- glmmTMB(dependent ~ AGE3+SEX+AGE_DIFF_CAT+(1 | GEOLEVEL1), 
                 family = binomial,
                 weights=PWEIGHT, 
                 data = your_data)

summary(model_2)
#tab_model(model_0,model_1,model_2)

# model 3: age + sex + age_diff + edattain +ethnicvn, ownership + urban +empstat ######
model_3 <- glmmTMB(dependent ~ 
                   AGE3 + 
                   SEX + 
                   AGE_DIFF_CAT+
                   EDATTAIN + 
                   ethnicvn +
                   ownership+
                   urban + 
                   empstat +(1 | GEOLEVEL1), 
                 family = binomial,
                 weights=PWEIGHT, 
                 data = your_data)

summary(model_3)
#summary(model_3b)
# model 4: age + sex + age_diff + man_older + edattain +ethnicvn, ownership + urban +empstat + old_ratio + nrm + hwr ####
model_4 <- glmmTMB(dependent ~  
                   AGE3 + 
                   SEX + 
                   AGE_DIFF_CAT+
                   EDATTAIN + 
                   ethnicvn +
                   ownership+
                   urban + 
                   empstat +
                   old_ratio+
                   wer+
                   whor+
                   whor2+(1 | GEOLEVEL1), 
                 family = binomial,
                 weights=PWEIGHT, 
                 data = your_data)

summary(model_4)


model_4b <- glmmTMB(dependent ~  
                     AGE3 + 
                     SEX + 
                     AGE_DIFF_CAT+
                     EDATTAIN + 
                     ethnicvn +
                     ownership+
                     urban*whor + 
                     empstat +
                     old_ratio+
                     wer+
                     #whor+
                     whor2+(1 | GEOLEVEL1), 
                   family = binomial,
                   weights=PWEIGHT, 
                   data = your_data)

summary(model_4b)


library(sjPlot)
tab_model(model_0,model_1,model_2,model_3,model_4,model_4b)

tab_model(model_4b)

### figure 4 predicted probabilities #######

library(ggplot2)
library(ggeffects)
# 1 Get predicted probabilities: sex and age #######
pred_sex <- ggpredict(model_4b, 
                      terms = c("AGE [all]", "SEX"),
                      bias_correction = TRUE)
class(pred_sex)

pred_sex2<-as.data.frame(pred_sex)


pred_sex2<-pred_sex2|>
  mutate(facet="Couple-only living by age and sex")

# Plot
g1<-ggplot(pred_sex2, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  facet_wrap(~facet)+
  labs(
  #  title = "Couple-only living by age and sex",
    x = "\nAge",
    y = "Predicted Probability\n",
    color = "Sex",
    fill = "Sex"
  ) +
  scale_y_continuous(labels = scales::percent,
                     limits=c(0.4,.55))+
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"), labels = c("Men", "Women")) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"), labels = c("Men", "Women")) +
  theme_bw()+
  theme(legend.position = c(.95, 0.05),  # bottom-right (1 = right, 0 = bottom)
        legend.justification = c(1, 0),
        legend.background = element_rect(fill=NA,
                                         color = NA),
        strip.text = element_text(size = 11)) 


g1

# 2 get predicted probabilities: age difference couples  #######
age_dif_effect <- ggpredict(model_4b, terms = "AGE_DIFF_CAT",
                            bias_correction = TRUE)

class(age_dif_effect)

age_dif_effect2<-as.data.frame(age_dif_effect)


class(age_dif_effect$x)
levels(age_dif_effect$x)

age_dif_effect2<-age_dif_effect2|>
  mutate(age_dif=fct_recode(x,
                            "Man +<5" = "0",
                            "Woman\nolder" = "1",
                            #"Man 3-5\nyears older" = "2",
                            "Man +5-10" = "2",
                            "Man +>10" = "3"),
         age_dif=fct_relevel(age_dif,
                             c(  "Woman\nolder",
                                 "Man +<5",
                             
                              # "Man 3-5\nyears older",
                               "Man +5-10",
                               "Man +>10" ))
  )



levels(age_dif_effect2$age_dif)


age_dif_effect2<-age_dif_effect2|>
  mutate(facet="Couple-only living by couple's age gap")

g2<-ggplot(age_dif_effect2, aes(x = age_dif, y = predicted)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),color = "purple") +
  scale_y_continuous(labels = scales::percent,
                     limits=c(.4,.5))+
  facet_wrap(~facet)+
  labs(
  #  title = "Couple-only living by couple's age gap",
    x = "\nAge gap", 
    y = "Predicted Probability\n"
  ) +
  theme_bw()+
  theme(legend.position = "none",  # bottom-right (1 = right, 0 = bottom)
        legend.justification = c(1, 0),
        legend.background = element_rect(fill=NA,
                                         color = NA),
        strip.text  = element_text(size = 11)) 


g2

# 3 get predicted probabilities: Education #######
edu_effect <- ggpredict(model_4b, terms = "EDATTAIN",
                        bias_correction = TRUE)

class(edu_effect)

edu_effect2<-as.data.frame(edu_effect)



edu_effect2<-edu_effect2|>
  mutate(age_dif=fct_recode(x,
                            "Less than primary\ncompleted" = "1",
                            "Primary\ncompleted" = "2",
                            "Secondary\ncompleted" = "3",
                            "University\ncompleted" = "4"),
         age_dif=fct_relevel(age_dif,
                             c("Less than primary\ncompleted",
                               "Primary\ncompleted",
                               "Secondary\ncompleted",
                               "University\ncompleted"))
  )

levels(edu_effect2$age_dif)

edu_effect2<-edu_effect2|>
  mutate(facet="Couple-only living by educational level")


g3<-ggplot(edu_effect2, aes(x = age_dif, y = predicted)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), color = "purple") +
  scale_y_continuous(labels = scales::percent,
                     limits=c(.42,.52))+
  facet_wrap(~facet)+
  labs(
 #   title = "Couple-only living by educational level",
    x = "\nEducational attaiment", 
    y = "Predicted Probability\n"
  ) +
  theme_bw()+
  theme(legend.position = "none",  # bottom-right (1 = right, 0 = bottom)
        legend.justification = c(1, 0),
        legend.background = element_rect(fill=NA,
                                         color = NA),
        strip.text = element_text(size = 11)) 

g3

# 4 Get predicted probabilities: share of permanent housing by type of residence #######
pred_urb <- ggpredict(model_4b, 
                      terms = c("whor [all]", "urban"),
                      bias_correction = TRUE)

class(pred_urb)

pred_urb2<-as.data.frame(pred_urb)

pred_urb2<-pred_urb2|>
  mutate(facet="Couple-only living by permanent housing (%) and type of residence")

# Plot
g4 <- ggplot(pred_urb2, 
             aes(x = x / 100, 
                 y = predicted, 
                 color = group)) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), 
              alpha = 0.2) +
  scale_y_continuous(labels = scales::percent, limits = c(.3, .55)) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  facet_wrap(~facet)+
  labs(
   # title = "Couple-only living by permanent housing (%) and type of residence",
    x = "\nPermanent Dwellings (%)",
    y = "Predicted Probability\n",
    color = "Residence",
    fill = "Residence"
  ) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"),
                     labels = c("Rural", "Urban")) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"),
                    labels = c("Rural", "Urban")) +
  theme_bw() +
  theme(
    legend.position = c(.95, 0.05),
    # bottom-right (1 = right, 0 = bottom)
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = NA, color = NA),
    strip.text = element_text(size = 11)
  )  # anchor point)



g4

# Arrange plots ########

library(gridExtra)

a<-grid.arrange(
  g1,  g2, g3,g4,
  ncol = 2
)

ggsave(paste("G:\\Shared drives\\CORESIDENCE\\TEAM FOLDERS\\Juan Galeano\\VNM\\",
             "Multiplot.png",sep=""), # name of the file of the image
       plot=a,
       scale = .9, 
       dpi = 300,     
       height =10, #25  #10 
       width = 12)
