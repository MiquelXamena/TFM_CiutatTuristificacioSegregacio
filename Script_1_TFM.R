setwd("~/Uni/3. MÁSTER_METRO_UAB/_TFM")

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(stargazer)
library(readr)
library(xlsx)
library(stringr)
library(MLID)
library("sf")
library(sysfonts)
library(extrafont)
library(showtext)
library(car)
library(caret)
library(lmtest)
library(arm)
library(psych)
library(haven)
library(clustertend)
library(factoextra)
library(NbClust)
library(clValid)
library(igraph)
library(fitdistrplus)





font_add_google("Lato")
showtext_auto()


summary(Dades_Segreg_lm)


#######################################################################
#DADES
#######################################################################

#Segregacio
Renda <- read.xlsx("Dades/Renda_INE/RendaINE.xls", sheetName = "1")
Renda <- Renda %>% 
  filter(str_detect(SeccCensal_Brut, "sección")) %>% 
  mutate(SeccioCensal=str_extract(SeccCensal_Brut, "[:digit:]+")) %>% 
  filter(str_detect(SeccioCensal, "^07040|^07036|^07011|^07045|^07020|^07063|^07010|^07056|^07053|^07004|^07031")

NivellEstudis <- read.xlsx("Dades/NivelEstudios/NivelEstudios_2021.xlsx", sheetName = 1)

NivellEstudis <- NivellEstudis %>% 
  filter(str_detect(SeccioCensal, "^07040|^07036|^07011|^07045|^07020|^07063|^07010|^07056|^07053|^07004|^07031"))

Procedencia2019 <- read.csv2("Dades/LugarNacimiento_2022/Procedencia2019.csv", encoding = "UTF-8", dec = ",")

Procedencia2019 <- Procedencia2019 %>% 
  mutate(Total=as.numeric(as.vector(str_remove_all(Total, "\\.")))) %>% 
  filter(X.U.FEFF.Sexo =="Ambos Sexos") %>%  
  filter(`Sección`!="TOTAL") %>% 
  mutate(SeccioCensal=`Sección`) %>% 
  dplyr::select(-X.U.FEFF.Sexo) %>% 
  dplyr::select(-`Sección`) %>% 
  filter(str_detect(SeccioCensal, "^07040|^07036|^07011|^07045|^07020|^07063|^07010|^07056|^07053|^07004|^07031"))

Procedencia_wdr <- Procedencia2019 %>% 
  pivot_wider(names_from = `País.de.nacimiento`,
              values_from = Total)

Procedencia_IDH <- Procedencia_wdr %>% 
  mutate(BajoIDH=(`Total África`+Venezuela+Ecuador+Bolivia+Paraguay+Pakistán)) %>% 
  mutate(AltoIDH_mod=(`Nacidos en España`+Alemania+Francia+`Reino Unido`+Italia+Portugal))
  mutate(AltoIDH_mod=unlist(AltoIDH_mod)) %>% 
  mutate(BajoIDH=unlist(BajoIDH))
  
Procedencia_IDH <- as.data.frame(Procedencia_IDH) %>% 
  mutate(CMUN=str_remove(str_extract(SeccioCensal, "^....."), "^..")) %>% 
  mutate(Perc_BajoIDH=(BajoIDH/`Total Población`)*100) %>% 
  mutate(Perc_AltoIDH=(AltoIDH_mod/`Total Población`)*100) 


Perc_Procedenica <- Procedencia_IDH %>% 
  dplyr::select(SeccioCensal, Perc_BajoIDH, Perc_AltoIDH)

#Dades Turisme


#Turisme CAIB
TurismoCAIB_hab <- read.csv("Dades/Turisme/Habitatges_Tur_stics_Mallorca.csv", fileEncoding = "UTF-8")
TurismoCAIB_allo <- read.csv("Dades/Turisme/Allotjaments_Tur_stics_Mallorca.csv", fileEncoding = "UTF-8")
Restaura <- read.csv("Dades/Turisme/RestauracioOci/Empreses_Restauraci__Entreteniment_Mallorca.csv", fileEncoding = "UTF-8")
Airbnb <- read.csv("Dades/Turisme/INSIDE_AIRBNB/listings (1).csv", encoding = "UTF-8")


Allotj_CAIB_Data <- TurismoCAIB_allo %>% 
  mutate(any=format(as.Date(Inici.d.activitat,format="%d/%m/%Y"),"%Y"))

Hab_CAIB_Data <- TurismoCAIB_hab %>% 
  mutate(any=format(as.Date(Inici.d.activitat,format="%d/%m/%Y"),"%Y"))













############################################################################
#Calcul Segregacio i regressio
############################################################################

#Segregacio Per Secc Censal
IndexEdu_Palma <- id(filter(NivellEstudis,CMUN =="040"), vars = c("PercNoNivelSuperior", "PercNivelSuperior"))
IndexEdu <- id(NivellEstudis, vars = c("PercNoNivelSuperior", "PercNivelSuperior"))
ImpactEdu <- impacts(NivellEstudis, c("PercNoNivelSuperior", "PercNivelSuperior"), c("CMUN", "SeccioCensal"))
head(ImpactEdu, n = 5)

IndexProced_2019_Plm <- id(filter(Procedencia_IDH,CMUN =="040"), vars = c("BajoIDH","AltoIDH_mod"))
IndexProced_2019 <- id(Procedencia_IDH, vars = c("BajoIDH","AltoIDH_mod"))
ImpactProced <- impacts(Procedencia_IDH, c("BajoIDH","AltoIDH_mod"), c("SeccioCensal","CMUN"))
head(ImpactProced, n = 5)

IndexProced_esp <- id(Procedencia_IDH, vars = c("BajoIDH","Nacidos en España"))
ImpactProced_esp <- impacts(Procedencia_IDH, c("BajoIDH","Nacidos en España"), c("SeccioCensal", "CMUN"))
head(ImpactProced_esp, n = 5)


ImpactProced_data <- as.data.frame(ImpactProced$SeccioCensal) %>% 
  mutate(SegQuali_proced=cut(scldMean, breaks= c(-10,-1.5,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació inferior"))) %>% 
  mutate(SeccioCensal=rownames(ImpactProced$SeccioCensal)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact)

ImpactProced_data_esp <- as.data.frame(ImpactProced_esp$SeccioCensal) %>% 
  mutate(SegQuali_prod_esp=cut(scldMean, breaks= c(-10,-1.5,1.5,200), labels=c("Segregació superior", "No significatiu", "Segregació inferior"))) %>% 
  mutate(SeccioCensal=rownames(ImpactProced$SeccioCensal)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd_esp=impact)

ImpactEdu_data <- as.data.frame(ImpactEdu$SeccioCensal) %>% 
  mutate(SegQuali_Edu=cut(scldMean, breaks= c(-10,-1.5,1.5,200), labels=c("Segregació superior", "No significatiu", "Segregació inferior"))) %>% 
  mutate(SeccioCensal=rownames(ImpactEdu$SeccioCensal)) %>% 
  rename(MD_Edu=scldMean,
         impact_edu=impact)

table(ImpactEdu_data$SegQuali)
table(ImpactProced_data$SegQuali)

#segregacio per renda (barris i crisi)
    #vell

    #nou
Normalrenda <- fitdist(Renda$MediUC_Renda_2019,"norm")
summary(Normalrenda)

HighRenda <- 19014.602+(1.28*3806.374)
LowRenda <- 19014.602-(1.28*3806.374)


Renda2019 <- Renda %>% 
  dplyr::select(MediUC_Renda_2019, SeccioCensal) %>% 
  mutate(Quantils=cut(MediUC_Renda_2019, 
                      breaks=c(0, 16450,20650,34650),
                      labels=c("Renda vulnerable", "No significatiu", "Renda alta"))) %>% 
  mutate(ISSR=cut(MediUC_Renda_2019,
                  breaks=c(0, LowRenda,HighRenda,34650),
                labels=c("Segregació inferior", "No significatiu", "Segregació superior")))
TaulaNombreBarrisISSR <- Renda2019 %>% 
  group_by(ISSR) %>% 
  summarise(n=n(),
            mitjana=mean(MediUC_Renda_2019),
            sd=sd(MediUC_Renda_2019))



        #HISTOGRAMA
dadesNormalRenda <- data.frame("Normal"=rnorm(339, mean = 19014.602, sd=3806.374), 
                               "SeccioCensal"=Renda2019$SeccioCensal)
DadesGrafRenda <- Renda2019 %>% 
  dplyr::select(MediUC_Renda_2019,ISSR,SeccioCensal) %>% 
  full_join(dadesNormalRenda) %>% 
  pivot_longer(cols = c("MediUC_Renda_2019", "Normal"),names_to = "Obervacio", values_to = "Renda")

Graf_Renda_Normal <- ggplot(DadesGrafRenda, aes(x=Renda, colour=Obervacio, fill=Obervacio))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.2)+
  geom_density(alpha=0.4)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))


#Regressio renda-segregacio

renda_Seg_proc <- full_join(Renda2019, ImpactProced_data, by="SeccioCensal")
renda_seg_edu <- full_join(Renda2019,ImpactEdu_data, by="SeccioCensal")
seg_seg_eduIPrc <- full_join(ImpactProced_data,ImpactEdu_data, by="SeccioCensal")

renda_Seg_proc_lm <- lm(MediUC_Renda_2019~MD_Proced ,renda_Seg_proc)
renda_Seg_edu_lm <- lm(MediUC_Renda_2019~MD_Edu, renda_seg_edu )


stargazer(renda_Seg_edu_lm,renda_Seg_proc_lm, type = "text")

Grafic_Seg_renda <- ggplot(renda_Seg_proc, aes(MD_Proced, MediUC_Renda_2019))+
  geom_point()+
  geom_smooth()+
  labs(x="Diferència mitjana segons país de naix. (ID)",
       y="Mediana de renda (2019)")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))
        
Grafic_SegEdu_renda <- ggplot(renda_seg_edu, aes(MD_Edu, MediUC_Renda_2019))+
  geom_point()+
  geom_smooth()+
  labs(x="Diferència mitjana segons nivell d'estudis(ID)",
       y="Mediana de renda (2019)")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))

names(renda_seg_edu)


#Segregacio Procedencia (NO)

NormalProcd <- fitdist(Perc_Procedenica$Perc_BajoIDH,"norm")
summary(NormalProcd)

HighProced <- 5.760629+(1.28*5.615124)
LowProced <- 5.760629-(1.28*5.615124)

Perc_Procedenica <- Perc_Procedenica %>% 
  mutate(ISSR_proc=cut(Perc_BajoIDH,
                  breaks=c(0, LowProced,HighProced,34650),
                  labels=c("Segregació superior", "No significatiu", "Barri vulnerable"))) 

TaulaNombreBarrisISSR_Proced <- Perc_Procedenica %>% 
  group_by(ISSR_proc) %>% 
  summarise(n=n(),
            mitjana=mean(Perc_BajoIDH),
            sd=sd(Perc_BajoIDH))
















############################################################################
#Regressio Turisme i Segregacio
############################################################################

# Dades
Places_alt <- read.xlsx("Dades/Turisme/PlacesAlt.xlsx", sheetName = "PlacesAlt") %>% 
  dplyr::select(NUMPOINTS,CUSEC) %>%
  mutate(PlacesAlt=NUMPOINTS,
         SeccioCensal=CUSEC) %>% 
  dplyr::select(SeccioCensal,PlacesAlt)


Places_baix <- read.xlsx("Dades/Turisme/PlacesBaix.xlsx", sheetName = "PlacesBaix") %>% 
  mutate(SeccioCensal=CUSEC) %>% 
  dplyr::select(SeccioCensal,Places_Baix)

areaCUSEC <- read.xlsx("Dades/AreaSeccioCensal/AreaSeccioCensal.xlsx", sheetName = 1) %>% 
  mutate(SeccioCensal=CUSEC) %>% 
  dplyr::select(SeccioCensal,AREA) %>% 
  mutate(AREA=AREA/1000)

Poblacion2019 <- Procedencia2019 %>% 
  filter(País.de.nacimiento=="Total Población") %>% 
  mutate(Pobl=Total) %>% 
  dplyr::select(SeccioCensal, Pobl)
  
turisme_airbnbHotel <- st_read("Cartografia/Turismo/Turisme_Airbnb/CompletTurisme.shp")

turisme_airbnbHotel <- as.data.frame(turisme_airbnbHotel)

Turismo_Secc <- turisme_airbnbHotel %>% 
  dplyr::select(CUSEC, CUMUN,CSEC,CDIS,CMUN,CPRO,CCA,NumAirbnb,Numhabitat,Places) %>% 
  mutate(SeccioCensal=CUSEC)

ImpactEdu_data_res <- ImpactEdu_data %>% 
  dplyr::select(SeccioCensal,MD_Edu,impact_edu, SegQuali_Edu)
ImpactProced_data_res <- ImpactProced_data %>% 
  dplyr::select(SeccioCensal, MD_Proced, impact_procd, SegQuali_proced)

Dades_Migrant <- Procedencia_IDH %>% 
  dplyr::select(SeccioCensal,BajoIDH, AltoIDH_mod)



Dades_Segreg_lm <- full_join(Turismo_Secc, ImpactProced_data_res) %>% 
  full_join(ImpactEdu_data_res) %>% 
  full_join(Renda2019) %>% 
  full_join(Poblacion2019) %>% 
  full_join(areaCUSEC) %>% 
  full_join(Places_alt) %>% 
  full_join(Places_baix) %>% 
  full_join(Dades_Migrant) %>% 
  full_join(Perc_Procedenica) %>%  
  mutate(densiAirbnb=as.numeric((NumAirbnb/Pobl)*1000)) %>% 
  mutate(log_densiAirbnb=log1p(densiAirbnb)) %>% 
  mutate(log_densiAirbnb=na_if(log_densiAirbnb, "-Inf")) %>% 
  mutate(densiPlaces=as.numeric((Places/Pobl)*1000)) %>% 
  mutate(log_densiPlaces=log1p(densiPlaces)) %>% 
  mutate(log_densiPlaces=na_if(log_densiPlaces, "-Inf")) %>% 
  mutate(log_Airbnb=log1p(NumAirbnb)) %>% 
  mutate(log_Airbnb=na_if(log_Airbnb, "-Inf")) %>% 
  mutate(log_places=log1p(Places)) %>% 
  mutate(log_places=na_if(log_places, "-Inf")) %>% 
  mutate(log_places_baix=log1p(Places_Baix)) %>% 
  mutate(log_places_baix=na_if(log_places_baix, "-Inf")) %>% 
  mutate(log_places_alt=log1p(PlacesAlt)) %>% 
  mutate(log_places_alt=na_if(log_places_alt, "-Inf")) %>% 
  mutate(Ciutat=ifelse(str_detect(Dades_Segreg_lm$SeccioCensal, "^07040"),"Palma", "PartForana"))


ggplot(Dades_Segreg_lm, aes(x=MediUC_Renda_2019, y=Places_Baix))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = c(14142.44, 23886.76))


ggplot(Dades_Segreg_lm, aes(x=MediUC_Renda_2019, y=densiAirbnb))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = c(14142.44, 23886.76))


  #Normalitat dades
shapiro.test(Dades_Segreg_lm$log_Airbnb)
shapiro.test(Dades_Segreg_lm$log_densiAirbnb)
shapiro.test(Dades_Segreg_lm$log_densiPlaces)
shapiro.test(Dades_Segreg_lm$log_places)

  #regressions lineals
lm_procd_densiAirbnb <- lm(data=Dades_Segreg_lm, impact_procd~densiAirbnb+AREA)
lm_procd_Airbnb <- lm(data=Dades_Segreg_lm, impact_procd~NumAirbnb+AREA+Pobl)
lm_procd_densiPlaces <- lm(data=Dades_Segreg_lm, impact_procd~densiPlaces+AREA)
lm_procd_Places <- lm(data=Dades_Segreg_lm, impact_procd~log_places+AREA+Pobl)
lm_procd_Places_air <- lm(data=Dades_Segreg_lm, impact_procd~log_Airbnb+log_places+AREA+Pobl)

lm_edu_densiAirbnb <- lm(data=Dades_Segreg_lm, impact_edu~log_densiAirbnb+AREA)
lm_edu_Airbnb <- lm(data=Dades_Segreg_lm, impact_edu~log_Airbnb+AREA+Pobl)
lm_edu_densiPlaces <- lm(data=Dades_Segreg_lm, impact_edu~densiPlaces+AREA)
lm_edu_Places <- lm(data=Dades_Segreg_lm, impact_edu~log_places+AREA+Pobl)
lm_edu_Places_air <- lm(data=Dades_Segreg_lm, impact_edu~log_Airbnb+log_places+AREA+Pobl)

      #bo

lm_renda_densiAirbnb <- lm(data=Dades_Segreg_lm, MediUC_Renda_2019~densiAirbnb+AREA+Ciutat)
lm_renda_Airbnb <- lm(data=Dades_Segreg_lm, MediUC_Renda_2019~NumAirbnb+AREA+Pobl+Ciutat)
lm_renda_densiPlaces <- lm(data=Dades_Segreg_lm, MediUC_Renda_2019~densiPlaces+AREA+Ciutat)
lm_renda_Places <- lm(data=Dades_Segreg_lm, MediUC_Renda_2019~Places+AREA+Pobl+Ciutat)
lm_renda_Places_air <- lm(data=Dades_Segreg_lm, MediUC_Renda_2019~NumAirbnb+Places+AREA+Pobl+Ciutat)

lm_MDProcd_densiAirbnb <- lm(data=Dades_Segreg_lm, MD_Proced~densiAirbnb+AREA+Ciutat)
lm_MDProcd_Airbnb <- lm(data=Dades_Segreg_lm, MD_Proced~NumAirbnb+AREA+Pobl+Ciutat)
lm_MDProcd_densiPlaces <- lm(data=Dades_Segreg_lm, MD_Proced~densiPlaces+AREA+Ciutat)
lm_MDProcd_Places <- lm(data=Dades_Segreg_lm, MD_Proced~Places+AREA+Pobl+Ciutat)
lm_MDProcd_Places_air <- lm(data=Dades_Segreg_lm, MD_Proced~NumAirbnb+Places+AREA+Pobl+Ciutat)

lm_MDedu_densiAirbnb <- lm(data=Dades_Segreg_lm, MD_Edu~densiAirbnb+AREA+Ciutat)
lm_MDedu_Airbnb <- lm(data=Dades_Segreg_lm, MD_Edu~NumAirbnb+AREA+Pobl+Ciutat)
lm_MDedu_densiPlaces <- lm(data=Dades_Segreg_lm, MD_Edu~densiPlaces+AREA+Ciutat)
lm_MDedu_Places <- lm(data=Dades_Segreg_lm, MD_Edu~Places+AREA+Pobl+Ciutat)
lm_MDedu_Places_air <- lm(data=Dades_Segreg_lm, MD_Edu~NumAirbnb+Places+AREA+Pobl+Ciutat)


  #log

lm_renda_densiAirbnb_log <- lm(data=Dades_Segreg_lm, MediUC_Renda_2019~log_densiAirbnb+AREA+Ciutat)
lm_renda_Airbnb_log <- lm(data=Dades_Segreg_lm, MediUC_Renda_2019~log_Airbnb+AREA+Pobl+Ciutat)
lm_renda_densiPlaces_log <- lm(data=Dades_Segreg_lm, MediUC_Renda_2019~log_densiPlaces+AREA+Ciutat)
lm_renda_Places_log <- lm(data=Dades_Segreg_lm, MediUC_Renda_2019~log_places+AREA+Pobl+Ciutat)
lm_renda_Places_air_log <- lm(data=Dades_Segreg_lm, MediUC_Renda_2019~log_Airbnb+log_places+AREA+Pobl+Ciutat)

lm_MDProcd_densiAirbnb_log <- lm(data=Dades_Segreg_lm, MD_Proced~log_densiAirbnb+AREA+Ciutat)
lm_MDProcd_Airbnb_log <- lm(data=Dades_Segreg_lm, MD_Proced~log_Airbnb+AREA+Pobl+Ciutat)
lm_MDProcd_densiPlaces_log <- lm(data=Dades_Segreg_lm, MD_Proced~log_densiPlaces+AREA+Ciutat)
lm_MDProcd_Places_log <- lm(data=Dades_Segreg_lm, MD_Proced~log_places+AREA+Pobl+Ciutat)
lm_MDProcd_Places_air_log <- lm(data=Dades_Segreg_lm, MD_Proced~log_Airbnb+log_places+AREA+Pobl+Ciutat)

lm_MDedu_densiAirbnb_log <- lm(data=Dades_Segreg_lm, MD_Edu~log_densiAirbnb+AREA+Ciutat)
lm_MDedu_Airbnb_log <- lm(data=Dades_Segreg_lm, MD_Edu~log_Airbnb+AREA+Pobl+Ciutat)
lm_MDedu_densiPlaces_log <- lm(data=Dades_Segreg_lm, MD_Edu~log_densiPlaces+AREA+Ciutat)
lm_MDedu_Places_log <- lm(data=Dades_Segreg_lm, MD_Edu~log_places+AREA+Pobl+Ciutat)
lm_MDedu_Places_air_log <- lm(data=Dades_Segreg_lm, MD_Edu~log_Airbnb+log_places+AREA+Pobl+Ciutat)

#Alt i baix

lm_renda_PlacesAB_air <- lm(data=Dades_Segreg_lm, MediUC_Renda_2019~NumAirbnb+PlacesAlt+Places_Baix+AREA+Pobl+Ciutat)
lm_MDProcd_PlacesAB_air <- lm(data=Dades_Segreg_lm, MD_Proced~NumAirbnb+PlacesAlt+Places_Baix+AREA+Pobl+Ciutat)
lm_MDedu_PlacesAB_air <- lm(data=Dades_Segreg_lm, MD_Edu~NumAirbnb+PlacesAlt+Places_Baix+AREA+Pobl+Ciutat)

lm_renda_PlacesAB_air_log <- lm(data=Dades_Segreg_lm, MediUC_Renda_2019~log_Airbnb+log_places_baix+log_places_alt+AREA+Pobl+Ciutat)
lm_MDProcd_PlacesAB_air_log <- lm(data=Dades_Segreg_lm, MD_Proced~log_Airbnb+log_places_baix+log_places_alt+AREA+Pobl+Ciutat)
lm_MDedu_PlacesAB_air_log <- lm(data=Dades_Segreg_lm, MD_Edu~log_Airbnb+log_places_baix+log_places_alt+AREA+Pobl+Ciutat)


  #representacio taules
stargazer(lm_procd_densiAirbnb,lm_procd_Airbnb,
          lm_procd_densiPlaces, lm_procd_Places,lm_procd_Places_air,
          type = "text")

stargazer(lm_edu_Airbnb, lm_edu_Places_air,lm_edu_Places_air_renda,
          lm_edu_densiAirbnb, lm_edu_densiPlaces,lm_edu_Places,
          type="text")

    #bones de renda i median
stargazer(lm_renda_Airbnb, lm_renda_Places_air,
          lm_renda_densiAirbnb, lm_renda_densiPlaces,lm_renda_Places,
          type="text")

stargazer(lm_MDProcd_Airbnb, lm_MDProcd_Places_air,
          lm_MDProcd_densiAirbnb, lm_MDProcd_densiPlaces,lm_MDProcd_Places,
          type="text")

stargazer(lm_MDedu_Airbnb, lm_MDedu_Places_air,
          lm_MDedu_densiAirbnb, lm_MDedu_densiPlaces,lm_MDedu_Places,
          type="text")

#bones_log
stargazer(lm_renda_Airbnb_log, lm_renda_Places_air_log,
          lm_renda_densiAirbnb_log, lm_renda_densiPlaces,lm_renda_Places_log,
          type="text")

stargazer(lm_MDProcd_Airbnb_log, lm_MDProcd_Places_air_log,
          lm_MDProcd_densiAirbnb_log, lm_MDProcd_densiPlaces_log,lm_MDProcd_Places_log,
          type="text")

stargazer(lm_MDedu_Airbnb_log, lm_MDedu_Places_air_log,
          lm_MDedu_densiAirbnb_log, lm_MDedu_densiPlaces_log,lm_MDedu_Places_log,
          type="text")







  #definitiu

stargazer(lm_renda_Places_air_log,lm_renda_Places_air,
          lm_MDProcd_Places_air_log,lm_MDProcd_Places_air,
          lm_MDedu_Places_air_log,lm_MDedu_Places_air,lm_renda_PlacesAB_air, lm_MDProcd_PlacesAB_air,lm_MDedu_PlacesAB_air,
          lm_renda_PlacesAB_air_log, lm_MDProcd_PlacesAB_air_log, lm_MDedu_PlacesAB_air_log,
          type="text")






  #test

par(mfrow=c(2,2)) 

plot(lm_renda_Airbnb)
plot(lm_renda_Airbnb_log)
plot(lm_renda_Places_air)
plot(lm_renda_Places_air_log)
plot(lm_renda_Places)
plot(lm_renda_Places_log)
plot(lm_renda_densiPlaces)
plot(lm_renda_densiPlaces_log)


plot(lm_MDProcd_Airbnb)
plot(lm_MDProcd_Airbnb_log)
plot(lm_MDProcd_Places)
plot(lm_MDProcd_Places_log)
plot(lm_MDProcd_densiPlaces)
plot(lm_MDProcd_densiPlaces_log)
plot(lm_MDProcd_Places_air)
plot(lm_MDProcd_Places_air_log)


plot(lm_MDedu_Airbnb)
plot(lm_MDedu_Airbnb_log)
plot(lm_MDedu_Places)
plot(lm_MDedu_Places_log)
plot(lm_MDedu_densiPlaces)
plot(lm_MDedu_densiPlaces_log)
plot(lm_MDedu_Places_air)
plot(lm_MDedu_Places_air_log)










##########################################################################
#Alt baix
###########################################################################

    #RegressióAltBaix NO
Dades_AltBaix_lm <- Dades_Segreg_lm %>% 
  filter(ISSR!="No significatiu") %>% 
  mutate(RendaAlta=ifelse(ISSR=="Segregació superior", 1, 0))

Logit_Renda <- glm(RendaAlta ~ NumAirbnb+Places+AREA+Pobl+Ciutat+AltoIDH_mod,
             family = binomial(link="logit"),
             data = Dades_AltBaix_lm)
stargazer(Logit_Renda, type="text")

Logit_Renda_AltBaix <- glm(RendaAlta ~ NumAirbnb+PlacesAlt+Places_Baix+AREA+Pobl+Ciutat+AltoIDH_mod,
                   family = binomial(link="logit"),
                   data = Dades_AltBaix_lm)
stargazer(Logit_Renda_AltBaix, type="text")


Dades_Alt_lm <- Dades_Segreg_lm %>% 
  filter(ISSR!="Segregació superior")

lm_renda_Airbnb_alt <- lm(data=Dades_Alt_lm, MediUC_Renda_2019~NumAirbnb+AREA+Pobl+Ciutat)
lm_renda_Places_alt <- lm(data=Dades_Alt_lm, MediUC_Renda_2019~Places+AREA+Pobl+Ciutat)
lm_renda_Places_air_alt <- lm(data=Dades_Alt_lm, MediUC_Renda_2019~NumAirbnb+Places+AREA+Pobl+Ciutat)

stargazer(lm_renda_Airbnb_alt,lm_renda_Places_alt,lm_renda_Places_air_alt, type="text")


Dades_Baix_lm <- Dades_Segreg_lm %>% 
  filter(ISSR!="Segregació inferior")

lm_renda_Airbnb_baix <- lm(data=Dades_Baix_lm, MediUC_Renda_2019~NumAirbnb+AREA+Pobl+Ciutat)
lm_renda_Places_baix <- lm(data=Dades_Baix_lm, MediUC_Renda_2019~Places+AREA+Pobl+Ciutat)
lm_renda_Places_air_baix <- lm(data=Dades_Baix_lm, MediUC_Renda_2019~NumAirbnb+Places+AREA+Pobl+Ciutat)

stargazer(lm_renda_Airbnb_baix,lm_renda_Places_baix,lm_renda_Places_air_baix, type="text")


Dades_PlacesAlt_lm <- Dades_Segreg_lm %>% 
  filter(PlacesAlt!=0)

lm_PlacesAlt_renda <- lm(data=Dades_Baix_lm,PlacesAlt ~MediUC_Renda_2019+AREA+Pobl+Ciutat)
stargazer(lm_PlacesAlt_renda, type="text")

Dades_PlacesBaix_lm <- Dades_Segreg_lm %>% 
  filter(Places_Baix!=0)

lm_PlacesBaix_renda <- lm(data=Dades_Baix_lm,Places_Baix ~MediUC_Renda_2019+AREA+Pobl+Ciutat)
stargazer(lm_PlacesBaix_renda, type="text")











##########################
#Diferencia Mitjana de places alt i baix
##########################


#taula bàsica de mitjanes i sd

mitjanes_Baix <- Dades_Segreg_lm %>% 
  filter(Places_Baix!=0)
mitjanes_Alt <- Dades_Segreg_lm %>% 
  filter(PlacesAlt!=0)
summary(mitjanes_Baix$Places_Baix)
summary(mitjanes_Alt$PlacesAlt)


Dades_PlacesBaix_taula <- Dades_Segreg_lm %>% 
  pivot_longer(cols=c("Places_Baix", "PlacesAlt"),
               names_to = "CategoriaPlaces",
               values_to="Places_Quanti_categ") %>% 
  filter(Places_Quanti_categ>250) %>%
  group_by(CategoriaPlaces) %>% 
  summarize(MitjanaRenda=mean(MediUC_Renda_2019, na.rm=TRUE),
            sdRenda=sd(MediUC_Renda_2019, na.rm=TRUE),
            MitjanaEdu=mean(MD_Edu, na.rm=TRUE),
            sdEdu=sd(MD_Edu,  na.rm=TRUE),
            MitjanaProced=mean(MD_Proced, na.rm=TRUE),
            sdProced=sd(MD_Proced, na.rm=TRUE))


#Gràfics

Dades_PlacesBaix_Grph <- Dades_PlacesBaix_taula %>% 
  mutate(CategoriaPlaces=ifelse(CategoriaPlaces=="Places_Baix", "Baixa categoria", "Alta categoria"))


ggplot(Dades_PlacesBaix_Grph, aes(x=CategoriaPlaces, y=MitjanaRenda))+
  geom_point()+
  geom_errorbar(aes(ymin=MitjanaRenda-sdRenda, 
                    ymax=MitjanaRenda+sdRenda))+
  labs(x="Categoria de les places",
       y="Mediana de renda (2019)")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))

ggplot(Dades_PlacesBaix_Grph, aes(x=CategoriaPlaces, y=MitjanaEdu))+
  geom_point()+
  geom_errorbar(aes(ymin=MitjanaEdu-sdEdu, 
                    ymax=MitjanaEdu+sdEdu))+
  labs(x="Categoria de les places",
       y="Diferència mitjana segons nivell d'educació (ID)")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))

ggplot(Dades_PlacesBaix_Grph, aes(x=CategoriaPlaces, y=MitjanaProced))+
  geom_point()+
  geom_errorbar(aes(ymin=MitjanaProced-sdProced, 
                    ymax=MitjanaProced+sdProced))+
  labs(x="Categoria de les places",
       y="Diferència mitjana segons país de naixament (ID)")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))



  #t-test
Dades_PlacesBaix_test <- Dades_Segreg_lm %>% 
  pivot_longer(cols=c("Places_Baix", "PlacesAlt"),
               names_to = "CategoriaPlaces",
               values_to="Places_Quanti_categ") %>% 
  filter(Places_Quanti_categ>250) %>% 
  mutate(CategoriaPlaces=ifelse(CategoriaPlaces=="Places_Baix", "Baixa categoria", "Alta categoria"))


t.test(MediUC_Renda_2019~CategoriaPlaces,data=Dades_PlacesBaix_test)
t.test(MD_Edu~CategoriaPlaces,data=Dades_PlacesBaix_test)
t.test(MD_Proced~CategoriaPlaces,data=Dades_PlacesBaix_test)

















##########################################
#Index de tursitificació
##########################################

#Dades

names(Dades_Segreg_lm)
Dades_Segreg_lm <- Dades_Segreg_lm %>% 
  mutate(AreaSegregacio_Quali=ifelse(SegQuali_Edu=="Segregació superior"|
                                       ISSR=="Segregació superior", "Segregació superior",
                                     ifelse(SegQuali_Edu=="Segregació inferior"|
                                              ISSR=="Segregació inferior"|
                                              SegQuali_proced =="Segregació inferior", "Segregació inferior",
                                            "No significatiu"))) %>% 
  mutate(RestaRendaAlta=ifelse(AreaSegregacio_Quali!="Segregació superior"&
                                 Quantils=="Renda alta", "Renda alta", 
                               ifelse(AreaSegregacio_Quali!="Segregació inferior"&
                                        Quantils=="Renda vulnerable", "Renda vulnerable", "kk")))
Dades_Segreg_FINAL <- Dades_Segreg_lm %>% 
  dplyr::select(SeccioCensal, AreaSegregacio_Quali, densiAirbnb, Places, Pobl,RestaRendaAlta)


Dades_PlacesNou <- read.xlsx("Dades/Turisme/AllotjamentsNous/AllotjamentsNous.xlsx", sheetName = "AllotjamentsNous") %>% 
  mutate(SeccioCensal=CUSEC) %>% 
  dplyr::select(SeccioCensal, PlacesNou)

Dades_PlacesVellBaix <- read.xlsx("Dades/Turisme/AllotjamentVells1980/AllotjamentVells1980_baixa.xlsx", sheetName = "AllotjamentVells1980_baixa") %>% 
  mutate(SeccioCensal=CUSEC) %>% 
  dplyr::select(SeccioCensal, PlacesBaix1980)


Dades_IndexTurisme <- Dades_Cluster2 %>% 
  full_join(Dades_PlacesNou) %>% 
  full_join(Dades_PlacesVellBaix) %>% 
  full_join(Dades_Segreg_FINAL) %>% 
  mutate(PlacesMadures_perc =(PlacesBaix1980/Places)) %>% 
  mutate(PlacesNous_perc=(PlacesNou/Places)) %>% 
  mutate(PlacesMadures_perc=ifelse(is.na(PlacesMadures_perc), 0, PlacesMadures_perc)) %>% 
  mutate(PlacesNous_perc=ifelse(is.na(PlacesNous_perc), 0, PlacesNous_perc)) %>% 
  mutate(densiPlacesAltes=(PlacesAlt/Pobl)*1000) %>% 
  mutate(densiPlacesBaixes=(Places_Baix/Pobl)*1000) %>% 
  mutate(densiPlacesAltes=ifelse(is.na(densiPlacesAltes), 0, densiPlacesAltes)) %>%
  mutate(densiPlacesBaixes=ifelse(is.na(densiPlacesBaixes), 0, densiPlacesBaixes)) %>% 
  mutate(densiPlacesAltes = (densiPlacesAltes - min(densiPlacesAltes)) / (max(densiPlacesAltes) - min(densiPlacesAltes))) %>%
  mutate(densiPlacesBaixes =(densiPlacesBaixes - min(densiPlacesBaixes)) / (max(densiPlacesBaixes) - min(densiPlacesBaixes))) %>%
  mutate(densiAirbnb = (densiAirbnb - min(densiAirbnb)) / (max(densiAirbnb) - min(densiAirbnb))) %>%
  mutate(PlacesNou = (PlacesNou - min(PlacesNou)) / (max(PlacesNou) - min(PlacesNou))) %>%
  mutate(densiPlaces = (densiPlaces - min(densiPlaces)) / (max(densiPlaces) - min(densiPlaces))) %>%
  dplyr::select(SeccioCensal, densiPlacesAltes, densiAirbnb, PercNoPrin, PlacesNous_perc,
                densiPlacesBaixes,PlacesMadures_perc, PlacesNou, densiPlaces)


#reestructuracio
Index_Reestructuracio <- Dades_IndexTurisme %>% 
  dplyr::select(SeccioCensal, densiAirbnb, densiPlacesAltes, PercNoPrin)
rownames(Index_Reestructuracio) <- Index_Reestructuracio$SeccioCensal
Index_Reestructuracio <- Index_Reestructuracio %>% 
  dplyr::select(-SeccioCensal)

weights_Rt <- c(0.20, 0.50, 0.30)

Index_Reestructuracio$weighted_index <- rowSums(Index_Reestructuracio * weights_Rt)

summary(Index_Reestructuracio$weighted_index)

Index_Reestructuracio <- Index_Reestructuracio %>% 
  mutate(SeccioCensal=rownames(Index_Reestructuracio))

summary(Index_Reestructuracio)

Index_Reestructuracio2 <- Index_Reestructuracio %>%
  mutate(IndexReestructuracio=weighted_index) %>% 
  dplyr::select(IndexReestructuracio, SeccioCensal)


#madur
Index_madur <- Dades_IndexTurisme %>% 
  dplyr::select(SeccioCensal, densiPlaces, densiPlacesBaixes, PlacesMadures_perc )
rownames(Index_madur) <- Index_madur$SeccioCensal
Index_madur <- Index_madur %>% 
  dplyr::select(-SeccioCensal)

weights_madur <- c(0.1, 0.6, 0.35)

Index_madur$weighted_index <- rowSums(Index_madur * weights_madur)

summary(Index_madur$weighted_index)

Index_madur <- Index_madur %>% 
  mutate(SeccioCensal=rownames(Index_madur))

Index_madur2 <- Index_madur %>%
  mutate(IndexMadur=weighted_index) %>% 
  dplyr::select(IndexMadur, SeccioCensal)


#t-test
quantile(Index_madur2$IndexMadur, probs=c(.1,.9))
quantile(Index_Reestructuracio2$IndexReestructuracio, probs=c(.1,.9))

IndexosTurisme <- full_join(Index_madur2, Index_Reestructuracio2, 
                            by="SeccioCensal") %>%
  mutate(Index=ifelse(IndexMadur>=0.26&IndexReestructuracio>=IndexMadur, "Àrea de reestructuració",
                      ifelse(IndexReestructuracio>=0.26&IndexReestructuracio>=IndexMadur, "Àrea de reestructuració",
                             ifelse(IndexMadur>=0.14&IndexReestructuracio<IndexMadur, 
                                    "Àrea madura", "kk")))) %>% 
  full_join(Dades_Segreg_lm) %>% 
  filter(Index!="kk")

IndexosTurisme_Mapa <- full_join(Index_madur2, Index_Reestructuracio2, 
                            by="SeccioCensal") %>%
  mutate(Index=ifelse(IndexMadur>=0.26&IndexReestructuracio>=IndexMadur, "Àrea de reestructuració",
                      ifelse(IndexReestructuracio>=0.26&IndexReestructuracio>=IndexMadur, "Àrea de reestructuració",
                             ifelse(IndexMadur>=0.14&IndexReestructuracio<IndexMadur, 
                                    "Àrea madura", "kk"))))


t.test(MediUC_Renda_2019~Index,data=IndexosTurisme)
t.test(MD_Edu~Index,data=IndexosTurisme)
t.test(MD_Proced~Index,data=IndexosTurisme)



#Grafics dm


IndexosTurisme_group <- IndexosTurisme %>% 
  group_by(Index) %>% 
  summarize(MitjanaRenda=mean(MediUC_Renda_2019, na.rm=TRUE),
            sdRenda=sd(MediUC_Renda_2019, na.rm=TRUE),
            MitjanaEdu=mean(MD_Edu, na.rm=TRUE),
            sdEdu=sd(MD_Edu,  na.rm=TRUE),
            MitjanaProced=mean(MD_Proced, na.rm=TRUE),
            sdProced=sd(MD_Proced, na.rm=TRUE))


ggplot(IndexosTurisme_group, aes(x=Index, y=MitjanaRenda))+
  geom_point()+
  geom_errorbar(aes(ymin=MitjanaRenda-sdRenda, 
                    ymax=MitjanaRenda+sdRenda))+
  labs(x="Tipologia de model turístic",
       y="Mediana de renda (2019)")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))

ggplot(IndexosTurisme_group, aes(x=Index, y=MitjanaEdu))+
  geom_point()+
  geom_errorbar(aes(ymin=MitjanaEdu-sdEdu, 
                    ymax=MitjanaEdu+sdEdu))+
  labs(x="Tipologia de model turístic",
       y="Diferència mitjana segons nivell d'educació (ID)")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))

ggplot(IndexosTurisme_group, aes(x=Index, y=MitjanaProced))+
  geom_point()+
  geom_errorbar(aes(ymin=MitjanaProced-sdProced, 
                    ymax=MitjanaProced+sdProced))+
  labs(x="Tipologia de model turístic",
       y="Diferència mitjana segons país de naixament (ID)")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12)) 
  
  
  
  
#Grafics regressions


IndexosTurisme3 <- full_join(Index_madur2, Index_Reestructuracio2, 
                             by="SeccioCensal") %>%
  mutate(Index=ifelse(IndexMadur>=0.26&IndexReestructuracio>=IndexMadur, "Àrea de reestructuració",
                      ifelse(IndexReestructuracio>=0.26&IndexReestructuracio>=IndexMadur, "Àrea de reestructuració",
                             ifelse(IndexMadur>=0.14&IndexReestructuracio<IndexMadur, 
                                    "Àrea madura", "kk")))) %>% 
  full_join(Dades_Segreg_lm) %>% 
  pivot_longer(names_to = "Tipologia_índex",
               values_to = "Coeficient_índex",
               cols = c("IndexMadur", "IndexReestructuracio"))


GPH_RendaIndex <- ggplot(IndexosTurisme3, aes(x=MediUC_Renda_2019, y=Coeficient_índex, colour=Tipologia_índex))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = c(14142.44, 23886.76), linetype="dashed")+
  geom_vline(xintercept = c(18550, 19015), linetype="dotted", color="red")+
  labs(color=NULL,
       y="Coeficient dels índexs d'intensitat turís.",
       x="Mediana de renda per unitat de consum")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))
  
GPH_EduIndex <- ggplot(IndexosTurisme3, aes(x=MD_Edu, y=Coeficient_índex, colour=Tipologia_índex))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = c(-1.5, 1.5), linetype="dashed")+
  labs(color=NULL,
       y="Coeficient dels índexs d'intensitat turís.",
       x="Diferències mitjanes en segregació (Nivell educatiu)")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))

GPH_ProcedIndex <- ggplot(IndexosTurisme3, aes(x=MD_Proced, y=Coeficient_índex, colour=Tipologia_índex))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = c(-1.5, 1.5), linetype="dashed")+
  labs(color=NULL,
       y="Coeficient dels índexs d'intensitat turís.",
       x="Diferències mitjanes en segregació (País naix.)")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))







#Grafics filtrats
IndexosTurisme4 <- full_join(Index_madur2, Index_Reestructuracio2, 
                             by="SeccioCensal") %>%
  mutate(Index=ifelse(IndexMadur>=0.26&IndexReestructuracio>=IndexMadur, "Àrea de reestructuració",
                      ifelse(IndexReestructuracio>=0.26&IndexReestructuracio>=IndexMadur, "Àrea de reestructuració",
                             ifelse(IndexMadur>=0.14&IndexReestructuracio<IndexMadur, 
                                    "Àrea madura", "kk")))) %>% 
  full_join(Dades_Segreg_lm) 



GPH_EduIRT_F <- ggplot(filter(IndexosTurisme4, Index=="Àrea de reestructuració"), aes(x=MD_Edu, y=IndexReestructuracio))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = c(-1.5, 1.5), linetype="dashed")+
  labs(color=NULL,
       y="Índex de reestructuració turística",
       x="Diferències mitjanes en segregació (Nivell educatiu)")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))

GPH_RendaIRT_F <- ggplot(filter(IndexosTurisme4, Index=="Àrea de reestructuració"), aes(x=MediUC_Renda_2019, y=IndexReestructuracio))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = c(14142.44, 23886.76), linetype="dashed")+
  geom_vline(xintercept = c(18550, 19015), linetype="dotted", color="red")+
  labs(color=NULL,
       y="Índex de reestructuració turística",
       x="Mediana de renda per unitat de consum")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))




GPH_EduITM_F <- ggplot(filter(IndexosTurisme4, Index=="Àrea madura"), aes(x=MD_Edu, y=IndexMadur))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = c(-1.5, 1.5), linetype="dashed")+
  labs(color=NULL,
       y="Índex de turisme madur",
       x="Diferències mitjanes en segregació (Nivell educatiu)")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))

GPH_RendaITM_F <- ggplot(filter(IndexosTurisme4, Index=="Àrea madura"), aes(x=MediUC_Renda_2019, y=IndexMadur))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = c(14142.44, 23886.76), linetype="dashed")+
  geom_vline(xintercept = c(18550, 19015), linetype="dotted", color="red")+
  labs(color=NULL,
       y="Índex de turisme madur",
       x="Mediana de renda per unitat de consum")+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))















###########################################################################
#Longitudinals 
###########################################################################

##preparacio dades


#segregacio longitudinal
  #2019
ImapctProcd_2019 <- ImpactProced

Proced_data__2019_SC <- as.data.frame(ImapctProcd_2019$SeccioCensal) %>% 
  mutate(SegQuali_procd=cut(scldMean, breaks= c(-10,-1.5,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació"))) %>% 
  mutate(SeccioCensal=rownames(ImpactProced$SeccioCensal)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact) %>% 
  mutate(Any="2019") %>% 
  dplyr::select(SeccioCensal, starts_with("MD"), starts_with("impact"), starts_with("SegQual"), Any)

Proced_data__2019_MUN <- as.data.frame(ImapctProcd_2019$CMUN) %>% 
  mutate(SegQuali_procd=cut(scldMean, breaks= c(-10,-1,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació"))) %>% 
  mutate(CMUN=rownames(ImpactProced$CMUN)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact) %>% 
  mutate(Any="2019") %>% 
  dplyr::select(CMUN, starts_with("MD"), starts_with("impact"), starts_with("SegQual"), Any)



  #2017
Procedencia2017 <- read.csv2("Dades/LugarNacimiento/Procedencia2017.csv", encoding = "UTF-8", dec = ",")

Procedencia2017 <- Procedencia2017 %>% 
  mutate(Total=as.numeric(as.vector(str_remove_all(Total, "\\.")))) %>% 
  filter(X.U.FEFF.Sexo =="Ambos Sexos") %>%  
  filter(`Sección`!="TOTAL") %>% 
  mutate(SeccioCensal=`Sección`) %>% 
  dplyr::select(-X.U.FEFF.Sexo) %>% 
  dplyr::select(-`Sección`) %>% 
  filter(str_detect(SeccioCensal, "^07040|^07036|^07011|^07045|^07020|^07063|^07010|^07056|^07053|^07004|^07031")) %>% 
  pivot_wider(names_from = `País.de.nacimiento`,
              values_from = Total) %>% 
  mutate(BajoIDH=(`Total Africa`+Venezuela+Ecuador+Bolivia+Paraguay+Pakistán)) %>% 
  mutate(AltoIDH_mod=(`Nacidos en España`+Alemania+Francia+`Reino Unido`+Italia+Portugal)) %>% 
  mutate(AltoIDH_mod=unlist(AltoIDH_mod)) %>% 
  mutate(BajoIDH=unlist(BajoIDH))

Procedencia2017 <- as.data.frame(Procedencia2017) %>% 
  mutate(CMUN=str_remove(str_extract(SeccioCensal, "^....."), "^.."))

IndexProced <- id(Procedencia2017, vars = c("BajoIDH","AltoIDH_mod"))
ImpactProcd_2017 <- impacts(Procedencia2017, c("BajoIDH","AltoIDH_mod"), c("SeccioCensal","CMUN"))

Proced_data__2017_SC <- as.data.frame(ImpactProcd_2017$SeccioCensal) %>% 
  mutate(SegQuali_procd=cut(scldMean, breaks= c(-10,-1,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació"))) %>% 
  mutate(SeccioCensal=rownames(ImpactProcd_2017$SeccioCensal)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact) %>% 
  mutate(Any="2017") %>% 
  dplyr::select(SeccioCensal, starts_with("MD"), starts_with("impact"), starts_with("SegQual"), Any)

Proced_data__2017_MUN <- as.data.frame(ImpactProcd_2017$CMUN) %>% 
  mutate(SegQuali_procd=cut(scldMean, breaks= c(-10,-1,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació"))) %>% 
  mutate(CMUN=rownames(ImpactProcd_2017$CMUN)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact) %>% 
  mutate(Any="2017") %>% 
  dplyr::select(CMUN, starts_with("MD"), starts_with("impact"), starts_with("SegQual"), Any)

  #2021
Procedencia2021 <- read.csv2("Dades/LugarNacimiento/Procedencia2021.csv", encoding = "UTF-8", dec = ",")

Procedencia2021 <- Procedencia2021 %>% 
  mutate(Total=as.numeric(as.vector(str_remove_all(Total, "\\.")))) %>% 
  filter(X.U.FEFF.Sexo =="Ambos Sexos") %>%  
  filter(`Sección`!="TOTAL") %>% 
  mutate(SeccioCensal=`Sección`) %>% 
  dplyr::select(-X.U.FEFF.Sexo) %>% 
  dplyr::select(-`Sección`) %>% 
  filter(str_detect(SeccioCensal, "^07040|^07036|^07011|^07045|^07020|^07063|^07010|^07056|^07053|^07004|^07031")) %>% 
  pivot_wider(names_from = `País.de.nacimiento`,
              values_from = Total) %>% 
  mutate(BajoIDH=(`Total África`+Venezuela+Ecuador+Bolivia+Paraguay+Pakistán)) %>% 
  mutate(AltoIDH_mod=(`Nacidos en España`+Alemania+Francia+`Reino Unido`+Italia+Portugal)) %>% 
  mutate(AltoIDH_mod=unlist(AltoIDH_mod)) %>% 
  mutate(BajoIDH=unlist(BajoIDH))

Procedencia2021 <- as.data.frame(Procedencia2021) %>% 
  mutate(CMUN=str_remove(str_extract(SeccioCensal, "^....."), "^.."))

IndexProced <- id(Procedencia2021, vars = c("BajoIDH","AltoIDH_mod"))
ImpactProcd_2021 <- impacts(Procedencia2021, c("BajoIDH","AltoIDH_mod"), c("SeccioCensal","CMUN"))
head(ImpactProcd_2021, n = 5)


Proced_data__2021_SC <- as.data.frame(ImpactProcd_2021$SeccioCensal) %>% 
  mutate(SegQuali_procd=cut(scldMean, breaks= c(-10,-1,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació"))) %>% 
  mutate(SeccioCensal=rownames(ImpactProcd_2021$SeccioCensal)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact) %>% 
  mutate(Any="2021") %>% 
  dplyr::select(SeccioCensal, starts_with("MD"), starts_with("impact"), starts_with("SegQual"), Any)

Proced_data__2021_MUN <- as.data.frame(ImpactProcd_2021$CMUN) %>% 
  mutate(SegQuali_procd=cut(scldMean, breaks= c(-10,-1,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació"))) %>% 
  mutate(CMUN=rownames(ImpactProcd_2021$CMUN)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact) %>% 
  mutate(Any="2021") %>% 
  dplyr::select(CMUN, starts_with("MD"), starts_with("impact"), starts_with("SegQual"), Any)

  #2015
Procedencia2015 <- read.csv2("Dades/LugarNacimiento/Procedencia2015.csv", encoding = "UTF-8", dec = ",")

Procedencia2015 <- Procedencia2015 %>% 
  mutate(Total=as.numeric(as.vector(str_remove_all(Total, "\\.")))) %>% 
  filter(X.U.FEFF.Sexo =="Ambos Sexos") %>%  
  filter(`Sección`!="TOTAL") %>% 
  mutate(SeccioCensal=`Sección`) %>% 
  dplyr::select(-X.U.FEFF.Sexo) %>% 
  dplyr::select(-`Sección`) %>% 
  filter(str_detect(SeccioCensal, "^07040|^07036|^07011|^07045|^07020|^07063|^07010|^07056|^07053|^07004|^07031")) %>% 
  pivot_wider(names_from = `País.de.nacimiento`,
              values_from = Total) %>% 
  mutate(BajoIDH=(`Total Africa`+Venezuela+Ecuador+Bolivia+Paraguay+Pakistán)) %>% 
  mutate(AltoIDH_mod=(`Nacidos en España`+Alemania+Francia+`Reino Unido`+Italia+Portugal)) %>% 
  mutate(AltoIDH_mod=unlist(AltoIDH_mod)) %>% 
  mutate(BajoIDH=unlist(BajoIDH))

Procedencia2015 <- as.data.frame(Procedencia2015) %>% 
  mutate(CMUN=str_remove(str_extract(SeccioCensal, "^....."), "^.."))

IndexProced <- id(Procedencia2015, vars = c("BajoIDH","AltoIDH_mod"))
ImpactProcd_2015 <- impacts(Procedencia2015, c("BajoIDH","AltoIDH_mod"), c("SeccioCensal","CMUN"))
head(ImpactProcd_2015, n = 5)


Proced_data__2015_SC <- as.data.frame(ImpactProcd_2015$SeccioCensal) %>% 
  mutate(SegQuali_procd=cut(scldMean, breaks= c(-10,-1,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació"))) %>% 
  mutate(SeccioCensal=rownames(ImpactProcd_2015$SeccioCensal)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact) %>% 
  mutate(Any="2015") %>% 
  dplyr::select(SeccioCensal, starts_with("MD"), starts_with("impact"), starts_with("SegQual"), Any)

Proced_data__2015_MUN <- as.data.frame(ImpactProcd_2015$CMUN) %>% 
  mutate(SegQuali_procd=cut(scldMean, breaks= c(-10,-1,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació"))) %>% 
  mutate(CMUN=rownames(ImpactProcd_2015$CMUN)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact) %>% 
  mutate(Any="2015") %>% 
  dplyr::select(CMUN, starts_with("MD"), starts_with("impact"), starts_with("SegQual"), Any)

  #2013
Procedencia2013 <- read.csv2("Dades/LugarNacimiento/Procedencia2013.csv", encoding = "UTF-8", dec = ",")

Procedencia2013 <- Procedencia2013 %>% 
  mutate(Total=as.numeric(as.vector(str_remove_all(Total, "\\.")))) %>% 
  filter(X.U.FEFF.Sexo =="Ambos Sexos") %>%  
  filter(`Sección`!="TOTAL") %>% 
  mutate(SeccioCensal=`Sección`) %>% 
  dplyr::select(-X.U.FEFF.Sexo) %>% 
  dplyr::select(-`Sección`) %>% 
  filter(str_detect(SeccioCensal, "^07040|^07036|^07011|^07045|^07020|^07063|^07010|^07056|^07053|^07004|^07031")) %>% 
  pivot_wider(names_from = `País.de.nacimiento`,
              values_from = Total) %>% 
  mutate(BajoIDH=(`Total Africa`+Venezuela+Ecuador+Bolivia+Paraguay+Pakistán)) %>% 
  mutate(AltoIDH_mod=(`Nacidos en España`+Alemania+Francia+`Reino Unido`+Italia+Portugal)) %>% 
  mutate(AltoIDH_mod=unlist(AltoIDH_mod)) %>% 
  mutate(BajoIDH=unlist(BajoIDH))

Procedencia2013 <- as.data.frame(Procedencia2013) %>% 
  mutate(CMUN=str_remove(str_extract(SeccioCensal, "^....."), "^.."))

IndexProced <- id(Procedencia2013, vars = c("BajoIDH","AltoIDH_mod"))
ImpactProcd_2013 <- impacts(Procedencia2013, c("BajoIDH","AltoIDH_mod"), c("SeccioCensal","CMUN"))
head(ImpactProcd_2013, n = 5)


Proced_data__2013_SC <- as.data.frame(ImpactProcd_2013$SeccioCensal) %>% 
  mutate(SegQuali_procd=cut(scldMean, breaks= c(-10,-1,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació"))) %>% 
  mutate(SeccioCensal=rownames(ImpactProcd_2013$SeccioCensal)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact) %>% 
  mutate(Any="2013") %>% 
  dplyr::select(SeccioCensal, starts_with("MD"), starts_with("impact"), starts_with("SegQual"), Any)

Proced_data__2013_MUN <- as.data.frame(ImpactProcd_2013$CMUN) %>% 
  mutate(SegQuali_procd=cut(scldMean, breaks= c(-10,-1,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació"))) %>% 
  mutate(CMUN=rownames(ImpactProcd_2013$CMUN)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact) %>% 
  mutate(Any="2013") %>% 
  dplyr::select(CMUN, starts_with("MD"), starts_with("impact"), starts_with("SegQual"), Any)

  #2011
Procedencia2011 <- read.csv2("Dades/LugarNacimiento/Procedencia2011.csv", encoding = "UTF-8", dec = ",")

Procedencia2011 <- Procedencia2011 %>% 
  mutate(Total=as.numeric(as.vector(str_remove_all(Total, "\\.")))) %>% 
  filter(X.U.FEFF.Sexo =="Ambos Sexos") %>%  
  filter(`Sección`!="TOTAL") %>% 
  mutate(SeccioCensal=`Sección`) %>% 
  dplyr::select(-X.U.FEFF.Sexo) %>% 
  dplyr::select(-`Sección`) %>% 
  filter(str_detect(SeccioCensal, "^07040|^07036|^07011|^07045|^07020|^07063|^07010|^07056|^07053|^07004|^07031")) %>% 
  pivot_wider(names_from = `País.de.nacimiento`,
              values_from = Total) %>% 
  mutate(BajoIDH=(`Total Africa`+Venezuela+Ecuador+Bolivia+Paraguay+Pakistán)) %>% 
  mutate(AltoIDH_mod=(`Nacidos en España`+Alemania+Francia+`Reino Unido`+Italia+Portugal)) %>% 
  mutate(AltoIDH_mod=unlist(AltoIDH_mod)) %>% 
  mutate(BajoIDH=unlist(BajoIDH))

Procedencia2011 <- as.data.frame(Procedencia2011) %>% 
  mutate(CMUN=str_remove(str_extract(SeccioCensal, "^....."), "^.."))

IndexProced <- id(Procedencia2011, vars = c("BajoIDH","AltoIDH_mod"))
ImpactProcd_2011 <- impacts(Procedencia2011, c("BajoIDH","AltoIDH_mod"), c("SeccioCensal","CMUN"))
head(ImpactProcd_2011, n = 5)


Proced_data__2011_SC <- as.data.frame(ImpactProcd_2011$SeccioCensal) %>% 
  mutate(SegQuali_procd=cut(scldMean, breaks= c(-10,-1,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació"))) %>% 
  mutate(SeccioCensal=rownames(ImpactProcd_2011$SeccioCensal)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact) %>% 
  mutate(Any="2011") %>% 
  dplyr::select(SeccioCensal, starts_with("MD"), starts_with("impact"), starts_with("SegQual"), Any)

Proced_data__2011_MUN <- as.data.frame(ImpactProcd_2011$CMUN) %>% 
  mutate(SegQuali_procd=cut(scldMean, breaks= c(-10,-1,1.5,200), labels=c("Homogeneïtat", "No significatiu", "Segregació"))) %>% 
  mutate(CMUN=rownames(ImpactProcd_2011$CMUN)) %>% 
  rename(MD_Proced=scldMean,
         impact_procd=impact) %>% 
  mutate(Any="2011") %>% 
  dplyr::select(CMUN, starts_with("MD"), starts_with("impact"), starts_with("SegQual"), Any)

#unio
Proced_data_long_SC <- Proced_data__2021_SC %>% 
  full_join(Proced_data__2019_SC) %>% 
  full_join(Proced_data__2017_SC) %>% 
  full_join(Proced_data__2015_SC) %>% 
  full_join(Proced_data__2013_SC) %>% 
  full_join(Proced_data__2011_SC)

Proced_data_long_MUN <- Proced_data__2021_MUN %>% 
  full_join(Proced_data__2019_MUN) %>% 
  full_join(Proced_data__2017_MUN) %>% 
  full_join(Proced_data__2015_MUN) %>% 
  full_join(Proced_data__2013_MUN) %>% 
  full_join(Proced_data__2011_MUN)


##Municipal
Quali_long_Seg_MUN <- Proced_data_long_MUN %>% 
  group_by(SegQuali_procd, Any) %>% 
  summarise(Segregacio_MD=mean(MD_Proced, na.rm=TRUE),
            Segregacio_Impacte=mean(impact_procd, na.rm=TRUE)) %>% 
  mutate(Any=as.numeric(Any))

CMUN_long_Seg_MUN <- Proced_data_long_MUN %>% 
  group_by(CMUN, Any) %>% 
  summarise(Segregacio_MD=as.numeric(mean(MD_Proced, na.rm=TRUE)),
            Segregacio_Impacte=as.numeric(mean(impact_procd, na.rm=TRUE))) %>% 
  mutate(Any=as.numeric(Any)) %>% 
  mutate(Municipi=recode(CMUN, "004"="Algaida",
                         "010"="Bunyola",
                         "011"="Calvià",
                         "020"="Esporles",
                         "031"="Llucmajor",
                         "036"="Marratxí",
                         "040"="Palma",
                         "045"="Puigpunyent",
                         "053"="Sta. Eugènia",
                         "056"="Sta. Maria",
                         "063"="Valldemossa"))

Grafic_MUN_Long <- ggplot(CMUN_long_Seg_MUN, aes(Any, Segregacio_MD, colour=Municipi))+
  geom_point()+
  geom_line()+
  scale_color_brewer(palette="Paired")+
  annotate("text", x=2021, y=-1.1, label="Homogeneïtat", size=2.5, colour="grey")+
  labs(colour=NULL,
       y="Diferències mitjanes en segregació (ID)")+
  scale_x_continuous(breaks=c(2011,2013,2015,2017,2019,2021))+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12),
        legend.text =  element_text(family="Lato", size=17))

Grafic_MUN_Long


#Seccio
Quali_long_Seg_SC <- Proced_data_long_SC %>% 
  group_by(SegQuali_procd, Any) %>% 
  summarise(Segregacio_MD=mean(MD_Proced, na.rm=TRUE),
            Segregacio_MD_sd=sd(MD_Proced, na.rm=TRUE),
            Segregacio_Impacte=mean(impact_procd, na.rm=TRUE),
            n=n()) %>% 
  mutate(Any=as.numeric(Any))

Grafic_SC_Long <- ggplot(Quali_long_Seg_SC, aes(Any, Segregacio_MD, colour=SegQuali_procd))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Segregacio_MD-Segregacio_MD_sd, 
                    ymax=Segregacio_MD+Segregacio_MD_sd))+
  geom_hline(yintercept = c(-1,1.5), colour="grey", linetype="dashed")+
  labs(colour=NULL,
      y="Diferències mitjanes en segregació (Mitjana per SC)")+
  scale_x_continuous(breaks=c(2011,2013,2015,2017,2019,2021))+
  theme_tufte()+
  theme(text = element_text(family="Lato", size=12))


#turistificacio
Turisme_Grafic_long <- Dades_Segreg_lm %>% 
  dplyr::select(SeccioCensal, densiAirbnb, densiPlaces) %>% 
  mutate(QualiAir=ifelse(densiAirbnb>8, "Alta densitat", "No significatiu")) %>% 
  mutate(QualiPlaces=ifelse(densiPlaces>300, "Alta densitat", "No significatiu"))

Proced_data_long_SC_turisme <- full_join(Turisme_Grafic_long,Proced_data_long_SC)

Quali_long_Airbnb_SC <- Proced_data_long_SC_turisme %>% 
  group_by(QualiAir, Any) %>% 
  summarise(Segregacio_MD=mean(MD_Proced, na.rm=TRUE),
            Segregacio_MD_sd=sd(MD_Proced, na.rm=TRUE),
            Segregacio_Impacte=mean(impact_procd, na.rm=TRUE),
            n=n()) %>% 
  mutate(Any=as.numeric(Any))

Quali_long_Places_SC <- Proced_data_long_SC_turisme %>% 
  group_by(QualiPlaces, Any) %>% 
  summarise(Segregacio_MD=mean(MD_Proced, na.rm=TRUE),
            Segregacio_MD_sd=sd(MD_Proced, na.rm=TRUE),
            Segregacio_Impacte=mean(impact_procd, na.rm=TRUE),
            n=n()) %>% 
  mutate(Any=as.numeric(Any))

ggplot(Quali_long_Airbnb_SC, aes(Any, Segregacio_MD, colour=QualiAir))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Segregacio_MD-Segregacio_MD_sd, 
                    ymax=Segregacio_MD+Segregacio_MD_sd))+
  geom_hline(yintercept = c(-1,1.5), colour="grey", linetype="dashed")+
  labs(colour=NULL,
       y="Diferències mitjanes en segregació (ID)")+
  scale_x_continuous(breaks=c(2011,2013,2015,2017,2019,2021))+
  theme_tufte()+
  theme(text = element_text(family="Lato"))

ggplot(Quali_long_Places_SC, aes(Any, Segregacio_MD, colour=QualiPlaces))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Segregacio_MD-Segregacio_MD_sd, 
                    ymax=Segregacio_MD+Segregacio_MD_sd))+
  geom_hline(yintercept = c(-1,1.5), colour="grey", linetype="dashed")+
  labs(colour=NULL,
       y="Diferències mitjanes en segregació (ID)")+
  scale_x_continuous(breaks=c(2011,2013,2015,2017,2019,2021))+
  theme_tufte()+
  theme(text = element_text(family="Lato"))











############################################################################
############################################################################
#CLUSTER
############################################################################
############################################################################


#DADES

names(Dades_Segreg_lm)

Dades_Cluster <- Dades_Segreg_lm %>% 
  dplyr::select(SeccioCensal, MD_Edu, NumAirbnb, MediUC_Renda_2019,densiPlaces, Perc_BajoIDH)

Dades_Cluster2 <- Dades_Segreg_lm %>% 
  dplyr::select(SeccioCensal, MD_Edu, NumAirbnb, MediUC_Renda_2019,densiPlaces, Perc_BajoIDH)


Dades_Cl_Edu2011 <- read.xlsx("Dades/NivelEstudios/NivelEstudios_2011.xls", 
                              sheetName = "04") %>% 
  filter(str_detect(SeccionCensal, "^07040|^07036|^07011|^07045|^07020|^07063|^07010|^07056|^07053|^07004|^07031")) %>% 
  mutate(SeccioCensal=SeccionCensal) %>% 
  dplyr::select(-SeccionCensal)


ImpactEdu_11 <- impacts(Dades_Cl_Edu2011, c("PercSinEstudisSup", "PercEstudisSup"), 
                     levels=c("cmun", "SeccioCensal"))

Dades_Cl_Edu2011_C <- as.data.frame(ImpactEdu_11$SeccioCensal) %>% 
  mutate(SeccioCensal=rownames(ImpactEdu_11$SeccioCensal)) %>% 
  rename(MD_Edu_11=scldMean,
         impact_edu_11=impact) %>% 
  mutate(Any="2011") %>% 
  dplyr::select(SeccioCensal, starts_with("MD"))



segones <- read.xlsx("Dades/Habitatges/HabitatgePrin2021.xlsx", sheetName = "Hoja1") %>% 
  filter(str_detect(SeccioCensal, "^07040|^07036|^07011|^07045|^07020|^07063|^07010|^07056|^07053|^07004|^07031")) %>% 
  dplyr::select(SeccioCensal,PercNoPrin)

Dades_Cluster <- full_join(Dades_Cluster, Places_baix) %>% 
  full_join(Places_alt) %>% 
  full_join(Dades_Cl_Edu2011_C) %>% 
  mutate(VSegEdu=MD_Edu-MD_Edu_11) %>% 
  dplyr::select(-starts_with("MD")) %>% 
  dplyr::select(-starts_with("densi"))


Dades_Cluster2 <- full_join(Dades_Cluster2, Places_baix) %>% 
  full_join(Places_alt) %>% 
  full_join(Dades_Cl_Edu2011_C) %>% 
  full_join(segones) %>% 
  mutate(VSegEdu=MD_Edu-MD_Edu_11) %>% 
  dplyr::select(-starts_with("MD"))

#####CLUSTER

#comprovacio dades
rownames(Dades_Cluster2) <- Dades_Cluster2$SeccioCensal
Cl_dta <- Dades_Cluster2[,-1]


summary(Cl_dta)
nrow(Cl_dta)
Cl_dta <- na.omit(Cl_dta)

cor_cl_dta <- cor(Cl_dta)
cor_cl_dta2 <- as.data.frame(as.table(cor_cl_dta))
subset(cor_cl_dta2, Freq>0.9)

cities_sc <- scale(Cl_dta) 
summary(cities_sc)


dist_eucl <- dist(cities_sc, method="euclidean")
grafic_eucld <- fviz_dist(dist_eucl)


set.seed(150)
hopkins(cities_sc, n= nrow(cities_sc)-1)

get_clust_tendency(cities_sc, n= nrow(cities_sc)-1)

#Realitzacio cluster


  #Kmean (SÏ)
nb <- NbClust(cities_sc, distance="euclidean", min.nc=2,
              max.nc=10, method="complete")

set.seed(123)
kmeans_cities3 <- kmeans(cities_sc, 3, nstart = 25)


aggregate(Cl_dta, by=list(cluster=kmeans_cities3$cluster),mean)

fviz_cluster(list(data=cities_sc, cluster=kmeans_cities3$cluster))

cluster_plot <- fviz_cluster(kmeans_cities3, 
                             main = "Clúster segons segregació, turistificació i rendes",
             data=cities_sc, 
             star.plot=TRUE,#add segments from centroids to items
             repel=TRUE, #avoid level overlapping
             ggtheme=theme_classic())


cluster_original <- cbind(Cl_dta, cluster=kmeans_cities3$cluster)
names(cluster_original)

#comprovacio

library(clValid)
ag_km_di <- c( "kmeans")
cvalid <- clValid(cities_sc, nClust=3, clMethods = ag_km_di, method= "average", metric="euclidean", validation = "internal")
summary(cvalid)

cities_agnes_cp <- eclust(cities_sc, FUNcluster="agnes", k=NULL, hc_metric= "euclidean", hc_method= "average")
fviz_dend(cities_agnes_cp, color_labels_by_k = TRUE, rect=TRUE)



#taukes
taula_original <- cluster_original %>% 
  group_by(cluster) %>% 
  summarise(Med_NumAirbnb=median(NumAirbnb),
            Mitj_NumAirbnb=mean(NumAirbnb),
            sd_NumAirbnb=sd(NumAirbnb),
            Med_MediUC_Renda_2019=median(MediUC_Renda_2019),
            Mitj_MediUC_Renda_2019=mean(MediUC_Renda_2019),
            sd_MediUC_Renda_2019=sd(MediUC_Renda_2019),
            Med_PercNoPrin=median(PercNoPrin),
            Mitj_PercNoPrin=mean(PercNoPrin),
            sd_PercNoPrin=sd(PercNoPrin),
            Med_densiPlaces=median(densiPlaces),
            Mitj_densiPlaces=mean(densiPlaces),
            sd_densiPlaces=sd(densiPlaces),
            Med_Places_Baix=median(Places_Baix),
            Mitj_Places_Baix=mean(Places_Baix),
            sd_Places_Baix=sd(Places_Baix),
            Med_PlacesAlt=median(PlacesAlt),
            Mitj_PlacesAlt=mean(PlacesAlt),
            sd_PlacesAlt=sd(PlacesAlt),
            Med_VSegEdu=median(VSegEdu),
            Mitj_VSegEdu=mean(VSegEdu),
            sd_VSegEdu=sd(VSegEdu))

Taula_agregat_cluster3 <- aggregate(Cl_dta, by=list(cluster=kmeans_cities3$cluster),mean)














############################################################################
#Exportar cosetes
library("writexl")
write_xlsx(TurismoCAIB_hab,"Cartografia/_Dades/Turisme_hab.xlsx")
write_xlsx(ImpactProced_data,"Cartografia/_Dades/ImpacteProcd_Data_2.xlsx", )
write_xlsx(ImpactEdu_data,"Cartografia/_Dades/ImpacteEdu_Data_2.xlsx", )
write_xlsx(ImpactProced_data_esp,"Cartografia/_Dades/ImpacteProcd_ESP_Data_2.xlsx", )
write_xlsx(Renda2019,"Cartografia/_Dades/Renda2019_Resta.xlsx", )
write_xlsx(Dades_Segreg_FINAL,"Cartografia/_Dades/SegFinal_Q_Resta.xlsx", )

write_xlsx(IndexosTurisme_Mapa,"Cartografia/_Dades/IndexosTurisme.xlsx", )
write_xlsx(IndexosTurisme_Mapa2,"Cartografia/_Dades/IndexosTurisme_C.xlsx", )
write_xlsx(Index_Reestructuracio2,"Cartografia/_Dades/Index_Reestructuracio2.xlsx", )




cluster_original <- cluster_original %>% 
  mutate(SeccioCensal=rownames(cluster_original))


write_xlsx(cluster_original,"Cartografia/_Dades/Clusters.xlsx")
write_xlsx(Taula_agregat_cluster3, "Dades/TaulaAgregatClúster.xlsx")
write_xlsx(taula_original, "Dades/TaulaCompletClúster.xlsx")
write_xlsx(describe(cluster_original), "Dades/TaulaCompletDades.xlsx")


ggsave(Grafic_Seg_renda, filename = "Imatges/GraficsSegregacio/SegP_renda.png", dpi=200)
ggsave(Grafic_SegEdu_renda, filename = "Imatges/GraficsSegregacio/SegE_renda.png", dpi=200)

ggsave(Grafic_SC_Long, filename = "Imatges/GraficsSegregacio/Seg_SC_long.png", dpi=200)
ggsave(Grafic_MUN_Long, filename = "Imatges/GraficsSegregacio/Seg_MUN_long.png", dpi=200)
ggsave(cluster_plot, filename = "Imatges/GraficsSegregacio/cluster.png")
ggsave(grafic_eucld, filename = "Imatges/GraficsSegregacio/clusterplot")



ggsave(GPH_RendaIndex, filename = "Imatges/GraficsSegregacio/GPH_RendaIndex.png")
ggsave(GPH_EduIndex, filename = "Imatges/GraficsSegregacio/GPH_EduIndex.png")
ggsave(GPH_ProcedIndex, filename = "Imatges/GraficsSegregacio/GPH_ProcedIndex.png")

ggsave(GPH_EduIRT_F, filename = "Imatges/GraficsSegregacio/GPH_EduIRT_F.png")
ggsave(GPH_RendaIRT_F, filename = "Imatges/GraficsSegregacio/GPH_RendaIRT_F.png")
ggsave(GPH_EduITM_F, filename = "Imatges/GraficsSegregacio/GPH_EduITM_F.png")
ggsave(GPH_RendaITM_F, filename = "Imatges/GraficsSegregacio/GPH_RendaITM_F.png")


stargazer(lm_renda_Places_air_log,lm_renda_Places_air,
          lm_MDProcd_Places_air_log,lm_MDProcd_Places_air,
          lm_MDedu_Places_air_log,lm_MDedu_Places_air,
          lm_renda_PlacesAB_air, lm_renda_PlacesAB_air_log, 
          lm_MDedu_PlacesAB_air, lm_MDedu_PlacesAB_air_log,
          lm_MDProcd_PlacesAB_air,lm_MDProcd_PlacesAB_air_log,
          type="html", out = "Taules/RegressionsFINAL.html")
