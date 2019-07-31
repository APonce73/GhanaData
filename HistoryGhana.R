library(tidyverse)
library(readxl)
library(ggrepel)
library(haven)
library(vegan)
library(FD) # un paquete para la diversidad funcional, posiblement se va a eliminar



Tabla1 <- read_xlsx( path = "Indice_Valor_Uso_Ghana.xlsx", sheet = "Data_R", col_names = T)

Tabla1 %>%
  head()

Tabla1.2 <- Tabla1 %>%
  mutate(id_col = as.factor(seq(1:nrow(Tabla1))) ) %>%
  select(id_col, ID_HOUSEHOLD:Rawindex)

Tabla1.2$NAME_ENG <-  str_to_lower(Tabla1.2$NAME_ENG)

Tabla00 <- Tabla1.2 %>%
  select(id_col:NAME_SCIENT)

head(Tabla1.2)  

#Para ventas
Tabla2 <- Tabla1.2 %>%
  select(id_col, ABD.2.8, ABD.2.8.1) %>%
  gather(`ABD.2.8`, `ABD.2.8.1`, key = "Ventas", value = "Ventas1" ) %>%
  drop_na() %>%
  select(-`Ventas`) %>%
  mutate(Ventas2 = "Ventas") %>%
  filter(Ventas1 > 0) %>%
  unite(Ventas2, Ventas1, sep = "_", col = "Venta") %>%
  mutate(Value = 1) %>%
  spread(Venta, Value) %>%
  replace_na(list(Ventas_1 = 0)) %>%
  replace_na(list(Ventas_2 = 0)) %>%
  replace_na(list(Ventas_3 = 0)) %>%
  replace_na(list(Ventas_4 = 0))

head(Tabla2)
  
#Parte de la planta
Tabla3 <- Tabla1.2 %>%
  select(id_col, ABD.2.9:ABD.2.9.2) %>%
  gather(`ABD.2.9`,`ABD.2.9.1`, `ABD.2.9.2`, key = "PartePlanta", value = "Part_Plant" ) %>%
  drop_na() %>%
  select(-`PartePlanta`) %>%
  mutate(Part_Plant1 = "Part_Plant") %>%
  filter(Part_Plant > 0) %>%
  unite(Part_Plant1, Part_Plant, sep = "_", col = "Part_Plant") %>%
  mutate(Value = 1) %>%
  spread(Part_Plant, Value) %>%
  replace_na(list(Part_Plant_1 = 0)) %>%
  replace_na(list(Part_Plant_2 = 0)) %>%
  replace_na(list(Part_Plant_3 = 0)) %>%
  replace_na(list(Part_Plant_4 = 0)) %>%
  replace_na(list(Part_Plant_5 = 0)) %>%
  replace_na(list(Part_Plant_6 = 0)) %>%
  replace_na(list(Part_Plant_7 = 0)) %>%
  replace_na(list(Part_Plant_8 = 0)) %>%
  replace_na(list(Part_Plant_9 = 0)) 


#Parte los usos
Tabla4 <- Tabla1.2 %>%
  select(id_col, ABD.2.10:ABD.2.10.2) %>%
  gather(`ABD.2.10`, `ABD.2.10.1`,`ABD.2.10.2`, key = "Usos", value = "Usos1" ) %>%
  drop_na() %>%
  select(-`Usos`) %>%
  mutate(Usos2 = "Usos") %>%
  filter(Usos1 > 0) %>%
  unite(Usos2, Usos1, sep = "_", col = "Usos") %>%
  mutate(Value = 1) %>%
  spread(Usos, Value) %>%
  replace_na(list(Usos_1 = 0)) %>%
  replace_na(list(Usos_2 = 0)) %>%
  replace_na(list(Usos_3 = 0)) %>%
  replace_na(list(Usos_4 = 0)) %>%
  #replace_na(list(Usos_5 = 0)) %>%
  replace_na(list(Usos_6 = 0))


#Para consumo
Tabla5 <- Tabla1.2 %>%
  select(id_col, ABD.2.7) %>%
  gather(`ABD.2.7`, key = "Consumo", value = "Consumo1" ) %>%
  drop_na() %>%
  select(-`Consumo`) %>%
  mutate(Consumo2 = "Consumo") %>%
  filter(Consumo1 > 0) %>%
  unite(Consumo2, Consumo1, sep = "_", col = "Consumo") %>%
  mutate(Value = 1) %>%
  spread(Consumo, Value) %>%
  replace_na(list(Consumo_1 = 0)) %>%
  replace_na(list(Consumo_2 = 0)) %>%
  replace_na(list(Consumo_3 = 0)) %>%
  replace_na(list(Consumo_4 = 0))

head(Tabla5)

Tabla4
summary(Tabla4)

############# Esta es la tabla final que sirve para el FD
#############
TablaFinal <- Tabla00 %>%
  left_join(Tabla2, by = "id_col") %>%
  left_join(Tabla3, by = "id_col") %>%
  left_join(Tabla4, by = "id_col") %>%
  left_join(Tabla5, by = "id_col") %>%
  replace_na(list(Ventas_1 = 0)) %>%
  replace_na(list(Ventas_2 = 0)) %>%
  replace_na(list(Ventas_3 = 0)) %>%
  replace_na(list(Consumo_1 = 0)) %>%
  replace_na(list(Consumo_2 = 0)) %>%
  replace_na(list(Consumo_3 = 0))
  #filter(ID_FGD == 400)
  #filter(ID_FGD == 500)
  #filter(ID_FGD == 600)

#Hay que mover aqui 400, 500 y 600

#TablaFinal <- Tabla00 %>%
  #left_join(Tabla2, by = "id_col") %>%
 #left_join(Tabla3, by = "id_col") %>%
  #left_join(Tabla4, by = "id_col") %>%
  #left_join(Tabla5, by = "id_col") %>%
 #replace_na(list(Ventas_1 = 0)) %>%
  #replace_na(list(Ventas_2 = 0)) %>%
  #replace_na(list(Ventas_3 = 0)) %>%
  #replace_na(list(Consumo_1 = 0)) %>%
  #replace_na(list(Consumo_2 = 0)) %>%
  #replace_na(list(Consumo_3 = 0))
#filter(ID_FGD == 400)
#filter(ID_FGD == 500)
#filter(ID_FGD == 600)

head(TablaFinal)

######################################


#write.table(TablaFinal, file = "Tabladummy.txt", sep = "\t")
  
TablaF1 <- list(var = TablaFinal[,1:5], value = TablaFinal[,-c(1:5)])  

summary(TablaF1$value)
library(ade4)
library(vegan)
head(TablaF1)

dim(TablaF1$var)
DisTFD1.1 <- cascadeKM(TablaF1$value, 2,60, criterion = "calinski")
DisTFD1.1$partition
plot(DisTFD1.1)

dist1 <- vegdist(TablaF1$value, "euclidean", binary = T)
figura1 <- hclust(dist1, method = "ward.D")
plot(figura1, cex = 0.7)
#rect.hclust(figura1,3, border=2)
#rect.hclust(figura1,4, border=3)
rect.hclust(figura1, 7, border = 4)
grp <- cutree(figura1, 7)


PCO1 <- dudi.pco(dist1, scannf = F, nf = 4)
scatter(PCO1)
summary(PCO1)
names(PCO1)
PCO1$li
tail(PCO1$li)

PCO1$c1
dim(PCO1$li)

TablaW1 <- data.frame(TablaF1$var, PCO1$li[,1:2])
head(TablaW1)

#write.table(TablaW1, file = "TablaPCO.txt", sep = "\t")


TablaA <- TablaW1 %>%
  group_by(NAME_ENG) %>%
  summarize(NAME_ENG_Y = mean(A2))

TablaB <- TablaW1 %>%
  group_by(NAME_ENG) %>%
  summarize(NAME_ENG_X = mean(A1)) %>%
  left_join(TablaA, by = "NAME_ENG")

TablaC <- TablaW1 %>%
  group_by(ID_FGD) %>%
  summarize(FGD_Y = mean(A2))

TablaD <- TablaW1 %>%
  group_by(ID_FGD) %>%
  summarize(FGD_X = mean(A1)) %>%
  left_join(TablaC, by = "ID_FGD")


TablaF <- TablaW1 %>%
  group_by(ID_FGD, ID_HOUSEHOLD) %>%
  summarize(NAME_ENG_Y = mean(A2))

TablaG <- TablaW1 %>%
  group_by( ID_FGD, ID_HOUSEHOLD) %>%
  summarize(NAME_ENG_X = mean(A1)) %>%
  left_join(TablaF, by = c( "ID_FGD", "ID_HOUSEHOLD"))


head(TablaG)
dim(TablaG)

head(TablaB)
head(TablaD)

dim(TablaW1)

TablaE <- TablaG %>%
  #left_join(TablaB, by = "ID_HOUSEHOLD") %>%
  left_join(TablaD, by = "ID_FGD") %>%
  mutate(Colores = ID_FGD)

TablaE$Colores <- as.factor(TablaE$Colores) 
TablaE <- TablaE %>%
  dplyr::mutate(Colores = recode(Colores, "400" = "#9e0142")) %>%
  dplyr::mutate(Colores = recode(Colores, "500" = "#3288bd")) %>%
  dplyr::mutate(Colores = recode(Colores, "600" = "#f46d43"))
  #left_join(TablaG, by = "NAME_ENG")
  levels(as.factor(TablaE$Colores))
head(TablaE)
summary(TablaE)
head(as.data.frame(TablaE))

p1 <- ggplot(TablaE, aes(x = NAME_ENG_X, y = NAME_ENG_Y)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_point(color = "black", alpha = 0.7, size = 2) +
  geom_segment(aes(x = FGD_X, y = FGD_Y, xend = NAME_ENG_X, yend = NAME_ENG_Y, color = Colores), alpha = 0.7) +   
  geom_label(data = TablaD, aes(label = ID_FGD, x = FGD_X, y = FGD_Y)) +
  geom_label_repel(data = TablaB, aes(label = NAME_ENG, x = NAME_ENG_X, y = NAME_ENG_Y), box.padding = 0, point.padding = 0, size = 3)
  
  
p1


TablaFinal <- Tabla00 %>%
  #left_join(Tabla2, by = "id_col") %>%
  #left_join(Tabla3, by = "id_col") %>%
  #left_join(Tabla4, by = "id_col") %>%
  left_join(Tabla5, by = "id_col") %>%
  #replace_na(list(Ventas_1 = 0)) %>%
  #replace_na(list(Ventas_2 = 0)) %>%
  #replace_na(list(Ventas_3 = 0)) %>%
  replace_na(list(Consumo_1 = 0)) %>%
  replace_na(list(Consumo_2 = 0)) %>%
  replace_na(list(Consumo_3 = 0)) %>%
  #filter(ID_FGD == 400)
  #filter(ID_FGD == 500)
   filter(ID_FGD == 600)


head(TablaFinal)
###############################
#Para hacer FD 

head(TablaFinal)
#Para la tabla de los Traits
Traits1 <- TablaFinal %>%
  #select(NAME_SCIENT, Part_Plant_1:Part_Plant_9) %>%
  select(NAME_SCIENT:Consumo_3) %>%
  gather(`Ventas_1`:`Consumo_3`, key = TraitL, value = 'valor') %>%
  #select(NAME_SCIENT:Ventas_3) %>% #Para Ventas
  #gather(`Ventas_1`:`Ventas_3`, key = TraitL, value = 'valor') %>% #Para Ventas
  #select(NAME_SCIENT:Part_Plant_9) %>% #Para parte de la planta
  #gather(Part_Plant_1:Part_Plant_9, key = TraitL, value = 'valor') %>% #Para parte de la planta
  #select(NAME_SCIENT:Usos_6) %>% #Para Usos
  #gather(Usos_1:Usos_6, key = TraitL, value = 'valor') %>% #Para Usos
  #select(NAME_SCIENT:Consumo_3) %>% #Para Usos
  #gather(Consumo_1:Consumo_3, key = TraitL, value = 'valor') %>% #Para Usos
  group_by(NAME_SCIENT, TraitL) %>%
  summarize(NAME1 = sum(valor)) %>%
  spread(TraitL, NAME1)

head(Traits1)

Traits2 <- decostand(Traits1[,-1], method = "pa", na.rm = T )
Traits2.1 <- data.frame(Traits1$NAME_SCIENT, Traits2) %>%
  mutate(uno = "spp") %>%
  mutate(dos = seq(1:nrow(Traits2))) %>%
  unite(uno, dos, col = "ID_spp", sep = "_") %>%
  rename(NAME_SCIENT = "Traits1.NAME_SCIENT") %>%
  select(ID_spp, NAME_SCIENT:Ventas_3) %>% #Para todos
  #select(ID_spp, NAME_SCIENT:Usos_6) %>% #PAra parte de la planta
  #select(ID_spp, NAME_SCIENT:Consumo_3) %>% #PAra el consumo
  arrange(NAME_SCIENT)

head(Traits2.1)
#write.table(Traits2.1, file = "TablaResults/TablaTraits.txt", sep = "\t")
Traits2.2 <- Traits2.1 %>%  
  column_to_rownames(var = "ID_spp") %>%
  select(-NAME_SCIENT)
Traits2.2 <- Traits2.2[,apply(Traits2.2,2,sum) > 0 ]

#Tabla de las especies

TablaFinal$NAME_SCIENT <- as.factor(TablaFinal$NAME_SCIENT)
TablaSpp <- TablaFinal %>%
  select(id_col:NAME_SCIENT) %>%
  left_join(Traits2.1, by = "NAME_SCIENT") %>%
  select(ID_HOUSEHOLD,ID_FGD,ID_spp) %>%
  distinct() %>%
  #arrange(ID_spp) %>%
  mutate(value = 1) %>%
  spread(ID_spp, value) %>%
  replace(is.na(.), 0) %>%
 # select(ID_HOUSEHOLD, ID_FGD, spp_1, spp_2, spp_3, spp_4, spp_5, spp_6, spp_7, spp_8, spp_9, spp_10,
#         spp_11, spp_12) #Para 400
#  select(ID_HOUSEHOLD, ID_FGD, spp_1, spp_2, spp_3, spp_4, spp_5, spp_6, spp_7, spp_8, spp_9, spp_10,
#         spp_11, spp_12, spp_13, spp_14, spp_15, spp_16, spp_17, spp_18, spp_19, spp_20,
#         spp_21, spp_22) # para 500
  select(ID_HOUSEHOLD, ID_FGD, spp_1, spp_2, spp_3, spp_4, spp_5, spp_6, spp_7, spp_8, spp_9, spp_10,
         spp_11, spp_12, spp_13) #Para 600
 #select(ID_HOUSEHOLD, ID_FGD, spp_1, spp_2, spp_3, spp_4, spp_5, spp_6, spp_7, spp_8, spp_9, spp_10,
#        spp_11, spp_12, spp_13, spp_14, spp_15, spp_16, spp_17, spp_18, spp_19, spp_20,
 #       spp_21, spp_22, spp_23) # para todas

  TablaSpp1 <- TablaSpp %>%
    column_to_rownames(var = "ID_HOUSEHOLD") %>%
    select(-ID_FGD)

rm(ex3)
head(Traits2.2)
ex3 <- dbFD(Traits2.2, TablaSpp1)
ex3

FuncDiv1 <- data.frame(ex3$FRic, ex3$FEve, ex3$FDis, ex3$RaoQ)
names(FuncDiv1)
FuncDiv2 <- FuncDiv1 %>%
  rename(FRic = "ex3.FRic") %>%
  rename(FEve = "ex3.FEve") %>%
  #rename(FDiv = "ex3.FDiv") %>%
  rename(FDis = "ex3.FDis") %>%
  rename(RaoQ = "ex3.RaoQ")

TablaSpp2 <- TablaSpp %>%
  select(ID_HOUSEHOLD, ID_FGD)
TablaSpp2$ID_HOUSEHOLD <- as.character(TablaSpp2$ID_HOUSEHOLD)

FuncDiv2.1 <- FuncDiv2 %>%
  #dplyr::select(-"RaoQ") %>%
  rownames_to_column(var = "ID_HOUSEHOLD") %>%
  left_join(TablaSpp2, by = "ID_HOUSEHOLD") %>%
  mutate(CODE = ID_HOUSEHOLD) %>%
  column_to_rownames(var = "CODE") %>%
  select(ID_FGD, ID_HOUSEHOLD:RaoQ) %>%
  replace(is.na(.), 0)
  
head(FuncDiv2.1)

#Ventas
#write.table(FuncDiv2.1, file = "TablaResults/FunctionalResults400Ventas.txt", sep = "\t")
#write.table(FuncDiv2.1, file = "TablaResults/FunctionalResults500Ventas.txt", sep = "\t")
#write.table(FuncDiv2.1, file = "TablaResults/FunctionalResults600Ventas.txt", sep = "\t")

#Parte de la planta
#write.table(FuncDiv2.1, file = "TablaResults/FunctionalResults400PPlanta.txt", sep = "\t")
#write.table(FuncDiv2.1, file = "TablaResults/FunctionalResults500PPlanta.txt", sep = "\t")
#write.table(FuncDiv2.1, file = "TablaResults/FunctionalResults600PPlanta.txt", sep = "\t")

#Usos
#write.table(FuncDiv2.1, file = "TablaResults/FunctionalResults400Usos.txt", sep = "\t")
#write.table(FuncDiv2.1, file = "TablaResults/FunctionalResults500Usos.txt", sep = "\t")
#write.table(FuncDiv2.1, file = "TablaResults/FunctionalResults600Usos.txt", sep = "\t")

#Consumo
#write.table(FuncDiv2.1, file = "TablaResults/FunctionalResults400Consumo.txt", sep = "\t")
#write.table(FuncDiv2.1, file = "TablaResults/FunctionalResults500Consumo.txt", sep = "\t")
write.table(FuncDiv2.1, file = "TablaResults/FunctionalResults600Consumo.txt", sep = "\t")



TablaQ400 <- read.table( "TablaResults/FunctionalResults400.txt", sep = "\t" )
TablaQ500 <- read.table( "TablaResults/FunctionalResults500.txt", sep = "\t" )
TablaQ600 <- read.table( "TablaResults/FunctionalResults600.txt", sep = "\t" )

head(TablaQ400)
head(TablaQ500)
head(TablaQ600)

FuncDiv2 <- TablaQ400 %>%
  bind_rows(TablaQ500) %>%
  bind_rows(TablaQ600)

library(GGally)
ggpairs(FuncDiv2)

dim(FuncDiv2)
dim(TablaSpp)
head(TablaSpp2)


names(FuncDiv2)
head(FuncDiv2)
FuncDiv3 <- FuncDiv2 %>%
  select(-"RaoQ") %>%
  #rownames_to_column(var = "ID_HOUSEHOLD") %>%
  #right_join(TablaSpp2[,1:2], by = "ID_HOUSEHOLD") %>%
  mutate(CODE = ID_HOUSEHOLD) %>%
  column_to_rownames(var = "CODE") %>%
  drop_na()

dim(FuncDiv3)
dim(FuncDiv2)

str(FuncDiv3)
str(TablaSpp)
head(FuncDiv3)

TablaPCA <- list(Var = FuncDiv3[,c(1,2)], Value = FuncDiv3[,-c(1,2)])  
head(TablaPCA)
head(TablaPCA$Value)
PCA1 <- dudi.pca(TablaPCA$Value, scannf = F, nf = 4)
scatter(PCA1)
summary(PCA1)
names(PCA1)


######
###################
#Small function for do PCA 
# pca1 <- dudi.hillsmith(Tabla3$Value1, scannf = F, nf = 5)
# summary(pca1)
#
#  

#Funcion con 5 componentes del PCA
# renglon = a los valores por renglón
# columnas 0 los valores por columnas
# Variables = la variable categ´órica
#Titulo = Es el titulo de la grafica
#Multiplicador =  un valor para re-escalar la gr´áfica
#PCbiplot <- function(renglon, columnas, Variables, Eigen, Titulo, multiplicador){
#  #pca1
#  pca1a <- data.frame(renglon)
#  pca1b <- data.frame(columnas)
#  Variables <- Variables
#  Titulo <- as.character(c(Titulo))
#  PC1 <- round(100*(Eigen/sum(Eigen)),1)[1]
#  PC2 <- round(100*(Eigen/sum(Eigen)),1)[2]
  
#  p1 <- ggplot(pca1a,aes(x = Axis1, y = Axis2)) +
#    theme_classic() +
#    geom_hline(yintercept = 0, color = "gray70") +
#    geom_vline(xintercept = 0, color = "gray70") +
#    geom_point(aes(color = Variables), alpha = 0.7, size = 3) +
#    xlab(paste(PC1, "%", sep = " ")) +
#    ylab(paste(PC2, "%", sep = " ") ) + 
#    #xlim(-5, 6) + 
#    ggtitle(Titulo) 
#  
#  p2 <- p1 +
#    geom_point(data = pca1b, aes(x = multiplicador*Comp1, y = multiplicador*Comp2), alpha = 0) + 
#    geom_label_repel(data = pca1b, aes(x = multiplicador*Comp1, y = multiplicador*Comp2, label = rownames(pca1b)), 
#                     box.padding   = 0.7, point.padding = 0.5, size = 4, alpha = 0.7) +
#    geom_segment(data =  pca1b, aes(x = 0, y = 0, xend = multiplicador*Comp1, yend = multiplicador*Comp2), arrow = arrow(length = unit(0.2,"cm")), alpha = 0.7, color = "black")
#  
#  p2
#}

PCbiplot(PCA1$li, PCA1$co, as.factor(TablaPCA$Var$ID_FGD), PCA1$eig,"PCA", 2)
#

TablaLL <- data.frame(TablaPCA$Var, PCA1$li)
head(TablaLL)
TablaC_1 <- TablaLL %>%
  group_by(ID_FGD) %>%
  summarize(Axis_Y = mean(Axis2))

TablaD_1 <- TablaLL %>%
  group_by(ID_FGD) %>%
  summarize(Axis_X = mean(Axis1)) %>%
  left_join(TablaC_1, by = "ID_FGD")

TablaLL <- TablaLL %>%
  left_join(TablaD_1, by = "ID_FGD") %>%
  select(-c(Axis3, Axis4)) %>%
  mutate(Colores = ID_FGD)

TablaLL$Colores <- as.factor(TablaLL$Colores) 
TablaLL <- TablaLL %>%
  dplyr::mutate(Colores = recode(Colores, "400" = "#9e0142")) %>%
  dplyr::mutate(Colores = recode(Colores, "500" = "#3288bd")) %>%
  dplyr::mutate(Colores = recode(Colores, "600" = "#f46d43"))

head(TablaLL)

eig1 <- round((PCA1$eig[1]/sum(PCA1$eig))*100,1)
eig2 <- round((PCA1$eig[2]/sum(PCA1$eig))*100,1)

p1 <- ggplot(TablaLL) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_point(aes(x = Axis1, y = Axis2), alpha = 0.5, size = 2, color = "gray") +
  xlab(paste(eig1, "%", sep = " ")) +
  ylab(paste(eig2, "%", sep = " ") ) +
  geom_segment(aes(x = Axis_X, y = Axis_Y, xend = Axis1, yend = Axis2, color = Colores), alpha = 0.7) +
  geom_label(data = TablaD_1, aes(label = ID_FGD, x = Axis_X, y = Axis_Y)) +
  geom_segment(data =  PCA1$co, aes(x = 0, y = 0, xend = Comp1, yend = Comp2), arrow = arrow(length = unit(0.2,"cm")), alpha = 0.7, color = "black") +
  geom_label(data = PCA1$co, aes(x = Comp1, y = Comp2, label = rownames(PCA1$co)), size = 4, alpha = 0.7)


PCA1$co

p1
geom_segment(aes(x = FGD_X, y = FGD_Y, xend = NAME_ENG_X, yend = NAME_ENG_Y, color = Colores), alpha = 0.7) +   
  geom_label(data = TablaD, aes(label = ID_FGD, x = FGD_X, y = FGD_Y)) +
  geom_label_repel(data = TablaB, aes(label = NAME_ENG, x = NAME_ENG_X, y = NAME_ENG_Y), box.padding = 0, point.padding = 0, size = 3)


head(FuncDiv3)
p2 <- ggplot(FuncDiv3) +
  theme_classic() +
  geom_point(aes(x = FRic, y = FEve, color = factor(ID_FGD)), alpha = 0.5, size = 2) +
  geom_label_repel(
    aes(x = FRic, y = FEve, label = ifelse(FRic > 10, as.character(ID_HOUSEHOLD),'')),
    box.padding   = 0.35, point.padding = 0.5, size = 3, segment.color = 'grey50') 
p3 <- p2 +
  geom_label_repel(
    aes(x = FRic, y = FEve, label = ifelse(FEve < 0.6, as.character(ID_HOUSEHOLD),'')),
    box.padding   = 0.35,  point.padding = 0.5, size = 3, segment.color = 'grey50')
p4 <- p3 +
  geom_label_repel(
    aes(x = FRic, y = FEve, label = ifelse(FEve > 0.9, as.character(ID_HOUSEHOLD),'')),
    box.padding   = 0.35,  point.padding = 0.5, size = 3, segment.color = 'grey50')
p4


#Ventas

Ventas400 <- read.table( "TablaResults/FunctionalResults400Ventas.txt", sep = "\t" )
Ventas400$FRic <- decostand(Ventas400$FRic, method = "range")
Ventas400 <- Ventas400 %>%
  mutate(ID = 400)

Ventas500 <- read.table( "TablaResults/FunctionalResults500Ventas.txt", sep = "\t" )
Ventas500$FRic <- decostand(Ventas500$FRic, method = "range")
Ventas500 <- Ventas500 %>%
  mutate(ID = 500)

Ventas600 <- read.table( "TablaResults/FunctionalResults600Ventas.txt", sep = "\t" )
Ventas600$FRic <- decostand(Ventas600$FRic, method = "range")
Ventas600 <- Ventas600 %>%
  mutate(ID = 600)


VentasFinal <- Ventas400 %>%
  bind_rows(Ventas500) %>%
  bind_rows(Ventas600)

VentasProm <- VentasFinal %>%
  group_by(ID) %>%
  summarize(FRic1 = mean(FRic), FEve1 = mean(FEve))

VentasFinal <- VentasFinal %>%
  left_join(VentasProm, by = "ID")

p2 <- ggplot() +
  theme_classic() +
  geom_point(data = VentasFinal, aes(x = FRic, y = FEve, color = factor(ID_FGD)), alpha = 0.5, size = 2) +
  geom_label_repel( data = VentasFinal,
    aes(x = FRic, y = FEve, label = ifelse(FRic > mean(VentasFinal$FRic) & FEve > mean(VentasFinal$FEve),
                                           as.character(ID_HOUSEHOLD),'')),
    box.padding   = 0.35, point.padding = 0.5, size = 3, segment.color = 'grey50') +
  geom_hline(yintercept = mean(VentasFinal$FEve), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(VentasFinal$FRic), linetype = "dashed", color = "red") +
  geom_segment(data = VentasFinal, aes(x = FRic1, y = FEve1, xend = FRic, yend = FEve), alpha = 0.7, color = "gray") +
  geom_label(data = VentasFinal, aes(x = FRic1, y = FEve1, label = ID), size = 4, alpha = 0.7)

p2

#Parte de la planta
PPlanta400 <- read.table( "TablaResults/FunctionalResults400PPlanta.txt", sep = "\t" )
PPlanta400$FRic <- decostand(PPlanta400$FRic, method = "range")
as.vector(PPlanta400$FRic)
PPlanta400 <- PPlanta400 %>%
  mutate(ID = 400)
PPlanta500 <- read.table( "TablaResults/FunctionalResults500PPlanta.txt", sep = "\t" )
PPlanta500$FRic <- decostand(PPlanta500$FRic, method = "range")
as.vector(PPlanta500$FRic)
PPlanta500 <- PPlanta500 %>%
  mutate(ID = 500)
PPlanta600 <- read.table( "TablaResults/FunctionalResults600PPlanta.txt", sep = "\t" )
PPlanta600$FRic <- decostand(PPlanta600$FRic, method = "range")
as.vector(PPlanta600$FRic)
PPlanta600 <- PPlanta600 %>%
  mutate(ID = 600)

PPlantaFinal <- PPlanta400 %>%
  bind_rows(PPlanta500) %>%
  bind_rows(PPlanta600)

head(PPlantaFinal)

PPlantaProm <- PPlantaFinal %>%
  group_by(ID) %>%
  summarize(FRic1 = mean(FRic), FEve1 = mean(FEve))

PPlantaFinal <- PPlantaFinal %>%
  left_join(PPlantaProm, by = "ID")

p2 <- ggplot() +
  theme_classic() +
  geom_point(data = PPlantaFinal, aes(x = FRic, y = FEve, color = factor(ID_FGD)), alpha = 0.5, size = 2) +
  geom_label_repel( data = PPlantaFinal,
                    aes(x = FRic, y = FEve, label = ifelse(FRic > mean(PPlantaFinal$FRic) & FEve > mean(PPlantaFinal$FEve),
                                                          as.character(ID_HOUSEHOLD),'')),
                    box.padding   = 0.35, point.padding = 0.5, size = 3, segment.color = 'grey50') +
  geom_hline(yintercept = mean(PPlantaFinal$FEve), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(PPlantaFinal$FRic), linetype = "dashed", color = "red") +
  geom_segment(data = PPlantaFinal, aes(x = FRic1, y = FEve1, xend = FRic, yend = FEve), alpha = 0.7, color = "gray") +
  geom_label(data = PPlantaFinal, aes(x = FRic1, y = FEve1, label = ID), size = 4, alpha = 0.7)

p2


#Usos
Usos400 <- read.table( "TablaResults/FunctionalResults400Usos.txt", sep = "\t" )
Usos400$FRic <- decostand(Usos400$FRic, method = "range")
as.vector(Usos400$FRic)
Usos400 <- Usos400 %>%
  mutate(ID = 400)
Usos500 <- read.table( "TablaResults/FunctionalResults500Usos.txt", sep = "\t" )
Usos500$FRic <- decostand(Usos500$FRic, method = "range")
as.vector(Usos500$FRic)
Usos500 <- Usos500 %>%
  mutate(ID = 500)
Usos600 <- read.table( "TablaResults/FunctionalResults600Usos.txt", sep = "\t" )
Usos600$FRic <- decostand(Usos600$FRic, method = "range")
as.vector(Usos600$FRic)
Usos600 <- Usos600 %>%
  mutate(ID = 600)

UsosFinal <- Usos400 %>%
  bind_rows(Usos500) %>%
  bind_rows(Usos600)

head(UsosFinal)

UsosProm <- UsosFinal %>%
  group_by(ID) %>%
  summarize(FRic1 = mean(FRic), FEve1 = mean(FEve))

UsosFinal <- UsosFinal %>%
  left_join(UsosProm, by = "ID")

p2 <- ggplot() +
  theme_classic() +
  geom_point(data = UsosFinal, aes(x = FRic, y = FEve, color = factor(ID_FGD)), alpha = 0.5, size = 2) +
  geom_label_repel( data = UsosFinal,
                    aes(x = FRic, y = FEve, label = ifelse(FRic > mean(UsosFinal$FRic) & FEve > mean(UsosFinal$FEve),
                                                           as.character(ID_HOUSEHOLD),'')),
                    box.padding   = 0.35, point.padding = 0.5, size = 3, segment.color = 'grey50') +
  geom_hline(yintercept = mean(UsosFinal$FEve), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(UsosFinal$FRic), linetype = "dashed", color = "red") +
  geom_segment(data = UsosFinal, aes(x = FRic1, y = FEve1, xend = FRic, yend = FEve), alpha = 0.7, color = "gray") +
  geom_label(data = UsosFinal, aes(x = FRic1, y = FEve1, label = ID), size = 4, alpha = 0.7)

p2

#Consumo
Consumo400 <- read.table( "TablaResults/FunctionalResults400Consumo.txt", sep = "\t" )
Consumo400$FRic <- decostand(Consumo400$FRic, method = "range")
as.vector(Consumo400$FRic)
Consumo400 <- Consumo400 %>%
  mutate(ID = 400)
Consumo500 <- read.table( "TablaResults/FunctionalResults500Consumo.txt", sep = "\t" )
Consumo500$FRic <- decostand(Consumo500$FRic, method = "range")
as.vector(Consumo500$FRic)
Consumo500 <- Consumo500 %>%
  mutate(ID = 500)
Consumo600 <- read.table( "TablaResults/FunctionalResults600Consumo.txt", sep = "\t" )
Consumo600$FRic <- decostand(Consumo600$FRic, method = "range")
as.vector(Consumo600$FRic)
Consumo600 <- Consumo600 %>%
  mutate(ID = 600)

ConsumoFinal <- Consumo400 %>%
  bind_rows(Consumo500) %>%
  bind_rows(Consumo600)

head(ConsumoFinal)

ConsumoProm <- ConsumoFinal %>%
  group_by(ID) %>%
  summarize(FRic1 = mean(FRic), FEve1 = mean(FEve))

ConsumoFinal <- ConsumoFinal %>%
  left_join(ConsumoProm, by = "ID")

p2 <- ggplot() +
  theme_classic() +
  geom_point(data = ConsumoFinal, aes(x = FRic, y = FEve, color = factor(ID_FGD)), alpha = 0.5, size = 2) +
  geom_label_repel( data = ConsumoFinal,
                    aes(x = FRic, y = FEve, label = ifelse(FRic > mean(ConsumoFinal$FRic) & FEve > mean(ConsumoFinal$FEve),
                                                           as.character(ID_HOUSEHOLD),'')),
                    box.padding   = 0.35, point.padding = 0.5, size = 3, segment.color = 'grey50') +
  geom_hline(yintercept = mean(ConsumoFinal$FEve), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(ConsumoFinal$FRic), linetype = "dashed", color = "red") +
  geom_segment(data = ConsumoFinal, aes(x = FRic1, y = FEve1, xend = FRic, yend = FEve), alpha = 0.7, color = "gray") +
  geom_label(data = ConsumoFinal, aes(x = FRic1, y = FEve1, label = ID), size = 4, alpha = 0.7)

p2



###############################
##############################
#Hacer PCO para cada especie
head(TablaFinal)
dim(TablaFinal)

#Katcha = 400
Traits1 <- TablaFinal %>%
  #filter(ID_FGD == Katcha) %>%
  #select(NAME_SCIENT, Part_Plant_1:Part_Plant_9) %>%
  select(NAME_SCIENT:Consumo_3) %>%
  gather(`Ventas_1`:`Consumo_3`, key = TraitL, value = 'valor') %>%
  #select(NAME_SCIENT:Ventas_3) %>% #Para Ventas
  #gather(`Ventas_1`:`Ventas_3`, key = TraitL, value = 'valor') %>% #Para Ventas
  #select(NAME_SCIENT:Part_Plant_9) %>% #Para parte de la planta
  #gather(Part_Plant_1:Part_Plant_9, key = TraitL, value = 'valor') %>% #Para parte de la planta
  #select(NAME_SCIENT:Usos_6) %>% #Para Usos
  #gather(Usos_1:Usos_6, key = TraitL, value = 'valor') %>% #Para Usos
  #select(NAME_SCIENT:Consumo_3) %>% #Para Usos
  #gather(Consumo_1:Consumo_3, key = TraitL, value = 'valor') %>% #Para Usos
  group_by(NAME_SCIENT, TraitL) %>%
  summarize(NAME1 = sum(valor)) %>%
  spread(TraitL, NAME1)


Traits2 <- decostand(Traits1[,-1], method = "pa", na.rm = T )
Traits2.1 <- data.frame(Traits1$NAME_SCIENT, Traits2) %>%
  mutate(uno = "spp") %>%
  mutate(dos = seq(1:nrow(Traits2))) %>%
  unite(uno, dos, col = "ID_spp", sep = "_") %>%
  rename(NAME_SCIENT = "Traits1.NAME_SCIENT") %>%
  select(ID_spp, NAME_SCIENT:Ventas_3) %>% #Para todos
  #select(ID_spp, NAME_SCIENT:Usos_6) %>% #PAra parte de la planta
  #select(ID_spp, NAME_SCIENT:Consumo_3) %>% #PAra el consumo
  arrange(NAME_SCIENT)

TablaLL <- list(Var = Traits2.1[,1:2], Value = Traits2.1[,-c(1:2)])

Dist1 <- vegdist(TablaLL$Value, method = "euclidean")
PCO1 <- dudi.pco(Dist1, scannf = F, nf = 4)
Dolls <- round(100 * (PCO1$eig[1:2]/sum(PCO1$eig)),1)

TablaLL1 <- TablaLL$Var %>%
  bind_cols(PCO1$li)

#head(TablaLL1)
#write.table(TablaLL1, file = "TablaResults/PCO_AllData.txt", sep = "\t")

#################
#Para hacer los rangos

#1. Eliminar los valores negativos

uno <- apply(PCO1$li, 2, min)

uno1 <- PCO1$li[1] + (abs(uno[1]) + 1)
uno2 <- PCO1$li[2] + (abs(uno[2]) + 1)
uno3 <- PCO1$li[3] + (abs(uno[3]) + 1)
uno4 <- PCO1$li[4] + (abs(uno[4]) + 1)

TablaLL2 <- data.frame(uno1, uno2, uno3, uno4)

TablaLL3 <- decostand(TablaLL2, MARGIN = 1, method = "rank")

TablaLL4 <- data.frame(TablaLL1, TablaLL3)

#write.table(TablaLL4, file = "TablaResults/PCO_AllData1.txt", sep = "\t")
TablaLL4$A1.1 <- as.factor(TablaLL4$A1.1)
p2 <- ggplot() +
  theme_classic() +
  geom_point(data = TablaLL4, aes(x = A1, y = A2, color = A1.1), 
             alpha = 0.7, size = 3) +
  geom_label_repel( data = TablaLL4,
                    aes(x = A1, y = A2, label = NAME_SCIENT ),
                    box.padding   = 0.35, point.padding = 0.5, 
                    size = 3, segment.color = 'grey50') +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  labs(fill = "Group",
    #title = paste("Community:", Katcha),
    title = paste("All Communities"),
    #subtitle = "Todos los años",
    #caption = "Source:SNIB-Conabio",
    x = paste("PC1=",Dolls[1], "%"),
    y = paste("PC2=",Dolls[2], "%")
      )
  
#geom_segment(data = ConsumoFinal, aes(x = FRic1, y = FEve1, xend = FRic, yend = FEve), alpha = 0.7, color = "gray") +
#  geom_label(data = ConsumoFinal, aes(x = FRic1, y = FEve1, label = ID), size = 4, alpha = 0.7)

p2



