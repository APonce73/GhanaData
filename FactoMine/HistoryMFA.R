#install.packages("FactoMineR")

library(tidyverse)
library(readxl)
library(ggrepel)
library(haven)
library(vegan)
library(FD)
library(FactoMineR)
library(magrittr)
library(vegan)



Tabla1 <- read_xlsx( path = "Indice_Valor_Uso_Ghana.xlsx", sheet = "Data_R", col_names = T)

Tabla1.2 <- Tabla1 %>%
  mutate(id_col = as.factor(seq(1:nrow(Tabla1))) ) %>%
  select(id_col, ID_HOUSEHOLD:Rawindex)

Tabla1.2$NAME_ENG <-  str_to_lower(Tabla1.2$NAME_ENG)

Tabla00 <- Tabla1.2 %>%
  select(id_col:NAME_SCIENT)

head(Tabla1.2)  
dim(Tabla1.2)

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

head(TablaFinal)

Tabla1.1 <- TablaFinal %>%
  #select(-c(id_col, ID_FGD, NAME_SCIENT)) %>%
  select(-c(id_col, ID_HOUSEHOLD, ID_FGD, NAME_ENG)) %>%
  #unite(ID1, ID_HOUSEHOLD,  sep = "_") %>%
  group_by(NAME_SCIENT) %>%
  summarise_all(funs(sum)) %>%
  #mutate_if(is.integer, as.factor) %>%
  #mutate_if(is.numeric, as.factor) %>%
  column_to_rownames("NAME_SCIENT")

head(Tabla1.1)  


#Tabla1.1 <- Tabla1 %>%
#  select(-c(id_col, ID_HOUSEHOLD, NAME_SCIENT)) %>%
#  unite(ID1, ID_FGD, NAME_ENG, sep = "_") %>%
#  group_by(ID1) %>%
#  summarise_all(funs(sum)) %>%
#  #mutate_if(is.integer, as.factor) %>%
#  #mutate_if(is.numeric, as.factor) %>%
#  column_to_rownames("ID1")

#head(Tabla1.1)  
#
#
#Tabla1.1 <- Tabla1 %>%
#  select(-c(id_col, ID_HOUSEHOLD, ID_FGD ,NAME_ENG)) %>%
#  #unite(ID1, ID_FGD, NAME_ENG, sep = "_") %>%
#  group_by(NAME_SCIENT) %>%
#  summarise_all(funs(sum)) %>%
#  #mutate_if(is.integer, as.factor) %>%
#  #mutate_if(is.numeric, as.factor) %>%
#  column_to_rownames("NAME_SCIENT")

head(Tabla1.1)  
dim(Tabla1.1)


# Para el argumento type pueden ser las vaiables de cuatro tipos:
# "n" es usado para un grupo de variables categóricas
# "s" es usado para un grupo de vaiables continuas escaladas (scaled continuous variables)
# "c" es usado para un grupo de vaiables continuas sin escalar (unscaled continuous variables)
# "f" es usado para un grupo de frecuencias

res1 <- MFA(Tabla1.1, group = c(4,9,5,4), type = c("c","c","c","c"), 
           ncp = 5, name.group = c("venta","Part_Plant","Usos","Consumo"))


res1
summary(res1, nbelements = Inf)

print(res1)
res1$separate.analyses
res1$separate.analyses

#Variables
res1$quanti.var$coord

#Species
res1$ind$coord


plot(res1, cex = 0.8, select = "cos2 0.5", shadow = T)
plot(res1, cex = 0.8)
plot(res1, choix = "var", cex = 0.8, shadow = T) # las variables con un valor de coseno2 mayor a 0.8


#Variables
VarFF <- res1$quanti.var$coord %>%
  as.data.frame() %>%
  select(Dim.1, Dim.2) %>%
  rownames_to_column() %>%
  dplyr::rename(Variable = rowname) %>%
  mutate(Var = "Variables")


head(VarFF)


#Species
  EspecieFF <- res1$ind$coord %>%
    as.data.frame() %>%
    select(Dim.1, Dim.2) %>%
    rownames_to_column() %>%
    dplyr::rename(Variable = rowname) %>%
    mutate(Var = "Especies")
    
head(EspecieFF)


    
PC1 <- round(res1$eig[1,2],1)
PC2 <- round(res1$eig[2,2],1)

p1 <- ggplot(EspecieFF, aes(x = Dim.1, y = Dim.2)) +
  theme_classic() +
  geom_segment(data =  VarFF, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2), 
               arrow = arrow(length = unit(0.2,"cm")),  alpha = 0.7, color = "red") +
  geom_text_repel(data = VarFF, aes(x = Dim.1, y = Dim.2, label = Variable)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_point(alpha = 0.7, size = 1.5) +
  xlab(paste(PC1, "%", sep = " ")) +
  ylab(paste(PC2, "%", sep = " ")) + 
  geom_label_repel(data = EspecieFF, aes(x = Dim.1, y = Dim.2, 
                                         label = ifelse(Dim.1 > 0, Variable,'')), 
                   box.padding   = 0.7, point.padding = 0.5, size = 3, alpha = 0.7)
  

p1


p1.1 <- ggplot(EspecieFF, aes(x = Dim.1, y = Dim.2)) +
  theme_classic() +
  #geom_segment(data =  VarFF, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2), 
  #             arrow = arrow(length = unit(0.2,"cm")),  alpha = 0.7, color = "red") +
  #geom_text_repel(data = VarFF, aes(x = Dim.1, y = Dim.2, label = Variable)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_point(alpha = 0.7, size = 1.5) +
  xlab(paste(PC1, "%", sep = " ")) +
  ylab(paste(PC2, "%", sep = " ")) + 
  geom_label_repel(aes(label = Variable), 
                   box.padding   = 0.4, point.padding = 0.1, size = 3, alpha = 0.7)


p1.1




TTabla1 <- bind_rows(VarFF, EspecieFF) %>%
  select(Var, Variable, Dim.1, Dim.2) %>%
  mutate(Dim.1a = decostand(Dim.1, method = "max")) %>%
  mutate(Dim.2a = decostand(Dim.2, method = "max")) %>%
  select(Var, Variable, Dim.1a, Dim.2a)

head(TTabla1)
tail(TTabla1)

EspecieFF1 <- TTabla1 %>%
  filter(Var == "Especies")

head(EspeciesFF1)

VarFF1 <- TTabla1 %>%
  filter(Var == "Variables") %>%
  mutate(Variable2 = c(rep("Ventas",4), rep("Planta",9), rep("Usos",5), rep("Consumo",4))) %>%
  select(Var, Variable2, Variable, Dim.1a, Dim.2a)

#VarFF1$Variable
  
  #separate(Variable2, c("Variable3", "Variable4", "Variable5"), "_")
  #stringr::str_split_fixed(Variable2, "_", 2)

head(VarFF1)
head(EspecieFF1)


p1 <- ggplot(EspecieFF1, aes(x = Dim.1a, y = Dim.2a)) +
  theme_classic() +
  geom_segment(data =  VarFF1, aes(x = 0, y = 0, xend = Dim.1a, yend = Dim.2a, color = Variable2), 
               arrow = arrow(length = unit(0.2,"cm")),  alpha = 0.7) +
  geom_text_repel(data = VarFF1, aes(x = Dim.1a, y = Dim.2a, label = Variable, color = Variable2)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_point(alpha = 0.5, size = 1.5) +
  xlab(paste(PC1, "%", sep = " ")) +
  ylab(paste(PC2, "%", sep = " ")) + 
  geom_label_repel(data = EspecieFF1, aes(x = Dim.1a, y = Dim.2a, 
                                         label = ifelse(Dim.1a > 0, Variable,'')), 
                   box.padding   = 0.7, point.padding = 0.5, size = 3, alpha = 0.7)


p1


Tabla7 <- read_xlsx( path = "MEdiana_Valor_Uso.xlsx", sheet = "Sup_hogares", col_names = T)
head(Tabla7)


p1 <- ggplot(Tabla7, aes(x = log10(No_Hogares), y = log10(Superficie))) +
  theme_minimal() +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_text_repel(aes(label = Especies), point.padding = 0.5, size = 4, alpha = 0.7 ) +
  xlab("Log No. Hogares") +
  ylab("Log Superficie (ha)")
  
p1
