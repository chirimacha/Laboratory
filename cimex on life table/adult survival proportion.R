#leer csv
adult_surv<-read.csv(file="cimex adult survival.csv")
head(adult_surv)
#ASIGNANDO COLUMNAS para reshape, todavia no se como usarlo directamente, usare el modelo de Casey
#controles
#se comentaran los codigos para las repeticiones, solo se correra los pull

#co_1_m<-adult_surv$co_pil_m
#co_1_h<-adult_surv$co_pil_h
#co_2_m<-adult_surv$co_rep1_m
#co_2_h<-adult_surv$co_rep1_h
#co_3_m<-adult_surv$co_rep2_m
#co_3_h<-adult_surv$co_rep2_h
co_all_m<-adult_surv$co_pull_m
co_all_h<-adult_surv$co_pull_h
#infectados
#inf_1_m<-adult_surv$inf_pil_m
#inf_1_h<-adult_surv$inf_pil_h
#inf_2_m<-adult_surv$inf_rep1_m
#inf_2_h<-adult_surv$inf_rep1_h
#inf_3_m<-adult_surv$inf_rep2_m
#inf_3_h<-adult_surv$inf_rep2_h
inf_all_m<-adult_surv$inf_pull_m
inf_all_h<-adult_surv$inf_pull_h

#creando cuadro de datos con todas las variables para que pases de horizontal a vertical
#se creara  codigo para solo 4 var
#bdadult<-cbind(co_1_m,co_1_h,co_2_m,co_2_h,co_3_m,co_3_h,co_all_m,
 #              co_all_h,inf_1_m,inf_1_h,inf_2_m,inf_2_h,inf_3_m,inf_3_h,
 #              inf_all_m,inf_all_h)

bdadult<-cbind(co_all_m,co_all_h,inf_all_m,inf_all_h)

bdadult<-as.data.frame(t(bdadult))
#hasta aqui no tiene nombres las columnas
#creando nombreas a las columnas, en este caso le estamos asignando numeros del 1 al 47
colnames(bdadult)<-c(1:47)
#ahora crearemos una nueva columna llamada nombre de grupo (groupname)
#con los nombres en el mismo orden en que estan las variabes ingresadas 
#anteriormente en el cuadro de datos
#se creara solo para 4 var
#bdadult$groupname<-c("Co Rep 1 M","Co Rep 1 H","Co Rep 2 M","Co Rep 2 H","Co Rep 3 M","Co Rep 3 H","Co Pull M","Co Pull H",
#                     "Inf Rep 1 M","Inf Rep 1 H","Inf Rep 2 M","Inf Rep 2 H","Inf Rep 3 M","Inf Rep 3 H","Inf Pull M","Inf Pull H")

bdadult$groupname<-c("Co Pull M","Co Pull H","Inf Pull M","Inf Pull H")


#dandole forma vertical a la base de datos con reshape
newbd<-reshape(bdadult, varying = 1:47, v.names="Proportion", direction = "long", idvar = "groupname")

# Ploteando el grafico de proporciones de sobrevivencia ~ tiempo
ggplot(newbd, aes(x = time, y = Proportion, color = groupname, fill = groupname)) +
#  geom_line()
geom_line(aes(linetype=groupname, size= groupname)) + 
  scale_color_manual(values = c("#1F78B4","#1F78B4","#E31A1C","#E31A1C")) +
  scale_linetype_manual(values=c("dashed","solid","dashed","solid"))+
   scale_size_manual(values=c(1,1,1,1))+
  #scale_size_manual(values= c(1.5,15))+
  ggtitle("Cimex Adulthood Proportion Alive per week") + xlab("Time (Weeks)") + ylab("Proportion Alive") +
  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))

        

