
##Création d'une base de données finales
library(dplyr)
library(ggplot2)
install.packages("gt")
library(gt)
library(tidyverse)

install.packages("patchwork")
library(patchwork)



df_carac_col<-caracteristiques %>% select(eleve_id)
df_eval<-evaluateurs %>% select(eleve_id)
df_pres<-presences %>% select(V1)
df_resul<-resultats %>% select(V1)


getwd()

df_final <- left_join(left_join(left_join(caracteristiques, evaluateurs, by="eleve_id"), presences, by="eleve_id"), resultats, by="eleve_id")


df_final<-caracteristiques%>%
  left_join(evaluateurs,by="eleves_id")%>%
  left_join(presences,by="eleves_id")%>%
  left_join(resultats,by="eleves_id")
  
head(df_final)

table(df_final$programme)

## statistique descriptive

aggregate(cbind(parents_revenu,score0)~programme,data=df_final,mean)

table(df_final$programme,df_final$quartier)

table(df_final$programme,df_final$parents_education)

## Comparaison score initial moyen et revenu moyen 


df_final%>%
  group_by(programme)%>%
  summarise(
    score0_moyen=mean(score0,na.rm=TRUE),
    revenu_moyen=mean(parents_revenu,na.rm=TRUE)
  )


    
 ##vérification de correlation entre niveau d'étude et revenu

aggregate(parents_revenu~parents_education,df_final,mean)

##Comparaison score moyen initial et revenu moyen(variable numérique)
df_final%>%
  group_by(programme)%>%
  summarise(
    score0_moyen=mean(score0,na.rm=TRUE),
    revenu_moyen=mean(parents_revenu,na.rm=TRUE)
  )

##Répartition des éleves par quartier

ggplot(df_final, aes(x = quartier, fill = factor(programme))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels= scales::percent)+
  labs(
    x= "Quartier", 
    y = "Poucentage des élèves",  
    fill = "Participation", 
    title = "Répartition des élèves par quartier (%)"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##Repartition par niveau d'éducation des parents

ggplot(df_final, aes(x = parents_education, fill = factor(programme))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Niveau d'éducation des parents", 
    y = "Proportion", 
    fill = "Programme",
    title = "Participation au programme selon l'éducation des parents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 

# Graphique pour les non-traités
p1 <- df_final %>% 
  filter(programme == 0 | programme == "0") %>% 
  ggplot(aes(x = factor(programme), y = score0)) +
  geom_boxplot(fill = "tomato") + 
  scale_y_continuous(limits = c(0, 100)) + 
  labs(title = "Score Initial (Non-traités)", x = NULL, y = "Score")


# Graphique pour les participants
p2 <- df_final %>% 
  filter(programme == 1 | programme == "1") %>% 
  ggplot(aes(x = factor(programme), y = score0)) +
  geom_boxplot(fill = "steelblue") + 
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Score Initial (Traités)", x = NULL, y = "Score")+
  theme(legend.position="none")

p1 + p2


##Regression logistique

df_final$quartier<-as.factor(df_final$quartier)
df_final$evaluateur_id<-as.factor(df_final$evaluateur_id)
df_final$parents_education<-as.factor(df_final$parents_education)

modele1<-glm(programme~quartier+
               parents_education+
               parents_revenu+
               evaluateur_id+
               score0,
             data=df_final,
             family = binomial(link = "logit"))

summary(modele1)

##Score de propension
df_final$score_propension<-predict(modele1,type="response")
head(df_final$score_propension)

##Tableau récapitulatif des caractéristiques du Profil type 
resul_logit<- summary(modele1)$coefficients
resul_logit<-as.data.frame(resul_logit)
resul_logit$variable<-row.names(resul_logit)

resul_sign<-resul_logit[resul_logit[,4]<0.05,]
resul_sign<-resul_sign[order(abs(resul_sign[,1]),decreasing = TRUE),]
print(resul_sign[,c("variable","Estimate","Pr(>|z|)")])

resul_graph<-subset(resul_sign,variable="Intercept)")
ggplot(resul_graph,aes(x=reorder(variable,abs(Estimate)),y=Estimate))+
  geom_bar(stat="identity",fill="lightblue")+
  coord_flip()+
  labs(title="Profil type du participant",x="Variable",y="Coefficient")+
  theme_minimal()


##Score de propension moyen des non-participants et des participants
tapply(df_final$score_propension,df_final$programme,mean)


##Visualisons les résultats à l'aide d'un graphique
boxplot(score_propension~programme,
        data = df_final,
     main="score de propension par groupe",
     xlab="programme(0=non,1=oui)",
     ylab = "score de propension",
     col = c("lightblue","lightgreen"))

 
##  Création de la base de données pour l'anglais  
n<-nrow(df_final)

eleve_id<-c(df_final$eleve_id,df_final$eleve_id)
score<-c(df_final$score0,df_final$score1_anglais)
temps<-c(rep(0,n),rep(1,n))
programme<-c(df_final$programme,df_final$programme)

df_anglais<-data.frame(eleve_id,score,temps,programme)
head(df_anglais)
dim(df_anglais)

did_anglais<-lm(score~programme+temps+programme:temps,data=df_anglais)
summary(did_anglais)

table(df_anglais$temps,df_anglais$programme)



##Création de la base de données pour les mathématiques
eleve_id<-c(df_final$eleve_id,df_final$eleve_id)
score<-c(df_final$score0,df_final$score1_maths)
temps<-c(rep(0,n),rep(1,n))
programme<-c(df_final$programme,df_final$programme)

df_maths<-data.frame(eleve_id,score,temps,programme)
head(df_maths)
dim(df_maths)

did_maths<-lm(score~programme+temps+programme*temps,data=df_maths)
summary(did_maths)

##taux de présence 

tableau_profil <- df_final %>%
  group_by(programme) %>%
  summarise(
    revenu_moyen = mean(parents_revenu, na.rm = TRUE),
    score0_moyen = mean(score0, na.rm = TRUE),
    )


