#Porjet de recherche honor APES
#But : évaluer s'il existe un lien entre ES et AP, c-à-d.,
#si l'AP peut prédire avec une régression le score d'ES.

#*Variables :
   #1)Score total � l'�chelle de Rosenberg pour mesurer l'estime de soi = ES
   #Le nombre total d'exp�rience adverses diff�rentes v�cues par un participant = sumAP
   #L�ge de la premi�re exposition � une ex�p�rience adverse (� laquelle une exp�rience adverse
   #a eu lieu pour la premi�re fois) = ageMin

#Hypotheses : 
   #1-ES = B*sumAP
   #2- ES = B*âgePremiereExposition
   #3 - ES = BSumAP*Age

# �tapes du code:
   # suppositions de la régression (linéarité,autocorrel.,homoscédasticité,
   # normalité du terme d'erreur,valeurs influentes,(7) multicolinéarité)
   # reg simple ES-SumAce
   # reg simple ES-Âge
   # reg multiple ES = SumAP+Agemin+ SumAP*Age


setwd("C:/Users/Dylan/Desktop/Honor/APES")

##required packages

require(ggplot2)
require(foreign)


##Edit dataframe

fulldataset <- read.spss("ACE_combined_oct2020.sav",to.data.frame = T)

##extraction de l'age minimale d'exposition
   #L�ge de la premi�re exposition a �t� not�e pour chaque question du ACE-Q
   #Cette variable va �tre n�cessaire pour extraire l'�ge minimale parmi les 13 questions

All_first_age <-fulldataset[ , c("ACEQ_01_1", "ACEQ_02_1", "ACEQ_03_1",
                                 "ACEQ_04_1", "ACEQ_05_1", "ACEQ_06_1",
                                 "ACEQ_07_1", "ACEQ_08_1", "ACEQ_09_1", 
                                 "ACEQ_10_1", "ACEQ_11_1", "ACEQ_12_1",
                                 "ACEQ_13_1")]
#Nettoyage des donn�es
##remove all the 0 and replace them with NA
All_first_age[All_first_age == 0] <- NA

min_row <-200
i=1

#Encodage de l'age min dans une nouvelle variable du full data set
for (i in 1:nrow(All_first_age)){
  
   min_row <-  min(All_first_age[i,], na.rm = T)
  
   fulldataset$ageMin[i] <- min_row 
   i <- i +1

}
print( fulldataset$ageMin)



##couper les observations 86 � 97 car elles n'ont aucune
##valeur 
fulldataset<- fulldataset[1:85, ]

##enlever les participant qui n'ont aucune valeur pour RSE_sum (estime de soi)

cleardataset <-subset(fulldataset, fulldataset$RSE_Sum != "NA")


#FAire les trois r�gressions
attach(cleardataset)

reglin1 <- lm(RSE_Sum~ACEsum, data = cleardataset)
summary(reglin1)

reglin2 <- lm(RSE_Sum~ageMin, data = cleardataset)
summary(reglin2)
reglin3 <- lm(RSE_Sum~ageMin*ACEsum, data = cleardataset)
summary(reglin3)

detach(cleardataset)


##Suppositions de la régression

#Linéarit�
   attach(cleardataset)
  # RSE_sum_clear <-na.omit(RSE_Sum)
  #print(RSE_sum_clear)
   
   
   plot(reglin1$fitted.values,RSE_Sum,xlab = "valeurs pr�dites")
   abline(a=0,b=1)
   ##==>on voit que la tendance est lin�aire
   
   plot(reglin1,which=1)
   abline(h=0)
   
   #reglin2
   
   plot(reglin2$fitted.values,RSE_sum,xlab = "valeurs pr�dites")
   abline(a=0,b=1)
   ##==>on voit que la tendance est lin�aire
   
   plot(reglin2,which=1)
   abline(h=0)
   
   ##reglin3
   
   plot(reglin3$fitted.values,RSE_sum,xlab = "valeurs pr�dites")
   abline(a=0,b=1)
   ##==>tendance est lin�aire
   
   plot(reglin3,which=1)
   abline(h=0)
   ##==>tendance  lin�aire, mais attention � l'homosc�dasticit� - forme de cone
   
   
#Autocorr�lation

   
   cleardataset$fitted_reg1 <- reglin1$fitted.values
   attach(cleardataset)
   
   ##graphique var. ind�pendante ACEsum - r�sidus
   
   plot(ACEsum,reglin1$residuals,xlab = "ACEsum")
   abline(a=0,b=0)
   
   
   plot(ageMin,reglin2$residuals,xlab = "�ge minimal")
   abline(a=0,b=0)
   
   ##dans les deux graphiques on voit que les r�sidus oscillent autour de 0
   ##donc pas de probl�me d'autocorr�lation
   
##homosc�dasticit� pour les trois r�gressions
   
   
   plot(reglin1, which = 1)
   abline(h=0)
   
   plot(reglin2, which = 1)
   abline(h=0)
   
   plot(reglin3, which = 1)
   abline(h=0)
   

   #Scale-location plot avec les r�sidus standardis�s
   plot(reglin1, which = 3)
   abline(h=0)
   
   plot(reglin2, which = 3)
   abline(h=0)
   
   plot(reglin3, which = 3)
   abline(h=0)

   #l�g�tr tendance vers le haut  pour reglin2 et 3. Ne semble pas poser probl�me 
   
   
   
##Normalit� du terme d'erreur
   

   hist(reglin1$residuals)
   hist(reglin2$residuals)
   hist(reglin3$residuals)
   
   #les r�sidus se distribuent assez normalement
   
   #QQ-plot
   require(olsrr)
   ols_plot_resid_qq(reglin1)
   ols_plot_resid_qq(reglin2)
   ols_plot_resid_qq(reglin3)
   
   #Les quantiles de l'�chantillons ont une relation assez
   #lin�aire avec les quantiles th�oriques
   
##Valeurs influentes
   
   #Distance de Cook
   plot(reglin1, 4)
   plot(reglin2, 4)
   plot(reglin3, 4)
   
   #Observation 14 � surveiller D = 0,20. Ne semble toutefois pas probl�matique
   #car D < 1
   
   #Graphiques effet de levier avec les r�sidus
   
   plot(reglin1,5)
   plot(reglin2,5)
   plot(reglin13,5)
   
   #Aucune observations influentes
   
   
##Statistiques descriptives
   
   
   summary(RSE_Sum)
   summary(ACEsum)
   summary(ageMin)
   
   
   #Fr�quences pour chaque questions du ACE-Q (pour chaque type diff�rent d'AP)
   
   All_ACE_question <--
   
   for (i in 1:nrow(All_first_age)){
      
      min_row <-  min(All_first_age[i,], na.rm = T)
      
      fulldataset$ageMin[i] <- min_row 
      i <- i +1
      
   }
   
   #Nuage de point
   
   #ACEsum-estime
   require(ggplot2)
   ggplot(cleardataset,aes(x=ACEsum,y=RSE_Sum))+geom_point()+
      geom_smooth(method = "lm",se=F)
   
   #Age minimal et estime
   
   ggplot(cleardataset,aes(x=ageMin,y=RSE_Sum))+geom_point()+
      geom_smooth(method = "lm",se=F)
   
   #Interaction ACEsum*Age minimal
  
   ggplot(cleardataset,aes(x=ACEsum*ageMin,y=RSE_Sum))+geom_point()+
      geom_smooth(method = "lm",se=F)
   
   
   