#Porjet de recherche honor APES
#But : Ã©valuer s'il existe un lien entre ES et AP, c-Ã -d.,
#si l'AP peut prÃ©dire avec une rÃ©gression le score d'ES.

#*Variables :
   #1)Score total à l'échelle de Rosenberg pour mesurer l'estime de soi = ES
   #Le nombre total d'expérience adverses différentes vécues par un participant = sumAP
   #Lâge de la première exposition à une exépérience adverse (à laquelle une expérience adverse
   #a eu lieu pour la première fois) = ageMin

#Hypotheses : 
   #1-ES = B*sumAP
   #2- ES = B*Ã¢gePremiereExposition
   #3 - ES = BSumAP*Age

# Étapes du code:
   # suppositions de la rÃ©gression (linÃ©aritÃ©,autocorrel.,homoscÃ©dasticitÃ©,
   # normalitÃ© du terme d'erreur,valeurs influentes,(7) multicolinÃ©aritÃ©)
   # reg simple ES-SumAce
   # reg simple ES-Ã‚ge
   # reg multiple ES = SumAP+Agemin+ SumAP*Age


setwd("C:/Users/Dylan/Desktop/Honor/APES")

##required packages

require(ggplot2)
require(foreign)


##Edit dataframe

fulldataset <- read.spss("ACE_combined_oct2020.sav",to.data.frame = T)

##extraction de l'age minimale d'exposition
   #Lâge de la première exposition a été notée pour chaque question du ACE-Q
   #Cette variable va être nécessaire pour extraire l'âge minimale parmi les 13 questions

All_first_age <-fulldataset[ , c("ACEQ_01_1", "ACEQ_02_1", "ACEQ_03_1",
                                 "ACEQ_04_1", "ACEQ_05_1", "ACEQ_06_1",
                                 "ACEQ_07_1", "ACEQ_08_1", "ACEQ_09_1", 
                                 "ACEQ_10_1", "ACEQ_11_1", "ACEQ_12_1",
                                 "ACEQ_13_1")]
#Nettoyage des données
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



##couper les observations 86 à 97 car elles n'ont aucune
##valeur 
fulldataset<- fulldataset[1:85, ]

##enlever les participant qui n'ont aucune valeur pour RSE_sum (estime de soi)

cleardataset <-subset(fulldataset, fulldataset$RSE_Sum != "NA")


#FAire les trois régressions
attach(cleardataset)

reglin1 <- lm(RSE_Sum~ACEsum, data = cleardataset)
summary(reglin1)

reglin2 <- lm(RSE_Sum~ageMin, data = cleardataset)
summary(reglin2)
reglin3 <- lm(RSE_Sum~ageMin*ACEsum, data = cleardataset)
summary(reglin3)

detach(cleardataset)


##Suppositions de la rÃ©gression

#LinÃ©aritÃ
   attach(cleardataset)
  # RSE_sum_clear <-na.omit(RSE_Sum)
  #print(RSE_sum_clear)
   
   
   plot(reglin1$fitted.values,RSE_Sum,xlab = "valeurs prédites")
   abline(a=0,b=1)
   ##==>on voit que la tendance est linéaire
   
   plot(reglin1,which=1)
   abline(h=0)
   
   #reglin2
   
   plot(reglin2$fitted.values,RSE_sum,xlab = "valeurs prédites")
   abline(a=0,b=1)
   ##==>on voit que la tendance est linéaire
   
   plot(reglin2,which=1)
   abline(h=0)
   
   ##reglin3
   
   plot(reglin3$fitted.values,RSE_sum,xlab = "valeurs prédites")
   abline(a=0,b=1)
   ##==>tendance est linéaire
   
   plot(reglin3,which=1)
   abline(h=0)
   ##==>tendance  linéaire, mais attention à l'homoscédasticité - forme de cone
   
   
#Autocorrélation

   
   cleardataset$fitted_reg1 <- reglin1$fitted.values
   attach(cleardataset)
   
   ##graphique var. indépendante ACEsum - résidus
   
   plot(ACEsum,reglin1$residuals,xlab = "ACEsum")
   abline(a=0,b=0)
   
   
   plot(ageMin,reglin2$residuals,xlab = "âge minimal")
   abline(a=0,b=0)
   
   ##dans les deux graphiques on voit que les résidus oscillent autour de 0
   ##donc pas de problème d'autocorrélation
   
##homoscédasticité pour les trois régressions
   
   
   plot(reglin1, which = 1)
   abline(h=0)
   
   plot(reglin2, which = 1)
   abline(h=0)
   
   plot(reglin3, which = 1)
   abline(h=0)
   

   #Scale-location plot avec les résidus standardisés
   plot(reglin1, which = 3)
   abline(h=0)
   
   plot(reglin2, which = 3)
   abline(h=0)
   
   plot(reglin3, which = 3)
   abline(h=0)

   #légètr tendance vers le haut  pour reglin2 et 3. Ne semble pas poser problème 
   
   
   
##Normalité du terme d'erreur
   

   hist(reglin1$residuals)
   hist(reglin2$residuals)
   hist(reglin3$residuals)
   
   #les résidus se distribuent assez normalement
   
   #QQ-plot
   require(olsrr)
   ols_plot_resid_qq(reglin1)
   ols_plot_resid_qq(reglin2)
   ols_plot_resid_qq(reglin3)
   
   #Les quantiles de l'échantillons ont une relation assez
   #linéaire avec les quantiles théoriques
   
##Valeurs influentes
   
   #Distance de Cook
   plot(reglin1, 4)
   plot(reglin2, 4)
   plot(reglin3, 4)
   
   #Observation 14 à surveiller D = 0,20. Ne semble toutefois pas problématique
   #car D < 1
   
   #Graphiques effet de levier avec les résidus
   
   plot(reglin1,5)
   plot(reglin2,5)
   plot(reglin13,5)
   
   #Aucune observations influentes
   
   
##Statistiques descriptives
   
   
   summary(RSE_Sum)
   summary(ACEsum)
   summary(ageMin)
   
   
   #Fréquences pour chaque questions du ACE-Q (pour chaque type différent d'AP)
   
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
   
   
   