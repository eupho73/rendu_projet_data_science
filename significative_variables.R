fold <- paste(getwd(),"/",sep="")
db <- read.csv(paste(fold,"dataTrain_13_FINAL.csv",sep=""))

library(hash)


# table de contingence pour les subregions par régions pour claimValue

output <- hash()
for ( region in unique(db$region) ){
  temp <- db[db$region == region, ]
  tcClaims <- table(temp$subRegion, temp$catClaims)
  tcNumber <- table(temp$subRegion, temp$catClaimNumber)
  testchi2Claims <- chisq.test(tcClaims)
  testchi2Number <- chisq.test(tcNumber)
  plotClaims <- mosaicplot(tcClaims, shade = TRUE, las = 2)
  plotNumber <- mosaicplot(tcNumber, shade = TRUE, las = 2)
  
  output[[region]] = list(
    testClaims=testchi2Claims,
    plotClaims=plotClaims,
    testNumber=testchi2Number,
    plotNumber=plotNumber)
} 




# glm pour check la significativité des variables:

glm_claimNumber <- lm(claimNumber ~., data = db)
coef_table <- coef(summary(glm_claimNumber))
head(coef_table)

significative_val_ <- coef_table[coef_table[, 4] <= 0.05, ]
significative_val_


nrow(db[db$subRegion == "M4", ])


# glm sans subregions pour check uniquement la significativité des regions
df_noSubRegion <- db[, colnames(db)[colnames(db) != 'subRegion']]

glm_claimNumber_noSubRegion <- lm(claimNumber ~., data = df_noSubRegion)

coef_table_nosub <- coef(summary(glm_claimNumber_noSubRegion))
head(coef_table_nosub)

significative_val_nosub <- coef_table_nosub[coef_table_nosub[, 4] <= 0.05, ]
significative_val_nosub


# On remarque que sans subregions, c'est la variable citydensity qui prend le relais et devient significative

# et si on supprime maintenant citydensity

glm_claimNumber_3 <- lm(claimNumber ~. - subRegion - cityDensity, data = db)
coef_table_glm3 <- coef(summary(glm_claimNumber_3))
significative_val_3 <- coef_table_glm3[coef_table_glm3[, 4] <= 0.05, ]
significative_val_3




# glm pour check la significativité des variables // claimValue:

glm_claimValue <- lm(claimValue ~., data = db)
coef_table <- coef(summary(glm_claimValue))
head(coef_table)

significative_val_ <- coef_table[coef_table[, 4] <= 0.05, ]
significative_val_
