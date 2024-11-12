# BRIC MDS, Dimension Correlation Scores, Index Formulation

library(tidyverse)
set.seed(123)



finaldf = read.csv("/Volumes/SD Drive/Geo Data/BRIC ALL DATA.csv")

# Multidimensional Scaling ####
## Social Dimension ####
social = finaldf %>% 
  dplyr::select(WORKING,BORND,CARS,NBENEFITS,EDUC,EDUCSPEND,HEALTHSPEND,PSYCHRES,PSYCH,DOCTOR) 

dissimilarity_matrix <- as.dist(1 - abs(cor(social)))
mds_result <- cmdscale(dissimilarity_matrix, k = 2)
plot(mds_result)

mds_df <- as.data.frame(mds_result)
colnames(mds_df) <- c("Dim1", "Dim2")
mds_df$Variable <- rownames(mds_df)

ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Variable)) +
  geom_point() +
  geom_text(vjust = 1.5) +
  ggtitle("MDS Plot of Variables") +
  xlab("Dimension 1") +
  ylab("Dimension 2")

## Economic Dimension ####
econ = finaldf %>% 
  select(WOZ,FEMINC,GINI,ECONSPEND,BANKS,LABSPEND,WOMWORK,OWNHOME) %>% 
  mutate(ECONSPEND1 = ECONSPEND + LABSPEND) %>% 
  select(-ECONSPEND, -LABSPEND)

dissimilarity_matrix <- as.dist(1 - abs(cor(econ)))
mds_result <- cmdscale(dissimilarity_matrix, k = 2)
#plot(mds_result)

mds_df <- as.data.frame(mds_result)
colnames(mds_df) <- c("Dim1", "Dim2")
mds_df$Variable <- rownames(mds_df)

ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Variable)) +
  geom_point() +
  geom_text(vjust = 1.5) +
  ggtitle("MDS Plot of Variables") +
  xlab("Dimension 1") +
  ylab("Dimension 2")

## Institutional Dimension ####
inst = finaldf %>% 
  select(CRISISSPEND,SOLVENCY,SAFETYSPEND)

dissimilarity_matrix <- as.dist(1 - abs(cor(inst)))
mds_result <- cmdscale(dissimilarity_matrix, k = 2)
#plot(mds_result)

mds_df <- as.data.frame(mds_result)
colnames(mds_df) <- c("Dim1", "Dim2")
mds_df$Variable <- rownames(mds_df)

ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Variable)) +
  geom_point() +
  geom_text(vjust = 1.5) +
  ggtitle("MDS Plot of Variables") +
  xlab("Dimension 1") +
  ylab("Dimension 2")

## Infrastructure Dimension ####

infra = finaldf %>% 
  select(HOSPITAL,VACHOME,HOTELS,TRAIN,PHARMACY,FIRE,ROAD,AIRPORT,SCHOOLS)

dissimilarity_matrix <- as.dist(1 - abs(cor(infra)))
mds_result <- cmdscale(dissimilarity_matrix, k = 2)
#plot(mds_result)

mds_df <- as.data.frame(mds_result)
colnames(mds_df) <- c("Dim1", "Dim2")
mds_df$Variable <- rownames(mds_df)

ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Variable)) +
  geom_point() +
  geom_text(vjust = 1.5) +
  ggtitle("MDS Plot of Variables") +
  xlab("Dimension 1") +
  ylab("Dimension 2")

## Community Dimension ####
comm = finaldf %>% 
  select(COMMSPEND,RECRSPEND,VOLUNTEER,SPORTS,MIGRATION,CRIMES,VOTER,DAYCARE)

dissimilarity_matrix <- as.dist(1 - abs(cor(comm)))
mds_result <- cmdscale(dissimilarity_matrix, k = 2)
#plot(mds_result)

mds_df <- as.data.frame(mds_result)
colnames(mds_df) <- c("Dim1", "Dim2")
mds_df$Variable <- rownames(mds_df)

ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Variable)) +
  geom_point() +
  geom_text(vjust = 1.5) +
  ggtitle("MDS Plot of Variables") +
  xlab("Dimension 1") +
  ylab("Dimension 2")

## Environment Dimension ####
envi = finaldf %>% 
  select(LAND,GREENSPEND,WASTESPEND,GAS,ELEC,BUFFER,OPENSPACE,CULTIVATE,FLOOD) %>% 
  mutate(ENVISPEND = GREENSPEND + WASTESPEND) %>% 
  select(-GREENSPEND,-WASTESPEND)

dissimilarity_matrix <- as.dist(1 - abs(cor(envi)))
mds_result <- cmdscale(dissimilarity_matrix, k = 2)
#plot(mds_result)

mds_df <- as.data.frame(mds_result)
colnames(mds_df) <- c("Dim1", "Dim2")
mds_df$Variable <- rownames(mds_df)

ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Variable)) +
  geom_point() +
  geom_text(vjust = 1.5) +
  ggtitle("MDS Plot of Variables") +
  xlab("Dimension 1") +
  ylab("Dimension 2")

# Dimension Correlation ####

social = social %>% 
  mutate(score = rowMeans(select(.,1:10)))
econ = econ %>% 
  mutate(score = rowMeans(select(.,1:7)))
inst = inst %>% 
  mutate(score = rowMeans(select(.,1:3)))
infra = infra %>% 
  mutate(score = rowMeans(select(.,1:9)))
comm = comm %>% 
  mutate(score = rowMeans(select(.,1:8)))
envi = envi %>% 
  mutate(score = rowMeans(select(.,1:8)))

dimscores = data.frame(social$score,econ$score,inst$score,infra$score,comm$score,envi$score)
dimcor = cor(dimscores,method="spearman")
corrplot::corrplot(dimcor)

Hmisc::rcorr(as.matrix(dimscores))

# Robustness Check with PCA #### 

selectednames = c(Hmisc::Cs(WORKING,BORND,CARS,NBENEFITS,EDUC,EDUCSPEND,HEALTHSPEND,PSYCHRES,PSYCH,DOCTOR,WOZ,FEMINC,GINI,ECONSPEND,BANKS,LABSPEND,WOMWORK,OWNHOME,CRISISSPEND,SOLVENCY,SAFETYSPEND,HOSPITAL,VACHOME,HOTELS,TRAIN,PHARMACY,FIRE,ROAD,AIRPORT,SCHOOLS,COMMSPEND,RECRSPEND,VOLUNTEER,SPORTS,MIGRATION,CRIMES,VOTER,DAYCARE,LAND,GREENSPEND,WASTESPEND,GAS,ELEC,BUFFER,OPENSPACE,CULTIVATE,FLOOD))

setdiff(colnames(finaldf),selectednames)
pcadata = finaldf %>% 
  select(-c(setdiff(colnames(finaldf),selectednames))) %>% 
  mutate(ECONSPEND1 = ECONSPEND + LABSPEND) %>% 
  select(-ECONSPEND, -LABSPEND) %>% 
  mutate(ENVISPEND = GREENSPEND + WASTESPEND) %>% 
  select(-GREENSPEND,-WASTESPEND) %>% 
  rename(ECONSPEND = ECONSPEND1)

library(factoextra)
pca_result <- prcomp(pcadata)

#summary(pca_result)
#loadings = as.data.frame(pca_result$rotation) 

fviz_eig(pca_result,ncp=20) # according to this scree plot, we would take four PCs.

# If we use the Kaiser criterion, then we would not keep any components.
fviz_eig(pca_result, choice = "eigenvalue", addlabels=TRUE) 
eigenvalues <- (pca_result$sdev)^2
components_to_keep <- which(eigenvalues > 1)

# Parallel Analysis (Horn's Method)
paran::paran(pcadata,iterations=500,centile=95)
# If we use this method, we would keep 5 components

# Broken Stick Model
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100), geom = "bar") +
  geom_hline(yintercept = 100 / length(pca_result$sdev), linetype = "dashed", color = "red")
# If we use this method, we should keep 10 components...

# We use the scree plot (PC=4)

# The combinations from PCA just doesn't make sense compared to the construction of BRIC...
fviz_contrib(pca_result, choice = "var", axes = 1)
fviz_contrib(pca_result, choice = "var", axes = 2)
fviz_contrib(pca_result, choice = "var", axes = 3)
fviz_contrib(pca_result, choice = "var", axes = 4)

# make an index out of the four components
pc_scores <- pca_result$x
pcaindex <- pc_scores[, 1] + pc_scores[, 2] + pc_scores[, 3] + pc_scores[, 4] 
index_minmax <- (pcaindex - min(pcaindex)) / (max(pcaindex) - min(pcaindex))

# PCA optimized weights
loadings <- pca_result$rotation[, 1:4]
variance_explained <- summary(pca_result)$importance[2, 1:4]
optimized_weights <- loadings %*% variance_explained

write.csv(optimized_weights,"pcaweights.csv")

# Index Formulation ####
social = finaldf %>% 
  dplyr::select(GM_NAAM,GM_CODE,WORKING,BORND,CARS,NBENEFITS,EDUC,EDUCSPEND,HEALTHSPEND,PSYCHRES,PSYCH,DOCTOR) %>% 
  mutate(socialscore = rowMeans(select(.,3:12))) %>% 
  select(GM_NAAM,GM_CODE,socialscore)

econ = finaldf %>% 
  select(GM_NAAM,GM_CODE,WOZ,FEMINC,GINI,ECONSPEND,BANKS,LABSPEND,WOMWORK,OWNHOME) %>% 
  mutate(ECONSPEND1 = ECONSPEND + LABSPEND) %>% 
  select(-ECONSPEND, -LABSPEND) %>% 
  rename(ECONSPEND = ECONSPEND1) %>% 
  mutate(econscore = rowMeans(select(.,3:9))) %>% 
  select(GM_NAAM,GM_CODE,econscore)

inst = finaldf %>% 
  select(GM_NAAM,GM_CODE,CRISISSPEND,SOLVENCY,SAFETYSPEND) %>% 
  mutate(instscore = rowMeans(select(.,3:5))) %>% 
  select(GM_NAAM,GM_CODE,instscore)

infra = finaldf %>% 
  select(GM_NAAM,GM_CODE,HOSPITAL,VACHOME,HOTELS,TRAIN,PHARMACY,FIRE,ROAD,AIRPORT,SCHOOLS) %>% 
  mutate(infrascore = rowMeans(select(.,3:11))) %>% 
  select(GM_NAAM,GM_CODE,infrascore)

comm = finaldf %>% 
  select(GM_NAAM,GM_CODE,COMMSPEND,RECRSPEND,VOLUNTEER,SPORTS,MIGRATION,CRIMES,VOTER,DAYCARE) %>% 
  mutate(commscore = rowMeans(select(.,3:10))) %>% 
  select(GM_NAAM,GM_CODE,commscore)

envi = finaldf %>% 
  select(GM_NAAM,GM_CODE,LAND,GREENSPEND,WASTESPEND,GAS,ELEC,BUFFER,OPENSPACE,CULTIVATE,FLOOD) %>% 
  mutate(ENVISPEND = GREENSPEND + WASTESPEND) %>% 
  select(-GREENSPEND,-WASTESPEND) %>% 
  mutate(enviscore = rowMeans(select(.,3:10))) %>% 
  select(GM_NAAM,GM_CODE,enviscore)

bricall = social %>% 
  left_join(econ,join_by(GM_NAAM,GM_CODE)) %>% 
  left_join(inst,join_by(GM_NAAM,GM_CODE)) %>% 
  left_join(infra,join_by(GM_NAAM,GM_CODE)) %>% 
  left_join(comm,join_by(GM_NAAM,GM_CODE)) %>% 
  left_join(envi,join_by(GM_NAAM,GM_CODE)) %>% 
  mutate(across(where(is.numeric), ~ (.- min(.)) / (max(.) - min(.)))) %>%
  mutate(finalscore = rowSums(select(.,socialscore:enviscore)))

# Top and Bottom 10 Municipalities ####
top_10 <- bricall %>%
  arrange(desc(finalscore)) %>%  
  slice_head(n = 10)   
bottom_10 <- bricall %>%
  arrange(finalscore) %>%  
  slice_head(n = 10)   
topbot10 = rbind(top_10,bottom_10)

bricallranked <- bricall %>%
  mutate(
    rank_social = rank(-socialscore, ties.method = "min"),
    rank_econ = rank(-econscore, ties.method = "min"),
    rank_inst = rank(-instscore, ties.method = "min"),
    rank_infra = rank(-infrascore, ties.method = "min"),
    rank_comm = rank(-commscore, ties.method = "min"),
    rank_envi = rank(-enviscore, ties.method = "min"),
    rank_final = rank(-finalscore,ties.method = "min")
  )

topbot10 = topbot10 %>% left_join(bricallranked, join_by(GM_NAAM,GM_CODE,socialscore,econscore,instscore,infrascore,commscore,enviscore,finalscore))
write.csv(topbot10,"topbot10bric.csv")

# Make data for maps (ArcGIS) ####
gb2023 <- sf::read_sf("/Volumes/SD Drive/Geo Data/CBS Buurt:W:G Data/2023WijkBuurtkaart_2023_v1/gemeenten_2023_V1.shp")
gem = gb2023 %>% mutate(AANT_INW = na_if(AANT_INW,-99999999)) %>% drop_na() %>% 
  select(GM_NAAM,GM_CODE)
rm(gb2023)

bric 

## Social Score ####
score = social %>% inner_join(bricall,by=join_by(GM_NAAM,GM_CODE))

## Overall BRIC Score ####
bric = gem %>% 
  inner_join(social,by=join_by(GM_NAAM,GM_CODE)) %>% 
  inner_join(econ,by=join_by(GM_NAAM,GM_CODE)) %>% 
  inner_join(inst,by=join_by(GM_NAAM,GM_CODE)) %>% 
  inner_join(infra,by=join_by(GM_NAAM,GM_CODE)) %>% 
  inner_join(comm,by=join_by(GM_NAAM,GM_CODE)) %>% 
  inner_join(envi,by=join_by(GM_NAAM,GM_CODE)) 


bricall = bricall %>% select(GM_NAAM,GM_CODE,finalscore)
bric = bric %>% inner_join(bricall,by=join_by(GM_NAAM,GM_CODE))

spatial_df <- st_as_sf(bric, coords = c("lon", "lat"), crs = 28992)
st_write(spatial_df, "GEO BRIC FINAL.geojson", append = F)
