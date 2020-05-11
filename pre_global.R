library(tidyverse)
library(prioritizr)
library(rgdal)
library(here)
library(doParallel)
library(readxl)
# library(rdrop2)

#get country specific target value
count_tar <- function(PU = NULL, target = NULL){
  round(cellStats(PU,"sum") / 100 * target, 0)
}

ELSA_df <- read_xlsx(here("Costa Rica v2 Data.xlsx"), "Sheet1")


feat_df <- ELSA_df[!is.na(ELSA_df$Protect), ] %>% 
  mutate(feat_name = ifelse(!is.na(CR_data), CR_data, Global_data)) %>% 
  arrange(`Label-theme`, `Label-name`)

feat_stack <- stack(here("data/Features/", feat_df$feat_name))
#to avoid values <0
feat_stack$Forest_Water_Yield[feat_stack$Forest_Water_Yield < 0] <- NA
feat_stack_sc <- feat_stack / cellStats(feat_stack, max)
                                                         

#Possible cost layer:
HFP <- raster(here("data/Zones/", ELSA_df[ELSA_df$`Main Category` == "Human Footprint","CR_data"]))

#Protected areas
PA <- raster(here("data/Zones/", ELSA_df[ELSA_df$`Main Category` == "Protected Natural Areas", "CR_data"][1,]))
PA <- PA > 50
PA0 <- PA
PA0[] <- NA
# T_indigena<- raster("Indigenous Territories.tif")


# pu
pu0 <- raster(here("data/", ELSA_df[ELSA_df$`Main Category` == "Planning units","CR_data"]))
pu <- pu0
pu[is.na(HFP)] <- NA

###################################
# Zones
###################################

urban <- raster(here("data/Zones/", "Urban Non-Urban.tif")) 
agri <- raster(here("data/Zones/", "Agriculture.tif")) 
fsci <- raster(here("data/Zones/", "fsci_cri.tif"))
lzfm <- raster(here("data/Zones/", "Life Zones Forest and Mangrove.tif")) > 50
forest <- raster(here("data/Zones/", "Forest.tif")) > 50
mangrove <- raster(here("data/Zones/", "Mangroves.tif")) > 50

#Manage 
# In agriculture-10% threshold
Z_MG <- agri > 10 
Z_MG[urban > 30] <- NA
Z_MG[Z_MG == 0] <- NA

#Urban_Green
# In Urban-10% theshold
Z_UG <- urban > 10
Z_UG[Z_UG == 0] <- NA

#Protect
# Not in urban, not in agriculture-10% threshold, fsci >13
Z_PR <- fsci > 13
Z_PR[urban > 30] <- NA
Z_PR[agri > 30] <- NA
Z_PR[Z_PR == 0] <- NA

#Restore
# Not in urban or agriculture, in life zone forest or mangrove, not mangrove, not forest, in fsci_cri =<13
not_r <- sum(urban > 30, agri > 30, mangrove, forest, na.rm = T)
yes_r <- sum(lzfm, fsci <= 13, na.rm = T)
Z_RE <- yes_r > 0
Z_RE[not_r] <- NA
Z_RE[Z_RE == 0] <- NA

#PA locked in
Z_PR_PA <- Z_PR | PA
Z_RE_PA <- Z_RE
Z_MG_PA <- Z_MG
Z_UG_PA <- Z_UG

#PES locked in
pes_pro <- raster(here("data/Zones/", "Payment for ES - Protection.tif")) > 10
pes_res <- raster(here("data/Zones/", "Payment for ES - Restore.tif")) > 10
pes_man <- raster(here("data/Zones/", "Payment for ES - Manage.tif")) > 10

#Protect trumps Restore trumps Manage
pes_res[pes_pro] <- NA
pes_man[pes_pro | pes_res] <- NA

Z_PR_ES <- Z_PR | pes_pro
Z_RE_ES <- Z_RE | pes_res
Z_MG_ES <- Z_MG | pes_man
Z_UG_ES <- Z_UG

#PA and ES locked in
Z_PR_PE <- Z_PR | PA | pes_pro
Z_RE_PE <- Z_RE | pes_res
Z_MG_PE <- Z_MG | pes_man
Z_UG_PE <- Z_UG

#Protect trumps Restore trumps Manage
pa_pes_pro <- PA | pes_pro
pa_pes_res <- pes_res
pa_pes_res[pa_pes_pro] <- NA
pa_pes_man <- pes_man
pa_pes_man[pa_pes_pro | pa_pes_res] <- NA


pu1 <- stack(Z_PR, Z_RE, Z_MG, Z_UG)
pu1_pr <- stack(Z_PR_PA, Z_RE_PA, Z_MG_PA, Z_UG_PA)
pu1_es <- stack(Z_PR_ES, Z_RE_ES, Z_MG_ES, Z_UG_ES)
pu1_pe <- stack(Z_PR_PE, Z_RE_PE, Z_MG_PE, Z_UG_PE)

names(pu1) <- names(pu1_pr) <- 
  names(pu1_es) <- names(pu1_pe) <- c("Protect", "Restore", "Manage", "Urban_Green")


#HFP as cost
HFP_pu1 <- pu1 * HFP
HFP_pu1_pr <- pu1_pr * HFP
HFP_pu1_es <- pu1_es * HFP
HFP_pu1_pe <- pu1_pe * HFP

names(HFP_pu1) <- names(HFP_pu1_pr) <- 
  names(HFP_pu1_es) <- names(HFP_pu1_pe) <- c("Protect", "Restore", "Manage", "Urban_Green")


#combine zone/cost layer
pu_all <- list(area = list(avail = pu1,
                           pa = pu1_pr,
                           es = pu1_es,
                           pe = pu1_pe),
               
               human = list(avail = HFP_pu1,
                            pa = HFP_pu1_pr,
                            es = HFP_pu1_es,
                            pe = HFP_pu1_pe)
               )


# cells (km2) per zone
# colSums(!is.na(getValues(pu1)))

# Impacts setup 
zone_1_impacts <- feat_df$Protect
zone_2_impacts <- feat_df$Restore
zone_3_impacts <- feat_df$Manage
zone_4_impacts <- feat_df$`Urban Green`

# wgts <- matrix(1, ncol = 4, nrow = nlayers(feat_stack), dimnames = list(c(names(feat_stack)), c(names(pu1))))
wgts <- data.frame(Name = feat_df$`Label-name`,
               Theme = feat_df$`Label-theme`,
               feature = names(feat_stack),
               weight = rep(as.double(1), nlayers(feat_stack)),
               stringsAsFactors = FALSE)

impacts <- data.frame(Name = feat_df$`Label-name`,
                  Theme = feat_df$`Label-theme`,
                  feature = names(feat_stack),
                  Protect = zone_1_impacts, 
                  Restore = zone_2_impacts, 
                  Manage = zone_3_impacts,
                  Urban_Green = zone_4_impacts,
                  stringsAsFactors = FALSE
                  )


#features
zn1 <- feat_stack * impacts[,"Protect"]
zn2 <- feat_stack * impacts[,"Restore"] 
zn3 <- feat_stack * impacts[,"Manage"] 
zn4 <- feat_stack * impacts[,"Urban_Green"] 

### Create Zone file
zns <- zones("Protect" = zn1, "Restore" = zn2,  "Manage" = zn3, "Urban_Green" = zn4,
             feature_names = names(zn1))

# not used for now
# w1 <- matrix(0, ncol = nlayers(pu1), nrow = nlayers(zn1))                     
# w1[c(2,3,4,5,7),] <- 1
CBD_names <- names(feat_stack)[grep("CBD", feat_df$`Label-theme`)]
CBD <- feat_stack_sc[[CBD_names]]
CBD <- CBD / sapply(as.data.frame(CBD), function(x) max(x, na.rm = TRUE) ) 

UNFCCC_names <- names(feat_stack)[grep("UNFCCC", feat_df$`Label-theme`)]
UNFCCC <- feat_stack_sc[[UNFCCC_names]] 
UNFCCC <- UNFCCC / sapply(as.data.frame(UNFCCC), function(x) max(x, na.rm = TRUE) ) 


SDG_names <- names(feat_stack)[grep("SDG", feat_df$`Label-theme`)]
SDG <- feat_stack_sc[[SDG_names]] 
SDG <- SDG / sapply(as.data.frame(SDG), function(x) max(x, na.rm = TRUE) ) 


save.image(here("pre_global.RData"))

