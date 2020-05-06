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

ELSA_df <- read_xlsx(here("ELSA UGA_Global Comparison Table.xlsx"), "data_links")


feat_df <- ELSA_df[!is.na(ELSA_df$Protect), ] %>% 
  mutate(feat_name = ifelse(!is.na(UGA_data), UGA_data, Global_data)) %>% 
  arrange(`Label-theme`, `Label-name`)

feat_stack <- stack(here("data/features/", feat_df$feat_name))
feat_stack_sc <- feat_stack / cellStats(feat_stack, max)
                                                         

#Possible cost layer:
HFP <- raster(here("data/zones/", ELSA_df[ELSA_df$`Main Category` == "Cost","Global_data"]))

#Protected areas
PA <- raster(here("data/zones/", ELSA_df[ELSA_df$`Sub Category` == "Protected Areas", "Global_data"][1,]))
PA <- PA > 0.5
PA0 <- PA
PA0[] <- NA
# forest_reserv<-raster("Forest Reserve.tif")
# T_indigena<- raster("Indigenous Territories.tif")


# pu
pu0 <- raster(here("data/", ELSA_df[ELSA_df$`Main Category` == "Planning Units","UGA_data"]))
pu <- pu0
pu[is.na(HFP)] <- NA

HFP_zones <- raster(here("data/zones/", "hfp_uga.tif"))

# HFP < 14 ~ 50% of country
HFP_pro <- HFP < 12
HFP_pro[HFP_pro < 1] <- NA
HFP_pro_PA <- HFP_pro | PA

HFP_res <- HFP >= 12 & HFP <= 20
HFP_res[HFP_res < 1] <- NA
HFP_res_PA <- HFP_res
HFP_res_PA[HFP_pro_PA == 1] <- NA


HFP_mg <- HFP >= 12 & HFP <= 20
HFP_mg[HFP_mg < 1] <- NA
HFP_mg_PA <- HFP_mg
HFP_mg_PA[HFP_pro_PA == 1] <- NA


HFP_BAU<-HFP >= 30
HFP_BAU[HFP_BAU < 1] <- NA

# pu1 <- stack(HFP_pro, HFP_res, HFP_mg, HFP_BAU)
# names(pu1) <- c("Protect", "Restore", "Manage", "BAU")
pu1 <- stack(HFP_pro, HFP_res, HFP_mg)
pu1_pa <- stack(HFP_pro_PA, HFP_res_PA, HFP_mg_PA)

names(pu1) <- names(pu1_pa) <- c("Protect", "Restore", "Manage")

HFP_p <- HFP_r <- HFP_m <- HFP_b <- HFP
HFP_p[] <- ifelse(HFP_pro[], HFP_p[], NA)
HFP_r[] <- ifelse(HFP_res[], HFP_r[], NA)
HFP_m[] <- ifelse(HFP_mg[], HFP_m[], NA)
# HFP_b[] <- ifelse(HFP_BAU[], HFP_b[], NA)

HFP_p_pa <- HFP_r_pa <- HFP_m_pa <- HFP_b_pa <- HFP
HFP_p_pa[] <- ifelse(HFP_pro_PA[], HFP_p_pa[], NA)
HFP_r_pa[] <- ifelse(HFP_res_PA[], HFP_r_pa[], NA)
HFP_m_pa[] <- ifelse(HFP_mg_PA[], HFP_m_pa[], NA)
# HFP_b[] <- ifelse(HFP_BAU[], HFP_b[], NA)

# pu_hf <- stack(HFP_p, HFP_r, HFP_m, HFP_b)
# names(pu_hf) <- c("Protect", "Restore", "Manage", "BAU")
pu_hf <- stack(HFP_p, HFP_r, HFP_m)
pu_hf_pa <- stack(HFP_p_pa, HFP_r_pa, HFP_m_pa)
names(pu_hf) <- names(pu_hf_pa) <- c("Protect", "Restore", "Manage")

pu_all <- list(area = list(locked = pu1_pa,
                           avail = pu1),
               human = list(locked = pu_hf_pa,
                            avail = pu_hf)
               )


# cells (km2) per zone
# colSums(!is.na(getValues(pu1)))

# Impacts setup 
zone_1_impacts <- feat_df$Protect
zone_2_impacts <- feat_df$Restore
zone_3_impacts <- feat_df$Manage
# zone_4_impacts <- c(0, 0, 0, 0, 0, 1, 0, 0, 0)

# wgts <- matrix(1, ncol = 4, nrow = nlayers(feat_stack), dimnames = list(c(names(feat_stack)), c(names(pu1))))
wgts <- data.frame(Name = feat_df$`Label-name`,
                   Theme = feat_df$`Label-theme`,
                   feature = names(feat_stack),
                   weight = rep(as.double(1), nlayers(feat_stack)),
                   # row.names = names(feat_stack),
                   stringsAsFactors = FALSE)

impacts <-data.frame(Name = feat_df$`Label-name`,
                     Theme = feat_df$`Label-theme`,
                     feature = names(feat_stack),
                     Protect = zone_1_impacts, 
                     Restore = zone_2_impacts, 
                     Manage = zone_3_impacts
                     # BAU = zone_4_impacts,
                     # row.names = names(feat_stack))
                     )


#features
zn1 <- feat_stack * impacts[,"Protect"]
zn2 <- feat_stack * impacts[,"Restore"] 
zn3 <- feat_stack * impacts[,"Manage"] 
# zn4 <- feat_stack * impacts[,"BAU"] 

### Create Zone file
zns <- zones("Protect" = zn1, "Restore" = zn2,  "Manage" = zn3, #"BAU" = zn4,
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

