impacts.temp <- impacts
weights.temp <- wgts
weights.temp$weight <- 1


input <- list()
input$zone_1_target <- 35
input$zone_2_target <- 10
input$zone_3_target <- 20

zn1 <- feat_stack * impacts.temp[,"Protect"]
zn2 <- feat_stack * impacts.temp[,"Restore"] 
zn3 <- feat_stack * impacts.temp[,"Manage"] 
# zn4 <- feat_stack * impacts.temp[,"BAU"] 

### Create Zone file
zns <- zones("Protect" = zn1, "Restore" = zn2,  "Manage" = zn3, #"BAU" = zn4,
             feature_names = names(zn1))

# expand to different costs in the future
pu_temp <- pu_all[["area"]][["locked"]]


prob.ta <- problem(pu_temp, zns) %>%
  add_max_utility_objective(c(count_tar(pu0, input$zone_1_target), 
                              count_tar(pu0,input$zone_2_target), 
                              count_tar(pu0,input$zone_3_target))) %>%
  
  # count_tar(pu0,zone_4_target))) %>%
  add_gurobi_solver(gap = 0) 


  prob.ta <- prob.ta %>%
    add_locked_in_constraints(stack(PA, PA0, PA0))

  
# progress$set(message = 'Calculation in progress', detail = 'running prioritization', 
#              value = 0.5)

#all groups
prob.all <- prob.ta %>%
  add_feature_weights(as.matrix(matrix(rep(weights.temp$weight, 3), ncol = 3, nrow = nlayers(feat_stack))))
result <- prioritizr::solve(prob.all, force = TRUE)

feat_rep <- feature_representation(prob.all, result)

tmp <- impacts.temp[, c("feature", "Protect", "Restore", "Manage")] %>% 
  pivot_longer(-feature, names_to = "zone", values_to = "impact")

feat_rep <- left_join(feat_rep, tmp, by = c('feature' = 'feature', 'zone' = 'zone'))
feat_rep$relative_held <- feat_rep$relative_held * feat_rep$impact


#CBD
wt.CBD <- weights.temp
wt.CBD$weight[names(feat_stack) %notin% CBD_names] <- 0
prob.CBD <- prob.ta %>%
  add_feature_weights(as.matrix(matrix(rep(wt.CBD$weight, 3), ncol = 3, nrow = nlayers(feat_stack))))
res.CBD <- prioritizr::solve(prob.CBD, force = TRUE)

feat_rep_CBD <- feature_representation(prob.CBD, res.CBD)

#UNFCCC
wt.UNFCCC <- weights.temp
wt.UNFCCC$weight[names(feat_stack) %notin% UNFCCC_names] <- 0
prob.UNFCCC <- prob.ta %>%
  add_feature_weights(as.matrix(matrix(rep(wt.UNFCCC$weight, 3), ncol = 3, nrow = nlayers(feat_stack))))
res.UNFCCC <- prioritizr::solve(prob.UNFCCC, force = TRUE)

feat_rep_UNFCCC <- feature_representation(prob.UNFCCC, res.UNFCCC)

#SDG
wt.SDG <- weights.temp
wt.SDG$weight[names(feat_stack) %notin% SDG_names] <- 0
prob.SDG <- prob.ta %>%
  add_feature_weights(as.matrix(matrix(rep(wt.SDG$weight, 3), ncol = 3, nrow = nlayers(feat_stack))))
res.SDG <- prioritizr::solve(prob.SDG, force = TRUE)

feat_rep_SDG <- feature_representation(prob.SDG, res.SDG)


##TEST##
#######

rh_rep <- feat_rep %>% group_by(feature) %>% summarise(all_rh = sum(relative_held, na.rm = T))
rh_CBD <- feat_rep_CBD %>% group_by(feature) %>% summarise(CDB_rh = sum(relative_held, na.rm = T))
rh_UNFCCC <- feat_rep_UNFCCC %>% group_by(feature) %>% summarise(UNFCCC_rh = sum(relative_held, na.rm = T))
rh_SDG <- feat_rep_SDG %>% group_by(feature) %>% summarise(SDG_rh = sum(relative_held, na.rm = T))

feat_all <- feat_rep[feat_rep$zone == "Protect", c("feature")] %>% mutate(zone = "all") %>%
  left_join(rh_rep, by = 'feature') %>%
  left_join(rh_CBD, by = 'feature') %>%
  left_join(rh_UNFCCC, by = 'feature') %>%
  left_join(rh_SDG, by = 'feature')

feat_rep[feat_rep$zone == "Protect", c("feature")] %>% mutate(zone = "all") 
  
# zone_4_target <- 100 - input$zone_1_target - input$zone_2_target - input$zone_3_target

prob.ta <- problem(pu1, zns) %>%
  add_max_utility_objective(c(count_tar(pu0, input$zone_1_target), 
                              count_tar(pu0,input$zone_2_target), 
                              count_tar(pu0,input$zone_3_target))) %>%
  
  # count_tar(pu0,zone_4_target))) %>%
  add_gurobi_solver(gap = 0) %>%
  add_feature_weights(as.matrix(matrix(rep(weights.temp$weight, 3), ncol = 3, nrow = nlayers(feat_stack))))


result <- prioritizr::solve(prob.ta, force = TRUE)

rs1 <- result[[1]] * 1 + result[[2]] * 2 + result[[3]] * 3


input <- list()
input$zone_1_target <- 35
input$zone_2_target <- 10
input$zone_3_target <- 20
zone_4_target <- 100 - input$zone_1_target - input$zone_2_target - input$zone_3_target


p3_glob <- problem(pu1, zns) %>%
  add_max_utility_objective(c(count_tar(pu0, input$zone_1_target), 
                              count_tar(pu0,input$zone_2_target), 
                              count_tar(pu0,input$zone_3_target), 
                              count_tar(pu0,zone_4_target))) %>%
  # add_locked_in_constraints(stack(PA,PA,PA,PA)) %>%
  add_gurobi_solver(gap = 0)#%>%
#add_feature_weights(w1)

if(input$protected == "locked"){
  p3_glob <- p3_glob %>%
    add_locked_in_constraints(stack(PA, PA0, PA0, PA0))
}

s3_glob <- solve(p3_glob, force=TRUE)

rst <- s3_glob
names(rst) <- c("Protect",
                "Restore",
                "Manage",
                "BAU")

rst[rst == 0] <- NA
cols <- c('#4daf4a', '#984ea3', '#377eb8', '#e41a1c')

outl <- leaflet() %>% addTiles() %>%
  # Base groups
  addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
  addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
  addProviderTiles("Stamen.Terrain", group = "Terrain")# %>%



# Overlay groups
for(ii in 1:nlayers(rst))
  outl <- addRasterImage(outl, rst[[ii]], colors = cols[ii], opacity = 0.9, 
                         maxBytes = 8 * 1024 * 1024, group = names(rst)[ii], project = FALSE)

outl <- addLayersControl(outl, 
                         baseGroups = c("StreetMap", "Aerial", "Terrain"),
                         overlayGroups = names(rst),
                         options = layersControlOptions(collapsed = FALSE)
) %>%
  addLegend(colors = cols, labels = names(rst),
            title = "Legend") %>%
  addTiles(attribution = sprintf('<h5>Map created on %s via <a href="http://forbasin.forestry.ubc.ca/CDFCP_prioritization/" target="_blank">CDFCP conservation prioritization tool</a> developed by <a href="mailto:mail@richard-schuster.com">Richard Schuster</a> for the <a href="http://arcese.forestry.ubc.ca/marxan-tool-cdfcp/" target="_blank">Aecese Lab</a>.</h5>',Sys.Date())) 

outl  


setMinMax(s3_glob)
plot(category_layer(s3_glob), main="solution")

fr <- feature_representation(p3_glob, s3_glob)




tt <- impacts.temp %>% mutate(feature = row.names(impacts.temp)) %>% pivot_longer(-feature, names_to = "zone", values_to = "impact")

left_join(feat_rep, tt, by = c('feature' = 'feature', 'zone' = 'zone'))


tt <- data.frame(name= names(feat_stack_sc), 
           min = round(cellStats(feat_stack_sc, min),2), 
           max  = round(cellStats(feat_stack_sc, max),2))
