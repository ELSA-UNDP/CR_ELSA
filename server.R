##################################################################
##################################################################
## Shiny App for Costa Rica max_utility using prioritzr 
##
## Author: Richard Schuster (richard.schuster@glel.carleton.ca)
##
##################################################################
##################################################################


# Define server logic 
shinyServer(function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  

  values = reactiveValues(
    hot_wgt = wgts,
    #hot_tree = tree.lst,
    hot_imp = impacts
  )
  #setHot = function(x) values[["hot"]] <<- x
  
  calc = reactive({
    # load initial values
    df1 = values[["hot_wgt"]]
    #df2 = values[["hot_tree"]]
    df3 = values[["hot_imp"]]
    
    
    list(wgts = df1,
     #    tree = df2,
         impacts = df3)
  })
  
  #######################
  ## Edit Weights
  #######################
  output$hot_wgt = renderRHandsontable({
    if (!is.null(input$hot_wgt)) {
      DF = hot_to_r(input$hot_wgt)
      values[["hot_wgt"]] = DF      
    } else if (!is.null(values[["hot_wgt"]])) {
      DF = values[["hot_wgt"]]
    }

    #prevent rhandson from adding rows when user drags values
    if(nrow(DF) > nrow(wgts)){
      DF <- DF[1:nrow(wgts),]
      values[["hot_wgt"]] = DF      
    }
    
    #setHot(DF)
    rhandsontable(DF, readOnly = TRUE) %>% #, rowHeaderWidth = 200) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_col(c("weight"), readOnly = FALSE) %>%
      hot_col(col = 'feature', colWidths=0.1) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) #%>%
      # hot_validate_numeric(col = 1, min = 0, max = 10000) #%>%
      # hot_cols(renderer = "
      #    function (instance, td, row, col, prop, value, cellProperties) {
      #      Handsontable.renderers.TextRenderer.apply(this, arguments);
      #      if (col == 1 && (value > 100 || value < 0)) {
      #       td.style.background = 'red';
      #      }
      #    }")
  })

  #######################
  ## Edit impacts
  #######################
  output$hot_imp = renderRHandsontable({
    if (!is.null(input$hot_imp)) {
      DF = hot_to_r(input$hot_imp)
      values[["hot_imp"]] = DF      
    } else if (!is.null(values[["hot_imp"]])) {
      DF = values[["hot_imp"]]
    }
    
    #prevent rhandson from adding rows when user drags values
    if(nrow(DF) > nrow(impacts)){
      DF <- DF[1:nrow(impacts),]
      values[["hot_imp"]] = DF      
    }
    
    #setHot(DF)
    rhandsontable(DF, readOnly = TRUE) %>%
      hot_col(c("Protect", "Restore", "Manage"), readOnly = FALSE) %>%
      hot_col(col = 'feature', colWidths=0.1) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      # hot_col(c("Percent"), readOnly = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) #%>%
    #        hot_validate_numeric(col = 2, min = 0, max = 1.0) #%>%
    # hot_cols(renderer = "
    #    function (instance, td, row, col, prop, value, cellProperties) {
    #      Handsontable.renderers.TextRenderer.apply(this, arguments);
    #      if (col == 1 && (value > 100 || value < 0)) {
    #       td.style.background = 'red';
    #      }
    #    }")
  })
  
  
  #######################
  ## Gurobi reactive
  #######################
  my.data <- reactive({ 
    # Don't do anything until after the first button push.
    input$mrun
    # Note that just by virtue of checking the value of input$recalcButton,
    # we're now going to get called whenever it is pushed.    
    if(input$mrun == 0)
      return(NULL)    
    
    return(isolate({
      
      weights.temp <- calc()$wgts
      impacts.temp <- calc()$impacts
      
      progress <- Progress$new(session)
      progress$set(message = 'Setting up Analysis inputs', detail = "Please be patient...", value = 0.01)
      
      #features
      zn1 <- feat_stack * impacts.temp[,"Protect"]
      zn2 <- feat_stack * impacts.temp[,"Restore"] 
      zn3 <- feat_stack * impacts.temp[,"Manage"] 
      zn4 <- feat_stack * impacts.temp[,"Urban_Green"] 
      
      ### Create Zone file
      zns <- zones("Protect" = zn1, "Restore" = zn2,  "Manage" = zn3, "Urban_Green" = zn4,
                   feature_names = names(zn1))
      
      # expand to different costs in the future
      if(input$protect == FALSE & input$pes == FALSE){
        lock_flag <- 'avail'
      } 
      
      if(input$protect == TRUE & input$pes == FALSE) {
        lock_flag <- 'pa'
      }
      
      if(input$protect == FALSE & input$pes == TRUE) {
        lock_flag <- 'es'
      }
      
      if(input$protect == TRUE & input$pes == TRUE) {
        lock_flag <- 'pe'
      }

      pu_temp <- pu_all[[input$cost]][[lock_flag]]
        
      prob.ta <- problem(pu_temp, zns) %>%
        # add_min_shortfall_objective(c(count_tar(pu0, input$zone_1_target),
        #                               count_tar(pu0,input$zone_2_target),
        #                               count_tar(pu0,input$zone_3_target),
        #                               count_tar(pu0,input$zone_4_target))) %>%

        add_max_utility_objective(c(count_tar(pu0, input$zone_1_target),
                                    count_tar(pu0,input$zone_2_target),
                                    count_tar(pu0,input$zone_3_target),
                                    count_tar(pu0,input$zone_4_target))) %>%
        
                                    # count_tar(pu0,zone_4_target))) %>%
        
        add_default_solver(gap = 0)
        # add_gurobi_solver(gap = 0) 
      

      if(input$protect == TRUE & input$pes == FALSE) {
        prob.ta <- prob.ta %>%
          add_locked_in_constraints(stack(PA, PA0, PA0, PA0))
      }
      
      if(input$protect == FALSE & input$pes == TRUE) {
        prob.ta <- prob.ta %>%
          add_locked_in_constraints(stack(pes_pro, pes_res, pes_man, PA0))
      }
      
      if(input$protect == TRUE & input$pes == TRUE) {
        prob.ta <- prob.ta %>%
          add_locked_in_constraints(stack(pa_pes_pro, pa_pes_res, pa_pes_man, PA0))
      }
      
      #Boundary penalty factor
      if(input$blm > 0){
        prob.ta <- prob.ta %>%
          add_boundary_penalties(penalty = input$blm)
      }
      
      progress$set(message = 'Calculation in progress', detail = 'running prioritization 1/4', 
                   value = 0.2)
      
      #all groups
      prob.all <- prob.ta %>%
        # add_relative_targets(
        #   as.matrix(matrix(rep(ifelse(weights.temp$weight > 0, 1, 0), 4), 
        #                    ncol = 4, nrow = nlayers(feat_stack)))) %>%
        add_feature_weights(as.matrix(matrix(rep(weights.temp$weight, 4), ncol = 4, nrow = nlayers(feat_stack))))
      
      result <- prioritizr::solve(prob.all, force = TRUE)
      
      feat_rep <- feature_representation(prob.all, result)
      
      tmp <- impacts.temp[, c("feature", "Protect", "Restore", "Manage", "Urban_Green")] %>% 
        pivot_longer(-feature, names_to = "zone", values_to = "impact")
      
      feat_rep <- left_join(feat_rep, tmp, by = c('feature' = 'feature', 'zone' = 'zone'))
      feat_rep$relative_held <- feat_rep$relative_held * feat_rep$impact
      
      progress$set(message = 'Calculation in progress', detail = 'running prioritization 2/4', 
                   value = 0.4)
      
      #CBD
      wt.CBD <- weights.temp
      wt.CBD$weight[names(feat_stack) %notin% CBD_names] <- 0
      prob.CBD <- prob.ta %>%
        # add_relative_targets(
        #   as.matrix(matrix(rep(ifelse(wt.CBD$weight > 0, 1, 0), 4), 
        #                  ncol = 4, nrow = nlayers(feat_stack)))) %>%
        
        add_feature_weights(as.matrix(matrix(rep(wt.CBD$weight, 4), ncol = 4, nrow = nlayers(feat_stack))))
      res.CBD <- prioritizr::solve(prob.CBD, force = TRUE)
      
      feat_rep_CBD <- feature_representation(prob.CBD, res.CBD)
      feat_rep_CBD$relative_held <- feat_rep_CBD$relative_held * feat_rep$impact
      
      progress$set(message = 'Calculation in progress', detail = 'running prioritization 3/4', 
                   value = 0.6)
      
      #UNFCCC
      wt.UNFCCC <- weights.temp
      wt.UNFCCC$weight[names(feat_stack) %notin% UNFCCC_names] <- 0
      prob.UNFCCC <- prob.ta %>%
        # add_relative_targets(
        #   as.matrix(matrix(rep(ifelse(wt.UNFCCC$weight > 0, 1, 0), 4), 
        #                    ncol = 4, nrow = nlayers(feat_stack)))) %>%
        
        add_feature_weights(as.matrix(matrix(rep(wt.UNFCCC$weight, 4), ncol = 4, nrow = nlayers(feat_stack))))
      res.UNFCCC <- prioritizr::solve(prob.UNFCCC, force = TRUE)
      
      feat_rep_UNFCCC <- feature_representation(prob.UNFCCC, res.UNFCCC)
      feat_rep_UNFCCC$relative_held <- feat_rep_UNFCCC$relative_held * feat_rep$impact
      
      progress$set(message = 'Calculation in progress', detail = 'running prioritization 4/4', 
                   value = 0.8)
      
      #SDG
      wt.SDG <- weights.temp
      wt.SDG$weight[names(feat_stack) %notin% SDG_names] <- 0
      prob.SDG <- prob.ta %>%
        # add_relative_targets(
        #   as.matrix(matrix(rep(ifelse(wt.SDG$weight > 0, 1, 0), 4), 
        #                    ncol = 4, nrow = nlayers(feat_stack)))) %>%
        
        add_feature_weights(as.matrix(matrix(rep(wt.SDG$weight, 4), ncol = 4, nrow = nlayers(feat_stack))))
      res.SDG <- prioritizr::solve(prob.SDG, force = TRUE)
      
      feat_rep_SDG <- feature_representation(prob.SDG, res.SDG)
      feat_rep_SDG$relative_held <- feat_rep_SDG$relative_held * feat_rep$impact
      
      # write.csv(feat_rep, here::here("test.csv"))
      ################################################
      ##create rasters
      ################################################
      progress$set(message = 'Post processing', detail = "This will take a few seconds...", value = 0.9)
      
      # my.data()$res.fr
      rh_rep <- feat_rep %>% group_by(feature) %>% summarise(All_action = round(sum(relative_held, na.rm = T) * 100, 0))
      rh_CBD <- feat_rep_CBD %>% group_by(feature) %>% summarise(CDB_action = round(sum(relative_held, na.rm = T)* 100, 0))
      rh_UNFCCC <- feat_rep_UNFCCC %>% group_by(feature) %>% summarise(UNFCCC_action = round(sum(relative_held, na.rm = T)* 100, 0))
      rh_SDG <- feat_rep_SDG %>% group_by(feature) %>% summarise(SDG_action = round(sum(relative_held, na.rm = T)* 100, 0))
      
      feat_rep_tabl <- feat_rep[feat_rep$zone == "Protect", c("feature")] %>% #mutate(zone = "all") %>%
        left_join(rh_rep, by = 'feature') %>%
        left_join(rh_CBD, by = 'feature') %>%
        left_join(rh_UNFCCC, by = 'feature') %>%
        left_join(rh_SDG, by = 'feature') %>%
        add_column(Name = feat_df$`Label-name`,
                   Theme = feat_df$`Label-theme`, .before = 1) %>%
        dplyr::select (-c(feature))
      
      # feat_rep_tabl$feature <- feat_df$`Label-name`
      # names(feat_rep_tabl)[1] <- "Feature"

      rlist <- list(sel.fr = feat_rep, res.fr = feat_rep, rst = result,
                    res.CBD = res.CBD, res.UNFCCC = res.UNFCCC, res.SDG = res.SDG,
                    feat_rep = feat_rep, feat_rep_CBD = feat_rep_CBD, 
                    feat_rep_UNFCCC = feat_rep_UNFCCC, feat_rep_SDG = feat_rep_SDG,
                    feat_rep_tabl = feat_rep_tabl)
      
      progress$set(value = 1)
      progress$close() 
      
      return(rlist)
    }))

  })
  
  observe ({  my.data()
  })
  
  output$InMap <- renderLeaflet({
    leaflet_input
  }) 
  
  output$cadMap <- renderLeaflet({
    if(input$mrun == 0) {
      #print("Run prioritzr")
      
      progress <- Progress$new(session)
      progress$set(message = 'Generating map.', detail = "Please be patient...", value = 0.5)
      
      weights.temp <- calc()$wgts
      CBD_temp <- sum(CBD * weights.temp$weight[weights.temp$feature %in% CBD_names], na.rm = TRUE)
      CBD_temp <- CBD_temp / max(CBD_temp[], na.rm = TRUE)  * pu
      
      UNFCCC_temp <- sum(UNFCCC * weights.temp$weight[weights.temp$feature %in% UNFCCC_names], na.rm = TRUE)
      UNFCCC_temp <- UNFCCC_temp / max(UNFCCC_temp[], na.rm = TRUE)  * pu
      
      SDG_temp <- sum(SDG * weights.temp$weight[weights.temp$feature %in% SDG_names], na.rm = TRUE)
      SDG_temp <- SDG_temp / max(SDG_temp[], na.rm = TRUE)  * pu
      
      feat_temp <- sum(feat_stack_sc * weights.temp$weight, na.rm = TRUE)
      feat_temp <- feat_temp / max(feat_temp[], na.rm = TRUE)  * pu
      

      # cols <- c('#4daf4a', '#984ea3', '#377eb8', '#e41a1c')
      cols <- c('#4daf4a', '#984ea3', '#377eb8')
      
      pal.prior <- colorNumeric(c('#4daf4a', '#984ea3', '#377eb8'), c(1, 2, 3),
                                na.color = "transparent") 
      
      pal.cbd <-  colorNumeric(c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'), c(0, 0.2, 0.4, 0.6, 0.8, 1),
                               na.color = "transparent")
      pal.UNFCCC <-  colorNumeric(c('#2c7bb6','#abd9e9','#ffffbf','#fdae61','#d7191c'), c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                  na.color = "transparent")
      
      pal.sdg <-  colorNumeric(c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'), c(0, 0.2, 0.4, 0.6, 0.8, 1),
                               na.color = "transparent")
      
      pal.feat <- colorNumeric(c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'), c(0, 0.2, 0.4, 0.6, 0.8, 1),
                               na.color = "transparent")
      
      outl <- leaflet() %>% addTiles() %>%
        # Base groups
        addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
        addProviderTiles("Stamen.Terrain", group = "Terrain")# %>%
      
      #heat maps
      outl <- outl %>%
        addRasterImage(feat_temp, colors = pal.UNFCCC, opacity = 0.8, group = "All_HM",project = FALSE) %>%
        addRasterImage(CBD_temp, colors = pal.UNFCCC, opacity = 0.8, group = "CBD_HM",project = FALSE) %>%
        addRasterImage(UNFCCC_temp, colors = pal.UNFCCC, opacity = 0.8, group = "UNFCCC_HM",project = FALSE) %>%
        addRasterImage(SDG_temp, colors = pal.UNFCCC, opacity = 0.8, group = "SDG_HM",project = FALSE)
      
      outl <- addLayersControl(outl, 
                               baseGroups = c("StreetMap", "Aerial", "Terrain"),
                               overlayGroups = c("All_HM", "CBD_HM", "UNFCCC_HM", "SDG_HM"),
                               options = layersControlOptions(collapsed = FALSE)
      ) %>%
        addLegend(pal = pal.UNFCCC, values = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                  bins = c(0, 0.2, 0.4, 0.6, 0.8, 1), position = "topleft",group = "CBD_HM",title = "CBD_HM") %>%
        addLegend(pal = pal.UNFCCC, values = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                  bins = c(0, 0.2, 0.4, 0.6, 0.8, 1), position = "topleft",group = "UNFCCC_HM",title = "UNFCCC_HM") %>%
        addLegend(pal = pal.UNFCCC, values = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                  bins = c(0, 0.2, 0.4, 0.6, 0.8, 1), position = "topleft",group = "SDG_HM",title = "SDG_HM") %>%
        addLegend(pal = pal.UNFCCC, values = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                  bins = c(0, 0.2, 0.4, 0.6, 0.8, 1), position = "topleft",group = "All_HM",title = "All_HM") %>%
        
        
        # addTiles(attribution = sprintf('<h5>Map created on %s via <a href="https://forbasin.forestry.ubc.ca/CR_Shiny/" target="_blank">Costa Rica conservation prioritization tool</a> developed by <a href="mailto:richard.schuster@glel.carleton.ca">Richard Schuster</a>.</h5>',Sys.Date())) %>%
        hideGroup(c("CBD_HM","UNFCCC_HM","SDG_HM"))
      
      
      progress$set(value = 1)
      progress$close() 
      
      outl  
      
    } else {
      
      progress <- Progress$new(session)
      progress$set(message = 'Generating map.', detail = "Please be patient...", value = 0.5)
      
      weights.temp <- calc()$wgts
      CBD_temp <- sum(CBD * weights.temp$weight[weights.temp$feature %in% CBD_names], na.rm = TRUE)
      CBD_temp <- CBD_temp / max(CBD_temp[], na.rm = TRUE)  * pu
      
      UNFCCC_temp <- sum(UNFCCC * weights.temp$weight[weights.temp$feature %in% UNFCCC_names], na.rm = TRUE)
      UNFCCC_temp <- UNFCCC_temp / max(UNFCCC_temp[], na.rm = TRUE)  * pu
      
      SDG_temp <- sum(SDG * weights.temp$weight[weights.temp$feature %in% SDG_names], na.rm = TRUE)
      SDG_temp <- SDG_temp / max(SDG_temp[], na.rm = TRUE)  * pu
      
      feat_temp <- sum(feat_stack_sc * weights.temp$weight, na.rm = TRUE)
      feat_temp <- feat_temp / max(feat_temp[], na.rm = TRUE)  * pu
      
      #[[1]] for now, should allow for multiple eventually
      rst <- my.data()$rst
      rst.CBD <- my.data()$res.CBD
      rst.UNFCCC <- my.data()$res.UNFCCC
      rst.SDG <- my.data()$res.SDG
      
      names(rst) <- c("Protect",
                      "Restore",
                      "Manage",
                      "Urban_Green")#,
      # "BAU")
      
      rst[rst == 0] <- NA
      rst.CBD[rst.CBD == 0] <- NA
      rst.UNFCCC[rst.UNFCCC == 0] <- NA
      rst.SDG[rst.SDG == 0] <- NA
      
      cols <- c('#4daf4a', '#984ea3', '#377eb8', '#e41a1c')
      # cols <- c('#4daf4a', '#984ea3', '#377eb8')
      
      pal.prior <- colorNumeric(c('#4daf4a', '#984ea3', '#377eb8', '#e41a1c'), c(1, 2, 3, 4),
                                na.color = "transparent") 
      
      pal.cbd <-  colorNumeric(c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'), c(0, 0.2, 0.4, 0.6, 0.8, 1),
                               na.color = "transparent")
      pal.UNFCCC <-  colorNumeric(c('#2c7bb6','#abd9e9','#ffffbf','#fdae61','#d7191c'), c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                  na.color = "transparent")
      
      pal.sdg <-  colorNumeric(c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'), c(0, 0.2, 0.4, 0.6, 0.8, 1),
                               na.color = "transparent")
      
      pal.feat <- colorNumeric(c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'), c(0, 0.2, 0.4, 0.6, 0.8, 1),
                               na.color = "transparent")
      
      outl <- leaflet() %>% addTiles() %>%
        # Base groups
        addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
        addProviderTiles("Stamen.Terrain", group = "Terrain")# %>%
      
      #prioritization results
      outl <- outl %>% 
        addRasterImage(category_layer_light(rst), colors = pal.prior, opacity = 0.9, 
                       maxBytes = 8 * 1024 * 1024, group = "All_action", project = FALSE) %>% 
        addRasterImage(category_layer_light(rst.CBD), colors = pal.prior, opacity = 0.9, 
                       maxBytes = 8 * 1024 * 1024, group = "CBD_action", project = FALSE) %>% 
        addRasterImage(category_layer_light(rst.UNFCCC), colors = pal.prior, opacity = 0.9, 
                       maxBytes = 8 * 1024 * 1024, group = "UNFCCC_action", project = FALSE) %>% 
        addRasterImage(category_layer_light(rst.SDG), colors = pal.prior, opacity = 0.9, 
                       maxBytes = 8 * 1024 * 1024, group = "SDG_action", project = FALSE)
      
      
      #heat maps
      outl <- outl %>%
        addRasterImage(feat_temp, colors = pal.UNFCCC, opacity = 0.8, group = "All_HM",project = FALSE) %>%
        addRasterImage(CBD_temp, colors = pal.UNFCCC, opacity = 0.8, group = "CBD_HM",project = FALSE) %>%
        addRasterImage(UNFCCC_temp, colors = pal.UNFCCC, opacity = 0.8, group = "UNFCCC_HM",project = FALSE) %>%
        addRasterImage(SDG_temp, colors = pal.UNFCCC, opacity = 0.8, group = "SDG_HM",project = FALSE)
      
      outl <- addLayersControl(outl, 
                               baseGroups = c("StreetMap", "Aerial", "Terrain"),
                               overlayGroups = c("All_action", "CBD_action", "UNFCCC_action", "SDG_action",  
                                                 "All_HM", "CBD_HM", "UNFCCC_HM", "SDG_HM"),
                               options = layersControlOptions(collapsed = FALSE)
      ) %>%
        addLegend(colors = cols, labels = names(rst),
                  title = "Legend") %>%
        addLegend(pal = pal.UNFCCC, values = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                  bins = c(0, 0.2, 0.4, 0.6, 0.8, 1), position = "topleft",group = "CBD_HM",title = "CBD_HM") %>%
        addLegend(pal = pal.UNFCCC, values = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                  bins = c(0, 0.2, 0.4, 0.6, 0.8, 1), position = "topleft",group = "UNFCCC_HM",title = "UNFCCC_HM") %>%
        addLegend(pal = pal.UNFCCC, values = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                  bins = c(0, 0.2, 0.4, 0.6, 0.8, 1), position = "topleft",group = "SDG_HM",title = "SDG_HM") %>%
        addLegend(pal = pal.UNFCCC, values = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                  bins = c(0, 0.2, 0.4, 0.6, 0.8, 1), position = "topleft",group = "All_HM",title = "All_HM") %>%
        
        
        # addTiles(attribution = sprintf('<h5>Map created on %s via <a href="https://forbasin.forestry.ubc.ca/CR_Shiny/" target="_blank">Costa Rica conservation prioritization tool</a> developed by <a href="mailto:richard.schuster@glel.carleton.ca">Richard Schuster</a>.</h5>',Sys.Date())) %>%
        hideGroup(c("CBD_action", "UNFCCC_action", "SDG_action", "All_HM", "CBD_HM","UNFCCC_HM","SDG_HM"))

      progress$set(value = 1)
      progress$close() 
      
      outl 
    }

 
    
    
    # end individual run attribute table
    ########################################
    #prog$close()     
  })  
  

  ###############################
  # Summary Table + Download Results raster
  ###############################
  output$summary <- DT::renderDataTable(my.data()$feat_rep_tabl,
                                    options = list(dom = 'tipr',
                                                   autoWidth = TRUE,
                                                   pageLength = nrow(my.data()$feat_rep_tabl))
                                    )

  output$downloadSHP <- downloadHandler(

    filename = function() {
      paste('CR_layers', 'zip', sep='.')
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      progress <- Progress$new(session)
      progress$set(message = 'Preparing output files', detail = "Please be patient...", value = 0.5)
      
      
      weights.temp <- calc()$wgts
      CBD_temp <- sum(CBD * weights.temp$weight[row.names(weights.temp) %in% CBD_names], na.rm = TRUE)
      CBD_temp <- CBD_temp / max(CBD_temp[], na.rm = TRUE)  * pu
      
      UNFCCC_temp <- sum(UNFCCC * weights.temp$weight[row.names(weights.temp) %in% UNFCCC_names], na.rm = TRUE)
      UNFCCC_temp <- UNFCCC_temp / max(UNFCCC_temp[], na.rm = TRUE)  * pu
      
      SDG_temp <- sum(SDG * weights.temp$weight[row.names(weights.temp) %in% SDG_names], na.rm = TRUE)
      SDG_temp <- SDG_temp / max(SDG_temp[], na.rm = TRUE)  * pu
      
      feat_temp <- sum(feat_stack_sc * weights.temp$weight, na.rm = TRUE)
      feat_temp <- feat_temp / max(feat_temp[], na.rm = TRUE)  * pu
      
      #[[1]] for now, should allow for multiple eventually
      rst <- my.data()$rst
      rst.CBD <- my.data()$res.CBD
      rst.UNFCCC <- my.data()$res.UNFCCC
      rst.SDG <- my.data()$res.SDG
      
      names(rst) <- names(rst.CBD) <-  names(rst.UNFCCC) <- names(rst.SDG) <- c("Protect",
                                                                                "Restore",
                                                                                "Manage",
                                                                                "Urban_Green")
      
      rst[rst == 0] <- NA
      rst.CBD[rst.CBD == 0] <- NA
      rst.UNFCCC[rst.UNFCCC == 0] <- NA
      rst.SDG[rst.SDG == 0] <- NA
      
      #prioritizr outputs
      writeRaster(rst, "All_action.tif", overwrite = TRUE)
      writeRaster(rst.CBD, "CBD_action.tif", overwrite = TRUE)
      writeRaster(rst.UNFCCC, "UNFCC_action.tif", overwrite = TRUE)
      writeRaster(rst.SDG, "SDG_action.tif", overwrite = TRUE)
      
      #HM's
      writeRaster(feat_temp, "All_HM.tif", overwrite = TRUE)
      writeRaster(CBD_temp, "CBD_HM.tif", overwrite = TRUE)
      writeRaster(UNFCCC_temp, "UNFCC_HM.tif", overwrite = TRUE)
      writeRaster(SDG_temp, "SDG_HM.tif", overwrite = TRUE)
      
      files <- list.files(pattern = ".tif$")

      progress$set(value = 1)
      progress$close() 
      
      #create the zip file
      zip::zip(file, files)
    }
  )

  output$download_ssoln <- downloadHandler(

    filename = function() {
      paste('CR_summary_results-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(my.data()$feat_rep_tabl, file, row.names=F)
    }
  )

  output$download_selfr <- downloadHandler(

    filename = function() {
      paste('CR_property_selection-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(my.data()$sel.fr, file, row.names=F)
    }
  )
  


})


