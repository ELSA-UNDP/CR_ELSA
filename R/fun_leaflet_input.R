fun_leaflet_input <- function(layers = NULL){
  
  pals <- list()
  for(jj in 1:nlayers(layers)){
    pals[[jj]] <-  colorNumeric(c('#2c7bb6','#abd9e9','#ffffbf','#fdae61','#d7191c'), 
                                quantile(layers[[jj]]), na.color = "transparent")
  }
  
  outl <- leaflet() %>% addTiles() %>%
    # Base groups
    addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
    addProviderTiles("Stamen.Terrain", group = "Terrain")# %>%
  
  # Overlay groups
  for(ii in 1:nlayers(layers)){
    outl <- outl %>% addRasterImage(layers[[ii]], colors=pals[[ii]], opacity = 0.9, 
                                    maxBytes = 8 * 1024 * 1024, group = names(layers)[ii], project = FALSE) %>%
      addLegend(pal = pals[[ii]], values = quantile(layers[[ii]]), 
                bins = 5, position = "bottomleft",group = names(layers)[ii], 
                title = names(layers)[ii]) 
  }
  
  
  outl <- outl %>%  addLayersControl(
    baseGroups = c("StreetMap", "Aerial", "Terrain"),
    overlayGroups = names(layers),
    options = layersControlOptions(collapsed = TRUE)
    
  ) %>%
    hideGroup(names(layers)[-1])
  
  outl  
}
