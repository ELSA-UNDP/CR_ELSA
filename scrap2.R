pes_pro <- raster(here("data/Zones/", "Payment for ES - Protection.tif"))
pes_res <- raster(here("data/Zones/", "Payment for ES - Restore.tif"))
pes_man <- raster(here("data/Zones/", "Payment for ES - Manage.tif"))


rr <- pes_res > 10
mm <- pes_man > 10

xx <- sum(rr, mm * 2)

pp <- pes_pro > 10  | PA

x<- rst.SDG

out <- raster::setValues(x[[1]], 0)
out[raster::Which(is.na(x[[1]]))] <- NA_real_
# populate raster layer
for (i in seq_len(raster::nlayers(x)))
  out[raster::Which(x[[i]] == 1)] <- i
# return result
out


table(pes_pro[])/table(pu0[])

p1 <- prob.all
system.time(r1 <- prioritizr::solve(p1, force = TRUE))
plot(category_layer_light(r1))
     
p2 <- prob.all %>% add_boundary_penalties(1)
system.time(r2 <- prioritizr::solve(p2, force = TRUE))
plot(category_layer_light(r2))

pu_temp <- pu_all[["area"]][["avail"]]


# outl <- outl %>% 
#   addRasterImage(category_layer_light(pu_temp), colors = pal.prior, opacity = 0.9, 
#                  maxBytes = 8 * 1024 * 1024, group = "All_action", project = FALSE) 
# 


pal.prior <- colorNumeric(c('#4daf4a', '#984ea3', '#377eb8', '#e41a1c'), c(1, 2, 3, 4),
                          na.color = "transparent") 

leaflet() %>% addTiles() %>%
  # Base groups
  addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
  addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
  addProviderTiles("Stamen.Terrain", group = "Terrain") %>%

  addRasterImage(category_layer_light(pu_temp), colors = pal.prior, opacity = 0.9, 
                 maxBytes = 8 * 1024 * 1024, group = "Zones", project = FALSE) %>% 
  addLayersControl(baseGroups = c("StreetMap", "Aerial", "Terrain"),
                         overlayGroups = c("Zones"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(colors = c('#4daf4a', '#984ea3', '#377eb8', '#e41a1c'), labels = names(pu_temp),
            title = "Zones")

