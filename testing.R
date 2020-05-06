library(prioritizr)
data(sim_pu_raster, sim_features)

sim_pu_const <- sim_pu_raster
sim_pu_const[] <- 1

b1 <- floor(raster::cellStats(sim_pu_raster, "sum")) * 0.25

p1 <- problem(sim_pu_raster, sim_features) %>%
  add_max_utility_objective(budget = b1)

s1 <- solve(p1)
plot(s1)


b2 <- floor(raster::cellStats(sim_pu_const, "sum")) * 0.25

p2 <- problem(sim_pu_const, sim_features) %>%
  add_max_utility_objective(budget = b2)

s2 <- solve(p2)
plot(s2)

feature_representation(p2, s2)


b3 <- floor(raster::cellStats(sim_pu_const, "sum")) * 0.50

p3 <- problem(sim_pu_const, sim_features[[1:2]]) %>%
  add_max_utility_objective(budget = b3)

s3 <- solve(p3)
plot(s3)

feature_representation(p3, s3)
raster::cellStats(s3, "sum")

p4 <- problem(sim_pu_const, sim_features[[1]]) %>%
  add_max_utility_objective(budget = b3)

s4 <- solve(p4)
plot(s4)

feature_representation(p4, s4)
raster::cellStats(s4, "sum")

p5 <- problem(sim_pu_const, sim_features[[2]]) %>%
  add_max_utility_objective(budget = b3)

s5 <- solve(p5)
plot(s5)

feature_representation(p5, s5)
raster::cellStats(s5, "sum")

sc <- sum(s4,s5)
sc_val <- sc[]
sum(ifelse(sc_val >0, 1, 0))


#Zones

data(sim_pu_zones_stack, sim_features_zones)
b6 <- unname(floor(raster::cellStats(sim_pu_zones_stack, "sum")) * 0.25)
p6 <- problem(sim_pu_zones_stack, sim_features_zones) %>%
  add_max_utility_objective(budget = b6)
s6 <- solve(p6)

plot(s6)

feature_representation(p6, s6)
raster::cellStats(s6, "sum")


sum_pu_zones_const <- setValues(sim_pu_zones_stack, 1)

b7 <- unname(floor(raster::cellStats(sum_pu_zones_const, "sum")) * 0.25)

p7 <- problem(sum_pu_zones_const, sim_features_zones) %>%
  add_max_utility_objective(budget = b7)
s7 <- solve(p7)

plot(s7)

s7.1 <- s7[[1]] * 1 + s7[[2]] *2 + s7[[3]] *3
plot(s7.1)

feature_representation(p7, s7)
raster::cellStats(s7, "sum")

p8 <- problem(sum_pu_zones_const, 
              zones("zone_1" = sim_features_zones[[1]][[1:2]], "zone_2" = sim_features_zones[[2]][[1:2]], 
                    "zone_3" = sim_features_zones[[3]][[1:2]],
                    feature_names = names(sim_features_zones[[1]][[1:2]]))) %>%
  add_max_utility_objective(budget = b7)
s8 <- solve(p8)

plot(s8)

s8.1 <- s8[[1]] * 1 + s8[[2]] *2 + s8[[3]] *3
plot(s8.1)

feature_representation(p8, s8)
raster::cellStats(s8, "sum")


w9 <- matrix(0, ncol = nlayers(sum_pu_zones_const), nrow = nlayers(sim_features_zones[[1]]))                     
w9[1,] <- 100

p9 <- problem(sum_pu_zones_const, sim_features_zones) %>%
  add_max_utility_objective(budget = b7) %>%
  add_feature_weights(w9)
s9 <- solve(p9)

plot(s9)

s9.1 <- s9[[1]] * 1 + s9[[2]] *2 + s9[[3]] *3
plot(s9.1)

feature_representation(p9, s9)
raster::cellStats(s9, "sum")

