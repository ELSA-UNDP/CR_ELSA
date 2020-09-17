
weights.temp <- wgts
impacts.temp <- impacts

lock_flag <- 'pa'

pu_temp <- pu_all[['area']][[lock_flag]]

prob.ta <- problem(pu_temp, zns) %>%
  # add_min_shortfall_objective(c(count_tar(pu0, input$zone_1_target),
  #                               count_tar(pu0,input$zone_2_target),
  #                               count_tar(pu0,input$zone_3_target),
  #                               count_tar(pu0,input$zone_4_target))) %>%
  
  add_max_utility_objective(c(count_tar(pu0, 27),
                              count_tar(pu0,8),
                              count_tar(pu0,5),
                              count_tar(pu0,3))) %>%
  
  # count_tar(pu0,zone_4_target))) %>%
  
  # add_default_solver(gap = 0)
  add_rsymphony_solver(gap = 0)
 # add_gurobi_solver(gap = 0) 


prob.ta <- prob.ta %>%
  add_locked_in_constraints(stack(PA, PA0, PA0, PA0))


#all groups
prob.all <- prob.ta %>%
  add_feature_weights(as.matrix(matrix(rep(weights.temp$weight, 4), ncol = 4, nrow = nlayers(feat_stack))))

result <- prioritizr::solve(prob.all, force = TRUE)

attr(result, "runtime")


