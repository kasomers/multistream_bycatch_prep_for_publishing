library(data.table) # using this for sampling
library(tidyverse)
library(bycatch)

options(mc.cores = parallel::detectCores()) # run in parallel

# read in synthesized versions of haul-level data sets for CS EM Bottom Trawl 2015-2023
  # note the original datasets used for this research are confidential, so data were obscured using R library synthpop

  # Observer data loads as a dataframe named ob_haul
  load("ob_haul.RDA")
  
  # EM data loads as a data.table named em_haul
  load("em_haul.RDA")
  
# read in summarized data set for CS EM Bottom Trawl 2015-2024
  # note these data are not confidential, so are equivalent to those used in this research
  # full dataset: loads as dat_summ
    load("dat_summ.RDA")
  
  # only annual fleetwide retained: loads as fleet_summ_ret
    load("fleet_summ_ret.RDA")
    
    
  
# Case Study 1: Simulation of Partial EM Video Review
  
  # Setting up simulation
  
    # Set number of runs
      runs = 500
      
    # Set seed to make simulations reproducible
      set.seed(3210)
      
    # Create a list of species in data:
      all_cn = c("California Sea Lion", "Northern Elephant Seal")

    # Initialize lists for output
      simulated_data_input  <- list() # formerly bayes_in_full
      simulated_data_output <- list() # formerly exp_stats_full
      zero_runs      <- integer(0)

  # Re-sample data and fit models    
  for (i in 1:runs) {
    
    # Subsample EM data: 
      # Randomly select 10% of hauls per trip, with a minimum of 1 haul per trip
      em_haul_subsample <- data.frame(em_haul[, .SD[sample(.N, max(1, round(0.1 * .N)))], by = uni_trip_id])
      
    # Identify & summarize data from re-sampled hauls:
      
      # (1) Reviewed EM video with no observer on board
        # remove hauls that were also observed
          haul_dat_em <- em_haul_subsample[which(!(em_haul_subsample$uni_haul_id %in% unique(ob_haul$uni_haul_id))), ]
          
      # (2) Unreviewed EM video with an observer on board
        # remove hauls that had video reviewed
          haul_dat_obs <- ob_haul[which(!(ob_haul$uni_haul_id %in% unique(em_haul_subsample$uni_haul_id))), ]
            
      # (3) Reviewed EM video with an observer on board
        # select only hauls with reviewed video from observer dataset
          haul_dat_obs_em <- ob_haul[which(ob_haul$uni_haul_id %in% unique(em_haul_subsample$uni_haul_id)), ]
        
        # re-label those hauls to indicate they were both observed and had EM video reviewed
          haul_dat_obs_em$stream[haul_dat_obs_em$uni_haul_id %in% unique(em_haul_subsample$uni_haul_id)] <- "OBS_EM"
    
      # combine data from all 3 streams together:
        haul_dat = rbind(haul_dat_em, haul_dat_obs, haul_dat_obs_em)
              
      # summarize total retained catch by year and data stream
        sampled_summ_ret <- haul_dat %>% 
          group_by(year, stream) %>%
          summarise(monitored_retained_weight = sum(monitored_retained_weight, na.rm = TRUE), .groups = "drop_last") %>%
          data.frame()
          
      # Merge in fleetwide retained catch and calculate coverage rate for resampled data
        ret_summ <- merge(sampled_summ_ret, fleet_summ_ret, all = TRUE)
        ret_summ$CovRate <- (ret_summ$monitored_retained_weight / ret_summ$fleet_retained_weight) * 100
        ret_summ$run <- i
      
      # Summarize recorded takes in resampled data
        sampled_takes_summ <- haul_dat %>% 
          group_by(year, common_name, stream) %>%
          summarise(takes = sum(takes, na.rm = TRUE), .groups = "drop_last") %>%
          data.frame()
        
      # Check if NO species were recorded in this run's resampled data
      if (nrow(sampled_takes_summ) == 0 || all(is.na(sampled_takes_summ$common_name))) {
        zero_runs <- c(zero_runs, i)
      }
    
    # Loop over recorded species
        
      # initiate empty lists to hold input and output data for this run
        simulated_data_input_run  <- list()
        simulated_data_output_run <- list()
    
    for (cn_temp in all_cn) {
      
      # Check if this species was recorded in this run's resampled data
        has_takes <- cn_temp %in% sampled_takes_summ$common_name
      
      # if species not present, create input & output datasets with zero takes across all years
      if (!has_takes) {
        
        # 1) Build zero-take input frame
        simulated_data_input_init <- ret_summ
        simulated_data_input_init$common_name <- cn_temp
        simulated_data_input_init$takes   <- 0
        
        # 2) Build zero-estimate output frame
        simulated_data_output_init <- data.frame(
          common_name    = cn_temp,
          year       = min(ret_summ$year):max(ret_summ$year),
          est_mean   = 0,
          est_sd     = NA,
          est_median = NA,
          lower95    = NA,
          upper95    = NA,
          run        = i
        ) %>% data.frame()          
      } else {
      
      # if species present, fit bycatch model
        takes_summ_temp <- sampled_takes_summ[which(sampled_takes_summ$common_name == cn_temp), ]
        
        simulated_data_input_init <- merge(ret_summ, takes_summ_temp, all = TRUE)
        simulated_data_input_init$common_name <- cn_temp
        simulated_data_input_init$takes[is.na(simulated_data_input_init$takes)] <- 0
        
        # Reformat for multi-stream model
        d <- simulated_data_input_init %>%
          dplyr::mutate(takes = round(takes)) %>%
          pivot_longer(
            cols      = c("monitored_retained_weight", "takes", "CovRate"), 
            names_to  = "MetricName",
            values_to = "MetricVal"
          )
        
        d2_wide <- d %>%
          pivot_wider(
            names_from = c(MetricName, stream),
            names_sep  = "_",
            values_from = c(MetricVal)
          ) %>%
          data.frame()
        
        d2_wide[is.na(d2_wide)] <- 0
        
        # Fit model
        fit <- fit_bycatch(
          takes_OBS ~ 1,
          data        = d2_wide,
          time        = "year",
          effort      = "monitored_retained_weight_OBS",
          covrate_obs = "CovRate_OBS",
          takes_em    = "takes_EM",
          effort_em   = "monitored_retained_weight_EM",
          covrate_em  = "CovRate_EM",
          takes_both  = "takes_OBS_EM",
          effort_both = "monitored_retained_weight_OBS_EM",
          covrate_both= "CovRate_OBS_EM",
          family      = "poisson",
          time_varying= FALSE,
          iter        = 4000,
          control     = list(adapt_delta = 0.99, max_treedepth = 20)
        )
        
        # Save model fit
        model_name <- paste0("model_", cn_temp, "_run_", i)
        assign(x = as.character(model_name), fit)
        save(list = model_name, file = paste0(path_out, model_name, ".RDA"))
        
        # Calculate expansions
        extract_expansions <- bycatch::get_total(fit)
        
        simulated_data_output_init <- data.frame(
          common_name    = cn_temp,
          year       = min(ret_summ$year):max(ret_summ$year),
          est_mean   = apply(extract_expansions, 2, mean),
          est_sd     = apply(extract_expansions, 2, sd),
          est_median = apply(extract_expansions, 2, quantile, 0.500),
          lower95    = apply(extract_expansions, 2, quantile, 0.025),
          upper95    = apply(extract_expansions, 2, quantile, 0.975),
          run        = i
        ) %>% data.frame()
      }
      
      # Store results for this species
      simulated_data_input_run[[cn_temp]]  <- simulated_data_input_init
      simulated_data_output_run[[cn_temp]] <- simulated_data_output_init
    }
    
    # Combine species outputs for run 'i'
      simulated_data_input[[i]]  <- bind_rows(simulated_data_input_run)
      simulated_data_output[[i]] <- bind_rows(simulated_data_output_run)
      
  } 
      
# considate re-sampled datasets
  simulated_data_input  <- bind_rows(simulated_data_input)
  simulated_data_output <- bind_rows(simulated_data_output) 

  # output those datasets
    write.csv(simulated_data_input, paste0(path_out, "case_study_1_input_runs=", runs, ".csv"), 
          row.names=F)

    write.csv(simulated_data_output, paste0(path_out, "case_study_1_output_runs=", runs, ".csv"),
          row.names=F)


# Figure 1: Visualization for Case Study 1

  # Calculate known takes from census-monitored data for comparison
    census_takes = dat_summ %>% 
      filter(year < 2024, common_name %in% all_cn) %>%
      group_by(common_name, year) %>%
      summarise(takes = sum(takes), .groups = "drop_last") %>%
      data.frame()
    
  # Summarize simulated output across runs for each year and species
    sim_summary <- simulated_data_output %>%
      group_by(common_name, year) %>%
      summarise(
        # Median of the simulated point estimates (or mean)
        sim_median   = median(est_mean, na.rm = TRUE),
        # 95% simulation envelope across all runs
        sim_lower95  = quantile(est_mean, 0.025, na.rm = TRUE),
        sim_upper95  = quantile(est_mean, 0.975, na.rm = TRUE),
        # 50% simulation envelope (IQR)
        sim_lower50  = quantile(est_mean, 0.250, na.rm = TRUE),
        sim_upper50  = quantile(est_mean, 0.750, na.rm = TRUE),
        .groups = "drop"
      )

  # Create figure
    ggplot() +
        # Simulation 95% uncertainty interval (light band)
        geom_ribbon(
          data = sim_summary,
          aes(x = year, ymin = sim_lower95, ymax = sim_upper95, fill = common_name),
          alpha = 0.2
        ) +
        # Simulation 50% interquartile range (darker inner band)
        geom_ribbon(
          data = sim_summary,
          aes(x = year, ymin = sim_lower50, ymax = sim_upper50, fill = common_name),
          alpha = 0.35
        ) +
        # Simulation median line
        geom_line(
          data = sim_summary,
          aes(x = year, y = sim_median, color = common_name),
          linewidth = 1, linetype = "dashed"
        ) +
        # True observed values (bold points and solid line)
        geom_line(
          data = census_takes,
          aes(x = year, y = takes),
          color = "black", linewidth = 1.1
        ) +
        geom_point(
          data = census_takes,
          aes(x = year, y = takes),
          color = "black", size = 2.5
        ) +
        # Facet by species with free y-axes in case take rates differ vastly
        facet_wrap(~ common_name, scales = "free_y") +
        scale_x_continuous(breaks = unique(census_takes$year)) +
        labs(
          title = "Bycatch Simulation Estimates vs. Known True Takes",
          subtitle = "Dashed line/bands show simulated median, 50%, and 95% envelopes. Solid black line shows true takes.",
          x = "Year",
          y = "Total Estimated and Known Takes"
        ) +
        theme_bw(base_size = 12) +
        theme(
          legend.position = "none",
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold")
        )
    
  # Supplementary Figure 1: Distribution of Errors in Case Study 1
    # summarise totals of mean estimates by run and species  
      exp_stats_sum <- simulated_data_output %>%
        group_by(common_name, run) %>%
        summarise(est_mean_total = sum(est_mean))  
      
    # calculate error between estimates and known values
      simulated_data_summ_error <- exp_stats_sum %>%  
        full_join(census_takes %>% 
                    group_by(common_name) %>% 
                    summarise(takes = sum(takes), .groups="drop_last"), by="common_name") %>% 
        mutate(error = takes - est_mean_total)

    # Create figure  
      ggplot(simulated_data_summ_error, aes(x=error, fill = common_name))+
        geom_density()+
        facet_wrap(~common_name, scales='free')+
        geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1)+
        xlab("Error (True Takes - Estimated Takes)") +
        ylab("Density") +
        theme_bw(base_size = 12) +
        theme(
          legend.position = "none",
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold")
        )




# Case Study 2 - Estimate bycatch in 2024 for all species using models with:
  # (1) only observed data
  # (2) only EM video review data
  # (3) both observed and EM video review data
    
    # Create a list of model data sets:
      all_model_dat = unique(dat_summ$model_data)
      
  # Loop through these three datasets and all species within each of them    
    for(model_dat_temp in all_model_dat){
      
        dat_temp = dat_summ[which(dat_summ$model_data == model_dat_temp),]
      
      # Create a list of species in given data stream:
        all_cn = unique(dat_temp$common_name)
      
      # loop through species in given data stream
      for(cn_temp in all_cn){
      
        dat_temp_cn = dat_temp[which(dat_temp$common_name == cn_temp),]
        
        # confirm actual takes occurred, not just absence of takes in the data stream
          # in those cases, we don't fit the model
          takes_tot = sum(dat_temp_cn$takes)
          
        # single stream - EM or OB only
        if(model_dat_temp %in% c("Single Stream: EM", "Single Stream: OB")){
          
          # if takes occurred, fit model!
          if(takes_tot > 0){
            fit = fit_bycatch(takes ~ 1, 
                              data=dat_temp_cn, 
                              time="year", 
                              effort="monitored_retained_weight ", 
                              family="poisson",
                              time_varying = FALSE, 
                              covrate = "CovRate",
                              iter=4000)
          }
        }
        
        if(model_dat_temp == "Multi-Stream"){
          
          d = dat_temp %>%
            dplyr::mutate(takes = round(takes))%>%
            pivot_longer(cols=c("monitored_retained_weight", "takes", "CovRate"), names_to = "MetricName",
                         values_to = "MetricVal")
          
          d2_wide = d %>%
            pivot_wider(
              names_from=c(MetricName, stream),
              names_sep= "_",
              values_from=c(MetricVal)) %>%
            data.frame()
          
          d2_wide[is.na(d2_wide)] = 0
          
          fit =  fit_bycatch(
            takes_OBS ~ 1,  # Formula for the observer stream
            data = d2_wide,
            time = "year",
            effort = "monitored_retained_weight_OBS",
            covrate_obs = "CovRate_OBS",
            takes_em = "takes_EM",
            effort_em = "monitored_retained_weight_EM",
            covrate_em = "CovRate_EM",
            takes_both = "takes_OBS_EM",
            effort_both = "monitored_retained_weight_OBS_EM",
            covrate_both = "CovRate_OBS_EM",
            family="poisson",
            time_varying = FALSE,
            iter=4000,
            control = list(adapt_delta = 0.99, max_treedepth = 20)
          )
          
        }
        
        # if takes occurred for given spp in given data, save the model and est's                  
        if(takes_tot > 0){
          # save model
          model_name <- paste0("model_", 
                               cn_temp, "_",
                               model_dat_temp)
          assign(x=as.character(model_name), fit)
          save(list=model_name, file=paste0(path_out, model_name, ".RDA"))
          
          
          # calculate expansions
          extract_expansions <- bycatch::get_total(fit)
          
          model_output_init <- data.frame(
            common_name    = cn_temp,
            model_data = model_dat_temp,
            year       = min(d2_wide$year):max(d2_wide$year),
            est_mean   = apply(extract_expansions, 2, mean),
            est_sd     = apply(extract_expansions, 2, sd),
            est_median = apply(extract_expansions, 2, quantile, 0.500),
            lower95    = apply(extract_expansions, 2, quantile, 0.025),
            upper95    = apply(extract_expansions, 2, quantile, 0.975)
          )
          
          }
        
        # if takes did NOT occur in given data stream, everything is zero
        if(takes_tot == 0){
          model_output_init = data.frame(
            common_name    = cn_temp,
            model_data = model_dat_temp,
            year       = min(d2_wide$year):max(d2_wide$year),
            est_mean   = 0,
            est_sd     = 0,
            est_median = 0,
            lower95    = 0,
            upper95    = 0)
        }
        
        
        if(cn_temp == all_cn[1]){
          model_output = model_output_init
        }
        
        if(cn_temp == all_cn[1]){
          model_output = rbind(model_output, model_output_init)
        }
        
        
      } #end loop through species
      
      if(model_dat_temp == all_model_dat[1]){
        model_output_full = model_output
      }
      
      if(model_dat_temp != all_model_dat[1]){
        model_output_full = rbind(model_output_full, model_output)
      }
      
    } #end loop through sectors
      
# KS bad wolf - if Eric doesn't introduce a warning suppression here, add a note for users

      write.csv(simulated_data_output, paste0(path_out, "case_study_1_output_runs=", runs, ".csv"),
                row.names=F)
      
      
  # extract only estimates from 2024, as we're interested in comparing expansions
      # only for the year with partial EM video review    
    model_output_24 = model_output_full[model_output_full$year == 2024, ]
    
    # label species based on which data streams they were recorded in   
      model_output_24$data_source = NA

      # no cases of species recorded only by observers
      #  model_output_24$data_source[model_output_24$common_name %in% c()] = "Species Only on Hauls Observed"
      
      model_output_24$data_source[model_output_24$common_name %in% c("Pacific White-sided Dolphin", 
                                           "Petrel unid", "Sea Lion Unid")] = "Species Only on Hauls Reviewed by EM"
      model_output_24$data_source[model_output_24$common_name %in% c("Cassins Auklet", "California Sea Lion", 
                                           "Northern Elephant Seal", "Steller Sea Lion")] = 
            "Species on Hauls Both Observed and Reviewed by EM"
      
      # order for legibility
      model_output_24$data_source <- factor(model_output_24$data_source, 
                                            levels = c("Species Only on Hauls Reviewed by EM",
                                                        "Species Only on Hauls Observed",
                                                        "Species on Hauls Both Observed and Reviewed by EM"))
      
      # order species by number of recorded takes
      model_output_24$common_name    <- factor(model_output_24$common_name, 
                                               levels = c("Cassins Auklet", 
                                                          "Petrel unid", 
                                                          "Pacific White-sided Dolphin",
                                                          "Sea Lion Unid",
                                                          "Northern Elephant Seal",
                                                          "California Sea Lion",
                                                          "Steller Sea Lion"
                                               ))
      
    # label estimated takes based on which data were included in model:  
      model_output_24$Model = factor(model_output_24$Model, levels=c('Single Stream: EM',
                                                         'Single Stream: OB',
                                                         'Multi-Stream'))

  # generate figure
    ggplot(model_output_24, aes(x = common_name, y = est_mean, group = Model, color = Model)) +
      geom_point(position = position_dodge(width = 0.5), size =3)+
      facet_grid(~ data_source, scales = "free_x", space = "free_x", switch = "x") +
      geom_errorbar(aes(ymin = lower95, ymax = upper95, color = Model), 
                    position = position_dodge(width = 0.5), size = 1.15)+
      ylab("Estimated Bycatch in 2024") +
      theme_classic() +
      theme_bw(base_size = 14) +
      theme(
        # Move strip labels to the bottom and style them like axis labels
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.spacing = unit(0, "lines"),
        axis.title.x = element_blank(),
        strip.text = element_text(face = 'bold')
      )