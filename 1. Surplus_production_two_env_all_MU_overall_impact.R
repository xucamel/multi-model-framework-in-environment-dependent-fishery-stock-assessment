setwd("D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie")

library(dplyr)
library(TMB)
library(devtools)
library(reshape2)
library(rstan)
library(ggplot2)
library(gridExtra)
library(rstan)
library(bayesplot)

pub_theme <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "top",
    plot.margin = margin(10, 10, 10, 10)
  )

#### read data ####
# env
env_df <-  read.csv("Data/Final/E_scale.csv")

# env_df$scale_nutrition = scale(env_df$c_lold)
# write.csv(env_df,"Data/Final/E_scale.csv")

# read the four stock assessment results 
mu1_df = read.table("Env_dependent_model/Base/mu1_23_PR_YOY_HCR_OHTR.rep",header = TRUE, fill = TRUE, comment.char = "#")
mu2_df = read.table("Env_dependent_model/Base/mu2_23_PR_YOY_HCR.rep",header = TRUE, fill = TRUE, comment.char = "#")
mu3_df = read.table("Env_dependent_model/Base/mu3_23_PR_YOY_HCR.rep",header = TRUE, fill = TRUE, comment.char = "#")
mu4_df = read.table("Env_dependent_model/Base/mu4_23_PR_YOY_HCR.rep",header = TRUE, fill = TRUE, comment.char = "#")

# surplus production model input data frame mu1
mu1_sp_df = data.frame(Year=1975:2023)
mu1_sp_df$gillnet = as.numeric(mu1_df$ncount[(which(mu1_df=="obs_biomass_gillnet")+1):(which(mu1_df=="obs_biomass_trapnet")-1)])
mu1_sp_df$trapnet = as.numeric(mu1_df$ncount[(which(mu1_df=="obs_biomass_trapnet")+1):(which(mu1_df=="obs_biomass_angler")-1)])
mu1_sp_df$angler = as.numeric(mu1_df$ncount[(which(mu1_df=="obs_biomass_angler")+1):(which(mu1_df=="obs_lbs_gillnet")-1)])

# get the biomass value
start_index = which(mu1_df == "biomass_fish_at_age") + 1
end_index = which(mu1_df == "est_gillnet_catch") - 1

flattened_values = as.numeric(mu1_df$ncount[start_index:end_index])

mu1_sp_df$biomass_2 = flattened_values[seq(1, length(flattened_values), by = 5)]
mu1_sp_df$biomass_3 = flattened_values[seq(2, length(flattened_values), by = 5)]
mu1_sp_df$biomass_4 = flattened_values[seq(3, length(flattened_values), by = 5)]
mu1_sp_df$biomass_5 = flattened_values[seq(4, length(flattened_values), by = 5)]
mu1_sp_df$biomass_6 = flattened_values[seq(5, length(flattened_values), by = 5)]

# calculate total catch and biomass
mu1_sp_df$total_catch = rowSums(mu1_sp_df[, c("gillnet", "trapnet", "angler")])
mu1_sp_df$total_biomass = rowSums(mu1_sp_df[, c("biomass_2", "biomass_3", "biomass_4", "biomass_5", "biomass_6")])

# surplus production model input data frame mu2
mu2_sp_df = data.frame(Year=1975:2023)
mu2_sp_df$gillnet = as.numeric(mu2_df$Test[(which(mu2_df=="obs_biomass_gillnet")+1):(which(mu2_df=="obs_biomass_trapnet")-1)])
mu2_sp_df$trapnet[mu2_sp_df$Year>1986] = as.numeric(mu2_df$Test[(which(mu2_df=="obs_biomass_trapnet")+1):(which(mu2_df=="obs_biomass_angler")-1)]) # year 1987-2023
mu2_sp_df$trapnet[is.na(mu2_sp_df$trapnet)]=0
mu2_sp_df$angler = as.numeric(mu2_df$Test[(which(mu2_df=="obs_biomass_angler")+1):(which(mu2_df=="obs_lbs_gillnet")-1)])

# get the biomass value
start_index = which(mu2_df == "biomass_fish_at_age") + 1
end_index = which(mu2_df == "est_gillnet_catch") - 1

flattened_values = as.numeric(mu2_df$Test[start_index:end_index])

mu2_sp_df$biomass_2 = flattened_values[seq(1, length(flattened_values), by = 5)]
mu2_sp_df$biomass_3 = flattened_values[seq(2, length(flattened_values), by = 5)]
mu2_sp_df$biomass_4 = flattened_values[seq(3, length(flattened_values), by = 5)]
mu2_sp_df$biomass_5 = flattened_values[seq(4, length(flattened_values), by = 5)]
mu2_sp_df$biomass_6 = flattened_values[seq(5, length(flattened_values), by = 5)]

# calculate total catch and biomss
mu2_sp_df$total_catch = rowSums(mu2_sp_df[, c("gillnet", "trapnet", "angler")])
mu2_sp_df$total_biomass = rowSums(mu2_sp_df[, c("biomass_2", "biomass_3", "biomass_4", "biomass_5", "biomass_6")])

# surplus production model input data frame mu3
mu3_sp_df = data.frame(Year=1975:2023)
mu3_sp_df$gillnet = as.numeric(mu3_df$Test[(which(mu3_df=="obs_biomass_gillnet")+1):(which(mu3_df=="obs_biomass_trapnet")-1)])
mu3_sp_df$trapnet[mu3_sp_df$Year>1986] = as.numeric(mu3_df$Test[(which(mu3_df=="obs_biomass_trapnet")+1):(which(mu3_df=="obs_biomass_angler")-1)]) # year 1987-2023
mu3_sp_df$trapnet[is.na(mu3_sp_df$trapnet)]=0
mu3_sp_df$angler = as.numeric(mu3_df$Test[(which(mu3_df=="obs_biomass_angler")+1):(which(mu3_df=="obs_lbs_gillnet")-1)])

# get the biomass value
start_index = which(mu3_df == "biomass_fish_at_age") + 1
end_index = which(mu3_df == "est_gillnet_catch") - 1

flattened_values = as.numeric(mu3_df$Test[start_index:end_index])

mu3_sp_df$biomass_2 = flattened_values[seq(1, length(flattened_values), by = 5)]
mu3_sp_df$biomass_3 = flattened_values[seq(2, length(flattened_values), by = 5)]
mu3_sp_df$biomass_4 = flattened_values[seq(3, length(flattened_values), by = 5)]
mu3_sp_df$biomass_5 = flattened_values[seq(4, length(flattened_values), by = 5)]
mu3_sp_df$biomass_6 = flattened_values[seq(5, length(flattened_values), by = 5)]

# calculate total catch and biomss
mu3_sp_df$total_catch = rowSums(mu3_sp_df[, c("gillnet", "trapnet", "angler")])
mu3_sp_df$total_biomass = rowSums(mu3_sp_df[, c("biomass_2", "biomass_3", "biomass_4", "biomass_5", "biomass_6")])

# surplus production model input data frame MU4
mu4_sp_df = data.frame(Year=1975:2023)
mu4_sp_df$gillnet = as.numeric(mu4_df$Test[(which(mu4_df=="obs_biomass_gillnet")+1):(which(mu4_df=="obs_biomass_trapnet")-1)])

mu4_sp_df$trapnet[mu4_sp_df$Year>1985] = as.numeric(mu4_df$Test[(which(mu4_df=="obs_biomass_trapnet")+1):(which(mu4_df=="obs_biomass_angler")-1)]) # year 1987-2023
mu4_sp_df$trapnet[is.na(mu4_sp_df$trapnet)]=0

mu4_sp_df$angler[mu4_sp_df$Year>1988] = as.numeric(mu4_df$Test[(which(mu4_df=="obs_biomass_angler")+1):(which(mu4_df=="obs_gillnet_catch")-1)]) # year 1987-2023
mu4_sp_df$angler[is.na(mu4_sp_df$angler)]=0

# get the biomass value
start_index = which(mu4_df == "biomass_fish_at_age") + 1
end_index = which(mu4_df == "est_gillnet_catch") - 1

flattened_values = as.numeric(mu4_df$Test[start_index:end_index])

mu4_sp_df$biomass_2 = flattened_values[seq(1, length(flattened_values), by = 5)]
mu4_sp_df$biomass_3 = flattened_values[seq(2, length(flattened_values), by = 5)]
mu4_sp_df$biomass_4 = flattened_values[seq(3, length(flattened_values), by = 5)]
mu4_sp_df$biomass_5 = flattened_values[seq(4, length(flattened_values), by = 5)]
mu4_sp_df$biomass_6 = flattened_values[seq(5, length(flattened_values), by = 5)]

# calculate total catch and biomss
mu4_sp_df$total_catch = rowSums(mu4_sp_df[, c("gillnet", "trapnet", "angler")])
mu4_sp_df$total_biomass = rowSums(mu4_sp_df[, c("biomass_2", "biomass_3", "biomass_4", "biomass_5", "biomass_6")])
######
# the MU-specific biomass 

# combine the 4 MU
mu_all_sp_df = data.frame(Year=1975:2023)
mu_all_sp_df$Catch = mu1_sp_df$total_catch+mu2_sp_df$total_catch+mu3_sp_df$total_catch+mu4_sp_df$total_catch
mu_all_sp_df$Biomass = mu1_sp_df$total_biomass+mu2_sp_df$total_biomass+mu3_sp_df$total_biomass+mu4_sp_df$total_biomass

# merge the environmental data
mu_all_sp_df = mu_all_sp_df[mu_all_sp_df$Year<2021,]
mu_all_sp_df = left_join(mu_all_sp_df, env_df , c("Year"="year"))

# run the surplus production model
SPM_stan = stan_model(file="Code/SPM_catch_calculation_include_sp_two_env.stan")

input_df = mu_all_sp_df 

# assign 'tri_harvest' and 'stocking_weight' with 0
input_df$tri_harvest <- 0
input_df$stocking_weight <- 0

# scale the environmental data
env_i = input_df$scale_ice_on
env_i_2 = input_df$scale_nutrition
# stan data
stan_data <- list(
  N_1 = 46,
  Catch_1 = input_df$Catch,
  CPUE_1 = input_df$Biomass,
  Environment_1 = env_i,
  Environment_2 = env_i_2, 
  Stocking_1 = input_df$stocking_weight,
  Tribal_1 = input_df$stocking_weight,
  k_1_prior = c(max(input_df$Biomass),max(input_df$Biomass*10)),
  r_1_prior = c(0.3,0.8)
)

chain=3; iter=5000; warmup=2500; thin=1;

fit_SPM_stan <- sampling(SPM_stan,
                         data=stan_data,
                         chain=chain,
                         iter=iter,
                         warmup=warmup,
                         cores = chain,
                         thin=thin,
                         control = list(adapt_delta = 0.9999,max_treedepth = 20)
) 

# Extract the posterior draws
posterior_two_env <- as.matrix(fit_SPM_stan)
df_two_env <- as.data.frame(as.matrix(posterior_two_env))
write.csv(df_two_env,"Figure/Final/surplus_production_model_two_env_mcmc.csv",row.names = FALSE)

df_two_env <- read.csv("Figure/Final/surplus_production_model_two_env_mcmc.csv")

# 1. Select the msy columns (msy.1 to msy.46)
msy_cols <- paste0("MSY.", 1:46, ".")

# 2. Calculate Mean and 95% Credible Interval (2.5% and 97.5%)
# We use apply to iterate over columns
msy_summary <- data.frame(
  Year = 1:46,
  Mean = sapply(df_two_env[msy_cols], mean),
  Lower = sapply(df_two_env[msy_cols], quantile, probs = 0.025),
  Upper = sapply(df_two_env[msy_cols], quantile, probs = 0.975)
)

png("Figure/Final/msy_two_env_plot.png", 
    width = 6, 
    height = 6, 
    units = "in", 
    res = 600)

msy_base <- msy_summary$Mean[1]
ice_base <- env_df$ice_on[1]
nut_base <- env_df$c_lold[1]
msy_summary$Year <- 1975:2020

# 2. Plot
p <- ggplot(msy_summary, aes(x = Year)) +
  # Uncertainty Ribbon
  geom_ribbon(aes(ymin = Lower / msy_base, ymax = Upper / msy_base), 
              fill = "gray", alpha = 0.65) +
  
  # MSY Relative Line
  geom_line(aes(y = Mean / msy_base, color = "msy"), linewidth = 1.2) +
  
  # Ice On Relative Line
  geom_line(data = env_df, aes(x = year, y = ice_on / ice_base, color = "ice"), 
            linewidth = 0.8, linetype = "twodash") +
  
  # Nutrient Relative Line
  geom_line(data = env_df, aes(x = year, y = c_lold / nut_base, color = "phosphorus"), 
            linewidth = 0.8, linetype = "dotdash") +
  
  # Baseline Reference
  geom_hline(yintercept = 1, linetype = "solid", color = "black", alpha = 0.3) +
  
  # Color scale - Ordered and using Full Names
  scale_color_manual(
    name = NULL, 
    breaks = c("msy", "ice", "phosphorus"), # This defines the legend order
    labels = c("MSY", "Ice-on duration", "Total phosphorus loads"), # Full display names
    values = c("msy" = "black", 
               "ice" = "steelblue", 
               "phosphorus" = "firebrick")
  ) +
  
  # Labels
  labs(
    title = "",
    subtitle = "",
    x = "Year",
    y = "Relative Change (Ratio to first year)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)


# 3. Close the file connection (Ensure you have opened a device like pdf() or png() before this)
dev.off()

# Get the full summary table
summ_table <- summary(fit_SPM_stan)$summary

# Extract specifically for "impact_E_1" using its name
sp_output = data.frame(
  mu = "all", 
  ice_impact      = summ_table["impact_E_1", "mean"], 
  ice_impact_low  = summ_table["impact_E_1", "2.5%"], 
  ice_impact_high = summ_table["impact_E_1", "97.5%"]
)

sp_ice = sp_output
df_ice <- as.data.frame(as.matrix(posterior_ice))
write.csv(df_ice,"Figure/Final/surplus_production_model_ice_mcmc.csv",row.names = FALSE)


# plot
plot_data <- data.frame(val = df_ice$impact_E_1)
# Calculate the 99% bounds for shading
bounds <- quantile(plot_data$val, probs = c(0.005, 0.995))

fig_ice = ggplot(plot_data, aes(x = val)) +
  # 1. The main density curve
  geom_density(fill = "grey90", color = "black", size = 0.8) +
  
  # 2. Shade the 99% Credible Interval
  stat_density(aes(x = val), 
               geom = "area", 
               fill = "#4C78A8", 
               alpha = 0.6,
               bounds = c(bounds[1], bounds[2])) +
  
  # 3. The Reference Line at Zero
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.7) +
  
  # 4. Clean Labels and Theme
  labs(
    x = "Impact of ice-on duration on productivity",
    y = "Posterior Density",
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  pub_theme
ggsave("Figure/Final/Ice_impact_posterior_distribution.png", fig_ice, width = 4, height = 4, dpi = 600)

# hypoxia
# scale the environmental data
env_i = input_df$scale_nutrition 

# stan data
stan_data <- list(
  N_1 = 46,
  Catch_1 = input_df$Catch,
  CPUE_1 = input_df$Biomass,
  Environment_1 = env_i,
  Stocking_1 = input_df$stocking_weight,
  Tribal_1 = input_df$stocking_weight,
  k_1_prior = c(max(input_df$Biomass),max(input_df$Biomass*10)),
  r_1_prior = c(0.3,0.8)
)


chain=3; iter=3000; warmup=1500; thin=1;

fit_SPM_stan <- sampling(SPM_stan,
                         data=stan_data,
                         chain=chain,
                         iter=iter,
                         warmup=warmup,
                         cores = chain,
                         thin=thin,
                         control = list(adapt_delta = 0.9999,max_treedepth = 20)
) 

# Extract the posterior draws
posterior_hypoxia <- as.matrix(fit_SPM_stan)

# Plot the density
# Convert matrix to data frame
df_hypoxia <- as.data.frame(as.matrix(posterior_hypoxia))
write.csv(df_hypoxia,"Figure/Final/surplus_production_model_nutrient_mcmc.csv",row.names = FALSE)

# Get the full summary table
summ_table <- summary(fit_SPM_stan)$summary

# surplus production model output data frame
sp_output$hypoxia_impact=summary(fit_SPM_stan)$summary[8,"mean"]
sp_output$hypoxia_impact_low =  summary(fit_SPM_stan)$summary[8,"2.5%"]
sp_output$hypoxia_impact_high =  summary(fit_SPM_stan)$summary[8,"97.5%"]

# plot
plot_data <- data.frame(val = df_hypoxia$impact_E_1)
# Calculate the 99% bounds for shading
bounds <- quantile(plot_data$val, probs = c(0.005, 0.995))

fig_hypoxia = ggplot(plot_data, aes(x = val)) +
  # 1. The main density curve
  geom_density(fill = "grey90", color = "black", size = 0.8) +
  
  # 2. Shade the 99% Credible Interval
  stat_density(aes(x = val), 
               geom = "area", 
               fill = "#B22222", 
               alpha = 0.6,
               bounds = c(bounds[1], bounds[2])) +
  
  # 3. The Reference Line at Zero
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.7) +
  
  # 4. Clean Labels and Theme
  labs(
    x = "Impact of hypoxia extent on productivity",
    y = "Posterior Density",
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  pub_theme
ggsave("Figure/Final/Hypoxia_impact_posterior_distribution.png", fig_hypoxia, width = 4, height = 4, dpi = 600)

library(patchwork)

combined_fig <- fig_ice  | fig_hypoxia  # left | right

combined_fig
ggsave("Figure/Final/Env_impact_posterior_distribution.png", combined_fig, width = 7, height = 4, dpi = 600)

## read the mcmcm data
df_hypoxia <- read.csv("Figure/Final/surplus_production_model_hypoxia_mcmc.csv")
df_ice <- read.csv("Figure/Final/surplus_production_model_ice_mcmc.csv")
df_nutrient <- read.csv("Figure/Final/surplus_production_model_nutrient_mcmc.csv")

plot_data <- data.frame(val = df_hypoxia$impact_E_1)
# Calculate the 99% bounds for shading
bounds <- quantile(plot_data$val, probs = c(0.005, 0.995))

fig_hypoxia = ggplot(plot_data, aes(x = val)) +
  # 1. The main density curve
  geom_density(fill = "grey90", color = "black", size = 0.8) +
  
  # 2. Shade the 99% Credible Interval
  stat_density(aes(x = val), 
               geom = "area", 
               fill = "#B22222", 
               alpha = 0.6,
               bounds = c(bounds[1], bounds[2])) +
  
  # 3. The Reference Line at Zero
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.7) +
  
  # 4. Clean Labels and Theme
  labs(
    x = "Impact of hypoxia extent\n on productivity",
    y = "Posterior Density",
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  pub_theme

plot_data <- data.frame(val = df_ice$impact_E_1)
# Calculate the 99% bounds for shading
bounds <- quantile(plot_data$val, probs = c(0.005, 0.995))

fig_ice = ggplot(plot_data, aes(x = val)) +
  # 1. The main density curve
  geom_density(fill = "grey90", color = "black", size = 0.8) +
  
  # 2. Shade the 99% Credible Interval
  stat_density(aes(x = val), 
               geom = "area", 
               fill = "#4C78A8", 
               alpha = 0.6,
               bounds = c(bounds[1], bounds[2])) +
  
  # 3. The Reference Line at Zero
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.7) +
  
  # 4. Clean Labels and Theme
  labs(
    x = "Impact of ice-on duration\n on productivity",
    y = "Posterior Density",
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  pub_theme

plot_data <- data.frame(val = df_nutrient$impact_E_1)
# Calculate the 99% bounds for shading
bounds <- quantile(plot_data$val, probs = c(0.005, 0.995))

fig_nutrient = ggplot(plot_data, aes(x = val)) +
  # 1. The main density curve
  geom_density(fill = "grey90", color = "black", size = 0.8) +
  
  # 2. Shade the 99% Credible Interval
  stat_density(aes(x = val), 
               geom = "area", 
               fill = "#B22222", 
               alpha = 0.6,
               bounds = c(bounds[1], bounds[2])) +
  
  # 3. The Reference Line at Zero
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.7) +
  
  # 4. Clean Labels and Theme
  labs(
    x = "Impact of total phosphorus loads\n on productivity",
    y = "Posterior Density",
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  pub_theme

library(patchwork)

combined_fig <- fig_ice  | fig_nutrient  # left | right

combined_fig
ggsave("Figure/Final/Env_impact_posterior_distribution.png", combined_fig, width = 7, height = 4, dpi = 600)


combined_fig_sm <- fig_nutrient  | fig_hypoxia  # left | right
combined_fig_sm
ggsave("Figure/Final/Hypoxia_impact_posterior_distribution_sm.png", combined_fig_sm , width = 7, height = 4, dpi = 600)

