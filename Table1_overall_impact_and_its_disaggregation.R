setwd("D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie") 
# read the data 
rec_df = read.csv("Figure/Final/ssb_r_result_two_years_lag.csv") 
growth_df = read.csv("Figure/Final/growth_mu_df.csv") 
mortality_df = read.csv("Figure/Final/M_env_results.csv") 
productivity_df = read.csv("Figure/Final/overall_impact_productivity.csv") 
biomass_df = read.csv("Figure/Final/mu_all_biomass.csv") 

# weighted 

library(dplyr)

# Helper: weighted mean
wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

# Harmonize MU labels / driver labels
rec_df <- rec_df %>%
  filter(model %in% c("ice_on", "hypoxia", "nutrition")) %>%
  mutate(
    mu = as.numeric(mu),
    Driver = case_match(model,
                        c("ice_on", "ice-on") ~ "Ice-on",
                        "hypoxia"            ~ "Hypoxia",
                        "nutrition"          ~ "Nutrition",
                        .default             = model
    )
  )

growth_df$Driver <- growth_df$Environment
growth_df$mu <- as.numeric(1:4)

mortality_df <- mortality_df %>%
  mutate(mu = as.numeric(mu)) %>%
  mutate(Driver = case_match(par,
                             c("ice_on", "ice-on") ~ "Ice-on",
                             "hypoxia"            ~ "Hypoxia",
                             "nutrient"          ~ "Nutrition",
                             .default             = par))

# -------------------------
# 1) Recruitment (rec_df): biomass-weighted by MU, by driver
# -------------------------
rec_weighted <- rec_df %>%
  left_join(biomass_df, by = "mu") %>%
  group_by(Driver) %>%
  summarise(
    weighted_mean = wmean(c, biomass),
    weighted_low  = wmean(c_low_95,  biomass),
    weighted_high = wmean(c_up_95, biomass),
    .groups = "drop"
  ) %>%
  mutate(Process = "Recruitment") %>%
  select(Process, Driver, weighted_mean, weighted_low, weighted_high)

# -------------------------
# 2) Growth (growth_df): average juvenile/adult within MU, then biomass-weight
# -------------------------
growth_weighted <- growth_df %>%
  left_join(biomass_df, by = "mu") %>%
  group_by(Driver,Trait) %>%
  summarise(
    weighted_mean = wmean(Mean, biomass),
    weighted_low  = wmean(Lower,  biomass),
    weighted_high = wmean(Upper, biomass),
    .groups = "drop"
  ) %>%
  mutate(Process = Trait) %>%
  select(Process, Driver, weighted_mean, weighted_low, weighted_high)

# -------------------------
# 3) Mortality (mortality_df): biomass-weighted by MU, by driver
#   mortality uses columns: value, low, high
# -------------------------
mort_weighted <- mortality_df %>%
  left_join(biomass_df, by = "mu") %>%
  group_by(Driver) %>%
  summarise(
    weighted_mean = wmean(value, biomass),
    weighted_low  = wmean(low,   biomass),
    weighted_high = wmean(high,  biomass),
    .groups = "drop"
  ) %>%
  mutate(Process = "Mortality") %>%
  select(Process, Driver, weighted_mean, weighted_low, weighted_high)

# -------------------------
# Combine results
# -------------------------
results_weighted <- bind_rows(rec_weighted, growth_weighted, mort_weighted) %>%
  mutate(
    sign = ifelse(weighted_mean > 0, "+", "-"),
    magnitude = abs(weighted_mean)
  )

print(results_weighted)

productivity_long <- productivity_df %>%
  tidyr::pivot_longer(
    cols = c(ice.on, nutrient,hypoxia),
    names_to = "Driver",
    values_to = "weighted_mean"
  ) %>%
  mutate(
    Driver = ifelse(Driver == "ice_impact", "Ice-on", "Hypoxia"),
    Process = "Productivity",
    Trait = NA,
    weighted_low = NA,
    weighted_high = NA,
    sign = ifelse(weighted_mean > 0, "+", "-"),
    magnitude = abs(weighted_mean)
  ) %>%
  select(Process, Driver, weighted_mean, weighted_low, weighted_high, sign, magnitude)

productivity_long$Driver <- c("Ice-on","Nutrition","Hypoxia")

plot_df <- bind_rows(results_weighted, productivity_long)

library(ggplot2)

plot_df <- plot_df %>%
  mutate(
    Process = ifelse(Process == "Mortality", "Survival", Process),
    
    # flip the sign for survival
    weighted_mean = ifelse(Process == "Survival", -weighted_mean, weighted_mean),
    
    magnitude = abs(weighted_mean),
    
    sign = ifelse(weighted_mean > 0, "+", "-")
  )

plot_df$Process <- factor(
  plot_df$Process,
  levels = c(
    "Productivity",
    "Survival",
    "Adult growth",
    "Juvenile growth",
    "Recruitment"
  )
)

plot_df$Driver <- recode(plot_df$Driver,
                         "Hypoxia" = "Hypoxia extent",
                         "Ice-on" = "Ice-on duration",
                         "Nutrition" = "Total phosphorus loads")
main_figure_df <- plot_df[plot_df$Driver%in%c("Ice-on duration","Total phosphorus loads"),]

p <- ggplot(main_figure_df,
            aes(x = Driver,
                y = Process,
                label = sign,
                size = magnitude,
                color = Driver)) +
  
  geom_text(fontface = "bold") +
  
  scale_size(range = c(6,18)) +
  
  scale_color_manual(values = c(
    "Ice-on duration" = "#1f78b4",
    "Total phosphorus loads" = "#B22222"
  )) +
  
  labs(
    x = "Environmental driver",
    y = "Demographic process"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )+
  # separator line between demographic processes and productivity
  geom_hline(yintercept = 1.5, linewidth = 0.6, color = "black")

print(p)

ggsave(
  "Figure/Final/environment_impacts_summary.png",
  p,
  width = 6,
  height = 4.5,
  dpi = 600
)
