setwd("D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie")
library(ggplot2)
growth_mu_df <- data.frame(
  Environment = rep(c("Ice-on", "Ice-on", "Hypoxia", "Hypoxia"), each = 4),
  
  Trait = rep(c("Juvenile", "Adult",
                "Juvenile", "Adult"), each = 4),
  
  MU = rep(c("MU1","MU2","MU3","MU4"), times = 4),
  
  Mean = c(
    # Ice-on Juvenile
    0.03, -0.28, -0.17, -0.26,
    
    # Ice-on Adult
    0.31, 0.52, 0.57, -0.15,
    
    # Hypoxia Juvenile
    -0.22, -0.02, 0.02, -0.41,
    
    # Hypoxia Adult
    -0.01, 0.64, 0.69, 0.10
  ),
  
  Lower = c(
    -0.13, -0.48, -0.36, -0.47,
    -0.03, 0.09, 0.13, -0.61,
    -0.32, -0.19, -0.16, -0.59,   # <- assuming -0.59 instead of -59
    -0.16, 0.37, 0.42, -0.14
  ),
  
  Upper = c(
    0.20, -0.21, 0.04, -0.06,
    0.66, 1.00, 1.04, 0.30,
    -0.18, 0.04, 0.08, -0.24,
    0.15, 0.92, 0.98, 0.35
  )
)

growth_mu_df <- rbind(
  growth_mu_df,
  
  data.frame(
    Environment = rep("Nutrition", 8),
    
    Trait = rep(c("Juvenile", "Adult"), each = 4),
    
    MU = rep(c("MU1","MU2","MU3","MU4"), times = 2),
    
    Mean = c(
      # Juvenile
      -0.17, -0.10, 0.04, -0.72,
      # Adult
      0.01, 0.55, 0.49, 0.24
    ),
    
    Lower = c(
      -0.24, -0.25, -0.12, -0.89,
      -0.10, 0.34, 0.29, 0.10
    ),
    
    Upper = c(
      -0.10, 0.06, 0.19, -0.55,
      0.12, 0.78, 0.71, 0.43
    )
  )
)

growth_mu_df$MU <- factor(growth_mu_df$MU,
                          levels = c("MU1","MU2","MU3","MU4"))

growth_mu_df$Trait <- factor(growth_mu_df$Trait,
                             levels = c("Juvenile","Adult"))

growth_mu_df$Environment <- factor(growth_mu_df$Environment,
                                   levels = c("Ice-on","Nutrition","Hypoxia"))

write.csv(growth_mu_df,"Figure/Final/growth_mu_df.csv",row.names = FALSE)

main_figure_df <- growth_mu_df[growth_mu_df$Environment%in%c("Ice-on","Nutrition"),]
env_colors <- c("Ice-on" = "#4C78A8", "Nutrition" = "#B22222")

p_mu <- ggplot(main_figure_df, aes(x = Environment, y = Mean, color = Environment)) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1.0) +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    width = 0.12,
    linewidth = 1.0,
    position = position_dodge(width = 0.45)
  ) +
  geom_point(
    size = 3.8,
    position = position_dodge(width = 0.45)
  ) +
  facet_grid(Trait ~ MU) +
  scale_color_manual(values = env_colors, labels = c("Ice-on duration", "Total phosphorus loads")) +
  labs(
    x = "Environmental factors",
    y = "Effect on somatic growth",
    color = NULL
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    # panel + grid like your example
    panel.grid.major = element_line(color = "grey90", linewidth = 0.8),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(linewidth = 1.2, color = "grey20"),
    
    # facet strip
    strip.background = element_rect(fill = "grey85", color = "grey20", linewidth = 1.2),
    strip.text = element_text(size = 26, face = "plain"),
    
    # axes
    axis.title.x = element_text(size = 26, margin = margin(t = 18)),
    axis.title.y = element_text(size = 26, margin = margin(r = 10)),
    axis.text = element_text(size = 22),
    
    # legend bottom
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.key.width = unit(1.5, "cm"),
    
    # more breathing room
    plot.margin = margin(12, 12, 12, 12)
  )

p_mu

ggsave(
  "Figure/Final/environment_impacts_growth_rate_main_figure.png",
  p_mu,
  width = 11,
  height = 8,
  dpi = 600
)

# sm figure
sm_figure_df <- growth_mu_df[growth_mu_df$Environment %in% c("Hypoxia", "Nutrition"), ]

sm_figure_df$Environment <- factor(
  sm_figure_df$Environment,
  levels = c("Nutrition", "Hypoxia")
)

env_colors <- c(
  "Nutrition" = "#B22222",
  "Hypoxia"   = "#B22222"
)

env_shapes <- c(
  "Nutrition" = 16,  # Circle
  "Hypoxia"   = 17   # Triangle
)

p_mu <- ggplot(
  sm_figure_df,
  aes(x = Environment, y = Mean, color = Environment, shape = Environment)
) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1.0) +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    width = 0.12,
    linewidth = 1.0,
    position = position_dodge(width = 0.45)
  ) +
  geom_point(
    size = 3.8,
    position = position_dodge(width = 0.45)
  ) +
  facet_grid(Trait ~ MU) +
  scale_color_manual(
    values = env_colors,
    labels = c("Total phosphorus loads", "Hypoxia extent")
  ) +
  scale_shape_manual(
    values = env_shapes,
    labels = c("Total phosphorus loads", "Hypoxia extent")
  ) +
  labs(
    x = "Environmental factors",
    y = "Effect on somatic growth",
    color = NULL,
    shape = NULL
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.8),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(linewidth = 1.2, color = "grey20"),
    strip.background = element_rect(fill = "grey85", color = "grey20", linewidth = 1.2),
    strip.text = element_text(size = 25, face = "plain"),
    axis.title = element_text(size = 25, margin = margin(r = 10)),
    axis.text = element_text(size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.key.width = unit(1.5, "cm"),
    plot.margin = margin(12, 12, 12, 12)
  )

p_mu

ggsave(
  "Figure/Final/environment_impacts_growth_rate_sm_figure.png",
  p_mu,
  width = 11,
  height = 8,
  dpi = 600
)
