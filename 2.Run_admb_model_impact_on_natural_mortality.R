setwd("D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie/Env_dependent_model/M/20/ice_on")

library(R2admb)
# Set the ADMB environment variables
Sys.setenv(PATH = paste("D:/ADMB-13.2/bin", Sys.getenv("PATH"), sep=";"))
Sys.setenv(ADMB_HOME = "D:/ADMB-13.2")

# Verify the environment variables
Sys.getenv("PATH")
Sys.getenv("ADMB_HOME")
setup_admb()

# MU 2
compile_admb("mu2_20_PR_YOY_HCR")
run_admb("mu2_20_PR_YOY_HCR")
#run_admb("mu2_20_PR_YOY_HCR", extra.args = "-ainp mu2_20_PR_YOY_HCR.par")
run_admb("mu2_20_PR_YOY_HCR", extra.args = "-ainp mu2_20_PR_YOY_HCR.par -hess_step 10")
# check convergence
par_lines <- readLines("mu2_20_PR_YOY_HCR.par", n = 1)
cat("Convergence Info:", par_lines, "\n")

# Load the standard deviation file
std_data <- read.table("mu2_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))

# Filter for all parameters containing "M" or "mort" (depending on your TPL naming)
M_ice_on_2 <- std_data[grep("M", std_data$name)[1:2], ]
M_ice_on_2$par = "ice-on"
M_ice_on_2$mu = 2

# MU 3
compile_admb("mu3_20_PR_YOY_HCR")
run_admb("mu3_20_PR_YOY_HCR")
run_admb("mu3_20_PR_YOY_HCR", extra.args = "-ainp mu3_20_PR_YOY_HCR.par -hess_step 10")
# check convergence
par_lines <- readLines("mu3_20_PR_YOY_HCR.par", n = 1)
cat("Convergence Info:", par_lines, "\n")

# Load the standard deviation file
std_data <- read.table("mu3_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))

# Filter for all parameters containing "M" or "mort" (depending on your TPL naming)
M_ice_on_3 <- std_data[grep("M", std_data$name)[1:2], ]
M_ice_on_3$par = "ice-on"
M_ice_on_3$mu = 3

# MU 4
compile_admb("mu4_20_PR_YOY_HCR")
run_admb("mu4_20_PR_YOY_HCR")
run_admb("mu4_20_PR_YOY_HCR", extra.args = "-ainp mu4_20_PR_YOY_HCR.par -hess_step 10")
# check convergence
par_lines <- readLines("mu4_20_PR_YOY_HCR.par", n = 1)
cat("Convergence Info:", par_lines, "\n")

# Load the standard deviation file
std_data <- read.table("mu4_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))

# Filter for all parameters containing "M" or "mort" (depending on your TPL naming)
M_ice_on_4 <- std_data[grep("M", std_data$name)[1:2], ]
M_ice_on_4$par = "ice-on"
M_ice_on_4$mu = 4

# MU1 
compile_admb("mu1_23_PR_YOY_HCR_OHTR_M_env")
run_admb("mu1_23_PR_YOY_HCR_OHTR_M_env")
run_admb("mu1_23_PR_YOY_HCR_OHTR_M_env", extra.args = "-ainp mu1_23_PR_YOY_HCR_OHTR_M_env.par -hess_step 10")
# check convergence
par_lines <- readLines("mu1_23_PR_YOY_HCR_OHTR_M_env.par", n = 1)
cat("Convergence Info:", par_lines, "\n")

# Load the standard deviation file
std_data <- read.table("mu1_23_PR_YOY_HCR_OHTR_M_env.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))

# Filter for all parameters containing "M" or "mort" (depending on your TPL naming)
M_ice_on_1 <- std_data[grep("M", std_data$name)[1:2], ]
M_ice_on_1$par = "ice-on"
M_ice_on_1$mu = 1

# hypoxia
setwd("D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie/Env_dependent_model/M/20/nutrition")

# MU 2
compile_admb("mu2_20_PR_YOY_HCR")
run_admb("mu2_20_PR_YOY_HCR")
#run_admb("mu2_20_PR_YOY_HCR", extra.args = "-ainp mu2_20_PR_YOY_HCR.par")
run_admb("mu2_20_PR_YOY_HCR", extra.args = "-ainp mu2_20_PR_YOY_HCR.par -hess_step 10")
# check convergence
par_lines <- readLines("mu2_20_PR_YOY_HCR.par", n = 1)
cat("Convergence Info:", par_lines, "\n")

# Load the standard deviation file
std_data <- read.table("mu2_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))

# Filter for all parameters containing "M" or "mort" (depending on your TPL naming)
M_hypoxia_2 <- std_data[grep("M", std_data$name)[1:2], ]
M_hypoxia_2$par = "hypoxia"
M_hypoxia_2$mu = 2

# MU 3
compile_admb("mu3_20_PR_YOY_HCR")
run_admb("mu3_20_PR_YOY_HCR")
run_admb("mu3_20_PR_YOY_HCR", extra.args = "-ainp mu3_20_PR_YOY_HCR.par -hess_step 10")
# check convergence
par_lines <- readLines("mu3_20_PR_YOY_HCR.par", n = 1)
cat("Convergence Info:", par_lines, "\n")

# Load the standard deviation file
std_data <- read.table("mu3_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))

# Filter for all parameters containing "M" or "mort" (depending on your TPL naming)
M_hypoxia_3 <- std_data[grep("M", std_data$name)[1:2], ]
M_hypoxia_3$par = "hypoxia"
M_hypoxia_3$mu = 3

# MU 4
compile_admb("mu4_20_PR_YOY_HCR")
run_admb("mu4_20_PR_YOY_HCR")
run_admb("mu4_20_PR_YOY_HCR", extra.args = "-ainp mu4_20_PR_YOY_HCR.par -hess_step 10")
# check convergence
par_lines <- readLines("mu4_20_PR_YOY_HCR.par", n = 1)
cat("Convergence Info:", par_lines, "\n")

# Load the standard deviation file
std_data <- read.table("mu4_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))

# Filter for all parameters containing "M" or "mort" (depending on your TPL naming)
M_hypoxia_4 <- std_data[grep("M", std_data$name)[1:2], ]
M_hypoxia_4$par = "hypoxia"
M_hypoxia_4$mu = 4

# MU1 
compile_admb("mu1_23_PR_YOY_HCR_OHTR_M_env")
run_admb("mu1_23_PR_YOY_HCR_OHTR_M_env")
run_admb("mu1_23_PR_YOY_HCR_OHTR_M_env", extra.args = "-ainp mu1_23_PR_YOY_HCR_OHTR_M_env.par -hess_step 10")
# check convergence
par_lines <- readLines("mu1_23_PR_YOY_HCR_OHTR_M_env.par", n = 1)
cat("Convergence Info:", par_lines, "\n")

# Load the standard deviation file
std_data <- read.table("mu1_23_PR_YOY_HCR_OHTR_M_env.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))

# Filter for all parameters containing "M" or "mort" (depending on your TPL naming)
M_hypoxia_1 <- std_data[grep("M", std_data$name)[1:2], ]
M_hypoxia_1$par = "hypoxia"
M_hypoxia_1$mu = 1

# combine all results
M_env_results = rbind(M_hypoxia_1,M_hypoxia_2,M_hypoxia_3,M_hypoxia_4,M_ice_on_1,M_ice_on_2,M_ice_on_3,M_ice_on_4)

M_env_results$low <- M_env_results$value - 1.96 * M_env_results$std_dev
M_env_results$high <- M_env_results$value + 1.96 * M_env_results$std_dev
M_env_results = M_env_results[M_env_results$name=="M_env_impact",]

write.csv(M_env_results,"Figure/Final/M_env_results.csv",row.names = FALSE)

# Set order and labels
M_env_results <- read.csv("Figure/Final/M_env_results.csv")
M_env_results$par <- factor(
  M_env_results$par,
  levels = c("ice-on", "hypoxia"),
  labels = c("Ice-on duration", "Hypoxia extent")
)

# Relabel mu panels
M_env_results$mu <- factor(
  M_env_results$mu,
  levels = c(1,2,3,4),
  labels = c("MU1","MU2","MU3","MU4")
)

# Colors
env_colors <- c(
  "Ice-on duration" = "#4C78A8",
  "Hypoxia extent" = "#B22222"
)

ggplot(M_env_results,
       aes(x = par, y = value, color = par)) +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             color = "black",
             linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = low, ymax = high),
                width = 0.2) +
  facet_wrap(~mu, nrow = 1) +
  scale_color_manual(values = env_colors) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  labs(
    x = "Environmental factors",
    y = "Impacts"
  )

ggplot(M_env_results,
       aes(x = par, y = value, color = par)) +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             color = "black",
             linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = low, ymax = high),
                width = 0.2) +
  facet_wrap(~mu, nrow = 1) +
  scale_color_manual(values = env_colors) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),      # remove crowded labels
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = "Environmental factors",
    y = "Impacts",
    color = NULL
  )

ggsave(
  "D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie/Figure/Final/environment_effects_M.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 600
)

# read the data
# ice-on
setwd("D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie/Env_dependent_model/M/20/ice_on")

# mu2
std_data <- read.table("mu2_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))
M_ice_on_2 <- std_data[grep("M", std_data$name)[1:2], ]
M_ice_on_2$par = "ice-on"
M_ice_on_2$mu = 2

# mu3
std_data <- read.table("mu3_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))
M_ice_on_3 <- std_data[grep("M", std_data$name)[1:2], ]
M_ice_on_3$par = "ice-on"
M_ice_on_3$mu = 3

# mu4
std_data <- read.table("mu4_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))
M_ice_on_4 <- std_data[grep("M", std_data$name)[1:2], ]
M_ice_on_4$par = "ice-on"
M_ice_on_4$mu = 4

# mu1
std_data <- read.table("mu1_23_PR_YOY_HCR_OHTR_M_env.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))
M_ice_on_1 <- std_data[grep("M", std_data$name)[1:2], ]
M_ice_on_1$par = "ice-on"
M_ice_on_1$mu = 1

# nutrition
setwd("D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie/Env_dependent_model/M/20/nutrition")

# mu2
std_data <- read.table("mu2_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))
M_nutrient_2 <- std_data[grep("M", std_data$name)[1:2], ]
M_nutrient_2$par = "nutrient"
M_nutrient_2$mu = 2

# mu3
std_data <- read.table("mu3_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))
M_nutrient_3 <- std_data[grep("M", std_data$name)[1:2], ]
M_nutrient_3$par = "nutrient"
M_nutrient_3$mu = 3

# mu4
std_data <- read.table("mu4_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))
M_nutrient_4 <- std_data[grep("M", std_data$name)[1:2], ]
M_nutrient_4$par = "nutrient"
M_nutrient_4$mu = 4

# mu1
std_data <- read.table("mu1_23_PR_YOY_HCR_OHTR_M_env.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))
M_nutrient_1 <- std_data[grep("M", std_data$name)[1:2], ]
M_nutrient_1$par = "nutrient"
M_nutrient_1$mu = 1

# hypoxia
setwd("D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie/Env_dependent_model/M/20/hypoxia")

# mu2
std_data <- read.table("mu2_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))
M_hypoxia_2 <- std_data[grep("M", std_data$name)[1:2], ]
M_hypoxia_2$par = "hypoxia"
M_hypoxia_2$mu = 2

# mu3
std_data <- read.table("mu3_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))
M_hypoxia_3 <- std_data[grep("M", std_data$name)[1:2], ]
M_hypoxia_3$par = "hypoxia"
M_hypoxia_3$mu = 3

# mu4
std_data <- read.table("mu4_20_PR_YOY_HCR.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))
M_hypoxia_4 <- std_data[grep("M", std_data$name)[1:2], ]
M_hypoxia_4$par = "hypoxia"
M_hypoxia_4$mu = 4

# mu1
std_data <- read.table("mu1_23_PR_YOY_HCR_OHTR_M_env.std", skip = 1, 
                       col.names = c("index", "name", "value", "std_dev"))
M_hypoxia_1 <- std_data[grep("M", std_data$name)[1:2], ]
M_hypoxia_1$par = "hypoxia"
M_hypoxia_1$mu = 1

# combine all results
M_env_results = rbind(M_hypoxia_1,M_hypoxia_2,M_hypoxia_3,M_hypoxia_4,M_ice_on_1,M_ice_on_2,M_ice_on_3,M_ice_on_4,M_nutrient_1,M_nutrient_2,M_nutrient_3,M_nutrient_4)

M_env_results$low <- M_env_results$value - 1.96 * M_env_results$std_dev
M_env_results$high <- M_env_results$value + 1.96 * M_env_results$std_dev
M_env_results = M_env_results[M_env_results$name=="M_env_impact",]

write.csv(M_env_results,"D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie/Figure/Final/M_env_results.csv",row.names = FALSE)

# Set order and labels

M_env_results$par <- factor(
  M_env_results$par,
  levels = c("ice-on", "nutrient","hypoxia"),
  labels = c("Ice-on duration","Total phosphorus loads", "Hypoxia extent")
)

# Relabel mu panels
M_env_results$mu <- factor(
  M_env_results$mu,
  levels = c(1,2,3,4),
  labels = c("MU1","MU2","MU3","MU4")
)

# Colors
main_figure_df <- M_env_results[M_env_results$par%in%c("Ice-on duration","Total phosphorus loads"),]
env_colors <- c(
  "Ice-on duration" = "#4C78A8",
  "Total phosphorus loads" = "#B22222"
)

ggplot(main_figure_df ,
       aes(x = par, y = value, color = par)) +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             color = "black",
             linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = low, ymax = high),
                width = 0.2) +
  facet_wrap(~mu, nrow = 1) +
  scale_color_manual(values = env_colors) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),      # remove crowded labels
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = "Environmental factors",
    y = "Effect on natural mortality",
    color = NULL
  )

ggsave(
  "D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie/Figure/Final/environment_effects_M_main_figure.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 600
)

sm_figure_df <- M_env_results[M_env_results$par%in%c("Total phosphorus loads","Hypoxia extent"),]

env_colors <- c(
  "Total phosphorus loads" = "#B22222",
  "Hypoxia extent"   = "#B22222"
)

env_shapes <- c(
  "Total phosphorus loads" = 16,  # Circle
  "Hypoxia extent"   = 17   # Triangle
)

ggplot(sm_figure_df,
       aes(x = par, y = value, color = par, shape = par)) +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             color = "black",
             linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = low, ymax = high),
                width = 0.2) +
  facet_wrap(~mu, nrow = 1) +
  scale_color_manual(
    values = env_colors,
    labels = c("Total phosphorus loads", "Hypoxia extent")
  ) +
  scale_shape_manual(
    values = env_shapes,
    labels = c("Total phosphorus loads", "Hypoxia extent")
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = "Environmental factors",
    y = "Effect on natural mortality",
    color = NULL,
    shape = NULL
  )

ggsave(
  "D:/OneDrive/Postdoctor/Project_5_yellow_perch_lake_erie/Figure/Final/environment_effects_M_sm_figure.png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 600
)
