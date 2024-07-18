#load in data
mbb_team_box <- hoopR::load_mbb_team_box()
mbb_team_box$num_games <- 1
mbb_team_box$is_home_team <- mbb_team_box$team_home_away=="home"
mbb_team_box <- mbb_team_box %>%
  rename(offense = team_slug, defense = opponent_team_slug)
#build team matrices
team_matrix_offense <- model.matrix(~ offense + is_home_team, data = mbb_team_box)
team_matrix_defense <- model.matrix(~ defense + is_home_team, data = mbb_team_box)
#combine into one matrix
team_matrix <- cbind(team_matrix_offense, team_matrix_defense)

y <- mbb_team_box$team_score
w <- mbb_team_box$num_games

#apply regularized regression model
model_rasch <- glmnet::cv.glmnet(                                                                  
  x = team_matrix,                                                                                          
  y = y,                                                                                          
  weights = w,                                                                                    
  alpha = 0,          #ridge regression                                                                            
  standardize = FALSE                                                                             
)
#extract team coefficients
coef = coef(model_rasch , s = 'lambda.min')[ , 1]
head(coef)
intercept <- coef[1]
coef_no_int <- coef[-1]
coef_no_int[1]
midpoint <- length(coef_no_int)/2
coef_offense <- coef_no_int[1:midpoint]
coef_defense <- coef_no_int[(midpoint+1):length(coef_no_int)]
coef_offense[3]
coef_defense[3]
unique(mbb_team_box$team_id)
head(sort(coef_offense, decreasing=TRUE))
head(sort(coef_offense, decreasing=FALSE))
head(sort(coef_defense, decreasing=FALSE))
head(sort(coef_defense, decreasing=TRUE))
#take out home-court advantage for plotting
coef_offense_teams <- coef_offense[-length(coef_offense)]
coef_defense_teams <- coef_defense[-length(coef_defense)]

plot(coef_offense_teams, coef_defense_teams)

library(ggplot2)
library(ggimage)
library(showtext)
# Extract unique teams with logos
teams_with_logos <- mbb_team_box %>%
  filter(!is.na(team_logo)) %>%
  distinct(offense, team_logo)

# Create team_logo_data data frame
team_logo_data <- data.frame(
  team_id = teams_with_logos$offense,
  logo_url = teams_with_logos$team_logo
)
coef_data <- data.frame(off_eff = coef_offense_teams, def_eff = coef_defense_teams)
coef_data$team_id <- sub("^offense", "", rownames(coef_data))
coef_data_teams <- coef_data[-1, ]
merged_data <- merge(coef_data_teams, team_logo_data, by = "team_id")

# Function to read and display images in ggplot
read_img <- function(x) readPNG(url(x), native = TRUE)

# Initialize showtext for custom fonts
showtext_auto()

# Create a new column indicating whether a team is in the top 10 for offense
merged_data$top10_offense <- merged_data$team_id %in% merged_data$team_id[order(merged_data$off_eff, decreasing = TRUE)][1:10]

# Create a new column indicating whether a team is in the top 10 for defense
merged_data$top10_defense <- merged_data$team_id %in% merged_data$team_id[order(merged_data$def_eff, decreasing = FALSE)][1:10]


# Create a plot for all teams
library(ggplot2)
final_plot <- ggplot(merged_data, aes(x = off_eff, y = def_eff)) +
  geom_point( size = 3) +
  theme_minimal() +
  labs(title = "Offense vs Defense Efficiency",
       x = "Offense Efficiency",
       y = "Defense Efficiency") +
  theme(legend.position = "none")

# Add logos for the top 10 teams
final_plot <- final_plot +
  geom_image(aes(x = off_eff, y = def_eff, image = logo_url), size = 0.07, data = filter(merged_data, top10_offense | top10_defense))

#add caption
final_plot <- final_plot + labs(title = "CBB Offensive vs Defensive Efficiency (Regularized Regression Model)",
                                subtitle = 'Efficienies for all Division 1 teams in 2023-24. Top 10 offensive/defensive teams are shown with logos',
                                x = 'Offensive Efficiency',
                                y = 'Defensive Efficiency')
# Add red dotted lines at x = 0 and y = 0
final_plot <- final_plot +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") + theme(
    plot.title = element_text(size = 20, face = "bold")
  )

print(final_plot)
ggsave("~/Downloads/CBB_efficiency.png", final_plot, width = 3.75, height = 3)
