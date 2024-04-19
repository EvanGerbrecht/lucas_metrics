
library(MASS)

# select which y variable you are trying to predict
y.variable = "canopy_fuel"

# subsetting the dataframe to only have one predictor variable, but for you this might not be needed for only stand density
predictor.metrics = dplyr::select(master.table.metrics, y = all_of(y.variable), zmax:voxel.Oligophotic)

# model with all variables
fullModel = lm(y ~ ., data = predictor.metrics)
# model with the intercept only
nullModel = lm(y ~ 1, data = predictor.metrics)

model = stepAIC(nullModel, # start with a model containing no variables
                       direction = 'both', # run forward selection
                       scope = list(upper = fullModel, # the maximum to consider is a model with all
                                    lower = nullModel),
                       steps = 5, # limit the amount of predictor variables the model can use
                       trace = 0) # do not show the step-by-step process of model selection

summary(model)

# create dataframe with predicted values
gg.dt = predictor.variables %>%
  mutate(obs = y,
         predictions = as.numeric(model$fitted.values))

# plot the results!
ggplot(gg.dt, aes(x = obs, y = predictions)) +
  geom_point(colour = "red", size = 3) +
  geom_abline() +
  coord_equal() +
  theme_bw() +
  xlab("X label") +
  ylab("Y label")

# Example of manually made model, use if the StepAIC isnt working
test2model = lm(y ~ lad.lai + lad.lad_min + voxel.vFRcanopy, predictor.metrics.canopy)
summary(test2model)
