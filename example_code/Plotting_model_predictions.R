# For creating a figure from predictions ----

# New data grid for range of x_variable values
# Make sure all variables that are included in the model are here 
new_data <- expand.grid(
  X_variable = seq(min(DATA$X_variable), max(DATA$X_variable, by=1)), # sequence from min to max, with interval of 1 unit
  FACTOR = unique(DATA$FACTOR), # when you want a prediction for all levels of a factor
  COVARIATE = mean(DATA$COVARIATE, na.rm=T), # Setting a continuous covariate to its mean
  RANDOM = NA # You can set the random effect to NA, or alternatively choose one level of it 
)

# Make a prediction on the new grid
pred_data <- predict(MODEL, # Your statistical model
                     newdata = new_data, # The grid you predict on
                     type = "response", # If there is a link function or a transformation - this gets the prediction into the original scale
                     se.fit = TRUE) # Get the SE of the prediciton


# Add predicted values into the new dataframe
new_data$fit <- pred$fit
new_data$se.fit <- pred$se.fit
new_data$lower <- pred$fit - 1.96 * pred$se.fit
new_data$upper <- pred$fit + 1.96 * pred$se.fit

# EXTRA: If you don't get the values in the response scale while making the prediction, you can backtransform from log manually like this
# new_data$fit <- exp(pred$fit)
# new_data$se.fit <- pred$se.fit
# new_data$lower <- exp(pred$fit - 1.96 * pred$se.fit)
# new_data$upper <- exp(pred$fit + 1.96 * pred$se.fit)

# Plot the predictions
ggplot(new_data, aes(x = X_axis, y = fit, fill=FACTOR)) +
  theme_bw() +
  #geom_point(data=Leachate_full, aes(x=as.numeric(as.character(Porevolume)), y=TP, colour=Type), alpha=0.15) +
  geom_line(color = "black") + # A line through all fitted values
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  # Error band
  
# OR: Plot the predictions overlayed on a graph with the raw data
ggplot(DATA, aes(x = X_axis, y = RESPONSE, fill=FACTOR)) +
  theme_bw() +
  geom_point() +
  geom_line(data=new_data, aes(x = X_axis, y = fit), color = "black") + # A line through all fitted values
  geom_ribbon(data=new_data, aes(x=X_axis, y=fit, ymin = lower, ymax = upper), alpha = 0.2)  # Error band
