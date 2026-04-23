# The following script describes how to perform the General additive Models (GAMs) in a stepwise processes 
# Note - this is a pipeline code and would need to be repreated for additional functional diversity/geodiversity at resolutions and species groups
# and the following datasets would need to be substituted adapt the pipeline code:
# functional diversity: FDM_STANDISED_NO_ZERO, geodiversity metrics: GEODIVERSITY_Metrics

# Load the mgcv package

install.packages('mgcv')
library(mgcv)

#STEP 1: Cross-reference the geodiversity and functional diversity data
FD_GEO <- merge(GEODIVERSITY_Metrics, FDM_STANDISED_NO_ZERO, by = "Tile_name", all.x = TRUE)

# STEP 2: Perform the GAMs with all the geodiversity metrics of interest 

model_Full <- gam('Write the functional diversity' ~ 
                    s(LAN_shannon_diversity) +
                    s(GEO_shannon_diversity) +
                    s(SD_shannon_diversity) +
                    s(RANGE_elev) +
                    s(RANGE_slope) +
                    s(RIVER_COM) +
                    s(Canal_COM), 
                  family = Gamma(link = "log"), 
                  data = FD_GEO)


Summary_model_Full <- summary(model_Full)
AIC_model_Full <- AIC(model_Full)

# examine in console
print(Summary_model_Full)
print(AIC_model_Full)

# save the results to a file
sink("Full_model_summaries_.txt")

print(Summary_model_Full)
print(AIC_model_Full)

sink()

## Perform the automated GAM ##

#STEP 3: Select the geodiversity metrics to be used in the GAMs.

# Candidate predictors
all_predictors <- c("LAN_shannon_diversity", "GEO_shannon_diversity", "SD_shannon_diversity",
                    "RANGE_elev", "RANGE_slope", "RIVER_COM", "Canal_COM")

#STEP 4: Format the null model and the formate for the candiate models

# Initialise
current_predictors <- c()
remaining_predictors <- all_predictors

# Fit the null model
current_formula <- as.formula("write the functional diversity metric ~ 1")
current_model <- gam(current_formula, family = Gamma(link = "log"), data = FD_GEO)
current_aic <- AIC(current_model)
current_summary <- capture.output(summary(current_model))

# Format of the candiate models
model_history <- list(
  list(predictors = current_predictors, model = current_model, AIC = current_aic, summary = current_summary)
)

#STEP 5: Perform the GAM automation with a inverse gaussian distrubution or gamma 

cat("Start: Null model - AIC:", round(current_aic, 2), "\n")

improved <- TRUE
while (improved) {
  improved <- FALSE
  candidate_models <- list()
  
  # Try adding predictors
  for (pred in setdiff(all_predictors, current_predictors)) {
    test_predictors <- c(current_predictors, pred)
    formula_text <- paste("write the functional diversity metric ~", paste0("s(", test_predictors, ")", collapse = " + "))
    model_formula <- as.formula(formula_text)
    
    model_test <- tryCatch(
      gam(model_formula, family = Gamma(link = "log"), data = FD_GEO),
      error = function(e) NULL
    )
    
    if (!is.null(model_test)) {
      model_summary <- capture.output(summary(model_test))
      candidate_models[[paste0("add_", pred)]] <- list(
        action = "add",
        pred = pred,
        model = model_test,
        AIC = AIC(model_test),
        predictors = test_predictors,
        summary = model_summary
      )
    }
  }
  
  # Try dropping predictors (if more than one in model)
  if (length(current_predictors) > 1) {
    for (pred in current_predictors) {
      test_predictors <- setdiff(current_predictors, pred)
      formula_text <- if (length(test_predictors) == 0) {
        "write the functional diversity metric ~ 1"
      } else {
        paste("write the functional diversity metric ~", paste0("s(", test_predictors, ")", collapse = " + "))
      }
      model_formula <- as.formula(formula_text)
      
      model_test <- tryCatch(
        gam(model_formula, family = Gamma(link = "log"), data = FD_GEO),
        error = function(e) NULL
      )
      
      if (!is.null(model_test)) {
        model_summary <- capture.output(summary(model_test))
        candidate_models[[paste0("drop_", pred)]] <- list(
          action = "drop",
          pred = pred,
          model = model_test,
          AIC = AIC(model_test),
          predictors = test_predictors,
          summary = model_summary
        )
      }
    }
  }
}
# STEP 6: Save the candidate models and examine which is the best model

# Save candidate model summaries to file
sink("Candidate_model_summaries_write the functional diversity metric.txt")
for (i in seq_along(candidate_models)) {
  cat("\n==== Model", i, "====\n")
  cat("Predictors:", paste(candidate_models[[i]]$predictors, collapse = ", "), "\n")
  cat("AIC:", candidate_models[[i]]$AIC, "\n\n")
  cat(paste(candidate_models[[i]]$summary, collapse = "\n"))
  cat("\n\n")
}
sink()

## Plot the best model using ggplot ##

# STEP 7: initialize the best model note the number of the geodiversity metrics will change dependent on the resolution and species group

best_model <- gam('write the functional diversity metric' ~ 
                    s('write the geodiversity metric1') +
                    s('write the geodiversity metric2') +
                    s('write the geodiversity metric3')+
                    s('write the geodiversity metric4')
                  family = gamma(link = "log"), 
                  data = FD_GEO)

# STEP 8: Calculate how each geodiversity predictor affects the functional diversity by itself,
# while all other predictors are held constant at their mean.

predict_data_x1 <- data.frame(
  'geodiversity metric1' = seq(        # note this need be changed for step 8.1 in the for the other predictors 
    min(FD_GEO$'geodiversity metric1'), # note this need be changed for step 8.1 in the for the other predictors
    max(FD_GEO$'geodiversity metric1'), # note this need be changed  for step 8.1 in the for the other predictors
    length.out = 100
  ),
  'geodiversity metric2' = mean(FD_GEO$'geodiversity metric2'),
  'geodiversity metric3'  = mean(FD_GEO$'geodiversity metric3'),
  'geodiversity metric4'  = mean(FD_GEO$'geodiversity metric4')
)


predict_data_x1$Effect <- 'geodiversity metric1'
predict_data_x1$y_pred <- predict(best_model, newdata = predict_data_x1, type = "terms")[, "s('geodiversity metric1')"]

# STEP 8.1: Repeat this proccess for the length of the number of predictors so that 
# each geodiversity metric can show its effects with functional diversity

# STEP 9: Combine geodiversity predictor effects datasets
combined_data <- rbind(predict_data_x1, predict_data_x2,
                       predict_data_x3, predict_data_x4)

# STEP 10: Add predicted values and confidence intervals to your data
pred <- predict(best_model, newdata = combined_data, se.fit = TRUE)

combined_data$y_pred <- pred$fit
combined_data$ci_lower <- pred$fit - 1.96 * pred$se.fit  # Lower bound
combined_data$ci_upper <- pred$fit + 1.96 * pred$se.fit  # Upper bound

#STEP 11: generate the plot
ggplot(combined_data, aes(
  x = ifelse(Effect == "'geodiversity metric1'", 'geodiversity metric1', 
             ifelse(Effect == "'geodiversity metric2'", 'geodiversity metric2', ifelse(Effect == "'geodiversity metric3'", 'geodiversity metric3',
                                                                                       'geodiversity metric4'))),
  y = y_pred, 
  color = Effect
)) +
  # Add the confidence interval ribbon with a more subtle color
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = Effect), 
              alpha = 0.3) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("'geodiversity metric1'" = "#d62728", "'geodiversity metric2'" = "#FFCC33",
                                "'geodiversity metric3'" = "#2ca02c", "'geodiversity metric4'" = "#ff7f0e"
                                  
  )) +
  scale_fill_manual(values = c("'geodiversity metric1'" = "#d62728", "'geodiversity metric2'" = "#FFCC33",
                               "'geodiversity metric3'" = "#2ca02c", "'geodiversity metric4'" = "#ff7f0e"
  ))  +
  labs(
    title = "Effects of Landscape Features on write functional diversity metric",
    subtitle = "Model predictions of write functional diversity metric confidence intervals for different landscape features",
    x = "Landscape Features",
    y = "write functional diversity metric",
    color = "Landscape Feature",
    fill = "Confidence Interval"
  ) +
  theme_classic(base_size = 14) +  # Clean theme with larger base font size
  theme(
    legend.position = "none", 
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  facet_wrap(~Effect, scales = "free_x", labeller = labeller(Effect = c("'geodiversity metric1'" = "Edit for desire label",
                                                                        "'geodiversity metric2'" = "Edit for desire label",
                                                                        "'geodiversity metric3'" = "Edit for desire label",
                                                                        "'geodiversity metric4'" = "Edit for desire label"
                                                                        
  )))


