# Model draft of Table 1

# EU (experimental unit)=pot
# response var= times of roots to appear (day)
# predictor var = treatment

rootbiocharday <- lm(daysarrival~Treatments, data=rb_data)

# Model draft of Table 2

# EU = plot (=bag)
# response var= decomposition rate
# predictor var = shade treatment (2 levels) + amendments (5 levels)
# Split-plot design

# Simple model ignoring the experimental design - including an interaction
decomposition_mod <- lm(decomposition_rate ~ shade + amendment + 
                          shade:amendment, 
                        data=cocoa)

# response var= litter weight
# Added predictor variable of time
# including split-plot design

litterweight_mod <- lm(litter_weight ~ shade + 
                         amendment + 
                         time + 
                         shade/amendment, #amendment nested in shade
                       data=cocoa)

# Observational data
# comparing different models (2 levels)
# landuse types (3 levels)

temperature_mod <- lm(temperature ~ landuse*model, data=tempmods)

# Table 3

# Observational data
# response var= GHG emissions
# predictor vars = yield, feed, manure

globalwarm_mod <- lm(GHGemissions ~ yield + feed + manure, data=GHGdata)
  

# Model with excessive interactions
lm(resp ~ var1 * var2 * var3 * var4* var5, data=data)

# Instead, can be written with only the interactions that you have a hypothesis for

lm(resp ~ var1 + var2 + var3 + var4 + var5 +
     var1:var2 + 
     var1:var3 + 
     var1:var2:var3, 
   data=data)

