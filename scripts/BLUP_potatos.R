library(tidyverse)
library(readxl)
library(lme4)
#If you experiment some problem and you are working we the updated version 
# you can use the following when installing lme4
# remove.packages("Matrix")
# remove.packages("lme4")
# install.packages("lme4", type = "source")
# library(lme4)
library(agricolae)

# Analisys potatos peru the Mother/Baby Trial Design
data(RioChillon)

# Lets first explore the metadata
?RioChillon

#For this example we will work with the babies data
data_g <- RioChillon$babies

head(data_g)
str(data_g)

#We first perform a fixed effect model lets evaluate if the environment represented 
# by the farmer has an effect in yield

mod_fix <- lm(yield ~ farmer,data = data_g)

summary(mod_fix)
anova(mod_fix)

# now lets fit a mixed model with a random effect on the average 
mod_ran <- lmer(yield ~ farmer +(1|clon),data = data_g)

summary(mod_ran)

# Obtaining the BLUPs from the fitted model
ranef(mod_ran)

# We can also generate a data frame to work further with 
blups <- ranef(mod_ran)$clon|> #select the element
  rownames_to_column("clon") |> #generates a new variable with the row names which in our case are the clones
  as.data.frame() |> # convert to data frame
  rename("yield_blup"=`(Intercept)`)# we rename the variable to a more convenient name

# Exploring the distribution 
blups|> ggplot(aes(x=yield_blup))+ 
  geom_density()
  theme_bw()

# We perform a bar plot with the  ranking of blups 
blups |> ggplot(aes(x=reorder(clon,yield_blup), # we define the clones in x axis but reordered by the blup value
                     y=`yield_blup`, 
                     fill=yield_blup))+ 
  geom_col()+
  labs(x="Clon",y="Yield BLUPs")+
  theme_bw()


# In the workshop we worked with an different example where it 
# made sense to perform a random effect to the slope (sowing date effect).
# The data would not be openly available but yes the code

## reading data
# data_g <- read_xlsx("example_data/SANAZNIRcsv_ws.xlsx",
#                     na = c("NA","."))
# 
# head(data_g)
# str(data_g)
# #fixed effect model
# mod_fix_sd <- lm(Proteiness ~sowingdates,data = data_g)
# 
# summary(mod_fix_sd)
# 
# # random effect on the average 
# mod_ran <- lmer(Proteiness ~ sowingdates +(1|genotypes),data = data_g)
# 
# summary(mod_ran)
# 
# mod_ran
# 
# # getting the BLUPs
# mod_ran
# ranef(mod_ran)
# 
# # random effect on average and slope
# mod_ran_sd <- lmer(Proteiness ~ sowingdates +(sowingdates|genotypes),data = data_g)

# # random effect on average and slope
# mod_ran_sd <- lmer(Proteiness ~ sowingdates +(sowingdates|genotypes),
#                    data = data_g)
# 
# mod_ran_sd
# summary(mod_ran_sd)
# 
# # getting the BLUPs
# ranef(mod_ran_sd)
# 
# blups <- ranef(mod_ran_sd)$genotypes %>% 
#   as.data.frame() %>% mutate(mu=18.84159+`(Intercept)`, # the intercept for each genotype
#                              sowslope=0.45545+sowingdates, # the slope (response to the sowing date of each genotype)
#                              genotye=row_number())
#   
# type <- data_g %>% filter(sowingdates==1) %>% select(Type)
# 
# blups <- blups %>% bind_cols(type)
# 
# # Exploring  when exploreing we wanted to know how they where grouped
# 
# blups %>% ggplot(aes(x=Type,y=sowingdates))+ geom_boxplot()
# 
# blups %>% ggplot(aes(x=reorder(genotye,`(Intercept)`),
#                      y=`(Intercept)`, 
#                      fill=Type))+ geom_col()
# 
# blups %>% ggplot(aes(mu))+ geom_histogram()


