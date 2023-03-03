# IMPORT LIBRARIES --------------------
library(janitor)
library(tidyverse)
#___________________________________

# IMPORT DATA ----------------------
frogs <- read_csv('data/frogs_messy_data.csv')
#____________________________________

# Clean names
frogs <- janitor::clean_names(frogs)


# Rename coloumn names
frogs <- rename(frogs,
                'frogspawn_id'='frogspawn_sample_id',
                'temp_13'='temperature13',
                'temp_18'='temperature18',
                'temp_25'='temperature25')

# Check the coloumn names
colnames(frogs) 

# Sum the NA values for each coloumn
colSums(is.na(frogs))
 
frog_longer <- pivot_longer(frogs,cols = temp_13:temp_25,names_to = 'temp', values_to='days')


frog_longer_NA <- subset(frog_longer, is.finite(as.numeric(days)))

frog_longer_plot <- ggplot(frog_longer, aes(x= temp,y=days))+
  geom_boxplot()+
  geom_point()+
  geom_jitter()
frog_longer_plot

frog_longer_lm <- lm(days~temp, data=frog_longer_NA)
summary(frog_longer_lm)

means <- emmeans::emmeans(frog_longer_lm, specs = ~ temp)
means

means %>%
  as_tibble()%>%
  ggplot(aes(x=temp, y=emmean))+
  geom_pointrange(aes(ymin=lower.CL,ymax=upper.CL))

anova(frog_longer_lm)

performance::check_model(frog_longer_lm, check=c("normality","qq"))

performance::check_model(frog_longer_lm, check="homogeneity")

plot(frog_longer_lm, which=c(1,3))


                