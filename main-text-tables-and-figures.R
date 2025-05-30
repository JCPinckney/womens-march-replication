# Replication Script for "From Protest to Power: How the Women's March Worked"


# Prep for Analysis ####

# Set Working Directory to Current Script's Directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load Packages

library(tidyverse) # v. 2.0.0
library(stargazer) # v. 5.2.3
library(texreg) # v. 1.39.4
library(ivreg) # v. 0.6-5
library(ivtools) # v. 2.3.0
library(marginaleffects) # v. 0.26.0
library(sf) # v. 1.0-21
library(rnaturalearth) # v. 1.0.1
library(tigris) # v. 2.2.1

# Load Data

# March-Level Data for Mapping
womens.march.map.data <- read_rds("womens-marches.rds") 

# County-Level Data for Analysis
working.data <- read_rds("working-data.rds") 

# Generate Labels for Regression Tables

main.coef.map <- list("log.marchers" = "Marchers (log)",
                      "marchers.pc" ="Marchers PC",
                      "dem.share.14" = "Dem. Vote Share 2014",
                      "dem.2014.donations.share" = "Dem. Donat. Share 2014",
                      "urban.binary" = "Rural/Urban",
                      "white.perc" = "Percent White",
                      "hisp.perc" = "Percent Hispanic",
                      "black.perc" = "Percent Black",
                      "unemp.2017" = "Unemployment Rate",
                      "poverty.2017" = "Poverty Rate",
                      "log.median.inc.2017" = "Median Income (log)",
                      "log.pop" = "Population (log)",
                      "near.march.log" = "Nearby Marchers (log)",
                      "wtd.rain.historic" = "Historic Av. Prcp.",
                      "wtd.temp.historic" = "Historic Av. Temp.",
                      "(Intercept)" = "Constant")


# TABLES ####

# Table 1: Summary Statistics 

sumstats.data <- working.data %>% select(log.marchers,marchers.pc,wtd.rain,temp.diff, # IVs and Instruments
                                         dem.share.18,dem.2018.donations.share,indiv.binary,indiv.groups, #DVs
                                         dem.share.14,dem.2014.donations.share,white.perc,black.perc,hisp.perc,log.pop,
                                         unemp.2017,log.median.inc.2017,poverty.2017,near.march.log,
                                         wtd.rain.historic,wtd.temp.historic) %>% #Controls
  as.data.frame()

stargazer(sumstats.data,
          label = "tab:sumstats",
          covariate.labels = c("Marchers (log)","Marchers (per capita)","Precipitation (in)","Temp. Deviation",
                               "Dem. Vote Share 2018","Dem. Donations Share 2018","Indivisible (binary)","Indivisible (Num groups)",
                               "Dem. Vote Share 2014","Dem Donations Share 2014","Percent White","Percent Black","Percent Hispanic",
                               "Total Population (log)","Unemployment Rate","Median Income (log)","Poverty Rate",
                               "Nearby Large Marchers (log)", "Av. Historic Prcp. (in)","Av. Historic Temp."),
          header = F,
          title = "Summary Statistics",
          type = "html",
          out = "sumstats.doc"
)

# Table 2: Democratic Vote Share Models

naive.mod.1 <- lm(dem.share.18 ~ log.marchers
                  + dem.share.14 + urban.binary + white.perc + hisp.perc + black.perc + unemp.2017 + poverty.2017 + log.median.inc.2017 + log.pop + near.march.log, data = working.data)

naive.mod.2 <- lm(dem.share.18 ~ marchers.pc 
                  + dem.share.14 + urban.binary + white.perc + hisp.perc + black.perc + unemp.2017 + poverty.2017 + log.median.inc.2017 + log.pop + near.march.log, data = working.data)

full.rain.mod.1 <- ivreg(dem.share.18 ~  
                           dem.share.14 + urban.binary + white.perc + hisp.perc + black.perc + unemp.2017 + poverty.2017 + log.median.inc.2017 + log.pop + near.march.log + wtd.rain.historic + wtd.temp.historic       
                         | log.marchers
                         | wtd.rain + temp.diff
                         , data = working.data)


full.rain.mod.2 <- ivreg(dem.share.18 ~ 
                           dem.share.14 + urban.binary + white.perc + hisp.perc + black.perc + unemp.2017 + poverty.2017 + log.median.inc.2017 + log.pop + near.march.log + wtd.rain.historic + wtd.temp.historic  
                         | marchers.pc
                         | wtd.rain + temp.diff
                         , data = working.data)

htmlreg(list(naive.mod.1,naive.mod.2,full.rain.mod.1,full.rain.mod.2),
        custom.coef.map = main.coef.map,
        custom.header = list("Naive OLS" = 1:2,"2SLS IV Regression" = 3:4),
        digits = 3,
        include.rsquared = F,
        include.adjrs = F,
        label = "tab:vote-share",
        caption = "2018 Democratic Vote Share Models",
        caption.above = T,
        file = "vote-share.doc"
)

# Table 3: Indivisible Group Formation Models

naive.indiv.mod.1 <- lm(indiv.binary ~ log.marchers
                        + dem.share.14 + urban.binary + white.perc + hisp.perc + black.perc + unemp.2017 + poverty.2017 + log.median.inc.2017 + log.pop + near.march.log, data = working.data)

naive.indiv.mod.2 <- lm(indiv.binary ~ marchers.pc 
                        + dem.share.14 + urban.binary + white.perc + hisp.perc + black.perc + unemp.2017 + poverty.2017 + log.median.inc.2017 + log.pop + near.march.log, data = working.data)

# 2-Stage Model

rain.indiv.mod.1 <- ivreg(indiv.binary ~  
                            dem.share.14 + urban.binary + white.perc + hisp.perc + black.perc + unemp.2017 + poverty.2017 + log.median.inc.2017 + log.pop + near.march.log + wtd.rain.historic + wtd.temp.historic
                          | log.marchers
                          | wtd.rain + temp.diff
                          , data = working.data)


rain.indiv.mod.2 <- ivreg(indiv.binary ~ 
                            dem.share.14 + urban.binary + white.perc + hisp.perc + black.perc + unemp.2017 + poverty.2017 + log.median.inc.2017 + log.pop + near.march.log + wtd.rain.historic + wtd.temp.historic  
                          | marchers.pc
                          | wtd.rain + temp.diff
                          , data = working.data)

htmlreg(list(naive.indiv.mod.1,naive.indiv.mod.2,rain.indiv.mod.1,rain.indiv.mod.2),
        custom.coef.map = main.coef.map,
        custom.header = list("Naive OLS" = 1:2,"2SLS IV Regression" = 3:4),
        digits = 3,
        include.rsquared = F,
        include.adjrs = F,
        label = "tab:indivisible",
        caption = "Indivisible Group Formation Models",
        caption.above = T,
        file = "indivisible-tab.doc"
)


# Table 4: Democratic Donation Share Models

naive.donations.mod.1 <- lm(dem.2018.donations.share ~ log.marchers
                            + dem.2014.donations.share + urban.binary + white.perc + hisp.perc + black.perc + unemp.2017 + poverty.2017 + log.median.inc.2017 + log.pop + near.march.log, data = working.data)

naive.donations.mod.2 <- lm(dem.2018.donations.share ~ marchers.pc 
                            + dem.2014.donations.share + urban.binary + white.perc + hisp.perc + black.perc + unemp.2017 + poverty.2017 + log.median.inc.2017 + log.pop + near.march.log, data = working.data)

# 2-Stage Model

rain.donat.mod.1 <- ivreg(dem.2018.donations.share ~  
                            dem.2014.donations.share + urban.binary + white.perc + hisp.perc + black.perc + unemp.2017 + poverty.2017 + log.median.inc.2017 + log.pop + near.march.log + wtd.rain.historic + wtd.temp.historic 
                          | log.marchers
                          | wtd.rain + temp.diff
                          , data = working.data)


rain.donat.mod.2 <- ivreg(dem.2018.donations.share ~ 
                            dem.2014.donations.share + urban.binary + white.perc + hisp.perc + black.perc + unemp.2017 + poverty.2017 + log.median.inc.2017 + log.pop + near.march.log + wtd.rain.historic + wtd.temp.historic  
                          | marchers.pc
                          | wtd.rain + temp.diff
                          , data = working.data)

htmlreg(list(naive.donations.mod.1,naive.donations.mod.2,rain.donat.mod.1,rain.donat.mod.2),
        custom.coef.map = main.coef.map,
        custom.header = list("Naive OLS" = 1:2,"2SLS IV Regression" = 3:4),
        digits = 3,
        include.rsquared = F,
        include.adjrs = F,
        label = "tab:donations",
        caption = "Democratic Donation Share Models",
        caption.above = T,
        file = "donations-tab.doc"
)


# FIGURES ####

# Figure 1: Women's Marches Size and Location

plot.countries <- ne_countries(scale = "large") %>% 
  st_transform(crs = "NAD83")

plot.states <- states(cb = T,resolution = "20m") %>% 
  filter(!(STUSPS %in% c("AK","HI","VI","MP","GU","AS","PR")))

plot.marches <- womens.march.map.data %>% 
  st_transform(crs = "NAD83")

protest.map <- ggplot() +
  geom_sf(data = plot.countries,fill = "darkgray") +
  geom_sf(data = plot.states,fill = "antiquewhite") +
  geom_sf(data = filter(plot.marches, !(state %in% c("AK","HI","--"))),
          aes(size = est_best), alpha = 0.4) +
  scale_size_continuous(range = c(1,12),breaks = c(1000,10000,100000,500000),labels = scales::comma,name = "Estimated\nParticipants") +
  labs(caption = "Size and Location from Crowd Counting Consortium (crowdcounting.org)") +
  coord_sf(xlim = c(-125,-66),ylim = c(24,50)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "lightblue"),
        legend.key = element_rect(fill = "transparent",color = "transparent"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave("protest-map.png",protest.map,bg = "transparent",width = 7,height = 4,dpi = 1200)  

# Figure 2: Marginal Effects of Instrumented Participation on Democratic Vote Share

# Produce predictions for marginal effects figure

fig.data <- working.data %>% 
  reframe(across(c(dem.share.14,white.perc,hisp.perc,black.perc,unemp.2017,poverty.2017,log.median.inc.2017,log.pop,wtd.rain.historic,wtd.temp.historic,wtd.rain,temp.diff), ~ mean(.,na.rm = T)),
          urban.binary = 1,
          near.march.log = 0,
          log.marchers = log1p(c(0,100,1000,10000,100000)),
          marchers = c(0,100,1000,10000,100000))

predictions <- predict(full.rain.mod.1,fig.data)

fig.data <- mutate(fig.data,predicted = predictions,
                   lwr = predicted - 1.96*summary(full.rain.mod.1)$coefficients[2,2],
                   upr = predicted + 1.96*summary(full.rain.mod.1)$coefficients[2,2])

marg.effects <- ggplot(fig.data,aes(x = as.factor(marchers),y = predicted,ymin = lwr,ymax = upr)) +
  geom_errorbar(width = 0.2) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Predicted Democratic Vote Share", 
       x = "Number of Women's March Participants",
       caption = "Predicted Democratic vote share from instrumented model.\nError bars are a 95% confidence interval.") +
  theme_bw()

ggsave("marg-effects.png",marg.effects,bg = "transparent",width = 6,height = 4,dpi = 1200)

# Produce Simulated 2018 Election Results with No Women's March for in-text discussion

no.march.sim.data <- working.data %>% 
  mutate(log.marchers = 0) 

sim.fig.data <- select(working.data,dem.share.18,total.vote.18,dem.vote.18,marchers) %>% 
  mutate(sim.dem.vote.share = predict(full.rain.mod.1,newdata = no.march.sim.data),
         sim.dem.vote.share = if_else(is.nan(sim.dem.vote.share) | marchers == 0,dem.share.18,sim.dem.vote.share),
         sim.dem.votes = sim.dem.vote.share*total.vote.18,
         sim.vote.diff = dem.vote.18 - sim.dem.votes)

