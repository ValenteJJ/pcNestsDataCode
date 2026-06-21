
rm(list=ls())

load('modelSpAbund.RData')
all = modelSpAbund
rm(modelSpAbund)

load('modelSpAbundSinging.RData')
singing = modelSpAbund
rm(modelSpAbund)


plots = st_read('plots.shp')
# pcLocations = read.csv("pcLocations.csv")
# surveys = read.csv('surveys.csv') %>% 
#   mutate(date = as.Date(date, format='%m/%d/%Y'))
# nests = read.csv('nests.csv')
# birds = read.csv('birds.csv') %>% 
#   mutate(date = as.Date(date, format='%m/%d/%Y'))




predDf = expand.grid(plot = unique(plots$plot),
                     year = unique(plots$year)) %>%
  mutate(plot = as.character(plot),
         year = as.factor(year))

mm = model.matrix(~plot + year + year*plot, data=predDf)

predMCMCAll = predict(all, mm, ignore.RE=T)
predMCMCSinging = predict(singing, mm, ignore.RE=T)


predictedAll = data.frame(t(predMCMCAll$mu.0.samples)) %>%
  mutate(plot = predDf$plot, year = predDf$year) %>%
  pivot_longer(cols=c(-plot, -year), names_to='iteration') %>%
  mutate(iteration = str_replace(iteration, 'X', '')) %>%
  mutate(iteration = as.numeric(as.character(iteration))) %>%
  mutate(year = as.integer(as.character(year))) %>%
  mutate(plot = as.character(plot)) %>% 
  group_by(plot, year) %>% 
  summarise(allEst = mean(value),
            allLcl = quantile(value, probs=0.025),
            allUcl = quantile(value, probs=0.975)) %>% 
  ungroup()

predictedSinging = data.frame(t(predMCMCSinging$mu.0.samples)) %>%
  mutate(plot = predDf$plot, year = predDf$year) %>%
  pivot_longer(cols=c(-plot, -year), names_to='iteration') %>%
  mutate(iteration = str_replace(iteration, 'X', '')) %>%
  mutate(iteration = as.numeric(as.character(iteration))) %>%
  mutate(year = as.integer(as.character(year))) %>%
  mutate(plot = as.character(plot)) %>% 
  group_by(plot, year) %>% 
  summarise(singEst = mean(value),
            singLcl = quantile(value, probs=0.025),
            singUcl = quantile(value, probs=0.975)) %>% 
  ungroup()

tmp = predictedAll %>% 
  full_join(predictedSinging, by=c('plot', 'year'))

ggplot(tmp, aes(x=allEst, y=singEst))+
  geom_point()+
  geom_errorbar(aes(ymin=singLcl, ymax=singUcl), alpha=0.3)+
  geom_errorbarh(aes(xmin=allLcl, xmax=allUcl), alpha=0.3)+
  geom_abline(intercept=0, slope=1, linetype='dashed')+
  theme_bw()
