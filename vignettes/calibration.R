## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(dplyr)
library(ggplot2)
library(TriDimRegression)

## ----raw data, echo=TRUE, message=FALSE, warning=FALSE, fig.width= 5, out.width = "100%", fig.asp= 3/4, dpi = 200----

ggplot(data= EyegazeData, aes(x= x, y= y, color= target, fill= target)) +
  geom_point(data= EyegazeData %>% group_by(target, target_x, target_y) %>% summarise(.groups="drop"),
             aes(x= target_x, y= target_y), shape= 21, size= 10, fill= 'white') + 
  geom_point(alpha= 0.5, shape= 21, show.legend=FALSE) + 
  ggtitle('Raw eye gaze')

## ----Adjust gaze, fig.width= 5, out.width = "100%", fig.asp= 3/4, dpi = 200----
lm2aff <- fit_transformation(target_x + target_y ~ x + y,  EyegazeData, transformation = 'affine')
adjusted_gaze <- predict(lm2aff, summary=TRUE, probs=NULL)
colnames(adjusted_gaze) <- c('adjX', 'adjY')
adjusted_gaze <- cbind(EyegazeData, adjusted_gaze)


ggplot(data= adjusted_gaze, aes(x= adjX, y= adjY, color= target, fill= target)) +
  geom_point(data= adjusted_gaze %>% group_by(target, target_x, target_y) %>% summarise(.groups="drop"),
             aes(x= target_x, y= target_y), shape= 21, size= 10, fill= 'white') + 
  geom_point(alpha= 0.5, shape = 21, show.legend = FALSE) + 
  xlab('x')+
  ylab('y')+
  ggtitle('Adjusted eye gaze')

