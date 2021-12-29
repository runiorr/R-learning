library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

heart.data %>% summary

cor(heart.data$biking, heart.data$smoking)

hist(heart.data$heart.disease)

plot(heart.disease ~ biking + smoking, data = heart.data)

heart.disease.lm <- lm(heart.disease ~ biking + smoking, data = heart.data)

heart.disease.lm %>% summary

par(mfrow=c(2,2))
plot(heart.disease.lm)
par(mfrow=c(1,1))

plotting.data <- expand.grid(
  biking = seq(min(heart.data$biking), max(heart.data$biking), length.out=30),
  smoking=c(min(heart.data$smoking), mean(heart.data$smoking), max(heart.data$smoking)))

plotting.data$predicted.y <- predict.lm(heart.disease.lm, newdata = plotting.data)

plotting.data$smoking <- round(plotting.data$smoking, digits = 2)

plotting.data$smoking <- as.factor(plotting.data$smoking)

heart.plot <- ggplot(heart.data, aes(x=biking, y=heart.disease)) +
  geom_point()

heart.plot <- heart.plot +
  geom_line(data=plotting.data, aes(x=biking, y=predicted.y, color=smoking), size=1.25)

heart.plot <- 
  heart.plot +
    theme_bw() +
    labs(title = "Taxas de doenças cardíacas (% da população) \n como uma função de pedalar ao trabalho e fumar",
         x = "Pedalar até o trabalho (% da população)",
         y = "Doenças cardíacas (% da população)",
         color = "Fumar \n (% da população)")

heart.plot + annotate(geom="text", x=30, y=1.75, label=" = 15 + (-0.2*biking) + (0.178*smoking)")

heart.plot
