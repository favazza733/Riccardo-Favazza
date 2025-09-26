
library(rstanarm)
data<-read.csv("france_nonhousehold.csv")
data<-data[,-c(1,2,3,4,5,6,7)]
data<-data[,-c(1,5,6)]
data<-data[-c(81:240),]
data<-data[,-4]
colnames(data)[colnames(data) == "OBS_VALUE"] <- "energy_price"
data$post <- ifelse(data$TIME_PERIOD %in% c("2022-S2", "2023-S1", "2023-S2", "2024-S1", "2024-S2"), 1, 0)
data$Treatment<-ifelse(data$geo %in% c("Spain", "Portugal"),1,0)
View(data)

hicp<-read.csv("hicp2.csv")
hicp<-hicp[,-c(1,2,3,4,5)]
hicp<-hicp[,-c(4,5)]
colnames(hicp)[colnames(hicp) == "OBS_VALUE"] <- "hicp"
colnames(hicp)[colnames(hicp) == "TIME_PERIOD"] <- "year"
hicp<-hicp[-c(81:795), ]
View(hicp)

data$year<-as.numeric(substr(data$TIME_PERIOD, 1, 4))
data_comp <- merge(data, hicp, by = c("geo", "year"))
View(data_comp)

model_bayes <- stan_glm(
  energy_price ~ Treatment * post,
  data = data,
  family = gaussian(),
  prior = normal(-0.10, 0.02),       
  prior_intercept = normal(0.2, 0.05),  
  prior_aux = exponential(1),
  chains = 4, iter = 3000, seed = 42
)


model_bayes_2 <- stan_glm(
  energy_price ~ Treatment * post + hicp,
  data = data_comp,
  family = gaussian(),
  prior = normal(-0.04, 0.02),
  prior_intercept = normal(0.2, 0.05),
  prior_aux = exponential(1)
)

summary(model_bayes)

posterior <- as.matrix(model_bayes)
hist(posterior[, "Treatment:post"], 
     main = "ATET",
     xlab = "Estimated effect", breaks = 30)
plot(model_bayes, pars = "Treatment:post")
help(rstanarm)
quantile(posterior[,"Treatment:post"], 0.95)
summary(model_bayes2)

posterior_2 <- as.matrix(model_bayes_2)

hist(posterior_2[, "Treatment:post"],
     main = "ATET NON-HOUSEHOLD CONSUMERS",
     xlab = "Estimated effect", breaks=30, freq = F, col="skyblue")
quantile(posterior_2[,"Treatment:post"],0.975)


dens <- density(posterior_2[, "Treatment:post"])

plot(dens,
     main = "Non-Household posterior Density",
     xlab = "estimated effect",
     ylab = "Density",
     col = "navy",
     lwd = 2)

# Credible interval (95%)
ci <- quantile(posterior_2[, "Treatment:post"], c(0.025, 0.975))

# Add CI bounds (dashed lines)
abline(v = ci, col = "red", lty = 2)

# Add posterior mean (solid line)
abline(v = mean(posterior[, "Treatment:post"]), col = "darkblue", lwd = 2)


