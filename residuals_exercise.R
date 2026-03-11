library(ggplot2)
data("mtcars"); head(mtcars)

d <- mtcars
fit <- lm(mpg ~ wt, data = d) # fit the model
d$predicted <- predict(fit)   # Save the predicted values
d$residuals <- residuals(fit) # Save the residual values

ggplot(d, aes(x = wt, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = wt, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()


plot(fit, which=1, col=c("blue")) # Residuals vs Fitted Plot
plot(fit, which=2, col=c("red"))  # Q-Q Plot
plot(fit, which=3, col=c("blue"))  # Scale-Location Plot
plot(fit, which=5, col=c("blue"))  # Residuals vs Leverage

###############

# data transformation

mammal = c("Goat", "Sheep", "Deer", "Porcupine", "Bear", "Hippo", "Horse", "Camel", "Zebra", "Giraffe", "Elephant")
weight = c(2.75, 4, 0.48, 1.5, 0.37, 50, 30, 40, 40, 98, 113)
gestation = c(155, 175, 190, 210, 213, 243, 340, 380, 390, 457, 670)
birth_data = data.frame(mammal, weight, gestation)

birth_data$lnGestation <- log(birth_data$gestation)
model_transf = lm(lnGestation ~ weight, data = birth_data)
model = lm(gestation ~ weight, data = birth_data)



birth_data$lnWeight <- log(birth_data$weight)


summary(model)
summary(model_transf)



plot(model, which=1, col=c("blue")) # Residuals vs Fitted Plot
plot(model_transf, which=1, col=c("blue")) # Residuals vs Fitted Plot

plot(model, which=2, col=c("red"))  # Q-Q Plot
plot(model_transf, which=2, col=c("red"))  # Q-Q Plot

plot(model, which=3, col=c("blue"))  # Scale-Location Plot
plot(model_transf, which=3, col=c("blue"))  # Scale-Location Plot

plot(model, which=5, col=c("blue"))  # Residuals vs Leverage
plot(model_transf, which=5, col=c("blue"))  # Residuals vs Leverage


birth_data$predicted <- predict(model)   # Save the predicted values
birth_data$residuals <- residuals(model) # Save the residual values

ggplot(birth_data, aes(x = weight, y = gestation)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = weight, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()


birth_data$predicted_trans <- predict(model_transf)   # Save the predicted values
birth_data$residuals_trans <- residuals(model_transf) # Save the residual values

ggplot(birth_data, aes(x = weight, y = lnGestation)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = weight, yend = predicted_trans), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals_trans), size = abs(residuals_trans))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted_trans), shape = 1) +
  theme_bw()




ggplot(birth_data, aes(x = weight, y = lnGestation)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal()

ggplot(birth_data, aes(x = weight, y = gestation)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal()


