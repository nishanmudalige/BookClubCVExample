library(ISLR)
library(ISLR2)

data(Auto)

n = nrow(Auto)
MSE = NULL

# 6, 9, 12
set.seed(6)

training_set_indices = sample(n, n/2, replace = F)
validation_set_indices = (1:n)[!(1:n %in% training_set_indices)]

training_set = Auto[training_set_indices, ]
validation_set = Auto[validation_set_indices, ]

for(i in 1:10){
  
  n_valid = nrow(validation_set)

  model = lm(mpg ~ poly(horsepower, i), data = training_set)

  fitted_values = predict(model, validation_set)

  MSE[i] = (sum((validation_set$mpg - fitted_values)^2)) / (n_valid - length(model$coefficients))
}


plot(MSE, type = "b", pch = 16)




set.seed(1)

MSE_list = NULL

for(i in 1:10){
  
  training_set_indices = sample(n, n/2, replace = F)
  validation_set_indices = (1:n)[!(1:n %in% training_set_indices)]
  
  training_set = Auto[training_set_indices, ]
  validation_set = Auto[validation_set_indices, ]  
  
  for(j in 1:10){
    
    n_valid = nrow(validation_set)
    
    model = lm(mpg ~ poly(horsepower, j), data = training_set)
    
    fitted_values = predict(model, validation_set)
    
    MSE[j] = (sum((validation_set$mpg - fitted_values)^2)) / (n_valid - length(model$coefficients))
  }
  
  MSE_list[[i]] = MSE
}


colour_vec = adjustcolor(rainbow(10), alpha = 0.65)
plot(MSE_list[[1]], type = "b", pch = 16, col = colour_vec[1], ylim=c(16,25))

for(i in 2:9){
  lines(MSE_list[[i]], type="b", pch = 16, col = colour_vec[i])  
}



###

X = Auto$horsepower
Y = Auto$mpg

# LOOCV( cbind(X), Y)
# LOOCV( cbind(X, X^2, X^3), Y)

loocv_list = NULL
cv_data = NULL

for(i in 1:5){
  cv_data = cbind(cv_data, X^i)
  loocv_list[[i]] = LOOCV(cv_data, Y)  
}

loocv_df = do.call(rbind.data.frame, loocv_list)
names(loocv_df) = c("MSE", "SD of MSE")

loocv_df

