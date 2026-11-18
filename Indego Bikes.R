all_stations_end$New_Neighborhood
k <- 5
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ distance_miles + 
                   bike_lane * New_Neighborhood + 
                   parks * New_Neighborhood + 
                   train_station * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)

aggregate(end_count ~ End_Neighborhood, data = all_stations_end, FUN = length)
all_stations_end$New_Neighborhood <- ifelse(all_stations_end$End_Neighborhood == "Angora",
                                            "Cobbs Creek", 
                                      ifelse(all_stations_end$End_Neighborhood == "Belmont",
                                                   "Mantua",
                                      ifelse(all_stations_end$End_Neighborhood == "Carroll Park",
                                             "Overbrook",
                                      ifelse(all_stations_end$End_Neighborhood == "Cedar Park",
                                             "Spruce Hill",
                                      ifelse(all_stations_end$End_Neighborhood == "Devils Pocket",
                                             "Grays Ferry",
                                      ifelse(all_stations_end$End_Neighborhood == "Dickinson Narrows",
                                             "Pennsport",
                                      ifelse(all_stations_end$End_Neighborhood == "Dunlap",
                                             "Mill Creek",
                                      ifelse(all_stations_end$End_Neighborhood == "Franklinville",
                                             "Nicetown-Tioga",
                                      ifelse(all_stations_end$End_Neighborhood == "Girard Estates",
                                             "West Passyunk",
                                      ifelse(all_stations_end$End_Neighborhood == "Glenwood",
                                             "Hartranft",
                                      ifelse(all_stations_end$End_Neighborhood == "Haddington",
                                             "Overbrook",
                                      ifelse(all_stations_end$End_Neighborhood == "Melrose",
                                             "West Passyunk",
                                      ifelse(all_stations_end$End_Neighborhood == "Newbold",
                                            "Point Breeze",
                                      ifelse(all_stations_end$End_Neighborhood == "Olde Kensington",
                                             "Fishtown",
                                      ifelse(all_stations_end$End_Neighborhood == "Packer Park",
                                             "Navy Yard",
                                      ifelse(all_stations_end$End_Neighborhood == "Sharswood",
                                            "Brewerytown",
                                      ifelse(all_stations_end$End_Neighborhood == "Society Hill",
                                             "Old City",
                                      ifelse(all_stations_end$End_Neighborhood == "Squirrel Hill",
                                             "Kingsessing",
                                      ifelse(all_stations_end$End_Neighborhood == "Walnut Hill",
                                             "Spruce Hill",
                                      ifelse(all_stations_end$End_Neighborhood == "West Parkside",
                                             "Wynnefield",
                                      ifelse(all_stations_end$End_Neighborhood == "West Powelton",
                                             "Powelton Village",
                                      ifelse(all_stations_end$End_Neighborhood == "Yorktown",
                                             "Ludlow", all_stations_end$End_Neighborhood))))))))))))))))))))))
DescTools::Desc(all_stations_end$end_count)
length(subset(all_stations_end, all_stations_end$end_count >= 10000))
ten_k <- subset(all_stations_end, all_stations_end$end_count >= 10000) 
ten_k
ten_k$End_Station_Name
aggregate(end_count ~ bike_lane, data = all_stations_end, FUN = summary)
aggregate(end_count ~ trails, data = all_stations_end, FUN = summary)
aggregate(end_count ~ parks, data = all_stations_end, FUN = summary)
aggregate(end_count ~ train_station, data = all_stations_end, FUN = summary)
aggregate(end_count ~ New_Neighborhood, data = all_stations_end, FUN = median)
aggregate(end_count ~ New_Neighborhood, data = all_stations_end, FUN = mean)
aggregate(end_count ~ New_Neighborhood, data = all_stations_end, FUN = length)
summedup <- aggregate(end_count ~ New_Neighborhood, data = all_stations_end, FUN = sum)
aggregate(end_count ~ New_Neighborhood, data = ten_k, FUN = length)
sum(summedup$end_count)
128723/1252675
60201/1252675
55791/1252675
aggregate(end_count ~ Amenities, data = all_stations_end, FUN = length)
major_neighborhoods <- c("Rittenhouse", "Fitler Square", "Logan Square", "Market East",
                         "Washington Square West", "Graduate Hospital", "University City")
major_neighborhood_stations <- subset(all_stations_end, all_stations_end$New_Neighborhood %in% major_neighborhoods)
minor_neighborhood_stations <- subset(all_stations_end, !all_stations_end$New_Neighborhood %in% major_neighborhoods)
aggregate(end_count ~ bike_lane, data = major_neighborhood_stations, FUN = summary)
aggregate(end_count ~ train_station, data = major_neighborhood_stations, FUN = summary)
sum(major_neighborhood_stations$train_station)
sum(major_neighborhood_stations$bike_lane)
11560/6470
2591/2070
aggregate(end_count ~ trails, data = major_neighborhood_stations, FUN = summary)
aggregate(end_count ~ parks, data = major_neighborhood_stations, FUN = summary)
sum(major_neighborhood_stations$parks)
aggregate(distance_miles ~ bike_lane, data = major_neighborhood_stations, FUN = summary)
aggregate(distance_miles ~ train_station, data = major_neighborhood_stations, FUN = summary)
summary(all_stations_end$distance_miles)
summary(major_neighborhood_stations$distance_miles)
summary(minor_neighborhood_stations$distance_miles)
#INCLUDE ALL VARIABLES
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ distance_miles * New_Neighborhood + 
                   bike_lane * New_Neighborhood + 
                   parks * New_Neighborhood + 
                   trails * New_Neighborhood +
                   train_station * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#EXCLUDING DISTANCE
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   bike_lane * New_Neighborhood + 
                   parks * New_Neighborhood + 
                   trails * New_Neighborhood +
                   train_station * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#EXCLUDE BIKE LANE
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ distance_miles * New_Neighborhood + 
                   parks * New_Neighborhood + 
                   trails * New_Neighborhood +
                   train_station * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#EXCLUDE TRAILS
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ distance_miles * New_Neighborhood + 
                   bike_lane * New_Neighborhood + 
                   parks * New_Neighborhood + 
                   train_station * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#EXCLUDE PARKS
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ distance_miles * New_Neighborhood + 
                   bike_lane * New_Neighborhood + 
                   trails * New_Neighborhood +
                   train_station * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#EXCLUDE TRAIN STATION
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ distance_miles * New_Neighborhood + 
                   bike_lane * New_Neighborhood + 
                   parks * New_Neighborhood + 
                   trails * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#EXCLUDE PARKS
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ distance_miles * New_Neighborhood + 
                   bike_lane * New_Neighborhood + 
                   trails * New_Neighborhood +
                   train_station * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#EXCLUDE TRAIN STATION
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ distance_miles * New_Neighborhood + 
                   bike_lane * New_Neighborhood + 
                   parks * New_Neighborhood + 
                   trails * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#EXCLUDE PARKS
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ distance_miles * New_Neighborhood + 
                   bike_lane * New_Neighborhood + 
                   trails * New_Neighborhood +
                   train_station * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#EXCLUDE TRAIN STATION
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ distance_miles * New_Neighborhood + 
                   bike_lane * New_Neighborhood + 
                   parks * New_Neighborhood + 
                   trails * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#JUST BIKE LANE AND NEIGHBORHOOD
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~  
                   bike_lane * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#JUST NEIGHBORHOOD
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~  
                   New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#NEIGHBORHOOD BIKE LANE NO INTERACTION
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~  
                   New_Neighborhood + bike_lane,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#JUST TRAIL AND NEIGHBORHOOD
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~  
                   trails * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#JUST PARK AND NEIGHBORHOOD
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~  
                   parks * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#JUST NEIGHBORHOOD, TRAILS, AND PARKS
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~  
                   parks * New_Neighborhood +
                   trails * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#JUST TRAIL, BIKE LANE AND NEIGHBORHOOD
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~  
                   trails * New_Neighborhood +
                   bike_lane * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#JUST TRAIL, BIKE LANE (NO INTERACTION) AND NEIGHBORHOOD
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~  
                   trails * New_Neighborhood +
                   bike_lane,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#JUST TRAIL, TRAIN STATION AND NEIGHBORHOOD
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~  
                   trails * New_Neighborhood +
                   train_station * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#JUST DISTANCE AND NEIGHBORHOOD
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~  
                   distance_miles * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#INCLUDE ALL VARIABLES
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ distance_miles * New_Neighborhood + 
                   bike_lane * New_Neighborhood + 
                   parks * New_Neighborhood + 
                   trails * New_Neighborhood +
                   train_station * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#ALL VARIABLES EXCEPT NEIGHBORHOOD
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   distance_miles +
                   bike_lane + 
                   parks + 
                   trails +
                   train_station,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#INCLUDE ALL VARIABLES
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ distance_miles * New_Neighborhood + 
                   bike_lane * New_Neighborhood + 
                   parks * New_Neighborhood + 
                   trails * New_Neighborhood +
                   train_station * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#TRAIN STATION AND NEIGHBORHOOD
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

# Set factor levels on full dataset first
all_stations_end$New_Neighborhood <- factor(all_stations_end$New_Neighborhood)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  test$New_Neighborhood <- factor(test$New_Neighborhood,
                                  levels = levels(train$New_Neighborhood))
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   train_station * New_Neighborhood,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 3958.8  # Earth's radius in miles
  
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon1 <- lon1 * pi / 180
  lon2 <- lon2 * pi / 180
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  
  return(R * c)
}

# Rittenhouse Square Park coordinates
rittenhouse_lat <- 39.9493
rittenhouse_lon <- -75.1728

# Add distance column to your data
all_stations_end$dist_from_rittenhouse <- haversine_distance(
  all_stations_end$end_lat,  # replace with your lat column name
  all_stations_end$end_lon,  # replace with your lon column name
  rittenhouse_lat,
  rittenhouse_lon
)
all_stations_end$dist_from_rittenhouse
#DISTANCE
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   dist_from_rittenhouse,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#DISTANCE X TRAILS
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   dist_from_rittenhouse * trails,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#DISTANCE X TRAIN STATIONS
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   dist_from_rittenhouse * train_station,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#DISTANCE X TRAIN STATIONS X PARKS
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   dist_from_rittenhouse * train_station +
                 dist_from_rittenhouse * parks,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#DISTANCE X TRAIN STATIONS X BIKE LANE
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   dist_from_rittenhouse * train_station +
                 dist_from_rittenhouse * bike_lane,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#DISTANCE X TRAIN STATIONS X BIKE LANE X TRAILS
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   dist_from_rittenhouse * train_station +
                   dist_from_rittenhouse * bike_lane +
                   dist_from_rittenhouse * trails,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
summary(all_stations_end$dist_from_rittenhouse)
DescTools::Desc(all_stations_end$dist_from_rittenhouse)
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 3958.8  # Earth's radius in miles
  
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon1 <- lon1 * pi / 180
  lon2 <- lon2 * pi / 180
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  
  return(R * c)
}

# Rittenhouse Square Park coordinates
rittenhouse_lat <- 39.9493
rittenhouse_lon <- -75.1728

# Add distance column to your data
major_neighborhood_stations$dist_from_rittenhouse <- haversine_distance(
  major_neighborhood_stations$end_lat,  # replace with your lat column name
  major_neighborhood_stations$end_lon,  # replace with your lon column name
  rittenhouse_lat,
  rittenhouse_lon
)
major_neighborhood_stations$dist_from_rittenhouse
summary(major_neighborhood_stations$dist_from_rittenhouse)
summary(minor_neighborhood_stations$dist_from_rittenhouse)
all_stations_end$log_distance <- log(all_stations_end$dist_from_rittenhouse)
#DISTANCE LOG
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   log_distance,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#DISTANCE LOG X TRAIN STATION X BIKE LANE
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   log_distance * train_station +
                   log_distance * bike_lane,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
summary(all_stations_end$log_distance)
#DISTANCE LOG X TRAIN STATION X BIKE LANE X PARKS
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   log_distance * train_station +
                   log_distance * bike_lane +
                   log_distance * parks,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
summary(all_stations_end$log_distance)
#DISTANCE LOG X TRAIN STATION X BIKE LANE X PARKS X TRAILS
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   log_distance * train_station +
                   log_distance * bike_lane +
                   log_distance * parks +
                   log_distance * trails,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#DISTANCE LOG X ALL VARIABLES
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   log_distance * train_station +
                   log_distance * bike_lane +
                   log_distance * parks +
                   log_distance * trails +
                   log_distance * distance_miles,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#DISTANCE LOG EXCLUDING TRAILS
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   log_distance * train_station +
                   log_distance * bike_lane +
                   log_distance * parks +
                   log_distance * distance_miles,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
DescTools::Desc(all_stations_end$log_distance)
unique(all_stations_end$New_Neighborhood)
unique(all_stations_end$End_Neighborhood)
all_stations_end$ratio <- all_stations_end$distance_miles/all_stations_end$log_distance
summary(all_stations_end$ratio)
all_stations_end$net_effect <- 5926.1 * all_stations_end$distance_miles - 5751.6 * 
  all_stations_end$distance_miles * all_stations_end$log_distance
summary(all_stations_end$net_effect)
corner_stations <- c('"63rd St Station, MFL"', 'Overbrook Station', 'Manayunk Bridge', 
                     '23rd & Venango', 'Allegheny & Richmond', '15th & Kitty Hawk', '58th & Kingsessing')
# Add distance column to your data
all_stations_end$dist_from63mfl <- haversine_distance(
  all_stations_end$end_lat,  # replace with your lat column name
  all_stations_end$end_lon,  # replace with your lon column name
  39.9624,
  -75.2452
)
# Add distance column to your data
all_stations_end$dist_fromobvk <- haversine_distance(
  all_stations_end$end_lat,  # replace with your lat column name
  all_stations_end$end_lon,  # replace with your lon column name
  39.9906,
  -75.2490
)
# Add distance column to your data
all_stations_end$dist_frommnykbrdg <- haversine_distance(
  all_stations_end$end_lat,  # replace with your lat column name
  all_stations_end$end_lon,  # replace with your lon column name
  40.028,
  -75.2277
)
# Add distance column to your data
all_stations_end$dist_from23v <- haversine_distance(
  all_stations_end$end_lat,  # replace with your lat column name
  all_stations_end$end_lon,  # replace with your lon column name
  40.0096,
  -75.166
)
# Add distance column to your data
all_stations_end$dist_fromallrch <- haversine_distance(
  all_stations_end$end_lat,  # replace with your lat column name
  all_stations_end$end_lon,  # replace with your lon column name
  39.9829,
  -75.1012
)
# Add distance column to your data
all_stations_end$dist_from15kh <- haversine_distance(
  all_stations_end$end_lat,  # replace with your lat column name
  all_stations_end$end_lon,  # replace with your lon column name
  39.8899,
  -75.1768
)
# Add distance column to your data
all_stations_end$dist_from58kgs <- haversine_distance(
  all_stations_end$end_lat,  # replace with your lat column name
  all_stations_end$end_lon,  # replace with your lon column name
  39.9345,
  -75.2276
)
corner_station_values <- c('dist_from63mfl', 'dist_fromobvk', 'dist_frommnykbrdg',
                           'dist_from23v', 'dist_fromallrch', 'dist_from15kh')
all_stations_end$nearest_corner <- apply(all_stations_end[, corner_station_values], 1, function(x){corner_station_values[which.min(x)]})
table(all_stations_end$nearest_corner)
all_stations_end$corner_ratio <- ifelse(all_stations_end$nearest_corner == 'dist_from15kh',
                                        all_stations_end$dist_from_rittenhouse/4.1070043,
                                  ifelse(all_stations_end$nearest_corner == 'dist_from23v',
                                         all_stations_end$dist_from_rittenhouse/4.1813857,
                                  ifelse(all_stations_end$nearest_corner == 'dist_fromallrch',
                                         all_stations_end$dist_from_rittenhouse/4.444194,
                                  ifelse(all_stations_end$nearest_corner == 'dist_frommnykbrdg',
                                         all_stations_end$dist_from_rittenhouse/6.163305,
                                  ifelse(all_stations_end$nearest_corner == 'dist_fromovbk',
                                         all_stations_end$dist_from_rittenhouse/4.942145,
                                         all_stations_end$dist_from_rittenhouse/3.940608)))))
summary(all_stations_end$corner_ratio)
#CORNER RATIO
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   corner_ratio,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
#CORNER RATIO
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   corner_ratio * bike_lane +
                   corner_ratio * train_station +
                   corner_ratio * distance_miles,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}

sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
all_stations_end$corner_ratio_sq <- (all_stations_end$corner_ratio)**2
#CORNER RATIO
k <- 10
folds <- sample(rep(1:k, length.out = nrow(all_stations_end)))
mse_lm <- numeric(k)

for (i in 1:k) {
  train <- all_stations_end[folds != i, ]
  test  <- all_stations_end[folds == i, ]
  
  tryCatch({
    lm_fit <- lm(end_count ~ 
                   corner_ratio_sqrt * bike_lane +
                   corner_ratio_sqrt * train_station,
                 data = train)
    
    preds <- predict(lm_fit, newdata = test)
    mse_lm[i] <- mean((test$end_count - preds)^2, na.rm = TRUE)
    cat("Fold", i, "RMSE:", sqrt(mse_lm[i]), "\n")
    
  }, error = function(e) {
    cat("Fold", i, "failed:", conditionMessage(e), "\n")
  })
}
sqrt(mean(mse_lm[mse_lm > 0]))  # exclude failed folds
summary(lm_fit)
all_stations_end$corner_ratio_sqrt <- sqrt(all_stations_end$corner_ratio)
