shift_matrix <- read.csv('shift_matrix_20152016_withGoalVector.csv')

number_of_non_player_cols <- 6

modeldata <- as.data.frame(shift_matrix[, -((ncol(shift_matrix) - number_of_non_player_cols):ncol(shift_matrix)-1)])
modeldata$GoalVector <- as.numeric(as.character(modeldata$GoalVector))
model <- lm(GoalVector ~ ., data = modeldata)

write.table(model$coef, sep = ',', 'coefs.csv', col.names = F)
