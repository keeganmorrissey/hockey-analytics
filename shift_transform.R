data <- read.csv('nhl_shifts_20152016.csv')
data$StartSec <- data$Start + (data$Period - 1) * 20 * 60
data$EndSec <- data$End + (data$Period - 1) * 20 * 60

number_of_non_player_cols <- 6

cols <- unique(data$Player)
shift_matrix <- matrix(0, 0, length(cols))
colnames(shift_matrix) <- cols
shift_matrix <- cbind(shift_matrix, 'Game_Id' = NULL, 'Team' = NULL, 'Opponent' = NULL, 'ShiftStart' = NULL, 'ShiftLength' = NULL, 'ShiftEnd' = NULL)

for (game in unique(data$Game_Id)) {

    message(game)
    teams_in_game <- unique(data$Team[data$Game_Id == game])

    for (team in teams_in_game) {

        team_game_data <- data[data$Game_Id == game & data$Team == team, ]
        rows <- 1:max(team_game_data$EndSec)

        cur_shift_matrix <- matrix(0, length(rows), length(cols))
        colnames(cur_shift_matrix) <- cols
        cur_shift_matrix <- cbind(cur_shift_matrix, 'Game_Id' = 0, 'Team' = 0, 'Opponent' = 0, 'ShiftStart' = 0, 'ShiftLength' = 0, 'ShiftEnd' = 0)

        for (row in 1:length(team_game_data$Game_Id[team_game_data$Game_Id == game])) {

            cur_shift_matrix[(team_game_data$StartSec[row] + 1):(team_game_data$EndSec[row]), as.character(team_game_data$Player[row])] <- 1

        }

        new_shift <- c(1)
        shift_lengths <- numeric(0)
        cur_shift_length <- 1

        for (row in 2:length(rows)) {

            if (sum(cur_shift_matrix[row,] == cur_shift_matrix[row-1,]) == length(cols) + number_of_non_player_cols) cur_shift_length <- cur_shift_length + 1
            else {
                new_shift <- append(new_shift, row)
                shift_lengths <- append(shift_lengths, cur_shift_length)
                cur_shift_length <- 1
            }

        }
        shift_lengths <- append(shift_lengths, cur_shift_length)
        cur_shift_matrix[, 'Game_Id'] <- game
        cur_shift_matrix[, 'Team'] <- team
        cur_shift_matrix[, 'Opponent'] <- as.character(teams_in_game[teams_in_game != team])

        cur_shift_matrix_truncated <- cur_shift_matrix[new_shift, ]

        cur_shift_matrix_truncated[, 'ShiftStart'] <- new_shift
        cur_shift_matrix_truncated[, 'ShiftLength'] <- shift_lengths
        cur_shift_matrix_truncated[, 'ShiftEnd'] <- new_shift + shift_lengths - 1

        shift_matrix <- rbind(shift_matrix, cur_shift_matrix_truncated)

    }
   
}


boxscore_data <- read.csv('nhl_pbp_20152016.csv')

goal_data <- boxscore_data[boxscore_data$Event == 'GOAL', ]
goal_data$Seconds_Elapsed_Game <- goal_data$Seconds_Elapsed + (goal_data$Period - 1) * 60 * 20
shift_matrix <- cbind(shift_matrix, 'GoalVector' = 0)

for (row in 1:(length(goal_data$Game_Id))) {

    shift_matrix[shift_matrix[, 'Team'] == as.character(goal_data$Ev_Team[row]) & 
                    shift_matrix[, 'Game_Id'] == goal_data$Game_Id[row] & 
                    goal_data$Seconds_Elapsed_Game[row] > as.numeric(shift_matrix[, 'ShiftStart']) & 
                    goal_data$Seconds_Elapsed_Game[row] <= as.numeric(shift_matrix[, 'ShiftEnd']), 'GoalVector'] <- 1

    team_scored_on <- ''
    if (as.character(goal_data$Ev_Team[row]) != as.character(goal_data$Home_Team[row])) team_scored_on <- as.character(goal_data$Home_Team[row])
    else team_scored_on <- as.character(goal_data$Away_Team[row])

    shift_matrix[shift_matrix[, 'Team'] == team_scored_on & 
                    shift_matrix[, 'Game_Id'] == goal_data$Game_Id[row] & 
                    goal_data$Seconds_Elapsed_Game[row] > as.numeric(shift_matrix[, 'ShiftStart']) & 
                    goal_data$Seconds_Elapsed_Game[row] <= as.numeric(shift_matrix[, 'ShiftEnd']), 'GoalVector'] <- -1

}


modeldata <- as.data.frame(shift_matrix[, -((ncol(shift_matrix) - number_of_non_player_cols):ncol(shift_matrix)-1)])
modeldata$GoalVector <- as.numeric(as.character(modeldata$GoalVector))
model <- lm(GoalVector ~ ., data = modeldata)
