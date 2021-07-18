# Game-day Simulator for Baseball (R)
## this program simulates matches between home and away teams several times to predict the winner
library(lattice)  # graphics package for probability matrix visual
simulator <- function(home_mean,away_mean,niterations) { 
     # input runs scored means, output probability of winning for home team
  ##we input three datasets; mean of home and away game and number of iterations to the function and simulate them
     set.seed(1234)  # set to obtain reproducible results 
     away_game_score <- numeric(niterations) ## we assign away_game_score as a numeric variable
     home.game.score <- numeric(niterations) ## similarly we assign home.game.score as a numberic variable for iterations
     home_win <- numeric(niterations) ## the number of wins returned is stored as numeric var.
     i <- 1 ## initialize 1 as 1
     while (i < niterations + 1) { ## run a while loop till we get the condition where i less than iterations plus one is true
         away_game_score[i] <- rnbinom(1,mu=away_mean, size = 4) ## perform a negative binomial distribution for random density and set it to the iteration of away_game_score
         home.game.score[i] <- rnbinom(1,mu=home_mean, size = 4) ## repeat same for home game score
         if(away_game_score[i] > home.game.score[i]) home_win[i] <- 1 ## we run the if-else function to see if the away score is greater than home game score or if it is less then we keep running the iteration
         if(away_game_score[i] > home.game.score[i] || 
         away_game_score[i] < home.game.score[i]) i <- i + 1 
         }
     n_home_win <- sum(home_win)  ## we add all home_win and assign to n_home_win
     n_home_win/niterations  # return probability of away team winning 
     } 

niterations <- 10000  # use smaller number for testing ## we run the iterations for 10000 times.
# probability matrix for results... home team is rows, away team is columns
probmat <- matrix(data = NA, nrow = 9, ncol = 9,  ## In this probability matrix, number of rows is 9 and number of cols is 9
  dimnames = list(c(as.character(1:9)), c(as.character(1:9)))) 
for (index_home in 1:9) ## we run for loop for the index_home variable between 1 and 9
for (index_away in 1:9) ## we do the same for index_awau variable
if (index_home != index_away) {
     probmat[index_home,index_away] <- 
        simulator(index_home, index_away, niterations) 
     } ## we run the simulator function and assign it to the probability matrix if the index_home is not equal to index_away
pdf(file = "fig_sports_analytics_prob_matrix.pdf", width = 8.5, height = 8.5) ## create a pdf file for the probability matrix with width and height equal to 8.5
x <- rep(1:nrow(probmat),times=ncol(probmat)) ##use replicate function for the number of rows and cols and assign to variable x
y <- NULL ## nothing is assigned to y
for (i in 1:ncol(probmat)) y <- c(y,rep(i,times=nrow(probmat)))##assign ith iteration value from number of rows to y
probtext <- sprintf("%0.3f", as.numeric(probmat))  # fixed format 0.XXX
text_data_frame <- data.frame(x, y, probtext) ## use data.frame function and assign x,y prob text values to text_data_frame variable.
text_data_frame$probtext <- as.character(text_data_frame$probtext)
text_data_frame$probtext <- ifelse((text_data_frame$probtext == "NA"), 
    NA,text_data_frame$probtext)  # define diagonal cells as missing ## run ifelse function and return values text_data_frame$probtext if there is a value else, return NA.
text_data_frame <- na.omit(text_data_frame)  # diagonal cells
print(levelplot(probmat, cuts = 25, tick.number = 9, ## plot a graph for the results
    col.regions=colorRampPalette(c("violet", "white", "light blue")), ## choose the colors
    xlab = "Visiting Team Runs Expected", ## x-axis is visiting team runs expected
    ylab = "Home Team Runs Expected",     ## y-axis is home team runs expected
    panel = function(...) {
        panel.levelplot(...)  
        panel.text(text_data_frame$x, text_data_frame$y, 
        labels = text_data_frame$probtext)
        }))
dev.off()        
# Suggestion for the student: Develop simulators for football or basketball.    

