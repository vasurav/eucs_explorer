# Create the scoring Tibble
create_score_tibble <- function(score_winner = 15:2, score_loser = 14:0)
{
  expand_grid(score_winner = score_winner, score_loser = score_loser) %>% 
    filter(score_winner > score_loser) %>% 
    mutate(score_difference = score_winner - score_loser) %>% data.table()
}

# Plot something based on Point Diff
point_diff_plot <- function(df,
                            x_variable = "score_difference", 
                            y_variable = "game_rating_diff", 
                            color_variable = "score_winner",
                            label_variable = "score_loser")
{
  df %>% 
    ggplot(aes(x=!!sym(x_variable), 
               y=!!sym(y_variable), 
               color=!!sym(color_variable),
               label = !!sym(label_variable))) +
    geom_point() + 
    geom_line()
}


# Functions for the Rating
calc_rating <- function(score_winner, score_loser)
{
  r <- score_loser/(score_winner - 1)
  
  round(125 + 475*(sin(min(1, (1-r)*2)*0.4*pi))/sin(0.4*pi),2)
}

calc_rating_score_diff <- function(score_diff, score_winner = 15)
{
  calc_rating(score_winner, score_winner - score_diff)
}

calc_score_diff_rating_diff <- function(rating_diff, score_winner = 15)
{
  rating_table <- rating_point_diff(15, seq(7,15,0.01)) 
  
  rating_table$score_difference[which.min(abs(rating_table$game_rating_diff - abs(rating_diff)))]
}

rating_point_diff <- function(score_winner = 15:2, score_loser = 14:0)
{
  create_score_tibble(score_winner, score_loser) %>% 
    mutate(game_rating_diff = mapply(calc_rating, 
                                     score_winner = score_winner, 
                                     score_loser = score_loser))
}

rating_point_diff_plot <- function()
{
  rating_point_diff() %>% 
    mutate(score_winner = as_factor(score_winner)) %>% 
    point_diff_plot()
}

# Functions for the Weight
calc_weight <- function(score_winner, score_loser)
{
  min(
    1,
    sqrt(
      (score_winner + max(score_loser,
                          abs(0.5*(score_winner - 1))))/
        
        19
    )
  ) %>% 
    round(2)
}

weight_point_diff <- function()
{
  create_score_tibble() %>% 
    mutate(weight = mapply(calc_weight, 
                           score_winner = score_winner, 
                           score_loser = score_loser))
}

weight_point_diff_plot <- function()
{
  weight_point_diff() %>% 
    mutate(score_winner = as_factor(score_winner)) %>% 
    point_diff_plot(y_variable = "weight")
}
