calc_ranking <- function(score_winner, score_loser)
{
  r <- score_loser/(score_winner - 1)
  
  round(125 + 475*(sin(min(1, (1-r)*2)*0.4*pi))/sin(0.4*pi),2)
}

algo_point_diff_plot <- function()
{
  expand_grid(score_winner = c(15,13,11,9), score_loser = 14:0) %>% 
    filter(score_winner > score_loser) %>% 
    mutate(score_difference = score_winner - score_loser,
           rating_difference = mapply(calc_ranking, 
                                    score_winner = score_winner, 
                                    score_loser = score_loser)) %>% 
    mutate(score_winner = as_factor(score_winner)) %>% 
    ggplot(aes(x=score_difference, 
               y=rating_difference, 
               color=score_winner,
               label = score_loser)) +
    geom_point() + 
    geom_line()
}


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

weight_point_diff_plot <- function()
{
  expand_grid(score_winner = c(15:5), score_loser = 14:0) %>% 
    filter(score_winner > score_loser) %>% 
    mutate(score_difference = score_winner - score_loser,
           weight = mapply(calc_weight, 
                           score_winner = score_winner, 
                           score_loser = score_loser)) %>% 
    mutate(score_winner = as_factor(score_winner)) %>% 
    ggplot(aes(x=score_difference, 
               y=weight, 
               color=score_winner,
               label = score_loser)) +
    geom_point() + 
    geom_line()
}
