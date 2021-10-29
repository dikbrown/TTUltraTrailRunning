
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
# race_year_id	double	    Race ID
# rank	        double	    Racer rank
# runner	      character	  Runner name
# time	        double	    Runner time, hour:minute:seconds
# age	          double	    Runner age
# gender	      character	  Runner gender (Man = M, Woman = W)
# nationality	  character	  Nationality of runner


# Look at number of races per runner
race_count <- summary(factor(tolower(rankings$runner)))
top_racers <- race_count[1:9] %>%  
  data.frame() %>%
  rownames_to_column()

names(top_racers) <- c('runner', 'count')


# get average ranking for each runner
ave_rank <- rankings %>% group_by(runner) %>%
  summarize(ave_rank = mean(rank, na.rm = TRUE))
ave_rank$runner <- tolower(ave_rank$runner)
top_racers <- left_join(top_racers, ave_rank)
top_racers <- top_racers %>% arrange(count)
top_racers
top_racers$runner <- factor(top_racers$runner, levels = top_racers$runner)

ggplot(data = top_racers, aes(label = paste0('Average finish rank = ', round(ave_rank, 1)))) +
  geom_col(aes(y = runner, x = count), fill = '#785f19') +
  xlab('Number of Races') +
  ylab('Runner') +
  labs(title = 'Most prolific ultra trail runners',
       caption = 'Data source: International Trail Running Association (ITRA)\n\nPlot by Dick Brown | @dikbrown | linkedin.com/in/dickbrown') +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 24, color = 'white'),
        panel.background = element_rect(fill = '#197829'), 
        plot.background = element_rect(fill = '#197829'),
        axis.title = element_text(color = 'white'),
        axis.line = element_line(color = 'white'),
        axis.ticks = element_line(color = 'white'),
        axis.text = element_text(color = 'white'),
        text = element_text(color = 'white')) +
  geom_label(aes(y = runner, x = 5), hjust = 0, fill = 'gray') 
