gapminder %>%
  filter(continent=="Africa")%>%
  select(lifeExp)%>%
  t.test(mu=50)

library(patchwork)

library(gapminder)
library(ggplot2)

gapminder %>%
  drop_na(lifeExp) %>%
  filter(continent %in% "Africa") %>%
  ggplot(mapping = aes(lifeExp, color = continent, fill = continent)) +
  geom_density(alpha = 0.2, color = "steelblue", fill = "purple") +
  geom_vline(aes(xintercept = mean(lifeExp)),
             color = "blue",
             linetype = "dashed",
             linewidth = 1) +
  annotate("text", x = mean(gapminder$lifeExp),
           y = 0.1 * max(density(gapminder$lifeExp)$y),
           label = sprintf("Mean: %.2f", 
                mean(gapminder$lifeExp[which(gapminder$continent == "Africa")])),
           vjust = -10, hjust = -0.6, color = "blue", size = 4) +
  labs(title = "Density Plot with Mean (Africa)", x = "Life Expectancy")


#HYPOTHESIS TESTING
gapminder %>%
  filter(continent=="Africa") %>%
  select(lifeExp)%>%
  t.test(mu=50,conf.level=0.95)

my_ttest <-gapminder %>%
  filter(continent=="Africa") %>%
  select(lifeExp)%>%
  t.test(mu=50,conf.level=0.95)
attributes(my_ttest)

#TWO-SIDED TEST FOR DIFFERENCE OF MEANS
gapminder %>%
  filter(continent %in% c("Africa","Europe")) %>%
  t.test(lifeExp ~ continent, data= . )

#ONE SIDED TEST FOR DIFFERENCE OF MEANS
gapminder%>%
  filter(country %in% c("Ireland","Switzerland"))%>%
  t.test(lifeExp ~ country, data=.,
         alternative="less", conf.lebel=0.95)
  

 
