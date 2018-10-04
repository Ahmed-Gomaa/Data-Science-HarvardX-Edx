########### PH125.1x R Basics ###########

install.packages("dslabs")

library(dslabs)

########### PH125.2x Visualization ###########

data("murders")

head(murders)

library(ggplot2)

data("heights")

head(heights)

nrow(heights)

str(heights)

sapply(heights, class)

summary(heights$height)

gender_distribution = table(heights$sex)

gender_proportion = prop.table(gender_distribution)

barplot(gender_distribution)

pie(gender_distribution, labels = paste(round(gender_proportion, 2) * 100, "%", names(gender_distribution)), col = c("pink", "light green"))

index = heights$sex == "Male"

x = heights$height[index]

average = mean(x)

sd = sd(x)

c(average = average, sd = sd)

z = scale(x)

mean(abs(z) < 2)

mean(x <= 69.5)

length(x[x <= 69.5]) / length(x)

quantile(x, .5)

qnorm(p = .5, mean = average, sd = sd)

p = seq(from = .05, to = .95, by = .05)

observed_quantiles = quantile(x, probs = p)

theoretical_quantiles = qnorm(p, mean = average, sd = sd)

plot(theoretical_quantiles, observed_quantiles)

abline(a = 0, b = 1)

observed_quantiles = quantile(z, probs = p) 

theoretical_quantiles = qnorm(p)

plot(theoretical_quantiles, observed_quantiles)

abline(a = 0, b = 1)

library(tidyverse)

p = ggplot(data = murders)

class(p)

attributes(p)

p = p + geom_point(mapping = aes(x = population/10^6, y = total), size = 3)

p = p + geom_text(mapping = aes(x = population/10^6, y = total, label = abb), nudge_x = 1)

p = p + geom_text(mapping = aes(x = 10, y = 800, label = "relationship"))

p

p = ggplot(data = murders, mapping = aes(x = population/10^6, y = total, label = abb))

p = p + geom_point(size = 3)

p = p + geom_text(nudge_x = .075)

p = p + geom_text(mapping = aes(x = 10, y = 800, label = "relationship"))

p = p + scale_x_continuous(trans = "log10")

p = p + scale_y_continuous(trans = "log10")

p

p = p + scale_x_log10()

p = p + scale_y_log10()

p = p + xlab("pouplation")

p = p + ylab("total of murders")

p = p + ggtitle("Guns Murders")

p = p + geom_point(color = "blue")

p = p + geom_point(aes(col = region))

p = p + geom_abline(lty = 2, color = "red")

p = p + scale_color_discrete(name = "Region")

library(ggthemes)

p = p + theme_economist()

library(ggrepel)

p = p + geom_text_repel()

p

library(dplyr)

male_heights = filter(heights, sex == "Male")

p = ggplot(data = male_heights, aes(x = height))

p = p + geom_histogram(binwidth = 1, fill = "blue", col = "black")

p = p + xlab("Male hrights distribution")

p = p + ggtitle("Male hrights distribution")

p = ggplot(data = male_heights, aes(x = height))

p = p + geom_density(fill = "blue")

p = ggplot(data = male_heights, aes(sample = height))

p = p + geom_qq(dparams = c(average, sd))

p = p + geom_qq_line(dparams = c(average, sd), col = "blue")

p = p + geom_abline(col = "red")

library(gridExtra)

p = ggplot(data = male_heights, aes(x = height))

p1 = p + geom_histogram(binwidth = 1, fill = "blue", col = "black")

p2 = p + geom_histogram(binwidth = 2, fill = "blue", col = "black")

p3 = p + geom_histogram(binwidth = 3, fill = "blue", col = "black")

grid.arrange(p1, p2, p3, ncol = 3)

male_heights_summary = male_heights %>%
                       summarize(mean = mean(height), sd = sd(height), min = min(height), max = max(height), median = median(height), Q1 = quantile(height, .25), Q3 = quantile(height, .75), count = length(height))

male_heights_summary

murders_rated = mutate(murders, rate = total/population*10^5)

all_murders_rate = summarise(murders, total_rate = sum(total)/sum(population)*10^5)

class(all_murders_rate)

all_murders_rate = all_murders_rate %>% .$total_rate

class(all_murders_rate)

heights_grouped = group_by(heights, sex)

summarize(heights_grouped, mean = mean(height))

murders_rated_arranged = arrange(murders_rated, population)

murders_rated_arranged = arrange(murders_rated, desc(population))

murders_rated_arranged = arrange(murders_rated, region, desc(rate))

x = top_n(murders_rated_arranged, 10)

data("gapminder")

View(gapminder)

gapminder %>%
filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
select(country, infant_mortality)

names(gapminder_mutated_filtered)

gapminder_filtered = filter(gapminder, year %in% c(1962, 1980, 1990, 2000, 2012))

p = ggplot(data = gapminder_filtered)

p = p + geom_point(mapping = aes(x = fertility, y = life_expectancy, col = continent))

p = p + ds_theme_set()

p = p + facet_grid(continent ~ year)

p = p + facet_grid(. ~ year)

p = p + facet_wrap(. ~ year)

p

gapminder_filtered = filter(gapminder, country %in% c("United States", "Germany"))

p = ggplot(data = gapminder_filtered)

p = p + geom_line(mapping = aes(x = year, y = fertility, col = country))

p = p + geom_line(mapping = aes(x = year, y = fertility, group = country, col = country))

p

gapminder_mutated = mutate(gapminder, gdp_person_day = gdp/population/365)

gapminder_mutated_filtered = filter(gapminder_mutated, year == 1970 & !is.na(gdp))

p = ggplot(data = gapminder_mutated_filtered)

p = p + geom_histogram(mapping = aes(x = log2(gdp_person_day)), binwidth = 1, color = "black")

p = p + geom_histogram(mapping = aes(x = gdp_person_day), binwidth = 1, color = "black")

p = p + scale_x_continuous(trans = "log2")

length(levels(gapminder$region))

length(levels(gapminder$country))

p = p + geom_boxplot(mapping = aes(x = region, y = gdp_person_day, fill = continent))

p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

p

gapminder_mutated_filtered_mutated = mutate(gapminder_mutated_filtered, region = reorder(region, gdp_person_day, FUN = median))

p = ggplot(data = gapminder_mutated_filtered_mutated)

p = p + geom_boxplot(mapping = aes(x = region, y = gdp_person_day, fill = continent))

p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

p = p + xlab("")

p = p + scale_y_continuous(trans = "log2")

p = p + geom_point(mapping = aes(x = region, y = gdp_person_day))

p

unique(gapminder_mutated_filtered_mutated$region)

gapminder_mutated_filtered_mutated_mutated = mutate(gapminder_mutated_filtered_mutated, group = ifelse(region %in% c("Western Europe", "Northern America", "Northern Europe"), "West Country", "Developing"))

p = ggplot(data = gapminder_mutated_filtered_mutated_mutated)

p = p + geom_histogram(mapping = aes(gdp_person_day), binwidth = 1, color = "black")

p = p + facet_grid(group ~ continent)

p = p + scale_x_continuous(trans = "log2")

p

p = p + geom_boxplot(mapping = aes(x = region, y = gdp_person_day, fill = factor(group)))

p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

p = p + scale_y_continuous(trans = "log2")

p = p + geom_point(mapping = aes(x = region, y = gdp_person_day))

p

