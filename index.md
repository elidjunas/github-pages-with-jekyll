---
title: "Do COVID-19 testing rates predict nationwide health outcomes?"
author: "Eli Kliejunas"
output: html_document
---

```{r setup, echo=FALSE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
```

## Introduction 

When attempting to minimize adverse health outcomes caused by infectious diseases, testing is an essential tool. Not only does it facilitate the successful treatment of individuals by identifying diseases in their early stages before symptoms and complications arise, it also augments secondary disease prevention by reducing transmission. Ideally, with timely and reliable identification of those who have contracted a disease, their ability to pass the disease on to others can be reduced via measures such as masking, social distancing and, by extension, isolation. Reduced transmission subsequently eases the pressure on hospitals, which struggle with high patient volumes and (at times) staff shortages during pandemics. Moreover, as mentioned above, expansive testing and early detection of diseases also lighten the burden on medical staff and resources by enabling early treatment of the disease. Finally, on a larger scale, governments and other public health organizations benefit from comprehensive testing regimes because they provide data that guides the efficient allocation of limited resources such as medical equipment. 

For the reasons outlined above, public health experts agree that testing is a crucial element of infectious disease control. However, while expansive testing is a necessary component of containing outbreaks, it is also insufficient. Rigorous testing alone does not prevent disease transmission, hospitalizations, and deaths. For one, the tests themselves must be valid in terms of their actual ability to accurately distinguish those who have a specific disease from those who do not. Moreover, the insights derived from testing data must be paired with other public health measures in order to reduce the burden of disease. For example, widespread testing may reveal a measles outbreak in a community, but the outbreak will continue to worsen in the absence of subsequent interventions such as immunizations or social distancing. The burden placed on that community's health system will also intensify if resources and staff are not allocated in such a way that meets the increased demand for health services. 

The ongoing COVID-19 pandemic has laid bare the immense challenges of effectively responding to a global infectious disease outbreak. Since emerging in human populations at the end of 2019, the severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2) virus has led to many millions of deaths, hospitalizations, and other adverse health outcomes worldwide. Health systems have been strained like never before. National responses to the pandemic have varied widely and, subsequently, countries around the world have experienced notably different health outcomes stemming from the outbreak.    

This project aims to examine various countries' COVID-19 death and hospitalization numbers relative to the number of COVID-19 tests conducted. The dataset utilized for analysis, which compiles extensive health data from a number of official sources worldwide, is provided by the website Our World in Data (OWID) in the following link: <https://github.com/owid/covid-19-data/blob/master/public/data/README.md>. Specifically, I use this dataset to evaluate whether testing rates (standardized per 100,000 people) are negatively associated with hospitalizations and deaths (also standardized per 100,000 people), as would be expected by public health experts. In other words, did countries with higher testing rates exhibit lower rates of hospitalizations and deaths than other countries with lower testing rates? Though case numbers are also provided in the dataset, transmission outcomes are not measured in the analysis because case numbers are highly dependent on case detection rates (i.e., the number of detected cases relative to the total number of estimated cases in a population), which vary widely between countries and are very difficult to determine with accuracy. 

## Cleaning and transforming the data 

### "Glimpsing" the data 
After importing OWID's dataset into R, the `glimpse()` function summarizes the names and types of the information provided in each column as well as the dataset's dimensions (i.e., the number of rows and columns). 

```{r glimpse_data}
library(readr)
library(tibble)
library(dplyr)
covid_df <- read.csv("owid-covid-data.csv")
glimpse(covid_df)
```
As you can see, the dataset supplies many informative indicators of health outcomes related to the COVID-19 outbreak; in total, there are 67 variables measured in 153,393 observations. For the purposes of answering this project's question of whether increased testing is associated with lower death and hospitalization rates, only a handful of the 67 variables are necessary.

### Selecting for relevant data 
Therefore, a new dataframe is created that only selects for the relevant columns in the original `covid_df` dataset: country (`location`), date of the recorded data (`date`), total tests completed up to that point in time (`total_tests`), weekly hospital admissions (`weekly_hosp_admissions`), total deaths recorded up to that point in time (`total_deaths`), and population size (`population`). 

```{r select_data}
covid_select_df <- covid_df %>%
  select(location, date, total_tests, weekly_hosp_admissions, total_deaths, population)
glimpse(covid_select_df)
```
This new `covid_select_df` dataframe is easier to work with now that we have removed the information that is irrelevant to this particular project. The next step is to transform the data in such a way that allows us to answer the research question. Before doing that, though, we must examine the `population` values in the dataset closely, as they are instrumental in our calculations of tests per 100,000 people, hospitalizations per 100,000 people, and deaths per 100,000 people. Therefore, it is important to confirm that each country's population is held constant in the dataset such that the population values are not fluctuating over time. We can do this by using the `group_by` function to organize the dataset by country and then counting the number of different population values (using the `unique()` function) provided for each country over the course of the dataset. These numbers are calculated and shown in the `testing_population_numbers_df` dataframe: 

``` {r testing_population_numbers}
testing_population_numbers_df <- covid_df %>%
  group_by(location) %>%
  summarise(unique_population_numbers = length(unique(population)))
glimpse(testing_population_numbers_df)
unique(testing_population_numbers_df$unique_population_numbers) 
#confirms there is only 1 unique population value for each country in the dataset 
```
The code confirms that each country's population is held constant throughout the dataset, so we move on to next steps. 

### Making column names more informative
To make our modified dataframe easier to interpret, we proceed to change the name of the `location` column to `country` in `covid_select_df`. 

``` {r changing_location_column_name}
names(covid_select_df)[names(covid_select_df) == "location"] <- "country"
glimpse(covid_select_df)
```
### Summarising the data 
Now we move on to transforming the data in such a way that enables us to perform the statistical analyses necessary to answer our research question. Once again, we turn to the `group_by()` function, and then we use the `summarise` function to calculate new values based on the data in `covid_select_df`.

``` {r transforming_data}
covid_select_df$date <- as.Date(covid_select_df$date) 
#formats the date variable so that the min() and max() functions can be used on it to derive
#the earliest and latest dates in which data was recorded 
country_summaries_df <- covid_select_df %>%
  group_by(country) %>% 
  summarise(earliest_date = min(date), 
            #to get a sense of when each country started providing COVID-19 data
            latest_date = max(date), 
            #to get a sense of how up to date the OWID dataset is
            population_estimate = population[date == earliest_date], 
            tests_total = max(total_tests, na.rm = TRUE), 
            deaths_total = max(total_deaths, na.rm = TRUE), 
            hospitalizations_total = sum(weekly_hosp_admissions, na.rm = TRUE))
            #na.rm = TRUE removes missing data points 
glimpse(country_summaries_df)
```
As you can see in this preview of the new `country_summaries_df` dataframe, the data needs to be cleaned further in preparation for statistical analyses.

### Removing data that does not fit 
First, looking at the values listed in the `country` column, it appears that some of the countries are, in fact, continents. This research is interested in examining the possible associations between testing rates and death and hospitalization rates at the country level, though, not at the continent level. Thus, we want to remove the observations that provide data on the continent level. We do this by first reviewing all of the unique values listed in the `country` column using the `unique()` function: 
``` {r countries_not_continents}
unique(country_summaries_df$country)
# the 238 levels listed at the bottom of the output refer to the original number of 
# unique countries with observations in the dataset
```
Looking at this list of unique countries that are listed in the dataset, we can see that five five are continents: Africa, North America, South America, Asia, Europe, and Australia. Fortunately, Australia is also its own country, so we do not need to remove its data, but we will need to remove the data for the other 5 continents. We do so using the `subset()` function:

``` {r subsetting_countries}
country_summaries_df <- country_summaries_df %>%
  subset(country != "Africa" & 
         country != "Asia" & 
         country != "Europe" &  
         country != "North America" & 
         country != "South America")
glimpse(country_summaries_df)
```

### Removing countries without testing data 
Next, we observe that several of the values in the `tests_total`column, which were calculated by taking the maximum value of each country's `total_tests` column in the `covid_select_df` dataframe, are listed as `-Inf`. This indicates that every single value in that country's `total_tests` column is listed as `NA`. Without any testing data provided from those countries, we must remove them from our analyses, which we can accomplish using the `filter()` function: 

``` {r removing_zero_test_countries}
country_summaries_df <- country_summaries_df %>%
  filter(tests_total != -Inf)
glimpse(country_summaries_df)
```
Now all countries in the `country_summaries_df` dataframe have testing data to work with. 

### Checking for countries with no hospitalizations and deaths data 
However, several of the countries remaining in the dataframe have values of 0 in the `hospitalizations_total` column. We will have to remove those countries from analysis of the possible association between testing rates and hospitalizations. We must be careful, though, not to remove those countries necessarily from the analysis of the possible association between testing rates and deaths, as some of the countries with zero hospitalizations could still report deaths. The only remaining countries which we can safely remove from the prospective analysis with good reason are those, if any, that have reported zero hospitalizations and zero deaths. We search for countries that fit this criteria using the `which()` function: 

``` {r checking_zero_deaths_hospitalizations}
which(country_summaries_df$deaths_total == 0 & 
        country_summaries_df$hospitalizations_total == 0)
```
The code output reveals that there are no such countries remaining in the country_summaries_df dataframe that report zero hospitalizations and zero deaths. Therefore, we can now move on to our statistical analyses. 

### Calculating relevant figures 
The last step in preparing for statistical analyses is to transform the existing dataframe by calculating new values to reflect testing, hospitalization, and death rates standardised for population size. In other words, to answer our research questions of whether increased testing is associated with lower deaths and hospitalizations, we must take into account the fact that the various countries examined in the dataset have population sizes which range widely. Therefore, to establish figures that allow for relevant comparisons between countries, we will standardise tests, hospitalizations, and deaths per 100,000 people, which is common practice in the public health field. Additionally, we will create two separate dataframes from the original `country_summaries_df` dataframe so that, in the analysis of each outcome variable (hospitalizations and deaths), those countries who reported *either* zero hospitalizations *or* zero deaths can be removed while not affecting the country's inclusion in the analysis of the other outcome variable, which we know does not measure as zero because we've just confirmed that no remaining countries report zero hospitalizations *and* zero deaths. 

``` {r analysis_datasets}
hospitalizations_tests_df <- country_summaries_df %>%
  select(-deaths_total) %>%
  filter(hospitalizations_total != 0) %>%
  mutate(tests_per_100k = (tests_total / population_estimate) * 100000,
        hospitalizations_per_100k = (hospitalizations_total /   
                                       population_estimate) * 100000)

deaths_tests_df <- country_summaries_df %>%
  select(-hospitalizations_total) %>%
  filter(deaths_total != 0) %>%
  mutate(tests_per_100k = (tests_total / population_estimate) * 100000, 
         deaths_per_100k = (deaths_total / population_estimate) * 100000)

```

## Data exploration and analyses 

### Data visualisation of hospitalizations versus tests 
For the purposes of answering our research question of whether countries with higher testing rates are associated with lower hospitalization and death rates, we will generate scatterplots to visualise the data using the `ggplot2` package in R. Then, after visualising the data, we will conduct simple linear regression modelling to test whether there is a linear relationship between the two outcome variables and test rates. First, we take a look at the plot of hospitalizations versus test rates. 
``` {r data_visualisation}
library(ggplot2)
library(scales)
#hospitalizations vs. test rates 
ggplot(data = hospitalizations_tests_df, 
       mapping = aes(x = tests_per_100k, y = hospitalizations_per_100k)) +
  geom_point() +
  labs(x = "COVID 19 tests conducted per 100,000 people", 
       y = "Hospitalizations per 100,000 people", 
       title = "COVID-19 Hospitalization vs. Test Rates ") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme_linedraw() +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma, limits = c(0, 12500))

```

The plot does not show any discernible association between test rates and hospitalization rates. To drive this home further, we try fitting a best fit linear regression line to the plot to see how well such a model would fit the data: 

### Linear regression modelling of hospitalizations versus tests
``` {r linear_regression_fit}
ggplot(data = hospitalizations_tests_df, 
       mapping = aes(x = tests_per_100k, y = hospitalizations_per_100k)) +
  geom_point() +
  labs(x = "COVID 19 tests conducted per 100,000 people", 
       y = "Hospitalizations per 100,000 people", 
       title = "COVID-19 Hospitalization vs. Test Rates ") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme_linedraw() +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma, limits = c(0, 12500)) +
  geom_smooth(method = "lm") 
  
```

As the best fit linear regression line underscores, a linear model does not fit the observed data with reasonable accuracy. The results of linear regression analysis also confirm this: 
``` {r hospitalizations_linear_regression}
hospitalizations_lm <- lm(hospitalizations_per_100k ~ 
                            tests_per_100k, data = hospitalizations_tests_df)
summary(hospitalizations_lm)
```
The p-value (listed as Pr(>|t|)) is interpreted as the probability that the calculated coefficient for the tests_per_100k variable could be derived in a random sample if the null hypothesis — which states that there is no linear relationship between test rates and death rates — was true. The fact that the p-value is 0.59, much greater than the standard criteria of p<0.05 for declaring findings significant, leads us to accept the null hypothesis. Moreover, the multiple R-square value of 0.01 means that only 1% of the variation in hospitalization rates can be accounted for in the linear regression model; the remaining 99% of the variation may be caused by a multitude of other factors. 

### Interpreting the statistical analysis results 
Consequently, the data does not support the idea that higher test rates are associated with lower hospitalization rates. In fact, there doesn't appear to be any observable relationship between the two variables at all. We could try fitting other types of models to the data such as polynomial models or nonlinear models but, when looking at the actual data distribution, it is fairly clear that testing rates are not a good predictor of hospitalization rates. Furthermore, we have no reason to expect the relationship between the two variables to be anything but linear. The possible implications of this finding are discussed below, but first we must also take a look at the data on test rates versus death rates: 

### Data visualisation of deaths versus tests 

``` {r deaths_tests}
ggplot(data = deaths_tests_df, 
       mapping = aes(x = tests_per_100k, y = deaths_per_100k)) +
  geom_point() +
  labs(x = "COVID-19 tests conducted per 100,000 people", 
       y = "Deaths per 100,000 people", 
       title = "COVID-19 Death vs. Test Rates") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme_linedraw() +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma) 
  
```
Once again, the scatterplot of our predictor variable (tests conducted per 100,000 people) versus our outcome variable (deaths per 100,000 people) does not reveal any clear, discernible relationship between the two variables. Plotting the best fit linear regression line to the data further confirms this observation: 

``` {r deaths_best_fit_line}
ggplot(data = deaths_tests_df, 
       mapping = aes(x = tests_per_100k, y = deaths_per_100k)) +
  geom_point() +
  labs(x = "COVID-19 tests conducted per 100,000 people", 
       y = "Deaths per 100,000 people", 
       title = "COVID-19 Death vs. Test Rates") +
  theme(text = element_text(size = 10)) +
  theme_bw() +
  theme_linedraw() +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma) + 
  geom_smooth(method = "lm")
```

### Linear regression modelling of deaths versus tests 
The statistical results of the linear regression modelling also confirm our observations of the plotted data: 

``` {r deaths_regression_results}
deaths_lm <- lm(deaths_per_100k ~ tests_per_100k, data = deaths_tests_df)
summary(deaths_lm)
```
Yet again, the p-value of the regression coefficient for the tests_per_100k variable as well as the multiple R-squared value confirm that the null hypothesis is true: there is no linear relationship between test rates and death rates. 

## Conclusions     

One could look at the data analyses detailed above and conclude that, perhaps, testing is not as useful in reducing adverse health outcomes in a pandemic as public health experts believe. However, that conclusion is likely incorrect for several reasons. For one, testing has been repeatedly found to be an invaluable tool in controlling infectious disease outbreaks in the past 50 years. It is impossible to know the true extent of a disease's spread and its potential to infect more people without identifying which people have it. With this in mind, a more nuanced interpretation of the data would be to conclude that the efficacy of testing is highly influenced by other the manner in which testing regimes are conducted as well as other mediating factors. 

There are a number of different strategies for administering widespread testing in a pandemic, and all strategies are not equal. Though the quantity of tests conducted is important, what is most essential is that tests be targeted effectively at populations which are most likely to be infected. Scattershot testing administered indiscriminately can lead to extensive waste of limited resources. Test supplies and laboratory infrastructure can be unnecessarily exhausted if testing is not carried out judiciously, based on evidence-based predictions of which populations are most likely to experience an outbreak. Additionally, testing regimes may be hamstrung by the quality of tests and laboratory infrastructure that is available. For example, we know that some COVID-19 tests are more valid — meaning able to distinguish  individuals who have a disease from those who do not — than others. Cost and supply chain issues are just two examples of the many factors that can influence the *quality* of tests used by a given country. 

Even when the best, most strategic and targeted testing regimes are implemented with valid tests, other variables can still have major impacts on the outcomes achieved. The beneficial effects of successful testing protocols can easily be reversed by poor decisions made in other areas. For example, a country with the most advanced testing program can easily experience devastating death and hospitalization rates if the disease is very infectious and no other mitigation measures such as masking, social distancing, quarantines, or lockdowns are implemented earnestly in conjunction with testing. Treatment of the disease could also be quite poor in comparison to other health systems. In total, there are countless mediating factors which could prevent an otherwise effective testing regime from achieving lower hospitalization and death rates. 

While the data does not merit dismissing the efficacy of widespread testing, it is nonetheless notable that testing rates were not a predictor of hospitalization or death rates. We do not know the exact combination of mediating factors that would cause this, but it is apparent that testing is just one variable of many that can influence the effectiveness of a country's pandemic response. Future research could look at a number of other relevant variables (e.g., vaccination rates, days spent in lockdown, or population density) alongside testing rates in multiple regression modelling to determine which factors might be associated with hospitalization and death rates. 


```{r}
rmarkdown::render_site()```
