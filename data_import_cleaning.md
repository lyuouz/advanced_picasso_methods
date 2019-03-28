Data Import and Cleaning
================
Lyuou Zhang
3/27/2019

## NOAA data

### Step 1. pull NOAA data from its API

I pulled all the stations in NY and used their IDs to pull all the data
at these stations. `stations` is the dataset with all NY stations.
`nydat` is the dataset that has the NY weather data

``` r
nystationids <- ghcnd_stations() %>% 
  filter(state == "NY") %>% 
  distinct(id)

# Pull the desired weather data for all of these stations
nydat <- meteo_pull_monitors(nystationids$id, 
                             date_min = "2007-01-01", 
                             date_max = "2017-12-31", 
                             var = c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN"))
```

Note that this dataset does not have the county names. To find the
county for each station, we used the latitude and longitude of each
station and spatially join with the NYS county shapefile in GIS to get
the county names.

``` r
ny_county <- stations %>% 
  filter(state == "NY") %>% 
  dplyr::select(id:name, -state) %>% 
  .[!duplicated(.[c('id', 'latitude', 'longitude', 'elevation', 'name')]),]
```

### Step 2: Join station IDs with counties, and with NOAA data

Some work behind the scene in QGIS…

After we figured out the county of each weather station, joined that
back to the NOAA data in `nydat`

``` r
# import the ny county file joined with the county names from QGIS
ny_county_name <- read_csv('./data/ny_county_name.csv') %>% 
  janitor::clean_names() %>% 
  dplyr::select(-field_1) %>% 
  rename(county = name_1)
 

# join with nydata, and convert the temperature to degree celsius.
nydata <- nydat %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  left_join(., ny_county_name, by = 'id') %>% 
  mutate(
    tmax = tmax/10,
    tmin = tmin/10
  )
```

Yayyy\! So the final NOAA data will be `nydata`. Be sure to **remove all
the `NA` values** before you use it.

## Birth data

This one is a lot
easier.

``` r
births <- read_delim('./data/NYS_0717_groupby_CYM_OEGestWeekly.txt', delim = '\t') %>% 
  janitor::clean_names() %>% 
  filter(is.na(notes)) %>% 
  filter(county_code != '36999') %>% 
  select(-notes)
```

    ## Parsed with column specification:
    ## cols(
    ##   Notes = col_character(),
    ##   County = col_character(),
    ##   `County Code` = col_integer(),
    ##   Year = col_integer(),
    ##   `Year Code` = col_integer(),
    ##   Month = col_character(),
    ##   `Month Code` = col_integer(),
    ##   `OE Gestational Age Weekly` = col_character(),
    ##   `OE Gestational Age Weekly Code` = col_integer(),
    ##   Births = col_integer(),
    ##   `Average Birth Weight` = col_double()
    ## )

    ## Warning in rbind(names(probs), probs_f): number of columns of result is not
    ## a multiple of vector length (arg 1)

    ## Warning: 73 parsing failures.
    ## row # A tibble: 5 x 5 col     row col             expected actual      file                           expected   <int> <chr>           <chr>    <chr>       <chr>                          actual 1 23192 Average Birth … a double Not Applic… './data/NYS_0717_groupby_CYM_… file 2 23202 Average Birth … a double Not Applic… './data/NYS_0717_groupby_CYM_… row 3 23213 Average Birth … a double Not Applic… './data/NYS_0717_groupby_CYM_… col 4 23242 Average Birth … a double Not Applic… './data/NYS_0717_groupby_CYM_… expected 5 23273 Average Birth … a double Not Applic… './data/NYS_0717_groupby_CYM_…
    ## ... ................. ... ........................................................................... ........ ........................................................................... ...... ........................................................................... .... ........................................................................... ... ........................................................................... ... ........................................................................... ........ ...........................................................................
    ## See problems(...) for more details.

## SES and population data

This part includes **education**, **household income** and
**population** data import and cleaning. The data is from [American
FactFinder](https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml).

### Education

  - I cleaned the data and only kept columns that are percent of
    population by each education level for people age **25 and over**.  
  - I also created two new variables:
      - `without_hs` is the percent of population without a high school
        degree  
      - `college` is the percent of population with bachelors degree or
        higher

<!-- end list -->

``` r
education <- read_csv('./data/ACS_17_5YR_S1501_education/ACS_17_5YR_S1501_with_ann.csv') %>% 
  janitor::clean_names() %>% 
  dplyr::select(id2, contains('25_years_and_over'), -contains('total'), -contains('margin_of_error'), -contains('male')) %>% 
  dplyr::select(id2, percent_estimate_population_25_years_and_over_less_than_9th_grade:percent_estimate_population_25_years_and_over_graduate_or_professional_degree) %>% 
  rename(
    geoid = id2,
    less_than_9 = percent_estimate_population_25_years_and_over_less_than_9th_grade,
    grade_9_12 = percent_estimate_population_25_years_and_over_9th_to_12th_grade_no_diploma,
    high_school = percent_estimate_population_25_years_and_over_high_school_graduate_includes_equivalency,
    some_college = percent_estimate_population_25_years_and_over_some_college_no_degree,
    associates = percent_estimate_population_25_years_and_over_associates_degree,
    bachelors = percent_estimate_population_25_years_and_over_bachelors_degree,
    graduate_or_professional =  percent_estimate_population_25_years_and_over_graduate_or_professional_degree
  ) %>% 
  mutate(
    without_hs = less_than_9 + grade_9_12,
    college = bachelors + graduate_or_professional,
    geoid = as.character(geoid)
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   Geography = col_character(),
    ##   `Percent; Estimate; Population 18 to 24 years` = col_character(),
    ##   `Percent; Margin of Error; Population 18 to 24 years` = col_character(),
    ##   `Percent Male; Estimate; Population 18 to 24 years` = col_character(),
    ##   `Percent Male; Margin of Error; Population 18 to 24 years` = col_character(),
    ##   `Percent Female; Estimate; Population 18 to 24 years` = col_character(),
    ##   `Percent Female; Margin of Error; Population 18 to 24 years` = col_character(),
    ##   `Percent; Estimate; Population 18 to 24 years - Less than high school graduate` = col_double(),
    ##   `Percent; Margin of Error; Population 18 to 24 years - Less than high school graduate` = col_double(),
    ##   `Percent Male; Estimate; Population 18 to 24 years - Less than high school graduate` = col_double(),
    ##   `Percent Male; Margin of Error; Population 18 to 24 years - Less than high school graduate` = col_double(),
    ##   `Percent Female; Estimate; Population 18 to 24 years - Less than high school graduate` = col_double(),
    ##   `Percent Female; Margin of Error; Population 18 to 24 years - Less than high school graduate` = col_double(),
    ##   `Percent; Estimate; Population 18 to 24 years - High school graduate (includes equivalency)` = col_double(),
    ##   `Percent; Margin of Error; Population 18 to 24 years - High school graduate (includes equivalency)` = col_double(),
    ##   `Percent Male; Estimate; Population 18 to 24 years - High school graduate (includes equivalency)` = col_double(),
    ##   `Percent Male; Margin of Error; Population 18 to 24 years - High school graduate (includes equivalency)` = col_double(),
    ##   `Percent Female; Estimate; Population 18 to 24 years - High school graduate (includes equivalency)` = col_double(),
    ##   `Percent Female; Margin of Error; Population 18 to 24 years - High school graduate (includes equivalency)` = col_double(),
    ##   `Percent; Estimate; Population 18 to 24 years - Some college or associate's degree` = col_double()
    ##   # ... with 411 more columns
    ## )

    ## See spec(...) for full column specifications.

### Household income

I only keep the median income for each
household.

``` r
income <- read_csv('./data/ACS_17_5YR_S1903_income/ACS_17_5YR_S1903_with_ann.csv') %>% 
  janitor::clean_names() %>% 
  dplyr::select(id2, median_income_dollars_estimate_households) %>% 
  rename(
    geoid = id2,
    med_income = median_income_dollars_estimate_households
  ) %>% 
  mutate(geoid = as.character(geoid))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   Geography = col_character(),
    ##   `Percent Distribution; Estimate; Households - One race-- - White` = col_double(),
    ##   `Percent Distribution; Margin of Error; Households - One race-- - White` = col_double(),
    ##   `Percent Distribution; Estimate; Households - One race-- - Black or African American` = col_double(),
    ##   `Percent Distribution; Margin of Error; Households - One race-- - Black or African American` = col_double(),
    ##   `Median income (dollars); Estimate; Households - One race-- - Black or African American` = col_character(),
    ##   `Median income (dollars); Margin of Error; Households - One race-- - Black or African American` = col_character(),
    ##   `Percent Distribution; Estimate; Households - One race-- - American Indian and Alaska Native` = col_double(),
    ##   `Percent Distribution; Margin of Error; Households - One race-- - American Indian and Alaska Native` = col_double(),
    ##   `Median income (dollars); Estimate; Households - One race-- - American Indian and Alaska Native` = col_character(),
    ##   `Median income (dollars); Margin of Error; Households - One race-- - American Indian and Alaska Native` = col_character(),
    ##   `Percent Distribution; Estimate; Households - One race-- - Asian` = col_double(),
    ##   `Percent Distribution; Margin of Error; Households - One race-- - Asian` = col_double(),
    ##   `Median income (dollars); Estimate; Households - One race-- - Asian` = col_character(),
    ##   `Median income (dollars); Margin of Error; Households - One race-- - Asian` = col_character(),
    ##   `Percent Distribution; Estimate; Households - One race-- - Native Hawaiian and Other Pacific Islander` = col_double(),
    ##   `Percent Distribution; Margin of Error; Households - One race-- - Native Hawaiian and Other Pacific Islander` = col_double(),
    ##   `Median income (dollars); Estimate; Households - One race-- - Native Hawaiian and Other Pacific Islander` = col_character(),
    ##   `Median income (dollars); Margin of Error; Households - One race-- - Native Hawaiian and Other Pacific Islander` = col_character(),
    ##   `Percent Distribution; Estimate; Households - One race-- - Some other race` = col_double()
    ##   # ... with 87 more columns
    ## )

    ## See spec(...) for full column specifications.

### Population

Population should be in the model too
(log(N)).

``` r
pop <- read_csv('./data/ACS_17_5YR_DP05_population/ACS_17_5YR_DP05_with_ann.csv') %>% 
  janitor::clean_names() %>% 
  dplyr::select(id2, estimate_sex_and_age_total_population) %>% 
  rename(
    geoid = id2,
    pop = estimate_sex_and_age_total_population
  ) %>% 
  mutate(geoid = as.character(geoid))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   Geography = col_character(),
    ##   `Margin of Error; SEX AND AGE - Total population` = col_character(),
    ##   `Percent Margin of Error; SEX AND AGE - Total population` = col_character(),
    ##   `Percent; SEX AND AGE - Total population - Male` = col_double(),
    ##   `Percent Margin of Error; SEX AND AGE - Total population - Male` = col_double(),
    ##   `Percent; SEX AND AGE - Total population - Female` = col_double(),
    ##   `Percent Margin of Error; SEX AND AGE - Total population - Female` = col_double(),
    ##   `Estimate; SEX AND AGE - Total population - Sex ratio (males per 100 females)` = col_double(),
    ##   `Margin of Error; SEX AND AGE - Total population - Sex ratio (males per 100 females)` = col_double(),
    ##   `Percent; SEX AND AGE - Total population - Sex ratio (males per 100 females)` = col_character(),
    ##   `Percent Margin of Error; SEX AND AGE - Total population - Sex ratio (males per 100 females)` = col_character(),
    ##   `Margin of Error; SEX AND AGE - Under 5 years` = col_character(),
    ##   `Percent; SEX AND AGE - Under 5 years` = col_double(),
    ##   `Percent Margin of Error; SEX AND AGE - Under 5 years` = col_character(),
    ##   `Percent; SEX AND AGE - 5 to 9 years` = col_double(),
    ##   `Percent Margin of Error; SEX AND AGE - 5 to 9 years` = col_double(),
    ##   `Percent; SEX AND AGE - 10 to 14 years` = col_double(),
    ##   `Percent Margin of Error; SEX AND AGE - 10 to 14 years` = col_double(),
    ##   `Percent; SEX AND AGE - 15 to 19 years` = col_double(),
    ##   `Percent Margin of Error; SEX AND AGE - 15 to 19 years` = col_double()
    ##   # ... with 175 more columns
    ## )

    ## See spec(...) for full column specifications.

### Combine all the datasets

The final SES dataset is `ses.csv`

``` r
ses <- education %>% 
  left_join(., income, by = 'geoid') %>% 
  left_join(., pop, by = 'geoid')

write.csv(ses, './data/ses.csv')
```
