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
