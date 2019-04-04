Mixed Models
================
Lyuou Zhang
3/28/2019

## Data Import

We are studying the impact of temperature variability on the number of
preterm births, adjusted by SES status. We are using these data:

  - Temperature data from NOAA: `nydata_month.csv`  
  - Birth data from CDC WONDER: `birth_2007_2010.csv`  
  - SES data from ACS: `ses.csv`

Note that for analysis, I need to convert the birth data to binary: \#
of preterm birth and \# of not preterm birth

``` r
# read temp data
nydata_month <- read_csv('./data/nydata_month.csv') %>% 
  dplyr::select(-X1)
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_integer(),
    ##   county = col_character(),
    ##   year = col_integer(),
    ##   month = col_integer(),
    ##   temp = col_double(),
    ##   sd_temp = col_double()
    ## )

``` r
# read birth data
births <- read_csv('./data/birth_2007_2010.csv') %>% 
  dplyr::select(-X1, -year, -average_birth_weight, -month, -oe_gestational_age_weekly_code) %>% 
  mutate(
    county_code = as.character(county_code)
    ) %>% 
  spread(
    key = oe_gestational_age_weekly, value = births
  ) %>% 
  dplyr::select(-Unknown) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  mutate(
    preterm = `24 weeks` + `25 weeks` + `26 weeks` + `27 weeks` + `28 weeks` + `29 weeks` + `30 weeks` + `31 weeks` + `32 weeks` + `33 weeks` + `34 weeks` + `35 weeks` + `36 weeks` + `37 weeks`,
    non_preterm = `38 weeks` + `39 weeks` + `40 weeks` + `41 weeks` + `42 weeks` + `43 weeks`
    ) %>% 
  dplyr::select(-(`24 weeks`:`43 weeks`)) %>% 
  mutate(
    county = str_replace(county, ' County, NY', '')
  )
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_integer(),
    ##   county = col_character(),
    ##   county_code = col_integer(),
    ##   year = col_integer(),
    ##   year_code = col_integer(),
    ##   month = col_character(),
    ##   month_code = col_integer(),
    ##   oe_gestational_age_weekly = col_character(),
    ##   oe_gestational_age_weekly_code = col_integer(),
    ##   births = col_integer(),
    ##   average_birth_weight = col_double()
    ## )

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## please use list() instead
    ## 
    ## # Before:
    ## funs(name = f(.)
    ## 
    ## # After: 
    ## list(name = ~f(.))
    ## This warning is displayed once per session.

``` r
# import ses data
ses <- read_csv('./data/ses.csv') %>% 
  dplyr::select(-X1) %>% 
  mutate(geoid = as.character(geoid))
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_integer(),
    ##   geoid = col_integer(),
    ##   less_than_9 = col_double(),
    ##   grade_9_12 = col_double(),
    ##   high_school = col_double(),
    ##   some_college = col_double(),
    ##   associates = col_double(),
    ##   bachelors = col_double(),
    ##   graduate_or_professional = col_double(),
    ##   without_hs = col_double(),
    ##   college = col_double(),
    ##   med_income = col_integer(),
    ##   pop = col_integer()
    ## )

## Between and within subject variablity

Here I use **ICC** to compare between and within subject variability.

**Criteria:**  
\* ICC \> 0.75: Excellent reproducibility  
\* 0.4 \<= ICC \< 0.75: Fair to good reproducibility  
\* ICC \< 0.4: Poor reproducibility

Note that the variables to use in the ICC should be the percentage of
preterm births rather than the number of preterm births, because the
latter depends on the total population of that county.

I will save the ICC results of outcome in `icc_birth`

``` r
icc_birth <- births %>% 
  mutate(
    preterm_rate = preterm/(preterm + non_preterm)
  ) %>% 
  ICCest(county_code, preterm_rate, data = .)
```

Then the exposure variable, saved in `icc_temp`

``` r
icc_temp <- births %>% 
  left_join(., nydata_month, by = 'county') %>% 
  ICCest(county_code, sd_temp, data = .)
```

    ## NAs removed from rows:
    ##  297661 297662 297663 297664 297665 297666 297667 297668 297669 297670 297671 297672 297673 297674 297675 297676 297677 297678 297679 297680 297681 297682 297683 297684 297685 297686 297687 297688 297689 297690 297691 297692 297693 297694 297695 297696 297697 297698 297699 297700 297701 297702 297703 297704 297705 297706 297707 297708 297709 297710 297711 297712 297713 297714 297715 297716 297717 297718 297719 297720 297721 297722 297723 297724 297725 297726 297727 297728 297729 297730 297731 297732 297733 297734 297735 297736 297737 297738 297739 297740 297741 297742 297743 297744 297745 297746 297747 297748 297749 297750 297751 297752 297753 297754 297755 297756 297757 297758 297759 297760 297761 297762 297763 297764 297765 297766 297767 297768 297769 297770 297771 297772 297773 297774 297775 297776 297777 297778 297779 297780 297781 297782 297783 297784 297785 297786 297787 297788 297789 297790 297791 297792 297793 297794 297795 297796 297797 297798 297799 297800 297801 297802 297803 297804 297805 297806 297807 297808 297809 297810 297811 297812 297813 297814 297815 297816 297817 297818 297819 297820 297821 297822 297823 297824 297825 297826 297827 297828 297829 297830 297831 297832 297833 297834 297835 297836 297837 297838 297839 297840 297841 297842 297843 297844 297845 297846 297847 297848 297849 297850 297851 297852 297853 297854 297855 297856 297857 297858 297859 297860 297861 297862 297863 297864 297865 297866 297867 297868 297869 297870 297871 297872 297873 297874 297875 297876 297877 297878 297879 297880 297881 297882 297883 297884 297885 297886 297887 297888 297889 297890 297891 297892 297893 297894 297895 297896 297897 297898 297899 297900 297901 297902 297903 297904 297905 297906 297907 297908 297909 297910 297911 297912 297913 297914 297915 297916 297917 297918 297919 297920 297921 297922 297923 297924

    ## Warning in ICCest(county_code, sd_temp, data = .):

    ## Warning in ICCest(county_code, sd_temp, data = .): Missing levels of 'x'
    ## have been removed

  - ICC for Y: (0.5220499, 0.76557)
  - ICC for X: (0.051872, 0.1450301)
