Recoding Variables
================

    ## # A tibble: 6 x 1,074
    ##   case_identifier  caseid weight_panel weight_latino weight_18_24 weight_overall
    ##             <dbl>   <dbl>        <dbl> <lgl>         <lgl>                 <dbl>
    ## 1             779  3.82e8        0.503 NA            NA                    0.360
    ## 2            2108  3.82e8        0.389 NA            NA                    0.736
    ## 3            2597  3.82e8        0.684 NA            NA                    0.632
    ## 4            4148 NA            NA     NA            NA                   NA    
    ## 5            4460  3.82e8        0.322 NA            NA                    0.488
    ## 6            5225  3.82e8        0.594 NA            NA                    0.514
    ## # ... with 1,068 more variables: cassfullcd <dbl>, add_confirm_2018 <dbl>,
    ## #   inputzip_2018 <dbl>, votereg_2018 <dbl>, votereg_f_2018 <dbl>,
    ## #   regzip_2018 <dbl>, inputstate2_2018 <dbl>, vote18_2018 <dbl>,
    ## #   vote18_other_2018 <dbl>, trumpapp_2018 <dbl>, trumpfeel_2018 <dbl>,
    ## #   fav_trump_2018 <dbl>, fav_ryan_2018 <dbl>, fav_obama_2018 <dbl>,
    ## #   fav_hrc_2018 <dbl>, fav_sanders_2018 <dbl>, fav_putin_2018 <dbl>,
    ## #   fav_schumer_2018 <dbl>, fav_pelosi_2018 <dbl>, fav_comey_2018 <dbl>,
    ## #   fav_mueller_2018 <dbl>, fav_mcconnell_2018 <dbl>,
    ## #   systems_leader_2018 <dbl>, systems_army_2018 <dbl>,
    ## #   systems_democ_2018 <dbl>, governed_2018 <dbl>, view1_2018 <dbl>,
    ## #   satisf_dem_2018 <dbl>, view2_2018 <dbl>, inst_court_2018 <dbl>,
    ## #   inst_media_2018 <dbl>, inst_congress_2018 <dbl>, inst_justice_2018 <dbl>,
    ## #   inst_FBI_2018 <dbl>, inst_military_2018 <dbl>, inst_church_2018 <dbl>,
    ## #   inst_business_2018 <dbl>, track_2018 <dbl>, persfinretro_2018 <dbl>,
    ## #   econtrend_2018 <dbl>, life_2018 <dbl>, stranger_2018 <dbl>,
    ## #   trustgovt_2018 <dbl>, immi_contribution_2018 <dbl>,
    ## #   immi_naturalize_2018 <dbl>, immi_makedifficult_2018 <dbl>,
    ## #   immi_muslim_2018 <dbl>, immi_stay_2018 <dbl>, immi_num_2018 <dbl>,
    ## #   immi_region_eur_2018 <dbl>, immi_region_latin_2018 <dbl>,
    ## #   immi_region_mid_2018 <dbl>, immi_region_india_2018 <dbl>,
    ## #   immi_region_china_2018 <dbl>, immi_region_afr_2018 <dbl>,
    ## #   immi_legal_2018 <dbl>, tax_2018 <dbl>, tax_class_you_2018 <dbl>,
    ## #   tax_class_economy_2018 <dbl>, tax_class_poor_2018 <dbl>,
    ## #   tax_class_wealthy_2018 <dbl>, tax_class_middle_2018 <dbl>,
    ## #   tax_self_2018 <dbl>, tax_goal_growth_2018 <dbl>,
    ## #   tax_goal_corpor_2018 <dbl>, tax_goal_wealthy_2018 <dbl>,
    ## #   tax_goal_middle_2018 <dbl>, tax_goal_poor_2018 <dbl>,
    ## #   tax_goal_federal_2018 <dbl>, tax_goal_you_2018 <dbl>, tax_corp_2018 <dbl>,
    ## #   tax_rich_2018 <dbl>, taxdoug_2018 <dbl>, tax_sys_2018 <dbl>,
    ## #   regula_2018 <dbl>, marij_2018 <dbl>, nkorea_2018 <dbl>, speech_2018 <dbl>,
    ## #   amendment_2018 <dbl>, anthem_2018 <dbl>, CR_touch_2018 <dbl>,
    ## #   CR_careA_2018 <dbl>, CR_careB_2018 <dbl>, CR_careC_2018 <dbl>,
    ## #   CR_careD_2018 <dbl>, CR_careE_2018 <dbl>, CR_careF_2018 <dbl>,
    ## #   CR_careG_2018 <dbl>, CR_careH_2018 <dbl>, parties_2018 <dbl>,
    ## #   represent_dem_2018 <dbl>, represent_rep_2018 <dbl>, prio_dem_2018 <dbl>,
    ## #   prio_rep_2018 <dbl>, third_econ_2018 <dbl>, third_soc_2018 <dbl>,
    ## #   third_immi_2018 <dbl>, democracy_2018 <dbl>, elect_2018 <dbl>,
    ## #   house_amount_2018 <dbl>, ...

<br> <br>

## How does feeling toward immigration vary among Americans?

Variables: Immigration\_Should\_Be, Raised\_In\_US, Raised\_Religion,
Occupation, FT\_Trump, Politics\_OR\_Current\_Events

  

``` r
MyVoterData <- FullVoterData %>% 
  mutate(Immigration_Should_Be = ifelse(immi_makedifficult_2018 == 1, "Much Easier",
                                 ifelse(immi_makedifficult_2018 == 2, "Slightly Easier",
                                 ifelse(immi_makedifficult_2018 == 3, "No Change",
                                 ifelse(immi_makedifficult_2018 == 4, "Slightly Harder",
                                 ifelse(immi_makedifficult_2018 == 5, "Much Harder",
                                 ifelse(immi_makedifficult_2018 == 8, "Don't Know", NA)))))),
         Raised_In_US = ifelse(childhood_2018 == 1, "Yes",
                        ifelse(childhood_2018 == 2, "No", NA)),
         Raised_Religion = ifelse(relig_2018 == 1, "Protestant",
                           ifelse(relig_2018 == 2, "Roman Catholic",
                           ifelse(relig_2018 == 3, "Morman",
                           ifelse(relig_2018 == 4, "Eastern or Greek Orthodox",
                           ifelse(relig_2018 == 5, "Jewish",
                           ifelse(relig_2018 == 6, "Muslim",
                           ifelse(relig_2018 == 7, "Buddhist",
                           ifelse(relig_2018 == 8, "Hindu",
                           ifelse(relig_2018 == 9, "Atheist",
                           ifelse(relig_2018 == 10, "Agnostic",
                           ifelse(relig_2018 == 11, "Nothing in Particular",
                           ifelse(relig_2018 == 12, "Something Else", NA)))))))))))),
         Occupation = ifelse(occupationcat_baseline == 1, "Professional/Technical",
                      ifelse(occupationcat_baseline == 2, "Executive/Upper Management",
                      ifelse(occupationcat_baseline == 3, "Middle Management",
                      ifelse(occupationcat_baseline == 4, "Sales",
                      ifelse(occupationcat_baseline == 5, "Business Owner/Self-Employed",
                      ifelse(occupationcat_baseline == 6, "Clerical/Administrative",
                      ifelse(occupationcat_baseline == 7, "Military/Civilia Uniform Services",
                      ifelse(occupationcat_baseline == 8, "Retired",
                      ifelse(occupationcat_baseline == 9, "Homemaker",
                      ifelse(occupationcat_baseline == 10, "Student",
                      ifelse(occupationcat_baseline == 11, "Unemployed",
                      ifelse(occupationcat_baseline == 11, "Other", NA)))))))))))),
         FT_Trump = ifelse(trumpfeel_2018 == 1, "Like and Approve",
                    ifelse(trumpfeel_2018 == 2, "Like but Disapprove",
                    ifelse(trumpfeel_2018 == 3, "Dislike but Approve",
                    ifelse(trumpfeel_2018 == 4, "Dislike and Disapprove",
                    ifelse(trumpfeel_2018 == 8, "Not Sure", NA))))),
         Politics_OR_Current_Events = ifelse(polinterest_baseline == 1, "Very Interested",
                                      ifelse(polinterest_baseline == 2, "Somewhat Interested",
                                      ifelse(polinterest_baseline == 3, "Not Very Interested",
                                      ifelse(polinterest_baseline == 4, "Nothing", NA))))) %>% 
  select(Immigration_Should_Be, Raised_In_US, Raised_Religion, Occupation, FT_Trump, Politics_OR_Current_Events)

head(MyVoterData)
```

    ## # A tibble: 6 x 6
    ##   Immigration_Sho~ Raised_In_US Raised_Religion Occupation FT_Trump
    ##   <chr>            <chr>        <chr>           <chr>      <chr>   
    ## 1 Slightly Easier  Yes          Roman Catholic  Middle Ma~ Dislike~
    ## 2 Slightly Harder  Yes          Morman          Retired    Like an~
    ## 3 Much Harder      Yes          Roman Catholic  Professio~ Dislike~
    ## 4 <NA>             <NA>         <NA>            Professio~ <NA>    
    ## 5 Much Harder      Yes          Roman Catholic  Professio~ Dislike~
    ## 6 No Change        Yes          Roman Catholic  Retired    Dislike~
    ## # ... with 1 more variable: Politics_OR_Current_Events <chr>

  

## Tables

``` r
prop.table(table(MyVoterData$Raised_In_US, MyVoterData$Immigration_Should_Be),1) 
```

    ##      
    ##       Don't Know Much Easier Much Harder  No Change Slightly Easier
    ##   No  0.11557789  0.12060302  0.13065327 0.27135678      0.18341709
    ##   Yes 0.06669063  0.10965306  0.21966565 0.24555096      0.17670322
    ##      
    ##       Slightly Harder
    ##   No       0.17839196
    ##   Yes      0.18173647

``` r
prop.table(table(MyVoterData$Raised_Religion, MyVoterData$Immigration_Should_Be),1) 
```

    ##                            
    ##                             Don't Know Much Easier Much Harder  No Change
    ##   Agnostic                  0.09859155  0.16901408  0.07042254 0.26760563
    ##   Atheist                   0.06779661  0.28813559  0.10169492 0.15254237
    ##   Buddhist                  0.04347826  0.04347826  0.26086957 0.30434783
    ##   Eastern or Greek Orthodox 0.02564103  0.15384615  0.12820513 0.25641026
    ##   Hindu                     0.18181818  0.27272727  0.09090909 0.27272727
    ##   Jewish                    0.04854369  0.16990291  0.16504854 0.24271845
    ##   Morman                    0.10416667  0.21875000  0.14583333 0.22916667
    ##   Muslim                    0.00000000  0.20000000  0.04000000 0.32000000
    ##   Nothing in Particular     0.10340314  0.14267016  0.20026178 0.21596859
    ##   Protestant                0.05060353  0.09749304  0.22655525 0.27205200
    ##   Roman Catholic            0.05833764  0.09912235  0.22509035 0.23644812
    ##   Something Else            0.13859649  0.08421053  0.21929825 0.24035088
    ##                            
    ##                             Slightly Easier Slightly Harder
    ##   Agnostic                       0.25352113      0.14084507
    ##   Atheist                        0.25423729      0.13559322
    ##   Buddhist                       0.17391304      0.17391304
    ##   Eastern or Greek Orthodox      0.15384615      0.28205128
    ##   Hindu                          0.09090909      0.09090909
    ##   Jewish                         0.22330097      0.15048544
    ##   Morman                         0.18750000      0.11458333
    ##   Muslim                         0.20000000      0.24000000
    ##   Nothing in Particular          0.18062827      0.15706806
    ##   Protestant                     0.16480966      0.18848654
    ##   Roman Catholic                 0.18998451      0.19101704
    ##   Something Else                 0.13859649      0.17894737

``` r
prop.table(table(MyVoterData$Occupation, MyVoterData$Immigration_Should_Be),1)
```

    ##                                    
    ##                                     Don't Know Much Easier Much Harder
    ##   Business Owner/Self-Employed      0.03472222  0.04861111  0.21875000
    ##   Clerical/Administrative           0.06483791  0.07481297  0.26184539
    ##   Executive/Upper Management        0.03589744  0.12307692  0.21538462
    ##   Homemaker                         0.08401084  0.04336043  0.31165312
    ##   Middle Management                 0.04432133  0.08864266  0.20498615
    ##   Military/Civilia Uniform Services 0.05263158  0.07017544  0.22807018
    ##   Professional/Technical            0.04670051  0.11370558  0.20609137
    ##   Retired                           0.03363519  0.09702458  0.25743855
    ##   Sales                             0.03947368  0.07017544  0.27631579
    ##   Student                           0.10126582  0.17721519  0.13924051
    ##   Unemployed                        0.09235669  0.09235669  0.25477707
    ##                                    
    ##                                      No Change Slightly Easier Slightly Harder
    ##   Business Owner/Self-Employed      0.29861111      0.22222222      0.17708333
    ##   Clerical/Administrative           0.26184539      0.15960100      0.17705736
    ##   Executive/Upper Management        0.21025641      0.18461538      0.23076923
    ##   Homemaker                         0.24390244      0.13008130      0.18699187
    ##   Middle Management                 0.24930748      0.15235457      0.26038781
    ##   Military/Civilia Uniform Services 0.26315789      0.12280702      0.26315789
    ##   Professional/Technical            0.26700508      0.20203046      0.16446701
    ##   Retired                           0.25226391      0.15265201      0.20698577
    ##   Sales                             0.24561404      0.17105263      0.19736842
    ##   Student                           0.17721519      0.30379747      0.10126582
    ##   Unemployed                        0.24203822      0.14012739      0.17834395

``` r
prop.table(table(MyVoterData$FT_Trump, MyVoterData$Immigration_Should_Be),1) 
```

    ##                         
    ##                          Don't Know Much Easier Much Harder  No Change
    ##   Dislike and Disapprove 0.07434451  0.17789578  0.09359442 0.27049452
    ##   Dislike but Approve    0.05110220  0.04909820  0.28857715 0.24649299
    ##   Like and Approve       0.02311757  0.03236460  0.41941876 0.20805812
    ##   Like but Disapprove    0.08988764  0.05617978  0.16292135 0.30337079
    ##   Not Sure               0.35185185  0.05555556  0.15185185 0.17407407
    ##                         
    ##                          Slightly Easier Slightly Harder
    ##   Dislike and Disapprove      0.23796880      0.14570196
    ##   Dislike but Approve         0.13426854      0.23046092
    ##   Like and Approve            0.10105680      0.21598415
    ##   Like but Disapprove         0.14606742      0.24157303
    ##   Not Sure                    0.09629630      0.17037037

``` r
prop.table(table(MyVoterData$Politics_OR_Current_Events, MyVoterData$Immigration_Should_Be),1) 
```

    ##                      
    ##                       Don't Know Much Easier Much Harder  No Change
    ##   Not Very Interested 0.12624585  0.04318937  0.27574751 0.24584718
    ##   Nothing             0.27027027  0.05405405  0.24324324 0.21621622
    ##   Somewhat Interested 0.07367668  0.06080114  0.25178827 0.27038627
    ##   Very Interested     0.03517078  0.10517416  0.23469733 0.25228272
    ##                      
    ##                       Slightly Easier Slightly Harder
    ##   Not Very Interested      0.09966777      0.20930233
    ##   Nothing                  0.02702703      0.18918919
    ##   Somewhat Interested      0.13304721      0.21030043
    ##   Very Interested          0.19614474      0.17653027
