University of Illinois Survey On Neighborhood Health Analysis
========================================================
author: Nana Boateng
date:  March 30,2017
autosize: true

Introduction
========================================================

For more details on authoring R presentations please visit <https://support.rstudio.com/hc/en-us/articles/200486468>.

- Bullet 1
- Bullet 2
- Bullet 3

Slide With Code
========================================================



Dataset
========================================================


| ID|Gender | Age|Hypertension |Employment            |Hispanic |Education                                         |Hispanic_ancestry |race_white |race_black |race_Asian |race_pacific |race_Amer_Indian |race_other |race_dontknow |race_refused |Asian_ancestry |
|--:|:------|---:|:------------|:---------------------|:--------|:-------------------------------------------------|:-----------------|:----------|:----------|:----------|:------------|:----------------|:----------|:-------------|:------------|:--------------|
|  1|Male   |  24|Yes          |Self-employed,        |No       |Bachelor's Degree (Example: BA, AB, BS, BBA)      |.                 |Yes        |No         |No         |No           |No               |No         |No            |No           |.              |
|  2|Female |  76|Yes          |Primarily retired, or |No       |High School Graduate                              |.                 |Yes        |No         |No         |No           |No               |No         |No            |No           |.              |
|  3|Male   |  47|No           |Employed for wages,   |No       |Bachelor's Degree (Example: BA, AB, BS, BBA)      |.                 |Yes        |No         |No         |No           |No               |No         |No            |No           |.              |
|  4|Female |  42|No           |Employed for wages,   |No       |Bachelor's Degree (Example: BA, AB, BS, BBA)      |.                 |Yes        |No         |No         |No           |No               |No         |No            |No           |.              |
|  5|Female |  24|No           |Employed for wages,   |No       |Bachelor's Degree (Example: BA, AB, BS, BBA)      |.                 |Yes        |No         |No         |No           |No               |No         |No            |No           |.              |
|  6|Female |  39|No           |Employed for wages,   |No       |Master's Degree (Example: MA, MS, MENG, MED, MBA) |.                 |Yes        |No         |No         |No           |No               |No         |No            |No           |.              |


Structure of Dataset
========================================================

```r
str(data)
```

```
Classes 'tbl_df', 'tbl' and 'data.frame':	454 obs. of  17 variables:
 $ ID               : int  1 2 3 4 5 6 7 8 9 10 ...
 $ Gender           : chr  "Male" "Female" "Male" "Female" ...
 $ Age              : num  24 76 47 42 24 39 31 25 33 32 ...
 $ Hypertension     : chr  "Yes" "Yes" "No" "No" ...
 $ Employment       : chr  "Self-employed," "Primarily retired, or" "Employed for wages," "Employed for wages," ...
 $ Hispanic         : chr  "No" "No" "No" "No" ...
 $ Education        : chr  "Bachelor's Degree (Example: BA, AB, BS, BBA)" "High School Graduate" "Bachelor's Degree (Example: BA, AB, BS, BBA)" "Bachelor's Degree (Example: BA, AB, BS, BBA)" ...
 $ Hispanic_ancestry: chr  "." "." "." "." ...
 $ race_white       : chr  "Yes" "Yes" "Yes" "Yes" ...
 $ race_black       : chr  "No" "No" "No" "No" ...
 $ race_Asian       : chr  "No" "No" "No" "No" ...
 $ race_pacific     : chr  "No" "No" "No" "No" ...
 $ race_Amer_Indian : chr  "No" "No" "No" "No" ...
 $ race_other       : chr  "No" "No" "No" "No" ...
 $ race_dontknow    : chr  "No" "No" "No" "No" ...
 $ race_refused     : chr  "No" "No" "No" "No" ...
 $ Asian_ancestry   : chr  "." "." "." "." ...
```



========================================================




- Bullet 1 Exploratory Data Analysis.
- Bullet 1We explore summary of Hypertension by Age. The median age for those with hypertensive age is 54 whereas the median age for those without hypeertension is 33.
========================================================
![plot of chunk unnamed-chunk-5](chicago-figure/unnamed-chunk-5-1.png)

