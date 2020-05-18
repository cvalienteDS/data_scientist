---
title: "EDA"
author: "cvaliente"
date: "18/5/2020"
output: 
  html_document:
    keep_md: true
---






```r
library(dplyr)
require(ggplot2)
library(knitr)
library(kableExtra)
```





```r
df %>% dplyr::filter(multiplicador.importancia.del.partido != 0) %>%
  group_by(year) %>%
  summarise(n = n(),
            oddsPinnMEDIAN = median(odds_j1_pinnacle, na.rm = T),
            oddsAveMEDIAN = median(Q.1, na.rm = T),
            oddsPinn = mean(odds_j1_pinnacle, na.rm = T),
            oddsAve = mean(Q.1, na.rm = T)
  ) %>%
  mutate(dif = oddsPinnMEDIAN - oddsAveMEDIAN) %>%
  arrange(year) %>%
  mutate_all(round, 2) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> year </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> oddsPinnMEDIAN </th>
   <th style="text-align:right;"> oddsAveMEDIAN </th>
   <th style="text-align:right;"> oddsPinn </th>
   <th style="text-align:right;"> oddsAve </th>
   <th style="text-align:right;"> dif </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:right;"> 5920 </td>
   <td style="text-align:right;"> 1.95 </td>
   <td style="text-align:right;"> 1.83 </td>
   <td style="text-align:right;"> 2.70 </td>
   <td style="text-align:right;"> 2.59 </td>
   <td style="text-align:right;"> 0.12 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:right;"> 17540 </td>
   <td style="text-align:right;"> 1.95 </td>
   <td style="text-align:right;"> 1.84 </td>
   <td style="text-align:right;"> 2.55 </td>
   <td style="text-align:right;"> 2.45 </td>
   <td style="text-align:right;"> 0.11 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:right;"> 17526 </td>
   <td style="text-align:right;"> 1.94 </td>
   <td style="text-align:right;"> 1.95 </td>
   <td style="text-align:right;"> 2.53 </td>
   <td style="text-align:right;"> 2.53 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:right;"> 16656 </td>
   <td style="text-align:right;"> 1.94 </td>
   <td style="text-align:right;"> 1.91 </td>
   <td style="text-align:right;"> 2.57 </td>
   <td style="text-align:right;"> 2.53 </td>
   <td style="text-align:right;"> 0.03 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:right;"> 24618 </td>
   <td style="text-align:right;"> 1.97 </td>
   <td style="text-align:right;"> 1.85 </td>
   <td style="text-align:right;"> 2.67 </td>
   <td style="text-align:right;"> 2.38 </td>
   <td style="text-align:right;"> 0.12 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:right;"> 23632 </td>
   <td style="text-align:right;"> 1.93 </td>
   <td style="text-align:right;"> 1.86 </td>
   <td style="text-align:right;"> 2.62 </td>
   <td style="text-align:right;"> 2.44 </td>
   <td style="text-align:right;"> 0.07 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2018 </td>
   <td style="text-align:right;"> 24684 </td>
   <td style="text-align:right;"> 1.93 </td>
   <td style="text-align:right;"> 1.85 </td>
   <td style="text-align:right;"> 2.48 </td>
   <td style="text-align:right;"> 2.39 </td>
   <td style="text-align:right;"> 0.08 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2019 </td>
   <td style="text-align:right;"> 21756 </td>
   <td style="text-align:right;"> 1.92 </td>
   <td style="text-align:right;"> 1.86 </td>
   <td style="text-align:right;"> 2.43 </td>
   <td style="text-align:right;"> 2.32 </td>
   <td style="text-align:right;"> 0.06 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2020 </td>
   <td style="text-align:right;"> 704 </td>
   <td style="text-align:right;"> 1.93 </td>
   <td style="text-align:right;"> 1.87 </td>
   <td style="text-align:right;"> 2.34 </td>
   <td style="text-align:right;"> 2.30 </td>
   <td style="text-align:right;"> 0.06 </td>
  </tr>
</tbody>
</table>


```r
df %>% dplyr::filter(multiplicador.importancia.del.partido != 0) %>%
  group_by(year, atp.challenger_v2) %>%
  summarise(n = n(),
  ) %>%
  arrange(atp.challenger_v2) %>%
  mutate_if(is.numeric,round, 2) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> year </th>
   <th style="text-align:left;"> atp.challenger_v2 </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 2238 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 8458 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 8404 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 7056 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 7802 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 8160 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2018 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 7750 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2019 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 7024 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2020 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 226 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 3682 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 9082 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 9122 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 9600 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 16816 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 15472 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2018 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 16934 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2019 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 14732 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2020 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 478 </td>
  </tr>
</tbody>
</table>


```r
df %>% dplyr::filter(multiplicador.importancia.del.partido != 0) %>%
  group_by( partidos_cat, atp.challenger_v2) %>%
  summarise(n = n(),
            w_svpt = mean(w_svpt, na.rm = T),
            y = mean(J1...ser.points.won, na.rm = T),
            nivelJ1 = mean(nivelJ1, na.rm = T),
  ) %>%
  arrange(atp.challenger_v2,partidos_cat) %>%
  mutate_if(is.numeric,round, 2) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> partidos_cat </th>
   <th style="text-align:left;"> atp.challenger_v2 </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> w_svpt </th>
   <th style="text-align:right;"> y </th>
   <th style="text-align:right;"> nivelJ1 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> [0,10] </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 12000 </td>
   <td style="text-align:right;"> 76.04 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (10,20] </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 6172 </td>
   <td style="text-align:right;"> 77.54 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.05 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (20,30] </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 4881 </td>
   <td style="text-align:right;"> 76.18 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (30,40] </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 5895 </td>
   <td style="text-align:right;"> 76.52 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 1.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (40,50] </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 7457 </td>
   <td style="text-align:right;"> 76.70 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 1.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (50,60] </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 8969 </td>
   <td style="text-align:right;"> 78.38 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 1.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (60,70] </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 7566 </td>
   <td style="text-align:right;"> 79.09 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 1.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (70,80] </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 3170 </td>
   <td style="text-align:right;"> 78.69 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 1.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (80,90] </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 917 </td>
   <td style="text-align:right;"> 77.85 </td>
   <td style="text-align:right;"> 0.65 </td>
   <td style="text-align:right;"> 1.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (90,100] </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 88.23 </td>
   <td style="text-align:right;"> 0.65 </td>
   <td style="text-align:right;"> 1.13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> [0,10] </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 31774 </td>
   <td style="text-align:right;"> 69.30 </td>
   <td style="text-align:right;"> 0.59 </td>
   <td style="text-align:right;"> 0.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (10,20] </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 14555 </td>
   <td style="text-align:right;"> 71.07 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (20,30] </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 11307 </td>
   <td style="text-align:right;"> 70.90 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 1.01 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (30,40] </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 10631 </td>
   <td style="text-align:right;"> 71.13 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.01 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (40,50] </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 10526 </td>
   <td style="text-align:right;"> 70.90 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (50,60] </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 8953 </td>
   <td style="text-align:right;"> 71.61 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (60,70] </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 5922 </td>
   <td style="text-align:right;"> 71.13 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (70,80] </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 1956 </td>
   <td style="text-align:right;"> 70.33 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 1.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (80,90] </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 285 </td>
   <td style="text-align:right;"> 72.11 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (90,100] </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 88.11 </td>
   <td style="text-align:right;"> 0.58 </td>
   <td style="text-align:right;"> 1.07 </td>
  </tr>
</tbody>
</table>


```r
df %>% dplyr::filter(multiplicador.importancia.del.partido != 0) %>%
  group_by( year, atp.challenger_v2) %>%
  summarise(n = n(),
            w_svpt = mean(w_svpt, na.rm = T),
            y = mean(J1...ser.points.won, na.rm = T),
            nivelJ1 = mean(nivelJ1, na.rm = T),
  ) %>%
  arrange(atp.challenger_v2,year) %>%
  mutate_if(is.numeric,round, 2) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> year </th>
   <th style="text-align:left;"> atp.challenger_v2 </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> w_svpt </th>
   <th style="text-align:right;"> y </th>
   <th style="text-align:right;"> nivelJ1 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 2238 </td>
   <td style="text-align:right;"> 73.95 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.04 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 8458 </td>
   <td style="text-align:right;"> 74.96 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.06 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 8404 </td>
   <td style="text-align:right;"> 75.13 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.01 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 7056 </td>
   <td style="text-align:right;"> 77.85 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 1.03 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 7802 </td>
   <td style="text-align:right;"> 77.98 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.05 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 8160 </td>
   <td style="text-align:right;"> 78.85 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.06 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2018 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 7750 </td>
   <td style="text-align:right;"> 78.84 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 1.05 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2019 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 7024 </td>
   <td style="text-align:right;"> 79.27 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 1.05 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2020 </td>
   <td style="text-align:left;"> atp </td>
   <td style="text-align:right;"> 226 </td>
   <td style="text-align:right;"> 72.46 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 1.04 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 3682 </td>
   <td style="text-align:right;"> 69.95 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 1.03 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 9082 </td>
   <td style="text-align:right;"> 70.27 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 1.03 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 9122 </td>
   <td style="text-align:right;"> 70.81 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 0.96 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 9600 </td>
   <td style="text-align:right;"> 71.31 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.98 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 16816 </td>
   <td style="text-align:right;"> 69.66 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.98 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 15472 </td>
   <td style="text-align:right;"> 69.80 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2018 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 16934 </td>
   <td style="text-align:right;"> 70.71 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.99 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2019 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 14732 </td>
   <td style="text-align:right;"> 71.44 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 0.99 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2020 </td>
   <td style="text-align:left;"> challenger </td>
   <td style="text-align:right;"> 478 </td>
   <td style="text-align:right;"> 71.57 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.99 </td>
  </tr>
</tbody>
</table>


```r
thresold = 3
df %>% dplyr::filter(multiplicador.importancia.del.partido != 0) %>%
  group_by(year) %>%
  summarise(
    same_tour_informado = sum(Same.tournament.past.years.j1.games >= thresold),
    media_same_tour = mean(Same.tournament.past.years.j1[Same.tournament.past.years.j1.games >= thresold]),
    
    same_cluster_informado = sum(Same.cluster.past.years.j1.games >= thresold),
    media_same_cluster = mean(Same.cluster.past.years.j1[Same.cluster.past.years.j1.games >= thresold]),
    
    player_cluster_informado = sum(VS_playerCluster_j1.games >= thresold),
    media_player_cluster = mean(VS_playerCluster_j1[VS_playerCluster_j1.games >= thresold])
  ) %>%
  arrange(year) %>%
  mutate_if(is.numeric,round, 2) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> year </th>
   <th style="text-align:right;"> same_tour_informado </th>
   <th style="text-align:right;"> media_same_tour </th>
   <th style="text-align:right;"> same_cluster_informado </th>
   <th style="text-align:right;"> media_same_cluster </th>
   <th style="text-align:right;"> player_cluster_informado </th>
   <th style="text-align:right;"> media_player_cluster </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> NaN </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 1.02 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:right;"> 129 </td>
   <td style="text-align:right;"> 1.07 </td>
   <td style="text-align:right;"> 4344 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 4577 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:right;"> 1225 </td>
   <td style="text-align:right;"> 1.05 </td>
   <td style="text-align:right;"> 10136 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 10266 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:right;"> 2739 </td>
   <td style="text-align:right;"> 1.03 </td>
   <td style="text-align:right;"> 11211 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 11211 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:right;"> 3183 </td>
   <td style="text-align:right;"> 1.03 </td>
   <td style="text-align:right;"> 14714 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 13728 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:right;"> 3386 </td>
   <td style="text-align:right;"> 1.03 </td>
   <td style="text-align:right;"> 15645 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 14923 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2018 </td>
   <td style="text-align:right;"> 3436 </td>
   <td style="text-align:right;"> 1.03 </td>
   <td style="text-align:right;"> 16996 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 15759 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2019 </td>
   <td style="text-align:right;"> 3645 </td>
   <td style="text-align:right;"> 1.03 </td>
   <td style="text-align:right;"> 17227 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 16245 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2020 </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 1.02 </td>
   <td style="text-align:right;"> 513 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 522 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>


