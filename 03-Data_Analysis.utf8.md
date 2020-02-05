
<!-- rnb-text-begin -->

---
title: "Step III: Data Analysis"
subtitle: 'PACs and Small-dollar Donations'
author:
  - name: Nicholas R. Jenkins 
    affiliation: University of California, Riverside
    affiliation_url: https://www.ucr.edu
abstract: |
  This document shows all the code used to analyze the data for this project. 
  All analyses were done with R version 3.6.2 
  "Dark and Stormy Night" and Stan version 2.19.1 in RStudio. 
  
  Each step has been carefully documented and will replicate the results in the 
  manuscript. The code for this document can be downloaded using the button 
  in the top right corner. For any questions about this code, please contact 
  Nick Jenkins at 
  [nicholas.jenkins@email.ucr.edu](mailto:nicholas.jenkins@email.ucr.edu).
date: "2020-02-05"
output: 
  html_notebook: 
    highlight: pygments
    theme: paper
    toc: yes
    code_folding: 
    toc_float: true
bibliography: /Users/nick/Documents/Research/References/BibTeX/biblatex.bib
editor_options: 
  chunk_output_type: inline
---

# Environment Preperation

This section clears the current working environment and loads the packages used to visualize the data. I also create the function `comma()` to format any in-line output values to have thousands separators and only two digits. 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBDbGVhciBFbnZpcm9ubWVudCAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxucm0obGlzdCA9IGxzKCkpXG5cbiMgTG9hZCBQYWNrYWdlcyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cbnBhY2thZ2VzIDwtIGMoXCJ0aWR5dmVyc2VcIiwgXCJmb3JlaWduXCIsIFwic3RhcmdhemVyXCIsIFwibG1lNFwiLCBcIm11bHRjb21wXCIsIFxuICAgICAgICAgICAgICBcImxtZXJUZXN0XCIsIFwiZ2dtY21jXCIsIFwiZ2dyaWRnZXNcIiwgXCJyc3RhblwiLCBcInJzdGFuYXJtXCIsIFwiYnJtc1wiLFxuICAgICAgICAgICAgICBcInJldGhpbmtpbmdcIiwgXCJwYXRjaHdvcmtcIiwgXCJ0aWR5YmF5ZXNcIiwgXCJzanN0YXRzXCIsIFwic2pQbG90XCIsXG4gICAgICAgICAgICAgIFwiYmF5ZXNwbG90XCIsIFwic2ptaXNjXCIsIFwiZ2dwdWJyXCIsIFwiYnJvb21cIiwgXCJzY2FsZXNcIiwgXCJnZ3RoZW1lc1wiLFxuICAgICAgICAgICAgICBcImdncmVwZWxcIiwgXCJnZ3B1YnJcIilcbmxhcHBseShwYWNrYWdlcywgcmVxdWlyZSwgY2hhcmFjdGVyLm9ubHkgPSBUUlVFKVxuIyBGdW5jdGlvbnMgLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxuIyBJbmxpbmUgRm9ybWF0dGluZ1xuY29tbWEgPC0gZnVuY3Rpb24oeCkgZm9ybWF0KHgsIGRpZ2l0cyA9IDIsIGJpZy5tYXJrID0gXCIsXCIpXG5cbiMgSW52ZXJzZSBIeXBlcmJvbGljIFNpbiBUcmFuc2Zvcm1hdGlvbiBGdW5jdGlvblxuaWhzIDwtIGZ1bmN0aW9uKHgpIHtcbiAgICB5IDwtIGxvZyh4ICsgc3FydCh4IF4gMiArIDEpKVxuICAgIHJldHVybih5KVxufVxuXG4jIFNldCBHbG9iYWwgQ2h1bmsgT3B0aW9ucyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXG5rbml0cjo6b3B0c19jaHVuayRzZXQoXG4gIGVjaG8gPSBUUlVFLFxuICB3YXJuaW5nID0gRkFMU0UsXG4gIG1lc3NhZ2UgPSBGQUxTRSxcbiAgY29tbWVudCA9IFwiIyNcIixcbiAgUi5vcHRpb25zID0gbGlzdCh3aWR0aCA9IDcwKVxuKVxuYGBgIn0= -->

```r
# Clear Environment -----------------------------------------------------------
rm(list = ls())

# Load Packages ---------------------------------------------------------------
packages <- c("tidyverse", "foreign", "stargazer", "lme4", "multcomp", 
              "lmerTest", "ggmcmc", "ggridges", "rstan", "rstanarm", "brms",
              "rethinking", "patchwork", "tidybayes", "sjstats", "sjPlot",
              "bayesplot", "sjmisc", "ggpubr", "broom", "scales", "ggthemes",
              "ggrepel", "ggpubr")
lapply(packages, require, character.only = TRUE)
# Functions -------------------------------------------------------------------
# Inline Formatting
comma <- function(x) format(x, digits = 2, big.mark = ",")

# Inverse Hyperbolic Sin Transformation Function
ihs <- function(x) {
    y <- log(x + sqrt(x ^ 2 + 1))
    return(y)
}

# Set Global Chunk Options ----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "##",
  R.options = list(width = 70)
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# Import Data

This section imports the data cleaned in the "Step I: Data Cleaning" document to begin analyzing the data. 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# Vote Models

## Model of Vote Share

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdnZGVuc2l0eShhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSA1LCByYXRlID0gMSkpLCBcbiAgICAgICAgICB4ID0gXCJ2YWx1ZVwiLFxuICAgICAgICAgIGZpbGwgPSBcIiMwMEFGQkJcIixcbiAgICAgICAgICBwYWxldHRlID0gYyhcIiMwMEFGQkJcIiwgXCIjRTdCODAwXCIpKVxuXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJnYW1tYSgxZTQsIHNoYXBlID0gMjAsIHJhdGUgPSAxMDApKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlKSkgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFNsb3BlIFBhcmFtZXRlcnNcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMoYnJlYWtzID0gc2NhbGVzOjpwcmV0dHlfYnJlYWtzKG4gPSAxMCkpICtcbiAgY29sb3JfcGFsZXR0ZShcImpjb1wiKSArXG4gIHRoZW1lX2xpZ2h0KClcblxuIyBwcmlvciBzaW11bGF0aW9uXG4jIyBJbnRlcmNlcHRcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocm5vcm0oMWU0LCBleHAoNCksIGV4cCgzKSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiTm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgSW50ZXJjZXB0XCIpICtcbiAgc2NhbGVfeF9jb250aW51b3VzKGJyZWFrcyA9IHNjYWxlczo6cHJldHR5X2JyZWFrcyhuID0gMTApKSArXG4gIHRoZW1lKGF4aXMudGV4dC54ID0gZWxlbWVudF90ZXh0KGFuZ2xlID0gNDUsIHZqdXN0ID0gMSwgaGp1c3QgPSAxKSlcblxuIyMgU2xvcGUgUGFyYW1ldGVyc1xuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShybm9ybSgxZTQsIGV4cCgwKSwgZXhwKDQpKSksXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJub3JtYWxcIikpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBTbG9wZSBQYXJhbWV0ZXJzXCIpICtcbiAgc2NhbGVfeF9jb250aW51b3VzKGJyZWFrcyA9IHNjYWxlczo6cHJldHR5X2JyZWFrcyhuID0gMTApKSArXG4gIHhsaW0oLTIwMCwgMjAwKVxuXG4jIyBTY2FsZSBQYXJhbWV0ZXJcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTFcIiksXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNSkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShyZXhwKDFlNCwgMikpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcInJhdGU9MlwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNSkgK1xuICBmaWxsX3BhbGV0dGUocGFsZXR0ZSA9IFwiamNvXCIpICtcbiAgdGhlbWVfbGlnaHQoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2NhbGUgUGFyYW1ldGVyXCIpXG5cbiMjIFByb2JhYmlsaXR5IFBhcmFtZXRlclxuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShydW5pZigxZTQsIDAsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJ1bmlmb3JtXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMiwgMikpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMlwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNSkgK1xuICAgIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMS41LCAxLjUpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJiZXRhIDEuNVwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNykgK1xuICBmaWxsX3BhbGV0dGUocGFsZXR0ZSA9IFwiamNvXCIpICtcbiAgdGhlbWVfbGlnaHQoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXCIpXG5gYGAifQ== -->

```r
# likelihood simulation
ggdensity(as_tibble(rgamma(1e4, shape = 5, rate = 1)), 
          x = "value",
          fill = "#00AFBB",
          palette = c("#00AFBB", "#E7B800"))

ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  color_palette("jco") +
  theme_light()

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(4), exp(3))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(4))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  xlim(-200, 200)

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1"),
               alpha = 0.5) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  fill_palette(palette = "jco") +
  theme_light() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  fill_palette(palette = "jco") +
  theme_light() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludCBuO1xuICByZWFsIHZvdGVfcGN0W25dO1xuICByZWFsIG5vX2NvcnBfcGFjc1tuXTtcbiAgcmVhbCBuZXdfbWVtYmVyW25dO1xuICByZWFsIHBjdF93aGl0ZVtuXTtcbiAgcmVhbCBwY3RfYmxhY2tbbl07XG4gIHJlYWwgcGN0X2xhdGlub1tuXTtcbiAgcmVhbCBwY3RfYXNpYW5bbl07XG4gIHJlYWwgcGN0X2JhY2hlbG9yc1tuXTtcbiAgcmVhbCBwY3RfY2xpbnRvbl8xNltuXTtcbiAgcmVhbCBwY3RfZGVtX2hvdXNlXzE2W25dO1xuICByZWFsIGxvZ190b3Rfdm90aW5nX2FnZVtuXTtcbiAgcmVhbCBsb2dfbWVkaWFuX2hoX2luY29tZVtuXTtcbn1cblxucGFyYW1ldGVycyB7XG4gIHJlYWwgYTtcbiAgcmVhbCBiX25wO1xuICByZWFsIGJfbmV3O1xuICByZWFsIGJfd2hpdGU7XG4gIHJlYWwgYl9ibGFjaztcbiAgcmVhbCBiX2xhdGlubztcbiAgcmVhbCBiX2FzaWFuO1xuICByZWFsIGJfYmFjaDtcbiAgcmVhbCBiX2NsaW50O1xuICByZWFsIGJfaG91c2U7XG4gIHJlYWwgYl92cG9wO1xuICByZWFsIGJfbWhoO1xuICByZWFsPGxvd2VyID0gMD4gc2NhbGU7XG4gIHJlYWwgcHJvYjtcbn1cblxubW9kZWwge1xuICB2ZWN0b3Jbbl0gbXU7XG4gIFxuICAvLyBsaW5lYXIgbW9kZWxcbiAgZm9yIChpIGluIDE6bikge1xuICAgIG11W2ldID0gYSArIGJfbnAgKiBub19jb3JwX3BhY3NbaV0gKyBiX25ldyAqIG5ld19tZW1iZXJbaV0gKyBcbiAgICAgIGJfd2hpdGUgKiBwY3Rfd2hpdGVbaV0gKyBiX2JsYWNrICogcGN0X2JsYWNrW2ldICsgXG4gICAgICBiX2xhdGlubyAqIHBjdF9sYXRpbm9baV0gKyBiX2FzaWFuICogcGN0X2FzaWFuW2ldICsgXG4gICAgICBiX2JhY2ggKiBwY3RfYmFjaGVsb3JzW2ldICsgYl9jbGludCAqIHBjdF9jbGludG9uXzE2W2ldICsgXG4gICAgICBiX2hvdXNlICogcGN0X2RlbV9ob3VzZV8xNltpXSArIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXSArXG4gICAgICBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldO1xuICAgIG11W2ldID0gZXhwKG11W2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbiAgXG4gIC8vIHByaW9yc1xuICBhIH4gbm9ybWFsKDQsIDMpO1xuICBiX25wIH4gbm9ybWFsKDAsIDMuNSk7XG4gIGJfbmV3IH4gbm9ybWFsKDAsIDMuNSk7XG4gIGJfd2hpdGUgfiBub3JtYWwoMCwgMy41KTtcbiAgYl9ibGFjayB+IG5vcm1hbCgwLCAzLjUpO1xuICBiX2xhdGlubyB+IG5vcm1hbCgwLCAzLjUpO1xuICBiX2FzaWFuIH4gbm9ybWFsKDAsIDMuNSk7XG4gIGJfYmFjaCB+IG5vcm1hbCgwLCAzLjUpO1xuICBiX2NsaW50IH4gbm9ybWFsKDAsIDMuNSk7XG4gIGJfaG91c2UgfiBub3JtYWwoMCwgMy41KTtcbiAgYl92cG9wIH4gbm9ybWFsKDAsIDMuNSk7XG4gIGJfbWhoIH4gbm9ybWFsKDAsIDMuNSk7XG4gIHNjYWxlIH4gZXhwb25lbnRpYWwoMSk7XG4gIHByb2IgfiBiZXRhKDEuNSwgMS41KTtcbiAgXG4gIC8vIGxpa2VsaWhvb2RcbiAgZm9yICggaSBpbiAxOm4pIHtcbiAgICBpZiAodm90ZV9wY3RbaV0gPT0gMClcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMXwgcHJvYikpO1xuICAgIGVsc2VcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMHwgcHJvYikgKyBnYW1tYV9scGRmKHZvdGVfcGN0W2ldfCBtdVtpXS9zY2FsZSwgMS9zY2FsZSkpO1xuICAgIH0vL2kgXG59XG5cbmdlbmVyYXRlZCBxdWFudGl0aWVzIHtcbiAgdmVjdG9yW25dIG11O1xuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgbXVbaV0gPSBhICsgYl9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIGJfbmV3ICogbmV3X21lbWJlcltpXSArIFxuICAgICAgYl93aGl0ZSAqIHBjdF93aGl0ZVtpXSArIGJfYmxhY2sgKiBwY3RfYmxhY2tbaV0gKyBcbiAgICAgIGJfbGF0aW5vICogcGN0X2xhdGlub1tpXSArIGJfYXNpYW4gKiBwY3RfYXNpYW5baV0gKyBcbiAgICAgIGJfYmFjaCAqIHBjdF9iYWNoZWxvcnNbaV0gKyBiX2NsaW50ICogcGN0X2NsaW50b25fMTZbaV0gKyBcbiAgICAgIGJfaG91c2UgKiBwY3RfZGVtX2hvdXNlXzE2W2ldICsgYl92cG9wICogbG9nX3RvdF92b3RpbmdfYWdlW2ldICtcbiAgICAgIGJfbWhoICogbG9nX21lZGlhbl9oaF9pbmNvbWVbaV07XG4gICAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxufVxuYGBgIn0= -->

```stan
data {
  int n;
  real vote_pct[n];
  real no_corp_pacs[n];
  real new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_tot_voting_age[n];
  real log_median_hh_income[n];
}

parameters {
  real a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_clint;
  real b_house;
  real b_vpop;
  real b_mhh;
  real<lower = 0> scale;
  real prob;
}

model {
  vector[n] mu;
  
  // linear model
  for (i in 1:n) {
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_vpop * log_tot_voting_age[i] +
      b_mhh * log_median_hh_income[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  a ~ normal(4, 3);
  b_np ~ normal(0, 3.5);
  b_new ~ normal(0, 3.5);
  b_white ~ normal(0, 3.5);
  b_black ~ normal(0, 3.5);
  b_latino ~ normal(0, 3.5);
  b_asian ~ normal(0, 3.5);
  b_bach ~ normal(0, 3.5);
  b_clint ~ normal(0, 3.5);
  b_house ~ normal(0, 3.5);
  b_vpop ~ normal(0, 3.5);
  b_mhh ~ normal(0, 3.5);
  scale ~ exponential(1);
  prob ~ beta(1.5, 1.5);
  
  // likelihood
  for ( i in 1:n) {
    if (vote_pct[i] == 0)
      target += (bernoulli_lpmf(1| prob));
    else
      target += (bernoulli_lpmf(0| prob) + gamma_lpdf(vote_pct[i]| mu[i]/scale, 1/scale));
    }//i 
}

generated quantities {
  vector[n] mu;
  for (i in 1:n) {
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_vpop * log_tot_voting_age[i] +
      b_mhh * log_median_hh_income[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KHZvdGVfcGN0LCBub19jb3JwX3BhY3MsIG5ld19tZW1iZXIsIHBjdF93aGl0ZSwgcGN0X2JsYWNrLCBcbiAgICAgICAgICAgICAgICBwY3RfbGF0aW5vLCBwY3RfYXNpYW4sIHBjdF9iYWNoZWxvcnMsIHBjdF9jbGludG9uXzE2LFxuICAgICAgICAgICAgICAgIHBjdF9kZW1faG91c2VfMTYsIGxvZ190b3Rfdm90aW5nX2FnZSwgbG9nX21lZGlhbl9oaF9pbmNvbWUpICU+JVxuICBtdXRhdGUobm9fY29ycF9wYWNzID0gaWZlbHNlKG5vX2NvcnBfcGFjcyA9PSBcIllFU1wiLCAxLCAwKSxcbiAgICAgICAgIG5ld19tZW1iZXIgPSBpZmVsc2UobmV3X21lbWJlciA9PSBcIllFU1wiLCAxLCAwKSkgJT4lXG4gICNmaWx0ZXIodm90ZV9wY3QgIT0gMCkgJT4lXG4gIG11dGF0ZV9hdCgudmFycyA9IHZhcnMocGN0X3doaXRlOmxvZ19tZWRpYW5faGhfaW5jb21lKSwgLmZ1bnMgPSBjZW50ZXIpICU+JVxuICBjb21wb3NlX2RhdGEoKVxuXG4jIG1vZGVsIGVzdGltYXRpb24gLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXG52b3RlLjAxIDwtIGdsbSh2b3RlX3BjdCB+IG5vX2NvcnBfcGFjcyArIG5ld19tZW1iZXIgKyBwY3Rfd2hpdGUgKyBwY3RfYmxhY2sgKyBwY3RfbGF0aW5vICsgcGN0X2FzaWFuICsgcGN0X2JhY2hlbG9ycyArIHBjdF9jbGludG9uXzE2ICsgcGN0X2RlbV9ob3VzZV8xNiArIGxvZ19tZWRpYW5faGhfaW5jb21lICsgbG9nX3RvdF92b3RpbmdfYWdlLFxuICAgICAgICAgICAgICAgZmFtaWx5ID0gR2FtbWEobGluayA9IFwibG9nXCIpLFxuICAgICAgICAgICAgICAgZGF0YSA9IGFzLmRhdGEuZnJhbWUoc3Rhbi5kYXRhKSAlPiUgZmlsdGVyKHZvdGVfcGN0ICE9IDApICU+JSBtdXRhdGVfYXQoLnZhcnMgPSB2YXJzKHBjdF93aGl0ZTpsb2dfbWVkaWFuX2hoX2luY29tZSksIC5mdW5zID0gY2VudGVyKSlcblxudm90aW5nLmZpdCA8LSBzYW1wbGluZyh2b3RlLm1vZGVsLFxuICAgICAgICAgICAgICAgICAgICAgICBjaGFpbnMgPSA0LFxuICAgICAgICAgICAgICAgICAgICAgICBjb3JlcyA9IDQsXG4gICAgICAgICAgICAgICAgICAgICAgIGl0ZXIgPSA4MDAwLFxuICAgICAgICAgICAgICAgICAgICAgICB3YXJtdXAgPSAzMDAwLFxuICAgICAgICAgICAgICAgICAgICAgICBkYXRhID0gc3Rhbi5kYXRhKVxuXG50cmFjZXBsb3Qodm90aW5nLmZpdCwgXG4gICAgICAgICAgaW5jX3dhcm11cCA9IFRSVUUsIFxuICAgICAgICAgIHBhcnMgPSBjKFwiYVwiLCBcImJfbnBcIiwgXCJiX25ld1wiLCBcImJfd2hpdGVcIiwgXCJiX2JsYWNrXCIsIFwiYl9sYXRpbm9cIiwgXG4gICAgICAgICAgICAgICAgICAgXCJiX2FzaWFuXCIsIFwiYl9iYWNoXCIsIFwiYl9jbGludFwiLCBcImJfaG91c2VcIiwgXCJiX3Zwb3BcIiwgXG4gICAgICAgICAgICAgICAgICAgXCJiX21oaFwiLCBcInNjYWxlXCIpKVxuXG50aWR5KHZvdGUuMDEsIGNvbmYuaW50ID0gVFJVRSwgY29uZi5sZXZlbCA9IDAuODkpXG5wcmVjaXModm90aW5nLmZpdCwgZGlnaXRzID0gMywgcHJvYiA9IDAuODksIGRlcHRoID0gMilcblxucG9zdCA8LSBzcHJlYWRfZHJhd3Modm90aW5nLmZpdCwgbXVbaV0pICU+JSBtZWRpYW5faGRpKClcbmdncGxvdChkYXRhID0gZXN0aW1hdGlvbi5kYXRhLCBhZXMoeCA9IHZvdGVfcGN0KSkgK1xuICBnZW9tX2RlbnNpdHkoZmlsbCA9IFwiZ3JheVwiKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gcG9zdCwgYWVzKHggPSBtdSksIGZpbGwgPSBcInNreWJsdWVcIiwgYWxwaGEgPSAwLjcpXG5gYGAifQ== -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(vote_pct, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  #filter(vote_pct != 0) %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
vote.01 <- glm(vote_pct ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + pct_clinton_16 + pct_dem_house_16 + log_median_hh_income + log_tot_voting_age,
               family = Gamma(link = "log"),
               data = as.data.frame(stan.data) %>% filter(vote_pct != 0) %>% mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center))

voting.fit <- sampling(vote.model,
                       chains = 4,
                       cores = 4,
                       iter = 8000,
                       warmup = 3000,
                       data = stan.data)

traceplot(voting.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh", "scale"))

tidy(vote.01, conf.int = TRUE, conf.level = 0.89)
precis(voting.fit, digits = 3, prob = 0.89, depth = 2)

post <- spread_draws(voting.fit, mu[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = vote_pct)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = mu), fill = "skyblue", alpha = 0.7)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Model of Turnout

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDQpLCBleHAoMy41KSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiTm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgSW50ZXJjZXB0XCIpICtcbiAgc2NhbGVfeF9jb250aW51b3VzKGJyZWFrcyA9IHNjYWxlczo6cHJldHR5X2JyZWFrcyhuID0gMTApKSArXG4gIHRoZW1lKGF4aXMudGV4dC54ID0gZWxlbWVudF90ZXh0KGFuZ2xlID0gNDUsIHZqdXN0ID0gMSwgaGp1c3QgPSAxKSkgK1xuICB4bGltKC0yMDAsIDIwMClcblxuIyMgU2xvcGUgUGFyYW1ldGVyc1xuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShybm9ybSgxZTQsIGV4cCgwKSwgZXhwKDQpKSksXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJub3JtYWxcIikpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBTbG9wZSBQYXJhbWV0ZXJzXCIpICtcbiAgc2NhbGVfeF9jb250aW51b3VzKGJyZWFrcyA9IHNjYWxlczo6cHJldHR5X2JyZWFrcyhuID0gMTApKSArXG4gIHhsaW0oLTIwMCwgMjAwKVxuXG4jIyBTY2FsZSBQYXJhbWV0ZXJcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTFcIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDIpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTJcIiksIFxuICAgICAgICAgICAgICAgYWxwaGEgPSAwLjUpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBTY2FsZSBQYXJhbWV0ZXJcIilcblxuIyMgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJ1bmlmKDFlNCwgMCwgMSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcInVuaWZvcm1cIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAyLCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiYmV0YSAyXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gICAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAxLjUsIDEuNSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMS41XCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC43KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXCIpXG5gYGAifQ== -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(4), exp(3.5))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  xlim(-200, 200)

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(4))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  xlim(-200, 200)

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgdmFwX3R1cm5vdXRbbl07XG4gIHJlYWwgbm9fY29ycF9wYWNzW25dO1xuICByZWFsIG5ld19tZW1iZXJbbl07XG4gIHJlYWwgcGN0X3doaXRlW25dO1xuICByZWFsIHBjdF9ibGFja1tuXTtcbiAgcmVhbCBwY3RfbGF0aW5vW25dO1xuICByZWFsIHBjdF9hc2lhbltuXTtcbiAgcmVhbCBwY3RfYmFjaGVsb3JzW25dO1xuICByZWFsIHBjdF9jbGludG9uXzE2W25dO1xuICByZWFsIHBjdF9kZW1faG91c2VfMTZbbl07XG4gIHJlYWwgbG9nX21lZGlhbl9oaF9pbmNvbWVbbl07XG4gIHJlYWwgbG9nX3RvdF92b3RpbmdfYWdlW25dO1xufVxuXG5wYXJhbWV0ZXJzIHtcbiAgcmVhbDxsb3dlciA9IDA+IGE7XG4gIHJlYWwgYl9ucDtcbiAgcmVhbCBiX25ldztcbiAgcmVhbCBiX3doaXRlO1xuICByZWFsIGJfYmxhY2s7XG4gIHJlYWwgYl9sYXRpbm87XG4gIHJlYWwgYl9hc2lhbjtcbiAgcmVhbCBiX2JhY2g7XG4gIHJlYWwgYl9taGg7XG4gIHJlYWwgYl9jbGludDtcbiAgcmVhbCBiX2hvdXNlO1xuICByZWFsIGJfdnBvcDtcbiAgcmVhbDxsb3dlciA9IDA+IHNjYWxlO1xuICByZWFsIHByb2I7XG59XG5cbm1vZGVsIHtcbiAgLy8gbGluZWFyIG1vZGVsXG4gIHZlY3RvcltuXSBtdTtcbiAgZm9yIChpIGluIDE6bikge1xuICAgIG11W2ldID0gYSArIGJfbnAgKiBub19jb3JwX3BhY3NbaV0gKyBiX25ldyAqIG5ld19tZW1iZXJbaV0gKyBcbiAgICAgIGJfd2hpdGUgKiBwY3Rfd2hpdGVbaV0gKyBiX2JsYWNrICogcGN0X2JsYWNrW2ldICsgXG4gICAgICBiX2xhdGlubyAqIHBjdF9sYXRpbm9baV0gKyBiX2FzaWFuICogcGN0X2FzaWFuW2ldICsgXG4gICAgICBiX2JhY2ggKiBwY3RfYmFjaGVsb3JzW2ldICsgYl9jbGludCAqIHBjdF9jbGludG9uXzE2W2ldICsgXG4gICAgICBiX2hvdXNlICogcGN0X2RlbV9ob3VzZV8xNltpXSArIGJfbWhoICogbG9nX21lZGlhbl9oaF9pbmNvbWVbaV0gK1xuICAgICAgYl92cG9wICogbG9nX3RvdF92b3RpbmdfYWdlW2ldO1xuICAgIG11W2ldID0gZXhwKG11W2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbiAgXG4gIC8vIHByaW9yc1xuICBhIH4gbm9ybWFsKDMuNSwgMyk7XG4gIGJfbnAgfiBub3JtYWwoMCwgMy41KTtcbiAgYl9uZXcgfiBub3JtYWwoMCwgMy41KTtcbiAgYl93aGl0ZSB+IG5vcm1hbCgwLCAzLjUpO1xuICBiX2JsYWNrIH4gbm9ybWFsKDAsIDMuNSk7XG4gIGJfbGF0aW5vIH4gbm9ybWFsKDAsIDMuNSk7XG4gIGJfYXNpYW4gfiBub3JtYWwoMCwgMy41KTtcbiAgYl9iYWNoIH4gbm9ybWFsKDAsIDMuNSk7XG4gIGJfY2xpbnQgfiBub3JtYWwoMCwgMy41KTtcbiAgYl9ob3VzZSB+IG5vcm1hbCgwLCAzLjUpO1xuICBiX3Zwb3AgfiBub3JtYWwoMCwgMy41KTtcbiAgYl9taGggfiBub3JtYWwoMCwgMy41KTtcbiAgcHJvYiB+IGJldGEoMS41LCAxLjUpO1xuICBcbiAgLy8gbGlrZWxpaG9vZFxuICBmb3IgKCBpIGluIDE6bikge1xuICAgIGlmICh2YXBfdHVybm91dFtpXSA9PSAwKVxuICAgICAgdGFyZ2V0ICs9IChiZXJub3VsbGlfbHBtZigxfCBwcm9iKSk7XG4gICAgZWxzZVxuICAgICAgdGFyZ2V0ICs9IChiZXJub3VsbGlfbHBtZigwfCBwcm9iKSArIGdhbW1hX2xwZGYodmFwX3R1cm5vdXRbaV18IG11W2ldL3NjYWxlLCAxL3NjYWxlKSk7XG4gICAgfS8vaVxufVxuXG5nZW5lcmF0ZWQgcXVhbnRpdGllcyB7XG4gIHZlY3RvcltuXSBtdTtcbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgbXVbaV0gPSBhICsgYl9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIGJfbmV3ICogbmV3X21lbWJlcltpXSArIFxuICAgICAgYl93aGl0ZSAqIHBjdF93aGl0ZVtpXSArIGJfYmxhY2sgKiBwY3RfYmxhY2tbaV0gKyBcbiAgICAgIGJfbGF0aW5vICogcGN0X2xhdGlub1tpXSArIGJfYXNpYW4gKiBwY3RfYXNpYW5baV0gKyBcbiAgICAgIGJfYmFjaCAqIHBjdF9iYWNoZWxvcnNbaV0gKyBiX2NsaW50ICogcGN0X2NsaW50b25fMTZbaV0gKyBcbiAgICAgIGJfaG91c2UgKiBwY3RfZGVtX2hvdXNlXzE2W2ldICsgYl9taGggKiBsb2dfbWVkaWFuX2hoX2luY29tZVtpXSArXG4gICAgICBiX3Zwb3AgKiBsb2dfdG90X3ZvdGluZ19hZ2VbaV07XG4gIG11W2ldID0gZXhwKG11W2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbn1cbmBgYCJ9 -->

```stan
data {
  int<lower = 1> n;
  real vap_turnout[n];
  real no_corp_pacs[n];
  real new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
  real prob;
}

model {
  // linear model
  vector[n] mu;
  for (i in 1:n) {
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  a ~ normal(3.5, 3);
  b_np ~ normal(0, 3.5);
  b_new ~ normal(0, 3.5);
  b_white ~ normal(0, 3.5);
  b_black ~ normal(0, 3.5);
  b_latino ~ normal(0, 3.5);
  b_asian ~ normal(0, 3.5);
  b_bach ~ normal(0, 3.5);
  b_clint ~ normal(0, 3.5);
  b_house ~ normal(0, 3.5);
  b_vpop ~ normal(0, 3.5);
  b_mhh ~ normal(0, 3.5);
  prob ~ beta(1.5, 1.5);
  
  // likelihood
  for ( i in 1:n) {
    if (vap_turnout[i] == 0)
      target += (bernoulli_lpmf(1| prob));
    else
      target += (bernoulli_lpmf(0| prob) + gamma_lpdf(vap_turnout[i]| mu[i]/scale, 1/scale));
    }//i
}

generated quantities {
  vector[n] mu;
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KHZhcF90dXJub3V0LCBub19jb3JwX3BhY3MsIG5ld19tZW1iZXIsIHBjdF93aGl0ZSwgcGN0X2JsYWNrLCBcbiAgICAgICAgICAgICAgICBwY3RfbGF0aW5vLCBwY3RfYXNpYW4sIHBjdF9iYWNoZWxvcnMsIHBjdF9jbGludG9uXzE2LFxuICAgICAgICAgICAgICAgIHBjdF9kZW1faG91c2VfMTYsIGxvZ190b3Rfdm90aW5nX2FnZSwgbG9nX21lZGlhbl9oaF9pbmNvbWUpICU+JVxuICBmaWx0ZXIoIWlzLm5hKHZhcF90dXJub3V0KSkgJT4lXG4gIG11dGF0ZShub19jb3JwX3BhY3MgPSBpZmVsc2Uobm9fY29ycF9wYWNzID09IFwiWUVTXCIsIDEsIDApLFxuICAgICAgICAgbmV3X21lbWJlciA9IGlmZWxzZShuZXdfbWVtYmVyID09IFwiWUVTXCIsIDEsIDApKSAlPiVcbiAgbXV0YXRlX2F0KC52YXJzID0gdmFycyhwY3Rfd2hpdGU6bG9nX21lZGlhbl9oaF9pbmNvbWUpLCAuZnVucyA9IGNlbnRlcikgJT4lXG4gIGNvbXBvc2VfZGF0YSgpXG5cbiMgbW9kZWwgZXN0aW1hdGlvbiAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cbnR1cm5vdXQuMSA8LSBnbG0odmFwX3R1cm5vdXQgfiBub19jb3JwX3BhY3MgKyBuZXdfbWVtYmVyICsgcGN0X3doaXRlICsgcGN0X2JsYWNrICsgcGN0X2xhdGlubyArIHBjdF9hc2lhbiArIHBjdF9iYWNoZWxvcnMgKyBwY3RfY2xpbnRvbl8xNiArIHBjdF9kZW1faG91c2VfMTYgKyBsb2dfbWVkaWFuX2hoX2luY29tZSArIGxvZ190b3Rfdm90aW5nX2FnZSxcbiAgICAgICAgICAgICAgICAgZmFtaWx5ID0gR2FtbWEobGluayA9IFwibG9nXCIpLFxuICAgICAgICAgICAgICAgICAgZGF0YSA9IGFzLmRhdGEuZnJhbWUoc3Rhbi5kYXRhKSlcblxudHVybm91dC5maXQgPC0gc2FtcGxpbmcodHVybm91dC5tb2RlbCxcbiAgICAgICAgICAgICAgICAgICAgICAgIGNoYWlucyA9IDQsXG4gICAgICAgICAgICAgICAgICAgICAgICBjb3JlcyA9IDQsXG4gICAgICAgICAgICAgICAgICAgICAgICBpdGVyID0gODAwMCxcbiAgICAgICAgICAgICAgICAgICAgICAgIHdhcm11cCA9IDMwMDAsXG4gICAgICAgICAgICAgICAgICAgICAgICBkYXRhID0gc3Rhbi5kYXRhKVxuXG50cmFjZXBsb3QodHVybm91dC5maXQsIFxuICAgICAgICAgIGluY193YXJtdXAgPSBUUlVFLCBcbiAgICAgICAgICBwYXJzID0gYyhcImFcIiwgXCJiX25wXCIsIFwiYl9uZXdcIiwgXCJiX3doaXRlXCIsIFwiYl9ibGFja1wiLCBcImJfbGF0aW5vXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9hc2lhblwiLCBcImJfYmFjaFwiLCBcImJfY2xpbnRcIiwgXCJiX2hvdXNlXCIsIFwiYl92cG9wXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9taGhcIiwgXCJzY2FsZVwiKSlcblxudGlkeSh0dXJub3V0LjEsIGNvbmYuaW50ID0gVFJVRSwgY29uZi5sZXZlbCA9IDAuODkpXG5wcmVjaXModHVybm91dC5maXQsIHByb2IgPSAwLjg5KVxuXG5wb3N0IDwtIHNwcmVhZF9kcmF3cyh0dXJub3V0LmZpdCwgbXVbaV0pICU+JSBtZWRpYW5faGRpKClcbmdncGxvdChkYXRhID0gZXN0aW1hdGlvbi5kYXRhLCBhZXMoeCA9IHZhcF90dXJub3V0KSkgK1xuICBnZW9tX2RlbnNpdHkoZmlsbCA9IFwiZ3JheVwiKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gcG9zdCwgYWVzKHggPSBtdSksIGZpbGwgPSBcInNreWJsdWVcIiwgYWxwaGEgPSAwLjcpXG5gYGAifQ== -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(vap_turnout, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  filter(!is.na(vap_turnout)) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
turnout.1 <- glm(vap_turnout ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + pct_clinton_16 + pct_dem_house_16 + log_median_hh_income + log_tot_voting_age,
                 family = Gamma(link = "log"),
                  data = as.data.frame(stan.data))

turnout.fit <- sampling(turnout.model,
                        chains = 4,
                        cores = 4,
                        iter = 8000,
                        warmup = 3000,
                        data = stan.data)

traceplot(turnout.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh", "scale"))

tidy(turnout.1, conf.int = TRUE, conf.level = 0.89)
precis(turnout.fit, prob = 0.89)

post <- spread_draws(turnout.fit, mu[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = vap_turnout)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = mu), fill = "skyblue", alpha = 0.7)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# PAC Models

## Model of Total PAC Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDEzKSwgZXhwKDE1KSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiTm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgSW50ZXJjZXB0XCIpICtcbiAgc2NhbGVfeF9jb250aW51b3VzKGxhYmVscyA9IGRvbGxhcl9mb3JtYXQoKSkgK1xuICB0aGVtZShheGlzLnRleHQueCA9IGVsZW1lbnRfdGV4dChhbmdsZSA9IDQ1LCB2anVzdCA9IDEsIGhqdXN0ID0gMSkpXG5cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocm5vcm0oMWU0LCBsb2dpc3RpYygwKSwgbG9naXN0aWMoMSkpKSxcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcIk5vcm1hbFwiKSkgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIEludGVyY2VwdFwiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhsYWJlbHMgPSBkb2xsYXJfZm9ybWF0KCkpICtcbiAgdGhlbWUoYXhpcy50ZXh0LnggPSBlbGVtZW50X3RleHQoYW5nbGUgPSA0NSwgdmp1c3QgPSAxLCBoanVzdCA9IDEpKVxuXG4jIyBTbG9wZSBQYXJhbWV0ZXJzXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDApLCBleHAoMTQpKSksXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJub3JtYWxcIikpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBTbG9wZSBQYXJhbWV0ZXJzXCIpICtcbiAgc2NhbGVfeF9jb250aW51b3VzKGxhYmVscyA9IGRvbGxhcl9mb3JtYXQoKSkgXG5cbiMjIFNjYWxlIFBhcmFtZXRlclxuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShyZXhwKDFlNCwgMSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcInJhdGU9MVwiKSkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShyZXhwKDFlNCwgMikpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcInJhdGU9MlwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNSkgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFNjYWxlIFBhcmFtZXRlclwiKVxuXG4jIyBQcm9iYWJpbGl0eSBQYXJhbWV0ZXJcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocnVuaWYoMWU0LCAwLCAxKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwidW5pZm9ybVwiKSkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShyYmV0YSgxZTQsIDIsIDIpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJiZXRhIDJcIiksIFxuICAgICAgICAgICAgICAgYWxwaGEgPSAwLjUpICtcbiAgICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShyYmV0YSgxZTQsIDEuNSwgMS41KSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiYmV0YSAxLjVcIiksIFxuICAgICAgICAgICAgICAgYWxwaGEgPSAwLjcpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBQcm9iYWJpbGl0eSBQYXJhbWV0ZXJcIilcbmBgYCJ9 -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(13), exp(15))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, logistic(0), logistic(1))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(14))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgdG90X3BhY19tb25leVtuXTtcbiAgaW50IG5vX2NvcnBfcGFjc1tuXTtcbiAgaW50IG5ld19tZW1iZXJbbl07XG4gIHJlYWwgcGN0X3doaXRlW25dO1xuICByZWFsIHBjdF9ibGFja1tuXTtcbiAgcmVhbCBwY3RfbGF0aW5vW25dO1xuICByZWFsIHBjdF9hc2lhbltuXTtcbiAgcmVhbCBwY3RfYmFjaGVsb3JzW25dO1xuICByZWFsIHBjdF9jbGludG9uXzE2W25dO1xuICByZWFsIHBjdF9kZW1faG91c2VfMTZbbl07XG4gIHJlYWwgbG9nX21lZGlhbl9oaF9pbmNvbWVbbl07XG4gIHJlYWwgbG9nX3RvdF92b3RpbmdfYWdlW25dO1xufVxuXG5wYXJhbWV0ZXJzIHtcbiAgcmVhbCBwO1xuICByZWFsIHBfbnA7XG4gIHJlYWwgcF9uZXc7XG4gIHJlYWw8bG93ZXIgPSAwPiBhO1xuICByZWFsIGJfbnA7XG4gIHJlYWwgYl9uZXc7XG4gIHJlYWwgYl93aGl0ZTtcbiAgcmVhbCBiX2JsYWNrO1xuICByZWFsIGJfbGF0aW5vO1xuICByZWFsIGJfYXNpYW47XG4gIHJlYWwgYl9iYWNoO1xuICByZWFsIGJfbWhoO1xuICByZWFsIGJfY2xpbnQ7XG4gIHJlYWwgYl9ob3VzZTtcbiAgcmVhbCBiX3Zwb3A7XG4gIHJlYWw8bG93ZXIgPSAwPiBzY2FsZTtcbn1cblxubW9kZWwge1xuICB2ZWN0b3Jbbl0gcHJvYjtcbiAgdmVjdG9yW25dIG11O1xuICBcbiAgZm9yIChpIGluIDE6bikge1xuICAgIC8vIGxpbmVhciBtb2RlbFxuICAgIHByb2JbaV0gPSBwICsgcF9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIHBfbmV3ICogbmV3X21lbWJlcltpXTsgXG4gICAgcHJvYltpXSA9IGludl9sb2dpdChwcm9iW2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgICAvLyBsaW5lYXIgbW9kZWxcbiAgICBtdVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgICBtdVtpXSA9IGV4cChtdVtpXSk7IC8vIGludmVyc2UgbGluayBmdW5jdGlvblxuICB9XG4gIFxuICAvLyBwcmlvcnNcbiAgXG4gIC8vIEJpbm9taWFsIG1vZGVsXG4gIHAgfiBub3JtYWwoMCwgMSk7XG4gIHBfbnAgfiBub3JtYWwoMCwgMSk7XG4gIHBfbmV3IH4gbm9ybWFsKDAsIDEpO1xuICBcbiAgLy8gR2FtbWEgbW9kZWxcbiAgYSB+IG5vcm1hbCgxMywgMTUpO1xuICBiX25wIH4gbm9ybWFsKDAsIDE0KTtcbiAgYl9uZXcgfiBub3JtYWwoMCwgMTQpO1xuICBiX3doaXRlIH4gbm9ybWFsKDAsIDE0KTtcbiAgYl9ibGFjayB+IG5vcm1hbCgwLCAxNCk7XG4gIGJfbGF0aW5vIH4gbm9ybWFsKDAsIDE0KTtcbiAgYl9hc2lhbiB+IG5vcm1hbCgwLCAxNCk7XG4gIGJfYmFjaCB+IG5vcm1hbCgwLCAxNCk7XG4gIGJfY2xpbnQgfiBub3JtYWwoMCwgMTQpO1xuICBiX2hvdXNlIH4gbm9ybWFsKDAsIDE0KTtcbiAgYl92cG9wIH4gbm9ybWFsKDAsIDE0KTtcbiAgYl9taGggfiBub3JtYWwoMCwgMTQpO1xuICBzY2FsZSB+IGV4cG9uZW50aWFsKDIpO1xuICBcbiAgLy8gbGlrZWxpaG9vZFxuICBmb3IgKCBpIGluIDE6bikge1xuICAgIGlmICh0b3RfcGFjX21vbmV5W2ldID09IDApXG4gICAgICB0YXJnZXQgKz0gKGJlcm5vdWxsaV9scG1mKDEgfCBwcm9iW2ldKSk7XG4gICAgZWxzZVxuICAgICAgdGFyZ2V0ICs9IChiZXJub3VsbGlfbHBtZigwIHwgcHJvYltpXSkgKyBnYW1tYV9scGRmKHRvdF9wYWNfbW9uZXlbaV18IG11W2ldL3NjYWxlLCAxL3NjYWxlKSk7XG4gICAgfS8vaVxufVxuXG5nZW5lcmF0ZWQgcXVhbnRpdGllcyB7XG4gIHZlY3RvcltuXSBwcm9iO1xuICB2ZWN0b3Jbbl0gbXU7XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgICAgIHByb2JbaV0gPSBwICsgcF9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIHBfbmV3ICogbmV3X21lbWJlcltpXTtcbiAgICAgICAgcHJvYltpXSA9IGludl9sb2dpdChwcm9iW2ldKTtcbiAgICB9XG4gICAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgbXVbaV0gPSBhICsgYl9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIGJfbmV3ICogbmV3X21lbWJlcltpXSArIFxuICAgICAgYl93aGl0ZSAqIHBjdF93aGl0ZVtpXSArIGJfYmxhY2sgKiBwY3RfYmxhY2tbaV0gKyBcbiAgICAgIGJfbGF0aW5vICogcGN0X2xhdGlub1tpXSArIGJfYXNpYW4gKiBwY3RfYXNpYW5baV0gKyBcbiAgICAgIGJfYmFjaCAqIHBjdF9iYWNoZWxvcnNbaV0gKyBiX2NsaW50ICogcGN0X2NsaW50b25fMTZbaV0gKyBcbiAgICAgIGJfaG91c2UgKiBwY3RfZGVtX2hvdXNlXzE2W2ldICsgYl9taGggKiBsb2dfbWVkaWFuX2hoX2luY29tZVtpXSArXG4gICAgICBiX3Zwb3AgKiBsb2dfdG90X3ZvdGluZ19hZ2VbaV07XG4gIG11W2ldID0gZXhwKG11W2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbn1cbmBgYCJ9 -->

```stan
data {
  int<lower = 1> n;
  real tot_pac_money[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real p;
  real p_np;
  real p_new;
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  
  // Binomial model
  p ~ normal(0, 1);
  p_np ~ normal(0, 1);
  p_new ~ normal(0, 1);
  
  // Gamma model
  a ~ normal(13, 15);
  b_np ~ normal(0, 14);
  b_new ~ normal(0, 14);
  b_white ~ normal(0, 14);
  b_black ~ normal(0, 14);
  b_latino ~ normal(0, 14);
  b_asian ~ normal(0, 14);
  b_bach ~ normal(0, 14);
  b_clint ~ normal(0, 14);
  b_house ~ normal(0, 14);
  b_vpop ~ normal(0, 14);
  b_mhh ~ normal(0, 14);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    if (tot_pac_money[i] == 0)
      target += (bernoulli_lpmf(1 | prob[i]));
    else
      target += (bernoulli_lpmf(0 | prob[i]) + gamma_lpdf(tot_pac_money[i]| mu[i]/scale, 1/scale));
    }//i
}

generated quantities {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
        prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i];
        prob[i] = inv_logit(prob[i]);
    }
    
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KHRvdF9wYWNfbW9uZXksIG5vX2NvcnBfcGFjcywgbmV3X21lbWJlciwgcGN0X3doaXRlLCBwY3RfYmxhY2ssIFxuICAgICAgICAgICAgICAgIHBjdF9sYXRpbm8sIHBjdF9hc2lhbiwgcGN0X2JhY2hlbG9ycywgcGN0X2NsaW50b25fMTYsXG4gICAgICAgICAgICAgICAgcGN0X2RlbV9ob3VzZV8xNiwgbG9nX3RvdF92b3RpbmdfYWdlLCBsb2dfbWVkaWFuX2hoX2luY29tZSkgJT4lXG4gIG11dGF0ZShub19jb3JwX3BhY3MgPSBpZmVsc2Uobm9fY29ycF9wYWNzID09IFwiWUVTXCIsIDEsIDApLFxuICAgICAgICAgbmV3X21lbWJlciA9IGlmZWxzZShuZXdfbWVtYmVyID09IFwiWUVTXCIsIDEsIDApKSAlPiVcbiAgbXV0YXRlX2F0KC52YXJzID0gdmFycyhwY3Rfd2hpdGU6bG9nX21lZGlhbl9oaF9pbmNvbWUpLCAuZnVucyA9IGNlbnRlcikgJT4lXG4gIGNvbXBvc2VfZGF0YSgpXG5cbiMgbW9kZWwgZXN0aW1hdGlvbiAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cbiMjIEZyZXF1ZW50aXN0XG5wYWMubW9uZXkuMDEgPC0gZ2xtKHRvdF9wYWNfbW9uZXkgfiBub19jb3JwX3BhY3MgKyBuZXdfbWVtYmVyICsgcGN0X3doaXRlICsgcGN0X2JsYWNrICsgcGN0X2xhdGlubyArIHBjdF9hc2lhbiArIHBjdF9iYWNoZWxvcnMgKyBwY3RfY2xpbnRvbl8xNiArIHBjdF9kZW1faG91c2VfMTYgKyBsb2dfbWVkaWFuX2hoX2luY29tZSArIGxvZ190b3Rfdm90aW5nX2FnZSxcbiAgICAgICAgICAgICAgICAgICAgZmFtaWx5ID0gR2FtbWEobGluayA9IFwibG9nXCIpLFxuICAgICAgICAgICAgICAgICAgIGRhdGEgPSBhcy5kYXRhLmZyYW1lKHN0YW4uZGF0YSkgJT4lIGZpbHRlcih0b3RfcGFjX21vbmV5ICE9IDApICU+JSBtdXRhdGVfYXQoLnZhcnMgPSB2YXJzKHBjdF93aGl0ZTpsb2dfbWVkaWFuX2hoX2luY29tZSksIC5mdW5zID0gY2VudGVyKSlcblxuIyMgQmF5ZXNpYW5cbnRvdHBhYy5maXQgPC0gc2FtcGxpbmcodG90cGFjLm1vZGVsLFxuICAgICAgICAgICAgICAgICAgICAgICBjaGFpbnMgPSA0LFxuICAgICAgICAgICAgICAgICAgICAgICBjb3JlcyA9IDQsXG4gICAgICAgICAgICAgICAgICAgICAgIGl0ZXIgPSAxMDAwMCxcbiAgICAgICAgICAgICAgICAgICAgICAgd2FybXVwID0gNTAwMCxcbiAgICAgICAgICAgICAgICAgICAgICAgZGF0YSA9IHN0YW4uZGF0YSlcblxudHJhY2VwbG90KHRvdHBhYy5maXQsIFxuICAgICAgICAgIGluY193YXJtdXAgPSBUUlVFLCBcbiAgICAgICAgICBwYXJzID0gYyhcImFcIiwgXCJiX25wXCIsIFwiYl9uZXdcIiwgXCJiX3doaXRlXCIsIFwiYl9ibGFja1wiLCBcImJfbGF0aW5vXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9hc2lhblwiLCBcImJfYmFjaFwiLCBcImJfY2xpbnRcIiwgXCJiX2hvdXNlXCIsIFwiYl92cG9wXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9taGhcIiwgXCJzY2FsZVwiKSlcblxudGlkeShwYWMubW9uZXkuMDEsIGNvbmYuaW50ID0gVFJVRSwgY29uZi5sZXZlbCA9IDAuODksIGRpZ2l0cyA9IDMpXG5wcmVjaXModG90cGFjLmZpdCwgcHJvYiA9IDAuODksIGRpZ2l0cyA9IDMpXG5cbnBvc3QgPC0gc3ByZWFkX2RyYXdzKHRvdHBhYy5maXQsIG11W2ldKSAlPiUgbWVkaWFuX2hkaSgpXG5nZ3Bsb3QoZGF0YSA9IGVzdGltYXRpb24uZGF0YSwgYWVzKHggPSB0b3RfcGFjX21vbmV5KSkgK1xuICBnZW9tX2RlbnNpdHkoZmlsbCA9IFwiZ3JheVwiKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gcG9zdCwgYWVzKHggPSBtdSksIGZpbGwgPSBcInNreWJsdWVcIiwgYWxwaGEgPSAwLjcpXG5cbnRlc3QgPC0gbWFwMnN0YW4oXG4gIGFsaXN0KHRvdF9wYWNfbW9uZXkgfiBkemFnYW1tYTIocHJvYiwgbXUsIHNjYWxlKSxcbiAgICAgICAgbG9nKG11KSA8LSBhICsgYl9ucCAqIG5vX2NvcnBfcGFjcyxcbiAgICAgICAgbG9naXQocHJvYikgPC0gcCArIHBfbnAgKiBub19jb3JwX3BhY3MsXG4gICAgICAgIGEgfiBkbm9ybSgwLCAxMDApLFxuICAgICAgICBiX25wIH4gZG5vcm0oMCwgMTAwKSxcbiAgICAgICAgcCB+IGRub3JtKDAsIDEwMCksXG4gICAgICAgIHBfbnAgfiBkbm9ybSgwLCAxMDApLFxuICAgICAgICBzY2FsZSB+IGRleHAoMikpLFxuICBkYXRhID0gc3Rhbi5kYXRhXG4pXG5gYGAifQ== -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(tot_pac_money, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
## Frequentist
pac.money.01 <- glm(tot_pac_money ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + pct_clinton_16 + pct_dem_house_16 + log_median_hh_income + log_tot_voting_age,
                    family = Gamma(link = "log"),
                   data = as.data.frame(stan.data) %>% filter(tot_pac_money != 0) %>% mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center))

## Bayesian
totpac.fit <- sampling(totpac.model,
                       chains = 4,
                       cores = 4,
                       iter = 10000,
                       warmup = 5000,
                       data = stan.data)

traceplot(totpac.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh", "scale"))

tidy(pac.money.01, conf.int = TRUE, conf.level = 0.89, digits = 3)
precis(totpac.fit, prob = 0.89, digits = 3)

post <- spread_draws(totpac.fit, mu[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = tot_pac_money)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = mu), fill = "skyblue", alpha = 0.7)

test <- map2stan(
  alist(tot_pac_money ~ dzagamma2(prob, mu, scale),
        log(mu) <- a + b_np * no_corp_pacs,
        logit(prob) <- p + p_np * no_corp_pacs,
        a ~ dnorm(0, 100),
        b_np ~ dnorm(0, 100),
        p ~ dnorm(0, 100),
        p_np ~ dnorm(0, 100),
        scale ~ dexp(2)),
  data = stan.data
)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Model of Number of PAC Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDYpLCBleHAoMTIpKSksXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJOb3JtYWxcIikpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBJbnRlcmNlcHRcIikgK1xuICB0aGVtZShheGlzLnRleHQueCA9IGVsZW1lbnRfdGV4dChhbmdsZSA9IDQ1LCB2anVzdCA9IDEsIGhqdXN0ID0gMSkpXG5cbiMjIFNsb3BlIFBhcmFtZXRlcnNcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocm5vcm0oMWU0LCBleHAoMCksIGV4cCgxMikpKSxcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcIm5vcm1hbFwiKSkgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFNsb3BlIFBhcmFtZXRlcnNcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSBcblxuIyMgU2NhbGUgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAxKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0xXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0yXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2NhbGUgUGFyYW1ldGVyXCIpXG5cbiMjIFByb2JhYmlsaXR5IFBhcmFtZXRlclxuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShydW5pZigxZTQsIDAsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJ1bmlmb3JtXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMiwgMikpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMlwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNSkgK1xuICAgIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMS41LCAxLjUpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJiZXRhIDEuNVwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNykgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFByb2JhYmlsaXR5IFBhcmFtZXRlclwiKVxuYGBgIn0= -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(6), exp(12))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(12))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIGludCBudW1fcGFjX2NvbnRyaWJzW25dO1xuICBpbnQgbm9fY29ycF9wYWNzW25dO1xuICBpbnQgbmV3X21lbWJlcltuXTtcbiAgcmVhbCBwY3Rfd2hpdGVbbl07XG4gIHJlYWwgcGN0X2JsYWNrW25dO1xuICByZWFsIHBjdF9sYXRpbm9bbl07XG4gIHJlYWwgcGN0X2FzaWFuW25dO1xuICByZWFsIHBjdF9iYWNoZWxvcnNbbl07XG4gIHJlYWwgcGN0X2NsaW50b25fMTZbbl07XG4gIHJlYWwgcGN0X2RlbV9ob3VzZV8xNltuXTtcbiAgcmVhbCBsb2dfbWVkaWFuX2hoX2luY29tZVtuXTtcbiAgcmVhbCBsb2dfdG90X3ZvdGluZ19hZ2Vbbl07XG59XG5cbnBhcmFtZXRlcnMge1xuICByZWFsPGxvd2VyID0gMD4gYTtcbiAgcmVhbCBiX25wO1xuICByZWFsIGJfbmV3O1xuICByZWFsIGJfd2hpdGU7XG4gIHJlYWwgYl9ibGFjaztcbiAgcmVhbCBiX2xhdGlubztcbiAgcmVhbCBiX2FzaWFuO1xuICByZWFsIGJfYmFjaDtcbiAgcmVhbCBiX21oaDtcbiAgcmVhbCBiX2NsaW50O1xuICByZWFsIGJfaG91c2U7XG4gIHJlYWwgYl92cG9wO1xufVxuXG5tb2RlbCB7XG4gIC8vIGxpbmVhciBtb2RlbFxuICB2ZWN0b3Jbbl0gbGFtYmRhO1xuICBcbiAgZm9yIChpIGluIDE6bikge1xuICAgIGxhbWJkYVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgICBsYW1iZGFbaV0gPSBleHAobGFtYmRhW2ldKTsgLy8gaW52ZXJzZSBsb2ctbGlua1xuICB9XG4gIFxuICAvLyBwcmlvcnNcbiAgLy8gUG9pc3NvbiBtb2RlbFxuICBhIH4gbm9ybWFsKDYsIDEyKTtcbiAgYl9ucCB+IG5vcm1hbCgwLCAxMik7XG4gIGJfbmV3IH4gbm9ybWFsKDAsIDEyKTtcbiAgYl93aGl0ZSB+IG5vcm1hbCgwLCAxMik7XG4gIGJfYmxhY2sgfiBub3JtYWwoMCwgMTIpO1xuICBiX2xhdGlubyB+IG5vcm1hbCgwLCAxMik7XG4gIGJfYXNpYW4gfiBub3JtYWwoMCwgMTIpO1xuICBiX2JhY2ggfiBub3JtYWwoMCwgMTIpO1xuICBiX2NsaW50IH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9ob3VzZSB+IG5vcm1hbCgwLCAxMik7XG4gIGJfdnBvcCB+IG5vcm1hbCgwLCAxMik7XG4gIGJfbWhoIH4gbm9ybWFsKDAsIDEyKTtcbiAgXG4gIC8vIGxpa2VsaWhvb2RcbiAgZm9yIChpIGluIDE6bikge1xuICAgIG51bV9wYWNfY29udHJpYnNbaV0gfiBwb2lzc29uKGxhbWJkYVtpXSk7XG4gICAgfVxufVxuXG5cbmdlbmVyYXRlZCBxdWFudGl0aWVzIHtcbiAgdmVjdG9yW25dIGxhbWJkYTtcbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgbGFtYmRhW2ldID0gYSArIGJfbnAgKiBub19jb3JwX3BhY3NbaV0gKyBiX25ldyAqIG5ld19tZW1iZXJbaV0gKyBcbiAgICAgIGJfd2hpdGUgKiBwY3Rfd2hpdGVbaV0gKyBiX2JsYWNrICogcGN0X2JsYWNrW2ldICsgXG4gICAgICBiX2xhdGlubyAqIHBjdF9sYXRpbm9baV0gKyBiX2FzaWFuICogcGN0X2FzaWFuW2ldICsgXG4gICAgICBiX2JhY2ggKiBwY3RfYmFjaGVsb3JzW2ldICsgYl9jbGludCAqIHBjdF9jbGludG9uXzE2W2ldICsgXG4gICAgICBiX2hvdXNlICogcGN0X2RlbV9ob3VzZV8xNltpXSArIGJfbWhoICogbG9nX21lZGlhbl9oaF9pbmNvbWVbaV0gK1xuICAgICAgYl92cG9wICogbG9nX3RvdF92b3RpbmdfYWdlW2ldO1xuICBsYW1iZGFbaV0gPSBleHAobGFtYmRhW2ldKTsgLy8gaW52ZXJzZSBsb2ctbGlua1xuICB9XG59XG5gYGAifQ== -->

```stan
data {
  int<lower = 1> n;
  int num_pac_contribs[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
}

model {
  // linear model
  vector[n] lambda;
  
  for (i in 1:n) {
    lambda[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    lambda[i] = exp(lambda[i]); // inverse log-link
  }
  
  // priors
  // Poisson model
  a ~ normal(6, 12);
  b_np ~ normal(0, 12);
  b_new ~ normal(0, 12);
  b_white ~ normal(0, 12);
  b_black ~ normal(0, 12);
  b_latino ~ normal(0, 12);
  b_asian ~ normal(0, 12);
  b_bach ~ normal(0, 12);
  b_clint ~ normal(0, 12);
  b_house ~ normal(0, 12);
  b_vpop ~ normal(0, 12);
  b_mhh ~ normal(0, 12);
  
  // likelihood
  for (i in 1:n) {
    num_pac_contribs[i] ~ poisson(lambda[i]);
    }
}


generated quantities {
  vector[n] lambda;
  
  for (i in 1:n) {
  lambda[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  lambda[i] = exp(lambda[i]); // inverse log-link
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KG51bV9wYWNfY29udHJpYnMsIG5vX2NvcnBfcGFjcywgbmV3X21lbWJlciwgcGN0X3doaXRlLCBwY3RfYmxhY2ssIFxuICAgICAgICAgICAgICAgIHBjdF9sYXRpbm8sIHBjdF9hc2lhbiwgcGN0X2JhY2hlbG9ycywgcGN0X2NsaW50b25fMTYsXG4gICAgICAgICAgICAgICAgcGN0X2RlbV9ob3VzZV8xNiwgbG9nX3RvdF92b3RpbmdfYWdlLCBsb2dfbWVkaWFuX2hoX2luY29tZSkgJT4lXG4gIG11dGF0ZShub19jb3JwX3BhY3MgPSBpZmVsc2Uobm9fY29ycF9wYWNzID09IFwiWUVTXCIsIDEsIDApLFxuICAgICAgICAgbmV3X21lbWJlciA9IGlmZWxzZShuZXdfbWVtYmVyID09IFwiWUVTXCIsIDEsIDApKSAlPiVcbiAgZHJvcF9uYSgpICU+JVxuICBtdXRhdGVfYXQoLnZhcnMgPSB2YXJzKHBjdF93aGl0ZTpsb2dfbWVkaWFuX2hoX2luY29tZSksIC5mdW5zID0gY2VudGVyKSAlPiVcbiAgY29tcG9zZV9kYXRhKClcblxuIyBtb2RlbCBlc3RpbWF0aW9uIC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxuIyMgRnJlcXVlbnRpc3Rcbm51bS5wYWNzLjAxIDwtIGdsbShudW1fcGFjX2NvbnRyaWJzIH4gbm9fY29ycF9wYWNzICsgbmV3X21lbWJlciArIHBjdF93aGl0ZSArIHBjdF9ibGFjayArIHBjdF9sYXRpbm8gKyBwY3RfYXNpYW4gKyBwY3RfYmFjaGVsb3JzICsgbG9nX21lZGlhbl9oaF9pbmNvbWUgKyBwY3RfY2xpbnRvbl8xNiArIHBjdF9kZW1faG91c2VfMTYgKyBsb2dfdG90X3ZvdGluZ19hZ2UsXG4gICAgICAgICAgICAgICAgICAgZmFtaWx5ID0gcXVhc2lwb2lzc29uKGxpbmsgPSBcImxvZ1wiKSxcbiAgICAgICAgICAgICAgICAgIGRhdGEgPSBhcy5kYXRhLmZyYW1lKHN0YW4uZGF0YSkpXG5cbiMjIEJheWVzaWFuXG5udW1wYWMuZml0IDwtIHNhbXBsaW5nKG51bXBhYy5tb2RlbCxcbiAgICAgICAgICAgICAgICAgICAgICAgY2hhaW5zID0gNCxcbiAgICAgICAgICAgICAgICAgICAgICAgY29yZXMgPSA0LFxuICAgICAgICAgICAgICAgICAgICAgICBpdGVyID0gMTAwMDAsXG4gICAgICAgICAgICAgICAgICAgICAgIHdhcm11cCA9IDUwMDAsXG4gICAgICAgICAgICAgICAgICAgICAgIGRhdGEgPSBzdGFuLmRhdGEpXG5cbnRyYWNlcGxvdChudW1wYWMuZml0LCBcbiAgICAgICAgICBpbmNfd2FybXVwID0gVFJVRSwgXG4gICAgICAgICAgcGFycyA9IGMoXCJhXCIsIFwiYl9ucFwiLCBcImJfbmV3XCIsIFwiYl93aGl0ZVwiLCBcImJfYmxhY2tcIiwgXCJiX2xhdGlub1wiLCBcbiAgICAgICAgICAgICAgICAgICBcImJfYXNpYW5cIiwgXCJiX2JhY2hcIiwgXCJiX2NsaW50XCIsIFwiYl9ob3VzZVwiLCBcImJfdnBvcFwiLCBcbiAgICAgICAgICAgICAgICAgICBcImJfbWhoXCIpKVxuXG50aWR5KG51bS5wYWNzLjAxLCBjb25mLmludCA9IFRSVUUsIGNvbmYubGV2ZWwgPSAwLjg5LCBkaWdpdHMgPSAzKVxucHJlY2lzKG51bXBhYy5maXQsIHByb2IgPSAwLjg5LCBkaWdpdHMgPSAzKVxuXG5wb3N0IDwtIHNwcmVhZF9kcmF3cyhudW1wYWMuZml0LCBsYW1iZGFbaV0pICU+JSBtZWRpYW5faGRpKClcbmdncGxvdChkYXRhID0gZXN0aW1hdGlvbi5kYXRhLCBhZXMoeCA9IG51bV9wYWNfY29udHJpYnMpKSArXG4gIGdlb21fZGVuc2l0eShmaWxsID0gXCJncmF5XCIpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBwb3N0LCBhZXMoeCA9IGxhbWJkYSksIGZpbGwgPSBcInNreWJsdWVcIiwgYWxwaGEgPSAwLjcpXG5gYGAifQ== -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(num_pac_contribs, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  drop_na() %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
## Frequentist
num.pacs.01 <- glm(num_pac_contribs ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + log_median_hh_income + pct_clinton_16 + pct_dem_house_16 + log_tot_voting_age,
                   family = quasipoisson(link = "log"),
                  data = as.data.frame(stan.data))

## Bayesian
numpac.fit <- sampling(numpac.model,
                       chains = 4,
                       cores = 4,
                       iter = 10000,
                       warmup = 5000,
                       data = stan.data)

traceplot(numpac.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh"))

tidy(num.pacs.01, conf.int = TRUE, conf.level = 0.89, digits = 3)
precis(numpac.fit, prob = 0.89, digits = 3)

post <- spread_draws(numpac.fit, lambda[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = num_pac_contribs)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = lambda), fill = "skyblue", alpha = 0.7)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Model of Business PAC Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDEyLjUpLCBleHAoMTQpKSksXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJOb3JtYWxcIikpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBJbnRlcmNlcHRcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSArXG4gIHRoZW1lKGF4aXMudGV4dC54ID0gZWxlbWVudF90ZXh0KGFuZ2xlID0gNDUsIHZqdXN0ID0gMSwgaGp1c3QgPSAxKSlcblxuIyMgU2xvcGUgUGFyYW1ldGVyc1xuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShybm9ybSgxZTQsIGV4cCgwKSwgZXhwKDEzKSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwibm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhsYWJlbHMgPSBkb2xsYXJfZm9ybWF0KCkpIFxuXG4jIyBTY2FsZSBQYXJhbWV0ZXJcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTFcIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDIpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTJcIiksIFxuICAgICAgICAgICAgICAgYWxwaGEgPSAwLjUpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBTY2FsZSBQYXJhbWV0ZXJcIilcblxuIyMgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJ1bmlmKDFlNCwgMCwgMSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcInVuaWZvcm1cIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAyLCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiYmV0YSAyXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gICAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAxLjUsIDEuNSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMS41XCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC43KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXCIpXG5gYGAifQ== -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(12.5), exp(14))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(13))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgYnVzaW5lc3NfcGFjc1tuXTtcbiAgaW50IG5vX2NvcnBfcGFjc1tuXTtcbiAgaW50IG5ld19tZW1iZXJbbl07XG4gIHJlYWwgcGN0X3doaXRlW25dO1xuICByZWFsIHBjdF9ibGFja1tuXTtcbiAgcmVhbCBwY3RfbGF0aW5vW25dO1xuICByZWFsIHBjdF9hc2lhbltuXTtcbiAgcmVhbCBwY3RfYmFjaGVsb3JzW25dO1xuICByZWFsIHBjdF9jbGludG9uXzE2W25dO1xuICByZWFsIHBjdF9kZW1faG91c2VfMTZbbl07XG4gIHJlYWwgbG9nX21lZGlhbl9oaF9pbmNvbWVbbl07XG4gIHJlYWwgbG9nX3RvdF92b3RpbmdfYWdlW25dO1xufVxuXG5wYXJhbWV0ZXJzIHtcbiAgcmVhbCBwO1xuICByZWFsIHBfbnA7XG4gIHJlYWwgcF9uZXc7XG4gIHJlYWw8bG93ZXIgPSAwPiBhO1xuICByZWFsIGJfbnA7XG4gIHJlYWwgYl9uZXc7XG4gIHJlYWwgYl93aGl0ZTtcbiAgcmVhbCBiX2JsYWNrO1xuICByZWFsIGJfbGF0aW5vO1xuICByZWFsIGJfYXNpYW47XG4gIHJlYWwgYl9iYWNoO1xuICByZWFsIGJfbWhoO1xuICByZWFsIGJfY2xpbnQ7XG4gIHJlYWwgYl9ob3VzZTtcbiAgcmVhbCBiX3Zwb3A7XG4gIHJlYWw8bG93ZXIgPSAwPiBzY2FsZTtcbn1cblxubW9kZWwge1xuICB2ZWN0b3Jbbl0gcHJvYjtcbiAgdmVjdG9yW25dIG11O1xuICBcbiAgZm9yIChpIGluIDE6bikge1xuICAgIC8vIGxpbmVhciBtb2RlbFxuICAgIHByb2JbaV0gPSBwICsgcF9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIHBfbmV3ICogbmV3X21lbWJlcltpXTsgXG4gICAgcHJvYltpXSA9IGludl9sb2dpdChwcm9iW2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgICAvLyBsaW5lYXIgbW9kZWxcbiAgICBtdVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgICBtdVtpXSA9IGV4cChtdVtpXSk7IC8vIGludmVyc2UgbGluayBmdW5jdGlvblxuICB9XG4gIFxuICAvLyBwcmlvcnNcbiAgXG4gIC8vIEJpbm9taWFsIG1vZGVsXG4gIHAgfiBub3JtYWwoMCwgMSk7XG4gIHBfbnAgfiBub3JtYWwoMCwgMSk7XG4gIHBfbmV3IH4gbm9ybWFsKDAsIDEpO1xuICBcbiAgLy8gR2FtbWEgbW9kZWxcbiAgYSB+IG5vcm1hbCgxMi41LCAxNCk7XG4gIGJfbnAgfiBub3JtYWwoMCwgMTMpO1xuICBiX25ldyB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfd2hpdGUgfiBub3JtYWwoMCwgMTMpO1xuICBiX2JsYWNrIH4gbm9ybWFsKDAsIDEzKTtcbiAgYl9sYXRpbm8gfiBub3JtYWwoMCwgMTMpO1xuICBiX2FzaWFuIH4gbm9ybWFsKDAsIDEzKTtcbiAgYl9iYWNoIH4gbm9ybWFsKDAsIDEzKTtcbiAgYl9jbGludCB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfaG91c2UgfiBub3JtYWwoMCwgMTMpO1xuICBiX3Zwb3AgfiBub3JtYWwoMCwgMTMpO1xuICBiX21oaCB+IG5vcm1hbCgwLCAxMyk7XG4gIHNjYWxlIH4gZXhwb25lbnRpYWwoMik7XG4gIFxuICAvLyBsaWtlbGlob29kXG4gIGZvciAoIGkgaW4gMTpuKSB7XG4gICAgaWYgKGJ1c2luZXNzX3BhY3NbaV0gPT0gMClcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMXwgcHJvYltpXSkpO1xuICAgIGVsc2VcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMHwgcHJvYltpXSkgKyBnYW1tYV9scGRmKGJ1c2luZXNzX3BhY3NbaV18IG11W2ldL3NjYWxlLCAxL3NjYWxlKSk7XG4gICAgfS8vaVxufVxuXG5nZW5lcmF0ZWQgcXVhbnRpdGllcyB7XG4gIHZlY3RvcltuXSBwcm9iO1xuICB2ZWN0b3Jbbl0gbXU7XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgLy8gbGluZWFyIG1vZGVsXG4gICAgcHJvYltpXSA9IHAgKyBwX25wICogbm9fY29ycF9wYWNzW2ldICsgcF9uZXcgKiBuZXdfbWVtYmVyW2ldOyBcbiAgICBwcm9iW2ldID0gaW52X2xvZ2l0KHByb2JbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxuICBcbiAgZm9yIChpIGluIDE6bikge1xuICBtdVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxufVxuYGBgIn0= -->

```stan
data {
  int<lower = 1> n;
  real business_pacs[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real p;
  real p_np;
  real p_new;
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  
  // Binomial model
  p ~ normal(0, 1);
  p_np ~ normal(0, 1);
  p_new ~ normal(0, 1);
  
  // Gamma model
  a ~ normal(12.5, 14);
  b_np ~ normal(0, 13);
  b_new ~ normal(0, 13);
  b_white ~ normal(0, 13);
  b_black ~ normal(0, 13);
  b_latino ~ normal(0, 13);
  b_asian ~ normal(0, 13);
  b_bach ~ normal(0, 13);
  b_clint ~ normal(0, 13);
  b_house ~ normal(0, 13);
  b_vpop ~ normal(0, 13);
  b_mhh ~ normal(0, 13);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    if (business_pacs[i] == 0)
      target += (bernoulli_lpmf(1| prob[i]));
    else
      target += (bernoulli_lpmf(0| prob[i]) + gamma_lpdf(business_pacs[i]| mu[i]/scale, 1/scale));
    }//i
}

generated quantities {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KGJ1c2luZXNzX3BhY3MsIG5vX2NvcnBfcGFjcywgbmV3X21lbWJlciwgcGN0X3doaXRlLCBwY3RfYmxhY2ssIFxuICAgICAgICAgICAgICAgIHBjdF9sYXRpbm8sIHBjdF9hc2lhbiwgcGN0X2JhY2hlbG9ycywgcGN0X2NsaW50b25fMTYsXG4gICAgICAgICAgICAgICAgcGN0X2RlbV9ob3VzZV8xNiwgbG9nX3RvdF92b3RpbmdfYWdlLCBsb2dfbWVkaWFuX2hoX2luY29tZSkgJT4lXG4gIG11dGF0ZShub19jb3JwX3BhY3MgPSBpZmVsc2Uobm9fY29ycF9wYWNzID09IFwiWUVTXCIsIDEsIDApLFxuICAgICAgICAgbmV3X21lbWJlciA9IGlmZWxzZShuZXdfbWVtYmVyID09IFwiWUVTXCIsIDEsIDApKSAlPiVcbiAgZHJvcF9uYSgpICU+JVxuICBtdXRhdGVfYXQoLnZhcnMgPSB2YXJzKHBjdF93aGl0ZTpsb2dfbWVkaWFuX2hoX2luY29tZSksIC5mdW5zID0gY2VudGVyKSAlPiVcbiAgY29tcG9zZV9kYXRhKClcblxuIyBtb2RlbCBlc3RpbWF0aW9uIC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxuIyMgRnJlcXVlbnRpc3RcbmJpei5wYWNzLjAxIDwtIGdsbShidXNpbmVzc19wYWNzIH4gbm9fY29ycF9wYWNzICsgbmV3X21lbWJlciArIHBjdF93aGl0ZSArIHBjdF9ibGFjayArIHBjdF9sYXRpbm8gKyBwY3RfYXNpYW4gKyBwY3RfYmFjaGVsb3JzICsgcGN0X2NsaW50b25fMTYgKyBwY3RfZGVtX2hvdXNlXzE2ICsgbG9nX21lZGlhbl9oaF9pbmNvbWUgKyBsb2dfdG90X3ZvdGluZ19hZ2UsXG4gICAgICAgICAgICAgICAgICAgZmFtaWx5ID0gR2FtbWEobGluayA9IFwibG9nXCIpLFxuICAgICAgICAgICAgICAgICAgIGRhdGEgPSBhcy5kYXRhLmZyYW1lKHN0YW4uZGF0YSkgJT4lIGZpbHRlcihidXNpbmVzc19wYWNzICE9IDApICU+JSBtdXRhdGVfYXQoLnZhcnMgPSB2YXJzKHBjdF93aGl0ZTpsb2dfbWVkaWFuX2hoX2luY29tZSksIC5mdW5zID0gY2VudGVyKSlcblxuIyMgQmF5ZXNpYW5cbmJpenBhYy5maXQgPC0gc2FtcGxpbmcoYml6cGFjLm1vZGVsLFxuICAgICAgICAgICAgICAgICAgICAgICBjaGFpbnMgPSA0LFxuICAgICAgICAgICAgICAgICAgICAgICBjb3JlcyA9IDQsXG4gICAgICAgICAgICAgICAgICAgICAgIGl0ZXIgPSAxMDAwMCxcbiAgICAgICAgICAgICAgICAgICAgICAgd2FybXVwID0gNTAwMCxcbiAgICAgICAgICAgICAgICAgICAgICAgZGF0YSA9IHN0YW4uZGF0YSlcblxudHJhY2VwbG90KGJpenBhYy5maXQsIFxuICAgICAgICAgIGluY193YXJtdXAgPSBUUlVFLCBcbiAgICAgICAgICBwYXJzID0gYyhcImFcIiwgXCJiX25wXCIsIFwiYl9uZXdcIiwgXCJiX3doaXRlXCIsIFwiYl9ibGFja1wiLCBcImJfbGF0aW5vXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9hc2lhblwiLCBcImJfYmFjaFwiLCBcImJfY2xpbnRcIiwgXCJiX2hvdXNlXCIsIFwiYl92cG9wXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9taGhcIiwgXCJzY2FsZVwiKSlcblxudGlkeShiaXoucGFjcy4wMSwgY29uZi5pbnQgPSBUUlVFLCBjb25mLmxldmVsID0gMC44OSwgZGlnaXRzID0gMylcbnByZWNpcyhiaXpwYWMuZml0LCBwcm9iID0gMC44OSwgZGlnaXRzID0gMylcblxucG9zdCA8LSBzcHJlYWRfZHJhd3MoYml6cGFjLmZpdCwgbXVbaV0pICU+JSBtZWRpYW5faGRpKClcbmdncGxvdChkYXRhID0gZXN0aW1hdGlvbi5kYXRhLCBhZXMoeCA9IGJ1c2luZXNzX3BhY3MpKSArXG4gIGdlb21fZGVuc2l0eShmaWxsID0gXCJncmF5XCIpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBwb3N0LCBhZXMoeCA9IG11KSwgZmlsbCA9IFwic2t5Ymx1ZVwiLCBhbHBoYSA9IDAuNylcbmBgYCJ9 -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(business_pacs, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  drop_na() %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
## Frequentist
biz.pacs.01 <- glm(business_pacs ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + pct_clinton_16 + pct_dem_house_16 + log_median_hh_income + log_tot_voting_age,
                   family = Gamma(link = "log"),
                   data = as.data.frame(stan.data) %>% filter(business_pacs != 0) %>% mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center))

## Bayesian
bizpac.fit <- sampling(bizpac.model,
                       chains = 4,
                       cores = 4,
                       iter = 10000,
                       warmup = 5000,
                       data = stan.data)

traceplot(bizpac.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh", "scale"))

tidy(biz.pacs.01, conf.int = TRUE, conf.level = 0.89, digits = 3)
precis(bizpac.fit, prob = 0.89, digits = 3)

post <- spread_draws(bizpac.fit, mu[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = business_pacs)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = mu), fill = "skyblue", alpha = 0.7)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# Ideological PAC Models

## Model of Ideological PAC Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDEwLjUpLCBleHAoMTMpKSksXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJOb3JtYWxcIikpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBJbnRlcmNlcHRcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSArXG4gIHRoZW1lKGF4aXMudGV4dC54ID0gZWxlbWVudF90ZXh0KGFuZ2xlID0gNDUsIHZqdXN0ID0gMSwgaGp1c3QgPSAxKSlcblxuIyMgU2xvcGUgUGFyYW1ldGVyc1xuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShybm9ybSgxZTQsIGV4cCgwKSwgZXhwKDEyKSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwibm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhsYWJlbHMgPSBkb2xsYXJfZm9ybWF0KCkpIFxuXG4jIyBTY2FsZSBQYXJhbWV0ZXJcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTFcIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDIpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTJcIiksIFxuICAgICAgICAgICAgICAgYWxwaGEgPSAwLjUpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBTY2FsZSBQYXJhbWV0ZXJcIilcblxuIyMgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJ1bmlmKDFlNCwgMCwgMSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcInVuaWZvcm1cIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAyLCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiYmV0YSAyXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gICAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAxLjUsIDEuNSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMS41XCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC43KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXCIpXG5gYGAifQ== -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(10.5), exp(13))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(12))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgaWRlb2xvZ2ljYWxfcGFjc1tuXTtcbiAgaW50IG5vX2NvcnBfcGFjc1tuXTtcbiAgaW50IG5ld19tZW1iZXJbbl07XG4gIHJlYWwgcGN0X3doaXRlW25dO1xuICByZWFsIHBjdF9ibGFja1tuXTtcbiAgcmVhbCBwY3RfbGF0aW5vW25dO1xuICByZWFsIHBjdF9hc2lhbltuXTtcbiAgcmVhbCBwY3RfYmFjaGVsb3JzW25dO1xuICByZWFsIHBjdF9jbGludG9uXzE2W25dO1xuICByZWFsIHBjdF9kZW1faG91c2VfMTZbbl07XG4gIHJlYWwgbG9nX21lZGlhbl9oaF9pbmNvbWVbbl07XG4gIHJlYWwgbG9nX3RvdF92b3RpbmdfYWdlW25dO1xufVxuXG5wYXJhbWV0ZXJzIHtcbiAgcmVhbCBwO1xuICByZWFsIHBfbnA7XG4gIHJlYWwgcF9uZXc7XG4gIHJlYWw8bG93ZXIgPSAwPiBhO1xuICByZWFsIGJfbnA7XG4gIHJlYWwgYl9uZXc7XG4gIHJlYWwgYl93aGl0ZTtcbiAgcmVhbCBiX2JsYWNrO1xuICByZWFsIGJfbGF0aW5vO1xuICByZWFsIGJfYXNpYW47XG4gIHJlYWwgYl9iYWNoO1xuICByZWFsIGJfbWhoO1xuICByZWFsIGJfY2xpbnQ7XG4gIHJlYWwgYl9ob3VzZTtcbiAgcmVhbCBiX3Zwb3A7XG4gIHJlYWw8bG93ZXIgPSAwPiBzY2FsZTtcbn1cblxubW9kZWwge1xuICB2ZWN0b3Jbbl0gcHJvYjtcbiAgdmVjdG9yW25dIG11O1xuICBcbiAgZm9yIChpIGluIDE6bikge1xuICAgIC8vIGxpbmVhciBtb2RlbFxuICAgIHByb2JbaV0gPSBwICsgcF9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIHBfbmV3ICogbmV3X21lbWJlcltpXTsgXG4gICAgcHJvYltpXSA9IGludl9sb2dpdChwcm9iW2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgICAvLyBsaW5lYXIgbW9kZWxcbiAgICBtdVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgICBtdVtpXSA9IGV4cChtdVtpXSk7IC8vIGludmVyc2UgbGluayBmdW5jdGlvblxuICB9XG4gIFxuICAvLyBwcmlvcnNcbiAgXG4gIC8vIEJpbm9taWFsIG1vZGVsXG4gIHAgfiBub3JtYWwoMCwgMSk7XG4gIHBfbnAgfiBub3JtYWwoMCwgMSk7XG4gIHBfbmV3IH4gbm9ybWFsKDAsIDEpO1xuICBcbiAgLy8gR2FtbWEgbW9kZWxcbiAgYSB+IG5vcm1hbCgxMC41LCAxMyk7XG4gIGJfbnAgfiBub3JtYWwoMCwgMTIpO1xuICBiX25ldyB+IG5vcm1hbCgwLCAxMik7XG4gIGJfd2hpdGUgfiBub3JtYWwoMCwgMTIpO1xuICBiX2JsYWNrIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9sYXRpbm8gfiBub3JtYWwoMCwgMTIpO1xuICBiX2FzaWFuIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9iYWNoIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9jbGludCB+IG5vcm1hbCgwLCAxMik7XG4gIGJfaG91c2UgfiBub3JtYWwoMCwgMTIpO1xuICBiX3Zwb3AgfiBub3JtYWwoMCwgMTIpO1xuICBiX21oaCB+IG5vcm1hbCgwLCAxMik7XG4gIHNjYWxlIH4gZXhwb25lbnRpYWwoMik7XG4gIFxuICAvLyBsaWtlbGlob29kXG4gIGZvciAoIGkgaW4gMTpuKSB7XG4gICAgaWYgKGlkZW9sb2dpY2FsX3BhY3NbaV0gPT0gMClcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMXwgcHJvYltpXSkpO1xuICAgIGVsc2VcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMHwgcHJvYltpXSkgKyBnYW1tYV9scGRmKGlkZW9sb2dpY2FsX3BhY3NbaV18IG11W2ldL3NjYWxlLCAxL3NjYWxlKSk7XG4gICAgfS8vaVxufVxuXG5nZW5lcmF0ZWQgcXVhbnRpdGllcyB7XG4gIHZlY3RvcltuXSBwcm9iO1xuICB2ZWN0b3Jbbl0gbXU7XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgLy8gbGluZWFyIG1vZGVsXG4gICAgcHJvYltpXSA9IHAgKyBwX25wICogbm9fY29ycF9wYWNzW2ldICsgcF9uZXcgKiBuZXdfbWVtYmVyW2ldOyBcbiAgICBwcm9iW2ldID0gaW52X2xvZ2l0KHByb2JbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxuICBcbiAgZm9yIChpIGluIDE6bikge1xuICBtdVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxufVxuYGBgIn0= -->

```stan
data {
  int<lower = 1> n;
  real ideological_pacs[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real p;
  real p_np;
  real p_new;
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  
  // Binomial model
  p ~ normal(0, 1);
  p_np ~ normal(0, 1);
  p_new ~ normal(0, 1);
  
  // Gamma model
  a ~ normal(10.5, 13);
  b_np ~ normal(0, 12);
  b_new ~ normal(0, 12);
  b_white ~ normal(0, 12);
  b_black ~ normal(0, 12);
  b_latino ~ normal(0, 12);
  b_asian ~ normal(0, 12);
  b_bach ~ normal(0, 12);
  b_clint ~ normal(0, 12);
  b_house ~ normal(0, 12);
  b_vpop ~ normal(0, 12);
  b_mhh ~ normal(0, 12);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    if (ideological_pacs[i] == 0)
      target += (bernoulli_lpmf(1| prob[i]));
    else
      target += (bernoulli_lpmf(0| prob[i]) + gamma_lpdf(ideological_pacs[i]| mu[i]/scale, 1/scale));
    }//i
}

generated quantities {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KGlkZW9sb2dpY2FsX3BhY3MsIG5vX2NvcnBfcGFjcywgbmV3X21lbWJlciwgcGN0X3doaXRlLCBwY3RfYmxhY2ssIFxuICAgICAgICAgICAgICAgIHBjdF9sYXRpbm8sIHBjdF9hc2lhbiwgcGN0X2JhY2hlbG9ycywgcGN0X2NsaW50b25fMTYsXG4gICAgICAgICAgICAgICAgcGN0X2RlbV9ob3VzZV8xNiwgbG9nX3RvdF92b3RpbmdfYWdlLCBsb2dfbWVkaWFuX2hoX2luY29tZSkgJT4lXG4gIG11dGF0ZShub19jb3JwX3BhY3MgPSBpZmVsc2Uobm9fY29ycF9wYWNzID09IFwiWUVTXCIsIDEsIDApLFxuICAgICAgICAgbmV3X21lbWJlciA9IGlmZWxzZShuZXdfbWVtYmVyID09IFwiWUVTXCIsIDEsIDApKSAlPiVcbiAgZHJvcF9uYSgpICU+JVxuICBtdXRhdGVfYXQoLnZhcnMgPSB2YXJzKHBjdF93aGl0ZTpsb2dfbWVkaWFuX2hoX2luY29tZSksIC5mdW5zID0gY2VudGVyKSAlPiVcbiAgY29tcG9zZV9kYXRhKClcblxuIyBtb2RlbCBlc3RpbWF0aW9uIC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxuIyMgRnJlcXVlbnRpc3RcbmlkZW9sLnBhY3MuMDEgPC0gZ2xtKGlkZW9sb2dpY2FsX3BhY3MgfiBub19jb3JwX3BhY3MgKyBuZXdfbWVtYmVyICsgcGN0X3doaXRlICsgcGN0X2JsYWNrICsgcGN0X2xhdGlubyArIHBjdF9hc2lhbiArIHBjdF9iYWNoZWxvcnMgKyBwY3RfY2xpbnRvbl8xNiArIHBjdF9kZW1faG91c2VfMTYgKyBsb2dfbWVkaWFuX2hoX2luY29tZSArIGxvZ190b3Rfdm90aW5nX2FnZSxcbiAgICAgICAgICAgICAgICAgICAgIGZhbWlseSA9IEdhbW1hKGxpbmsgPSBcImxvZ1wiKSxcbiAgICAgICAgICAgICAgICAgICAgIGRhdGEgPSBhcy5kYXRhLmZyYW1lKHN0YW4uZGF0YSkgJT4lIGZpbHRlcihpZGVvbG9naWNhbF9wYWNzICE9IDApICU+JSBtdXRhdGVfYXQoLnZhcnMgPSB2YXJzKHBjdF93aGl0ZTpsb2dfbWVkaWFuX2hoX2luY29tZSksIC5mdW5zID0gY2VudGVyKSlcblxuIyMgQmF5ZXNpYW5cbmlkZW9wYWMuZml0IDwtIHNhbXBsaW5nKGlkZW9wYWMubW9kZWwsXG4gICAgICAgICAgICAgICAgICAgICAgICBjaGFpbnMgPSA0LFxuICAgICAgICAgICAgICAgICAgICAgICAgY29yZXMgPSA0LFxuICAgICAgICAgICAgICAgICAgICAgICAgaXRlciA9IDEwMDAwLFxuICAgICAgICAgICAgICAgICAgICAgICAgd2FybXVwID0gNTAwMCxcbiAgICAgICAgICAgICAgICAgICAgICAgIGRhdGEgPSBzdGFuLmRhdGEpXG5cbnRyYWNlcGxvdChpZGVvcGFjLmZpdCwgXG4gICAgICAgICAgaW5jX3dhcm11cCA9IFRSVUUsIFxuICAgICAgICAgIHBhcnMgPSBjKFwiYVwiLCBcImJfbnBcIiwgXCJiX25ld1wiLCBcImJfd2hpdGVcIiwgXCJiX2JsYWNrXCIsIFwiYl9sYXRpbm9cIiwgXG4gICAgICAgICAgICAgICAgICAgXCJiX2FzaWFuXCIsIFwiYl9iYWNoXCIsIFwiYl9jbGludFwiLCBcImJfaG91c2VcIiwgXCJiX3Zwb3BcIiwgXG4gICAgICAgICAgICAgICAgICAgXCJiX21oaFwiLCBcInNjYWxlXCIpKVxuXG50aWR5KGlkZW9sLnBhY3MuMDEsIGNvbmYuaW50ID0gVFJVRSwgY29uZi5sZXZlbCA9IDAuODksIGRpZ2l0cyA9IDMpXG5wcmVjaXMoaWRlb3BhYy5maXQsIHByb2IgPSAwLjg5LCBkaWdpdHMgPSAzKVxuXG5wb3N0IDwtIHNwcmVhZF9kcmF3cyhpZGVvcGFjLmZpdCwgbXVbaV0pICU+JSBtZWRpYW5faGRpKClcbmdncGxvdChkYXRhID0gZXN0aW1hdGlvbi5kYXRhLCBhZXMoeCA9IGlkZW9sb2dpY2FsX3BhY3MpKSArXG4gIGdlb21fZGVuc2l0eShmaWxsID0gXCJncmF5XCIpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBwb3N0LCBhZXMoeCA9IG11KSwgZmlsbCA9IFwic2t5Ymx1ZVwiLCBhbHBoYSA9IDAuNylcbmBgYCJ9 -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(ideological_pacs, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  drop_na() %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
## Frequentist
ideol.pacs.01 <- glm(ideological_pacs ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + pct_clinton_16 + pct_dem_house_16 + log_median_hh_income + log_tot_voting_age,
                     family = Gamma(link = "log"),
                     data = as.data.frame(stan.data) %>% filter(ideological_pacs != 0) %>% mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center))

## Bayesian
ideopac.fit <- sampling(ideopac.model,
                        chains = 4,
                        cores = 4,
                        iter = 10000,
                        warmup = 5000,
                        data = stan.data)

traceplot(ideopac.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh", "scale"))

tidy(ideol.pacs.01, conf.int = TRUE, conf.level = 0.89, digits = 3)
precis(ideopac.fit, prob = 0.89, digits = 3)

post <- spread_draws(ideopac.fit, mu[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = ideological_pacs)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = mu), fill = "skyblue", alpha = 0.7)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# Labor PAC Models

## Model of Labor PAC Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDEwLjUpLCBleHAoMTMpKSksXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJOb3JtYWxcIikpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBJbnRlcmNlcHRcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSArXG4gIHRoZW1lKGF4aXMudGV4dC54ID0gZWxlbWVudF90ZXh0KGFuZ2xlID0gNDUsIHZqdXN0ID0gMSwgaGp1c3QgPSAxKSlcblxuIyMgU2xvcGUgUGFyYW1ldGVyc1xuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShybm9ybSgxZTQsIGV4cCgwKSwgZXhwKDEyKSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwibm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhsYWJlbHMgPSBkb2xsYXJfZm9ybWF0KCkpIFxuXG4jIyBTY2FsZSBQYXJhbWV0ZXJcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTFcIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDIpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTJcIiksIFxuICAgICAgICAgICAgICAgYWxwaGEgPSAwLjUpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBTY2FsZSBQYXJhbWV0ZXJcIilcblxuIyMgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJ1bmlmKDFlNCwgMCwgMSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcInVuaWZvcm1cIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAyLCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiYmV0YSAyXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gICAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAxLjUsIDEuNSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMS41XCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC43KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXCIpXG5gYGAifQ== -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(10.5), exp(13))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(12))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgbGFib3JfcGFjc1tuXTtcbiAgaW50IG5vX2NvcnBfcGFjc1tuXTtcbiAgaW50IG5ld19tZW1iZXJbbl07XG4gIHJlYWwgcGN0X3doaXRlW25dO1xuICByZWFsIHBjdF9ibGFja1tuXTtcbiAgcmVhbCBwY3RfbGF0aW5vW25dO1xuICByZWFsIHBjdF9hc2lhbltuXTtcbiAgcmVhbCBwY3RfYmFjaGVsb3JzW25dO1xuICByZWFsIHBjdF9jbGludG9uXzE2W25dO1xuICByZWFsIHBjdF9kZW1faG91c2VfMTZbbl07XG4gIHJlYWwgbG9nX21lZGlhbl9oaF9pbmNvbWVbbl07XG4gIHJlYWwgbG9nX3RvdF92b3RpbmdfYWdlW25dO1xufVxuXG5wYXJhbWV0ZXJzIHtcbiAgcmVhbCBwO1xuICByZWFsIHBfbnA7XG4gIHJlYWwgcF9uZXc7XG4gIHJlYWw8bG93ZXIgPSAwPiBhO1xuICByZWFsIGJfbnA7XG4gIHJlYWwgYl9uZXc7XG4gIHJlYWwgYl93aGl0ZTtcbiAgcmVhbCBiX2JsYWNrO1xuICByZWFsIGJfbGF0aW5vO1xuICByZWFsIGJfYXNpYW47XG4gIHJlYWwgYl9iYWNoO1xuICByZWFsIGJfbWhoO1xuICByZWFsIGJfY2xpbnQ7XG4gIHJlYWwgYl9ob3VzZTtcbiAgcmVhbCBiX3Zwb3A7XG4gIHJlYWw8bG93ZXIgPSAwPiBzY2FsZTtcbn1cblxubW9kZWwge1xuICB2ZWN0b3Jbbl0gcHJvYjtcbiAgdmVjdG9yW25dIG11O1xuICBcbiAgZm9yIChpIGluIDE6bikge1xuICAgIC8vIGxpbmVhciBtb2RlbFxuICAgIHByb2JbaV0gPSBwICsgcF9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIHBfbmV3ICogbmV3X21lbWJlcltpXTsgXG4gICAgcHJvYltpXSA9IGludl9sb2dpdChwcm9iW2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgICAvLyBsaW5lYXIgbW9kZWxcbiAgICBtdVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgICBtdVtpXSA9IGV4cChtdVtpXSk7IC8vIGludmVyc2UgbGluayBmdW5jdGlvblxuICB9XG4gIFxuICAvLyBwcmlvcnNcbiAgXG4gIC8vIFByb2JhYmlsaXR5IG1vZGVsXG4gIHAgfiBub3JtYWwoMCwgMSk7XG4gIHBfbnAgfiBub3JtYWwoMCwgMSk7XG4gIHBfbmV3IH4gbm9ybWFsKDAsIDEpO1xuICBcbiAgLy8gR2FtbWEgbW9kZWxcbiAgYSB+IG5vcm1hbCgxMC41LCAxMyk7XG4gIGJfbnAgfiBub3JtYWwoMCwgMTIpO1xuICBiX25ldyB+IG5vcm1hbCgwLCAxMik7XG4gIGJfd2hpdGUgfiBub3JtYWwoMCwgMTIpO1xuICBiX2JsYWNrIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9sYXRpbm8gfiBub3JtYWwoMCwgMTIpO1xuICBiX2FzaWFuIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9iYWNoIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9jbGludCB+IG5vcm1hbCgwLCAxMik7XG4gIGJfaG91c2UgfiBub3JtYWwoMCwgMTIpO1xuICBiX3Zwb3AgfiBub3JtYWwoMCwgMTIpO1xuICBiX21oaCB+IG5vcm1hbCgwLCAxMik7XG4gIHNjYWxlIH4gZXhwb25lbnRpYWwoMik7XG4gIFxuICAvLyBsaWtlbGlob29kXG4gIGZvciAoIGkgaW4gMTpuKSB7XG4gICAgaWYgKGxhYm9yX3BhY3NbaV0gPT0gMClcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMXwgcHJvYikpO1xuICAgIGVsc2VcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMHwgcHJvYikgKyBnYW1tYV9scGRmKGxhYm9yX3BhY3NbaV18IG11W2ldL3NjYWxlLCAxL3NjYWxlKSk7XG4gICAgfS8vaVxufVxuXG5nZW5lcmF0ZWQgcXVhbnRpdGllcyB7XG4gIHZlY3RvcltuXSBtdTtcbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgbXVbaV0gPSBhICsgYl9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIGJfbmV3ICogbmV3X21lbWJlcltpXSArIFxuICAgICAgYl93aGl0ZSAqIHBjdF93aGl0ZVtpXSArIGJfYmxhY2sgKiBwY3RfYmxhY2tbaV0gKyBcbiAgICAgIGJfbGF0aW5vICogcGN0X2xhdGlub1tpXSArIGJfYXNpYW4gKiBwY3RfYXNpYW5baV0gKyBcbiAgICAgIGJfYmFjaCAqIHBjdF9iYWNoZWxvcnNbaV0gKyBiX2NsaW50ICogcGN0X2NsaW50b25fMTZbaV0gKyBcbiAgICAgIGJfaG91c2UgKiBwY3RfZGVtX2hvdXNlXzE2W2ldICsgYl9taGggKiBsb2dfbWVkaWFuX2hoX2luY29tZVtpXSArXG4gICAgICBiX3Zwb3AgKiBsb2dfdG90X3ZvdGluZ19hZ2VbaV07XG4gIG11W2ldID0gZXhwKG11W2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbn1cbmBgYCJ9 -->

```stan
data {
  int<lower = 1> n;
  real labor_pacs[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real p;
  real p_np;
  real p_new;
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  
  // Probability model
  p ~ normal(0, 1);
  p_np ~ normal(0, 1);
  p_new ~ normal(0, 1);
  
  // Gamma model
  a ~ normal(10.5, 13);
  b_np ~ normal(0, 12);
  b_new ~ normal(0, 12);
  b_white ~ normal(0, 12);
  b_black ~ normal(0, 12);
  b_latino ~ normal(0, 12);
  b_asian ~ normal(0, 12);
  b_bach ~ normal(0, 12);
  b_clint ~ normal(0, 12);
  b_house ~ normal(0, 12);
  b_vpop ~ normal(0, 12);
  b_mhh ~ normal(0, 12);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    if (labor_pacs[i] == 0)
      target += (bernoulli_lpmf(1| prob));
    else
      target += (bernoulli_lpmf(0| prob) + gamma_lpdf(labor_pacs[i]| mu[i]/scale, 1/scale));
    }//i
}

generated quantities {
  vector[n] mu;
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KGxhYm9yX3BhY3MsIG5vX2NvcnBfcGFjcywgbmV3X21lbWJlciwgcGN0X3doaXRlLCBwY3RfYmxhY2ssIFxuICAgICAgICAgICAgICAgIHBjdF9sYXRpbm8sIHBjdF9hc2lhbiwgcGN0X2JhY2hlbG9ycywgcGN0X2NsaW50b25fMTYsXG4gICAgICAgICAgICAgICAgcGN0X2RlbV9ob3VzZV8xNiwgbG9nX3RvdF92b3RpbmdfYWdlLCBsb2dfbWVkaWFuX2hoX2luY29tZSkgJT4lXG4gIG11dGF0ZShub19jb3JwX3BhY3MgPSBpZmVsc2Uobm9fY29ycF9wYWNzID09IFwiWUVTXCIsIDEsIDApLFxuICAgICAgICAgbmV3X21lbWJlciA9IGlmZWxzZShuZXdfbWVtYmVyID09IFwiWUVTXCIsIDEsIDApKSAlPiVcbiAgZHJvcF9uYSgpICU+JVxuICBtdXRhdGVfYXQoLnZhcnMgPSB2YXJzKHBjdF93aGl0ZTpsb2dfbWVkaWFuX2hoX2luY29tZSksIC5mdW5zID0gY2VudGVyKSAlPiVcbiAgY29tcG9zZV9kYXRhKClcblxuIyBtb2RlbCBlc3RpbWF0aW9uIC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxuIyMgRnJlcXVlbnRpc3RcbmxhYm9yLnBhY3MuMDEgPC0gZ2xtKGxhYm9yX3BhY3MgfiBub19jb3JwX3BhY3MgKyBuZXdfbWVtYmVyICsgcGN0X3doaXRlICsgcGN0X2JsYWNrICsgcGN0X2xhdGlubyArIHBjdF9hc2lhbiArIHBjdF9iYWNoZWxvcnMgKyBwY3RfY2xpbnRvbl8xNiArIHBjdF9kZW1faG91c2VfMTYgKyBsb2dfbWVkaWFuX2hoX2luY29tZSArIGxvZ190b3Rfdm90aW5nX2FnZSxcbiAgICAgICAgICAgICAgICAgICAgIGZhbWlseSA9IEdhbW1hKGxpbmsgPSBcImxvZ1wiKSxcbiAgICAgICAgICAgICAgICAgICAgIGRhdGEgPSBhcy5kYXRhLmZyYW1lKHN0YW4uZGF0YSkgJT4lIGZpbHRlcihsYWJvcl9wYWNzICE9IDApICU+JSBtdXRhdGVfYXQoLnZhcnMgPSB2YXJzKHBjdF93aGl0ZTpsb2dfbWVkaWFuX2hoX2luY29tZSksIC5mdW5zID0gY2VudGVyKSlcblxuIyMgQmF5ZXNpYW5cbmxhYm9ycGFjLmZpdCA8LSBzYW1wbGluZyhsYWJvcnBhYy5tb2RlbCxcbiAgICAgICAgICAgICAgICAgICAgICAgICBjaGFpbnMgPSA0LFxuICAgICAgICAgICAgICAgICAgICAgICAgIGNvcmVzID0gNCxcbiAgICAgICAgICAgICAgICAgICAgICAgICBpdGVyID0gMTAwMDAsXG4gICAgICAgICAgICAgICAgICAgICAgICAgd2FybXVwID0gNTAwMCxcbiAgICAgICAgICAgICAgICAgICAgICAgICBkYXRhID0gc3Rhbi5kYXRhKVxuXG50cmFjZXBsb3QobGFib3JwYWMuZml0LCBcbiAgICAgICAgICBpbmNfd2FybXVwID0gVFJVRSwgXG4gICAgICAgICAgcGFycyA9IGMoXCJhXCIsIFwiYl9ucFwiLCBcImJfbmV3XCIsIFwiYl93aGl0ZVwiLCBcImJfYmxhY2tcIiwgXCJiX2xhdGlub1wiLCBcbiAgICAgICAgICAgICAgICAgICBcImJfYXNpYW5cIiwgXCJiX2JhY2hcIiwgXCJiX2NsaW50XCIsIFwiYl9ob3VzZVwiLCBcImJfdnBvcFwiLCBcbiAgICAgICAgICAgICAgICAgICBcImJfbWhoXCIsIFwic2NhbGVcIikpXG5cbnRpZHkobGFib3IucGFjcy4wMSwgY29uZi5pbnQgPSBUUlVFLCBjb25mLmxldmVsID0gMC44OSwgZGlnaXRzID0gMylcbnByZWNpcyhsYWJvcnBhYy5maXQsIHByb2IgPSAwLjg5LCBkaWdpdHMgPSAzKVxuXG5wb3N0IDwtIHNwcmVhZF9kcmF3cyhsYWJvcnBhYy5maXQsIG11W2ldKSAlPiUgbWVkaWFuX2hkaSgpXG5nZ3Bsb3QoZGF0YSA9IGVzdGltYXRpb24uZGF0YSwgYWVzKHggPSBsYWJvcl9wYWNzKSkgK1xuICBnZW9tX2RlbnNpdHkoZmlsbCA9IFwiZ3JheVwiKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gcG9zdCwgYWVzKHggPSBtdSksIGZpbGwgPSBcInNreWJsdWVcIiwgYWxwaGEgPSAwLjcpXG5gYGAifQ== -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(labor_pacs, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  drop_na() %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
## Frequentist
labor.pacs.01 <- glm(labor_pacs ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + pct_clinton_16 + pct_dem_house_16 + log_median_hh_income + log_tot_voting_age,
                     family = Gamma(link = "log"),
                     data = as.data.frame(stan.data) %>% filter(labor_pacs != 0) %>% mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center))

## Bayesian
laborpac.fit <- sampling(laborpac.model,
                         chains = 4,
                         cores = 4,
                         iter = 10000,
                         warmup = 5000,
                         data = stan.data)

traceplot(laborpac.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh", "scale"))

tidy(labor.pacs.01, conf.int = TRUE, conf.level = 0.89, digits = 3)
precis(laborpac.fit, prob = 0.89, digits = 3)

post <- spread_draws(laborpac.fit, mu[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = labor_pacs)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = mu), fill = "skyblue", alpha = 0.7)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# Party Models

## Model of Leadership PAC Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDEwLjUpLCBleHAoMTMpKSksXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJOb3JtYWxcIikpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBJbnRlcmNlcHRcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSArXG4gIHRoZW1lKGF4aXMudGV4dC54ID0gZWxlbWVudF90ZXh0KGFuZ2xlID0gNDUsIHZqdXN0ID0gMSwgaGp1c3QgPSAxKSlcblxuIyMgU2xvcGUgUGFyYW1ldGVyc1xuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShybm9ybSgxZTQsIGV4cCgwKSwgZXhwKDEyKSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwibm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhsYWJlbHMgPSBkb2xsYXJfZm9ybWF0KCkpIFxuXG4jIyBTY2FsZSBQYXJhbWV0ZXJcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTFcIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDIpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTJcIiksIFxuICAgICAgICAgICAgICAgYWxwaGEgPSAwLjUpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBTY2FsZSBQYXJhbWV0ZXJcIilcblxuIyMgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJ1bmlmKDFlNCwgMCwgMSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcInVuaWZvcm1cIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAyLCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiYmV0YSAyXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gICAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAxLjUsIDEuNSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMS41XCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC43KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXCIpXG5gYGAifQ== -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(10.5), exp(13))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(12))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgbGVhZGVyc2hpcF9wYWNzW25dO1xuICBpbnQgbm9fY29ycF9wYWNzW25dO1xuICBpbnQgbmV3X21lbWJlcltuXTtcbiAgcmVhbCBwY3Rfd2hpdGVbbl07XG4gIHJlYWwgcGN0X2JsYWNrW25dO1xuICByZWFsIHBjdF9sYXRpbm9bbl07XG4gIHJlYWwgcGN0X2FzaWFuW25dO1xuICByZWFsIHBjdF9iYWNoZWxvcnNbbl07XG4gIHJlYWwgcGN0X2NsaW50b25fMTZbbl07XG4gIHJlYWwgcGN0X2RlbV9ob3VzZV8xNltuXTtcbiAgcmVhbCBsb2dfbWVkaWFuX2hoX2luY29tZVtuXTtcbiAgcmVhbCBsb2dfdG90X3ZvdGluZ19hZ2Vbbl07XG59XG5cbnBhcmFtZXRlcnMge1xuICByZWFsIHA7XG4gIHJlYWwgcF9ucDtcbiAgcmVhbCBwX25ldztcbiAgcmVhbDxsb3dlciA9IDA+IGE7XG4gIHJlYWwgYl9ucDtcbiAgcmVhbCBiX25ldztcbiAgcmVhbCBiX3doaXRlO1xuICByZWFsIGJfYmxhY2s7XG4gIHJlYWwgYl9sYXRpbm87XG4gIHJlYWwgYl9hc2lhbjtcbiAgcmVhbCBiX2JhY2g7XG4gIHJlYWwgYl9taGg7XG4gIHJlYWwgYl9jbGludDtcbiAgcmVhbCBiX2hvdXNlO1xuICByZWFsIGJfdnBvcDtcbiAgcmVhbDxsb3dlciA9IDA+IHNjYWxlO1xufVxuXG5tb2RlbCB7XG4gIHZlY3RvcltuXSBwcm9iO1xuICB2ZWN0b3Jbbl0gbXU7XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgLy8gbGluZWFyIG1vZGVsXG4gICAgcHJvYltpXSA9IHAgKyBwX25wICogbm9fY29ycF9wYWNzW2ldICsgcF9uZXcgKiBuZXdfbWVtYmVyW2ldOyBcbiAgICBwcm9iW2ldID0gaW52X2xvZ2l0KHByb2JbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxuICBcbiAgZm9yIChpIGluIDE6bikge1xuICAgIC8vIGxpbmVhciBtb2RlbFxuICAgIG11W2ldID0gYSArIGJfbnAgKiBub19jb3JwX3BhY3NbaV0gKyBiX25ldyAqIG5ld19tZW1iZXJbaV0gKyBcbiAgICAgIGJfd2hpdGUgKiBwY3Rfd2hpdGVbaV0gKyBiX2JsYWNrICogcGN0X2JsYWNrW2ldICsgXG4gICAgICBiX2xhdGlubyAqIHBjdF9sYXRpbm9baV0gKyBiX2FzaWFuICogcGN0X2FzaWFuW2ldICsgXG4gICAgICBiX2JhY2ggKiBwY3RfYmFjaGVsb3JzW2ldICsgYl9jbGludCAqIHBjdF9jbGludG9uXzE2W2ldICsgXG4gICAgICBiX2hvdXNlICogcGN0X2RlbV9ob3VzZV8xNltpXSArIGJfbWhoICogbG9nX21lZGlhbl9oaF9pbmNvbWVbaV0gK1xuICAgICAgYl92cG9wICogbG9nX3RvdF92b3RpbmdfYWdlW2ldO1xuICAgIG11W2ldID0gZXhwKG11W2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbiAgXG4gIC8vIHByaW9yc1xuICBcbiAgLy8gUHJvYmFiaWxpdHkgbW9kZWxcbiAgcCB+IG5vcm1hbCgwLCAxKTtcbiAgcF9ucCB+IG5vcm1hbCgwLCAxKTtcbiAgcF9uZXcgfiBub3JtYWwoMCwgMSk7XG4gIFxuICAvLyBHYW1tYSBtb2RlbFxuICBhIH4gbm9ybWFsKDEwLjUsIDEzKTtcbiAgYl9ucCB+IG5vcm1hbCgwLCAxMik7XG4gIGJfbmV3IH4gbm9ybWFsKDAsIDEyKTtcbiAgYl93aGl0ZSB+IG5vcm1hbCgwLCAxMik7XG4gIGJfYmxhY2sgfiBub3JtYWwoMCwgMTIpO1xuICBiX2xhdGlubyB+IG5vcm1hbCgwLCAxMik7XG4gIGJfYXNpYW4gfiBub3JtYWwoMCwgMTIpO1xuICBiX2JhY2ggfiBub3JtYWwoMCwgMTIpO1xuICBiX2NsaW50IH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9ob3VzZSB+IG5vcm1hbCgwLCAxMik7XG4gIGJfdnBvcCB+IG5vcm1hbCgwLCAxMik7XG4gIGJfbWhoIH4gbm9ybWFsKDAsIDEyKTtcbiAgc2NhbGUgfiBleHBvbmVudGlhbCgyKTtcbiAgXG4gIC8vIGxpa2VsaWhvb2RcbiAgZm9yICggaSBpbiAxOm4pIHtcbiAgICBpZiAobGVhZGVyc2hpcF9wYWNzW2ldID09IDApXG4gICAgICB0YXJnZXQgKz0gKGJlcm5vdWxsaV9scG1mKDF8IHByb2JbaV0pKTtcbiAgICBlbHNlXG4gICAgICB0YXJnZXQgKz0gKGJlcm5vdWxsaV9scG1mKDB8IHByb2JbaV0pICsgZ2FtbWFfbHBkZihsZWFkZXJzaGlwX3BhY3NbaV18IG11W2ldL3NjYWxlLCAxL3NjYWxlKSk7XG4gICAgfVxufVxuXG5nZW5lcmF0ZWQgcXVhbnRpdGllcyB7XG4gIHZlY3RvcltuXSBwcm9iO1xuICB2ZWN0b3Jbbl0gbXU7XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgLy8gbGluZWFyIG1vZGVsXG4gICAgcHJvYltpXSA9IHAgKyBwX25wICogbm9fY29ycF9wYWNzW2ldICsgcF9uZXcgKiBuZXdfbWVtYmVyW2ldOyBcbiAgICBwcm9iW2ldID0gaW52X2xvZ2l0KHByb2JbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxuICBcbiAgZm9yIChpIGluIDE6bikge1xuICBtdVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxufVxuYGBgIn0= -->

```stan
data {
  int<lower = 1> n;
  real leadership_pacs[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real p;
  real p_np;
  real p_new;
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  
  // Probability model
  p ~ normal(0, 1);
  p_np ~ normal(0, 1);
  p_new ~ normal(0, 1);
  
  // Gamma model
  a ~ normal(10.5, 13);
  b_np ~ normal(0, 12);
  b_new ~ normal(0, 12);
  b_white ~ normal(0, 12);
  b_black ~ normal(0, 12);
  b_latino ~ normal(0, 12);
  b_asian ~ normal(0, 12);
  b_bach ~ normal(0, 12);
  b_clint ~ normal(0, 12);
  b_house ~ normal(0, 12);
  b_vpop ~ normal(0, 12);
  b_mhh ~ normal(0, 12);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    if (leadership_pacs[i] == 0)
      target += (bernoulli_lpmf(1| prob[i]));
    else
      target += (bernoulli_lpmf(0| prob[i]) + gamma_lpdf(leadership_pacs[i]| mu[i]/scale, 1/scale));
    }
}

generated quantities {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KGxlYWRlcnNoaXBfcGFjcywgbm9fY29ycF9wYWNzLCBuZXdfbWVtYmVyLCBwY3Rfd2hpdGUsIHBjdF9ibGFjaywgXG4gICAgICAgICAgICAgICAgcGN0X2xhdGlubywgcGN0X2FzaWFuLCBwY3RfYmFjaGVsb3JzLCBwY3RfY2xpbnRvbl8xNixcbiAgICAgICAgICAgICAgICBwY3RfZGVtX2hvdXNlXzE2LCBsb2dfdG90X3ZvdGluZ19hZ2UsIGxvZ19tZWRpYW5faGhfaW5jb21lKSAlPiVcbiAgbXV0YXRlKG5vX2NvcnBfcGFjcyA9IGlmZWxzZShub19jb3JwX3BhY3MgPT0gXCJZRVNcIiwgMSwgMCksXG4gICAgICAgICBuZXdfbWVtYmVyID0gaWZlbHNlKG5ld19tZW1iZXIgPT0gXCJZRVNcIiwgMSwgMCkpICU+JVxuICBkcm9wX25hKCkgJT4lXG4gIG11dGF0ZV9hdCgudmFycyA9IHZhcnMocGN0X3doaXRlOmxvZ19tZWRpYW5faGhfaW5jb21lKSwgLmZ1bnMgPSBjZW50ZXIpICU+JVxuICBjb21wb3NlX2RhdGEoKVxuXG4jIG1vZGVsIGVzdGltYXRpb24gLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXG4jIyBGcmVxdWVudGlzdFxubGRyLnBhY3MuMDEgPC0gZ2xtKGxlYWRlcnNoaXBfcGFjcyB+IG5vX2NvcnBfcGFjcyArIG5ld19tZW1iZXIgKyBwY3Rfd2hpdGUgKyBwY3RfYmxhY2sgKyBwY3RfbGF0aW5vICsgcGN0X2FzaWFuICsgcGN0X2JhY2hlbG9ycyArIHBjdF9jbGludG9uXzE2ICsgcGN0X2RlbV9ob3VzZV8xNiArIGxvZ19tZWRpYW5faGhfaW5jb21lICsgbG9nX3RvdF92b3RpbmdfYWdlLFxuICAgICAgICAgICAgICAgICAgIGZhbWlseSA9IEdhbW1hKGxpbmsgPSBcImxvZ1wiKSxcbiAgICAgICAgICAgICAgICAgICBkYXRhID0gYXMuZGF0YS5mcmFtZShzdGFuLmRhdGEpICU+JSBmaWx0ZXIobGVhZGVyc2hpcF9wYWNzICE9IDApICU+JSBtdXRhdGVfYXQoLnZhcnMgPSB2YXJzKHBjdF93aGl0ZTpsb2dfbWVkaWFuX2hoX2luY29tZSksIC5mdW5zID0gY2VudGVyKSlcblxuIyMgQmF5ZXNpYW5cbmxkcnBhYy5maXQgPC0gc2FtcGxpbmcobGRycGFjLm1vZGVsLFxuICAgICAgICAgICAgICAgICAgICAgICBjaGFpbnMgPSA0LFxuICAgICAgICAgICAgICAgICAgICAgICBjb3JlcyA9IDQsXG4gICAgICAgICAgICAgICAgICAgICAgIGl0ZXIgPSAxMDAwMCxcbiAgICAgICAgICAgICAgICAgICAgICAgd2FybXVwID0gNTAwMCxcbiAgICAgICAgICAgICAgICAgICAgICAgZGF0YSA9IHN0YW4uZGF0YSlcblxudHJhY2VwbG90KGxkcnBhYy5maXQsIFxuICAgICAgICAgIGluY193YXJtdXAgPSBUUlVFLCBcbiAgICAgICAgICBwYXJzID0gYyhcImFcIiwgXCJiX25wXCIsIFwiYl9uZXdcIiwgXCJiX3doaXRlXCIsIFwiYl9ibGFja1wiLCBcImJfbGF0aW5vXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9hc2lhblwiLCBcImJfYmFjaFwiLCBcImJfY2xpbnRcIiwgXCJiX2hvdXNlXCIsIFwiYl92cG9wXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9taGhcIiwgXCJzY2FsZVwiKSlcblxudGlkeShsZHIucGFjcy4wMSwgY29uZi5pbnQgPSBUUlVFLCBjb25mLmxldmVsID0gMC44OSwgZGlnaXRzID0gMylcbnByZWNpcyhsZHJwYWMuZml0LCBwcm9iID0gMC44OSwgZGlnaXRzID0gMylcblxucG9zdCA8LSBzcHJlYWRfZHJhd3MobGRycGFjLmZpdCwgbXVbaV0pICU+JSBtZWRpYW5faGRpKClcbmdncGxvdChkYXRhID0gZXN0aW1hdGlvbi5kYXRhLCBhZXMoeCA9IGxlYWRlcnNoaXBfcGFjcykpICtcbiAgZ2VvbV9kZW5zaXR5KGZpbGwgPSBcImdyYXlcIikgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IHBvc3QsIGFlcyh4ID0gbXUpLCBmaWxsID0gXCJza3libHVlXCIsIGFscGhhID0gMC43KVxuYGBgIn0= -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(leadership_pacs, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  drop_na() %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
## Frequentist
ldr.pacs.01 <- glm(leadership_pacs ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + pct_clinton_16 + pct_dem_house_16 + log_median_hh_income + log_tot_voting_age,
                   family = Gamma(link = "log"),
                   data = as.data.frame(stan.data) %>% filter(leadership_pacs != 0) %>% mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center))

## Bayesian
ldrpac.fit <- sampling(ldrpac.model,
                       chains = 4,
                       cores = 4,
                       iter = 10000,
                       warmup = 5000,
                       data = stan.data)

traceplot(ldrpac.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh", "scale"))

tidy(ldr.pacs.01, conf.int = TRUE, conf.level = 0.89, digits = 3)
precis(ldrpac.fit, prob = 0.89, digits = 3)

post <- spread_draws(ldrpac.fit, mu[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = leadership_pacs)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = mu), fill = "skyblue", alpha = 0.7)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Model of Committee PAC Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDEwLjUpLCBleHAoMTMpKSksXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJOb3JtYWxcIikpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBJbnRlcmNlcHRcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSArXG4gIHRoZW1lKGF4aXMudGV4dC54ID0gZWxlbWVudF90ZXh0KGFuZ2xlID0gNDUsIHZqdXN0ID0gMSwgaGp1c3QgPSAxKSlcblxuIyMgU2xvcGUgUGFyYW1ldGVyc1xuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShybm9ybSgxZTQsIGV4cCgwKSwgZXhwKDEyKSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwibm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhsYWJlbHMgPSBkb2xsYXJfZm9ybWF0KCkpIFxuXG4jIyBTY2FsZSBQYXJhbWV0ZXJcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTFcIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDIpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTJcIiksIFxuICAgICAgICAgICAgICAgYWxwaGEgPSAwLjUpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBTY2FsZSBQYXJhbWV0ZXJcIilcblxuIyMgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJ1bmlmKDFlNCwgMCwgMSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcInVuaWZvcm1cIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAyLCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiYmV0YSAyXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gICAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAxLjUsIDEuNSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMS41XCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC43KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXCIpXG5gYGAifQ== -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(10.5), exp(13))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(12))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgZGlyX3BhY19jYW5kX2NvbW1pdHRlZVtuXTtcbiAgaW50IG5vX2NvcnBfcGFjc1tuXTtcbiAgaW50IG5ld19tZW1iZXJbbl07XG4gIHJlYWwgcGN0X3doaXRlW25dO1xuICByZWFsIHBjdF9ibGFja1tuXTtcbiAgcmVhbCBwY3RfbGF0aW5vW25dO1xuICByZWFsIHBjdF9hc2lhbltuXTtcbiAgcmVhbCBwY3RfYmFjaGVsb3JzW25dO1xuICByZWFsIHBjdF9jbGludG9uXzE2W25dO1xuICByZWFsIHBjdF9kZW1faG91c2VfMTZbbl07XG4gIHJlYWwgbG9nX21lZGlhbl9oaF9pbmNvbWVbbl07XG4gIHJlYWwgbG9nX3RvdF92b3RpbmdfYWdlW25dO1xufVxuXG5wYXJhbWV0ZXJzIHtcbiAgcmVhbCBwO1xuICByZWFsIHBfbnA7XG4gIHJlYWwgcF9uZXc7XG4gIHJlYWw8bG93ZXIgPSAwPiBhO1xuICByZWFsIGJfbnA7XG4gIHJlYWwgYl9uZXc7XG4gIHJlYWwgYl93aGl0ZTtcbiAgcmVhbCBiX2JsYWNrO1xuICByZWFsIGJfbGF0aW5vO1xuICByZWFsIGJfYXNpYW47XG4gIHJlYWwgYl9iYWNoO1xuICByZWFsIGJfbWhoO1xuICByZWFsIGJfY2xpbnQ7XG4gIHJlYWwgYl9ob3VzZTtcbiAgcmVhbCBiX3Zwb3A7XG4gIHJlYWw8bG93ZXIgPSAwPiBzY2FsZTtcbn1cblxubW9kZWwge1xuICB2ZWN0b3Jbbl0gcHJvYjtcbiAgdmVjdG9yW25dIG11O1xuICBcbiAgZm9yIChpIGluIDE6bikge1xuICAgIC8vIGxpbmVhciBtb2RlbFxuICAgIHByb2JbaV0gPSBwICsgcF9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIHBfbmV3ICogbmV3X21lbWJlcltpXTsgXG4gICAgcHJvYltpXSA9IGludl9sb2dpdChwcm9iW2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgICAvLyBsaW5lYXIgbW9kZWxcbiAgICBtdVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgICBtdVtpXSA9IGV4cChtdVtpXSk7IC8vIGludmVyc2UgbGluayBmdW5jdGlvblxuICB9XG4gIFxuICAvLyBwcmlvcnNcbiAgXG4gIC8vIEJpbm9taWFsIG1vZGVsXG4gIHAgfiBub3JtYWwoMCwgMSk7XG4gIHBfbnAgfiBub3JtYWwoMCwgMSk7XG4gIHBfbmV3IH4gbm9ybWFsKDAsIDEpO1xuICBcbiAgLy8gR2FtbWEgbW9kZWxcbiAgYSB+IG5vcm1hbCgxMC41LCAxMyk7XG4gIGJfbnAgfiBub3JtYWwoMCwgMTIpO1xuICBiX25ldyB+IG5vcm1hbCgwLCAxMik7XG4gIGJfd2hpdGUgfiBub3JtYWwoMCwgMTIpO1xuICBiX2JsYWNrIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9sYXRpbm8gfiBub3JtYWwoMCwgMTIpO1xuICBiX2FzaWFuIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9iYWNoIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9jbGludCB+IG5vcm1hbCgwLCAxMik7XG4gIGJfaG91c2UgfiBub3JtYWwoMCwgMTIpO1xuICBiX3Zwb3AgfiBub3JtYWwoMCwgMTIpO1xuICBiX21oaCB+IG5vcm1hbCgwLCAxMik7XG4gIHNjYWxlIH4gZXhwb25lbnRpYWwoMik7XG4gIFxuICAvLyBsaWtlbGlob29kXG4gIGZvciAoIGkgaW4gMTpuKSB7XG4gICAgaWYgKGRpcl9wYWNfY2FuZF9jb21taXR0ZWVbaV0gPT0gMClcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMXwgcHJvYltpXSkpO1xuICAgIGVsc2VcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMHwgcHJvYltpXSkgKyBnYW1tYV9scGRmKGRpcl9wYWNfY2FuZF9jb21taXR0ZWVbaV18IG11W2ldL3NjYWxlLCAxL3NjYWxlKSk7XG4gICAgfS8vaVxufVxuXG5nZW5lcmF0ZWQgcXVhbnRpdGllcyB7XG4gIHZlY3RvcltuXSBwcm9iO1xuICB2ZWN0b3Jbbl0gbXU7XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgLy8gbGluZWFyIG1vZGVsXG4gICAgcHJvYltpXSA9IHAgKyBwX25wICogbm9fY29ycF9wYWNzW2ldICsgcF9uZXcgKiBuZXdfbWVtYmVyW2ldOyBcbiAgICBwcm9iW2ldID0gaW52X2xvZ2l0KHByb2JbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxuICBcbiAgZm9yIChpIGluIDE6bikge1xuICBtdVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxufVxuYGBgIn0= -->

```stan
data {
  int<lower = 1> n;
  real dir_pac_cand_committee[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real p;
  real p_np;
  real p_new;
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  
  // Binomial model
  p ~ normal(0, 1);
  p_np ~ normal(0, 1);
  p_new ~ normal(0, 1);
  
  // Gamma model
  a ~ normal(10.5, 13);
  b_np ~ normal(0, 12);
  b_new ~ normal(0, 12);
  b_white ~ normal(0, 12);
  b_black ~ normal(0, 12);
  b_latino ~ normal(0, 12);
  b_asian ~ normal(0, 12);
  b_bach ~ normal(0, 12);
  b_clint ~ normal(0, 12);
  b_house ~ normal(0, 12);
  b_vpop ~ normal(0, 12);
  b_mhh ~ normal(0, 12);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    if (dir_pac_cand_committee[i] == 0)
      target += (bernoulli_lpmf(1| prob[i]));
    else
      target += (bernoulli_lpmf(0| prob[i]) + gamma_lpdf(dir_pac_cand_committee[i]| mu[i]/scale, 1/scale));
    }//i
}

generated quantities {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KGRpcl9wYWNfY2FuZF9jb21taXR0ZWUsIG5vX2NvcnBfcGFjcywgbmV3X21lbWJlciwgcGN0X3doaXRlLCBwY3RfYmxhY2ssIFxuICAgICAgICAgICAgICAgIHBjdF9sYXRpbm8sIHBjdF9hc2lhbiwgcGN0X2JhY2hlbG9ycywgcGN0X2NsaW50b25fMTYsXG4gICAgICAgICAgICAgICAgcGN0X2RlbV9ob3VzZV8xNiwgbG9nX3RvdF92b3RpbmdfYWdlLCBsb2dfbWVkaWFuX2hoX2luY29tZSkgJT4lXG4gIG11dGF0ZShub19jb3JwX3BhY3MgPSBpZmVsc2Uobm9fY29ycF9wYWNzID09IFwiWUVTXCIsIDEsIDApLFxuICAgICAgICAgbmV3X21lbWJlciA9IGlmZWxzZShuZXdfbWVtYmVyID09IFwiWUVTXCIsIDEsIDApKSAlPiVcbiAgZHJvcF9uYSgpICU+JVxuICBtdXRhdGVfYXQoLnZhcnMgPSB2YXJzKHBjdF93aGl0ZTpsb2dfbWVkaWFuX2hoX2luY29tZSksIC5mdW5zID0gY2VudGVyKSAlPiVcbiAgY29tcG9zZV9kYXRhKClcblxuIyBtb2RlbCBlc3RpbWF0aW9uIC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLVxuIyMgRnJlcXVlbnRpc3RcbmNtdGUucGFjcy4wMSA8LSBnbG0oZGlyX3BhY19jYW5kX2NvbW1pdHRlZSB+IG5vX2NvcnBfcGFjcyArIG5ld19tZW1iZXIgKyBwY3Rfd2hpdGUgKyBwY3RfYmxhY2sgKyBwY3RfbGF0aW5vICsgcGN0X2FzaWFuICsgcGN0X2JhY2hlbG9ycyArIHBjdF9jbGludG9uXzE2ICsgcGN0X2RlbV9ob3VzZV8xNiArIGxvZ19tZWRpYW5faGhfaW5jb21lICsgbG9nX3RvdF92b3RpbmdfYWdlLFxuICAgICAgICAgICAgICAgICAgICAgZmFtaWx5ID0gR2FtbWEobGluayA9IFwibG9nXCIpLFxuICAgICAgICAgICAgICAgICAgICAgZGF0YSA9IGFzLmRhdGEuZnJhbWUoc3Rhbi5kYXRhKSAlPiUgZmlsdGVyKGRpcl9wYWNfY2FuZF9jb21taXR0ZWUgIT0gMCkgJT4lIG11dGF0ZV9hdCgudmFycyA9IHZhcnMocGN0X3doaXRlOmxvZ19tZWRpYW5faGhfaW5jb21lKSwgLmZ1bnMgPSBjZW50ZXIpKVxuXG4jIyBCYXllc2lhblxuY210cGFjLmZpdCA8LSBzYW1wbGluZyhjbXRwYWMubW9kZWwsXG4gICAgICAgICAgICAgICAgICAgICAgIGNoYWlucyA9IDQsXG4gICAgICAgICAgICAgICAgICAgICAgIGNvcmVzID0gNCxcbiAgICAgICAgICAgICAgICAgICAgICAgaXRlciA9IDEwMDAwLFxuICAgICAgICAgICAgICAgICAgICAgICB3YXJtdXAgPSA1MDAwLFxuICAgICAgICAgICAgICAgICAgICAgICBkYXRhID0gc3Rhbi5kYXRhKVxuXG50cmFjZXBsb3QoY210cGFjLmZpdCwgXG4gICAgICAgICAgaW5jX3dhcm11cCA9IFRSVUUsIFxuICAgICAgICAgIHBhcnMgPSBjKFwiYVwiLCBcImJfbnBcIiwgXCJiX25ld1wiLCBcImJfd2hpdGVcIiwgXCJiX2JsYWNrXCIsIFwiYl9sYXRpbm9cIiwgXG4gICAgICAgICAgICAgICAgICAgXCJiX2FzaWFuXCIsIFwiYl9iYWNoXCIsIFwiYl9jbGludFwiLCBcImJfaG91c2VcIiwgXCJiX3Zwb3BcIiwgXG4gICAgICAgICAgICAgICAgICAgXCJiX21oaFwiLCBcInNjYWxlXCIpKVxuXG50aWR5KGNtdGUucGFjcy4wMSwgY29uZi5pbnQgPSBUUlVFLCBjb25mLmxldmVsID0gMC44OSwgZGlnaXRzID0gMylcbnByZWNpcyhjbXRwYWMuZml0LCBwcm9iID0gMC44OSwgZGlnaXRzID0gMylcblxucG9zdCA8LSBzcHJlYWRfZHJhd3MoY210cGFjLmZpdCwgbXVbaV0pICU+JSBtZWRpYW5faGRpKClcbmdncGxvdChkYXRhID0gZXN0aW1hdGlvbi5kYXRhLCBhZXMoeCA9IGRpcl9wYWNfY2FuZF9jb21taXR0ZWUpKSArXG4gIGdlb21fZGVuc2l0eShmaWxsID0gXCJncmF5XCIpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBwb3N0LCBhZXMoeCA9IG11KSwgZmlsbCA9IFwic2t5Ymx1ZVwiLCBhbHBoYSA9IDAuNylcbmBgYCJ9 -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(dir_pac_cand_committee, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  drop_na() %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
## Frequentist
cmte.pacs.01 <- glm(dir_pac_cand_committee ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + pct_clinton_16 + pct_dem_house_16 + log_median_hh_income + log_tot_voting_age,
                     family = Gamma(link = "log"),
                     data = as.data.frame(stan.data) %>% filter(dir_pac_cand_committee != 0) %>% mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center))

## Bayesian
cmtpac.fit <- sampling(cmtpac.model,
                       chains = 4,
                       cores = 4,
                       iter = 10000,
                       warmup = 5000,
                       data = stan.data)

traceplot(cmtpac.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh", "scale"))

tidy(cmte.pacs.01, conf.int = TRUE, conf.level = 0.89, digits = 3)
precis(cmtpac.fit, prob = 0.89, digits = 3)

post <- spread_draws(cmtpac.fit, mu[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = dir_pac_cand_committee)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = mu), fill = "skyblue", alpha = 0.7)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Model of Direct Party Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDEyKSwgZXhwKDE0KSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiTm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgSW50ZXJjZXB0XCIpICtcbiAgc2NhbGVfeF9jb250aW51b3VzKGxhYmVscyA9IGRvbGxhcl9mb3JtYXQoKSkgK1xuICB0aGVtZShheGlzLnRleHQueCA9IGVsZW1lbnRfdGV4dChhbmdsZSA9IDQ1LCB2anVzdCA9IDEsIGhqdXN0ID0gMSkpXG5cbiMjIFNsb3BlIFBhcmFtZXRlcnNcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocm5vcm0oMWU0LCBleHAoMCksIGV4cCgxMykpKSxcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcIm5vcm1hbFwiKSkgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFNsb3BlIFBhcmFtZXRlcnNcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSBcblxuIyMgU2NhbGUgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAxKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0xXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0yXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2NhbGUgUGFyYW1ldGVyXCIpXG5cbiMjIFByb2JhYmlsaXR5IFBhcmFtZXRlclxuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShydW5pZigxZTQsIDAsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJ1bmlmb3JtXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMiwgMikpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMlwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNSkgK1xuICAgIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMS41LCAxLjUpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJiZXRhIDEuNVwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNykgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFByb2JhYmlsaXR5IFBhcmFtZXRlclwiKVxuYGBgIn0= -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(12), exp(14))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(13))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Models


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgZGlyX3BhY19wYXJ0eV9jb21taXR0ZWVbbl07XG4gIGludCBub19jb3JwX3BhY3Nbbl07XG4gIGludCBuZXdfbWVtYmVyW25dO1xuICByZWFsIHBjdF93aGl0ZVtuXTtcbiAgcmVhbCBwY3RfYmxhY2tbbl07XG4gIHJlYWwgcGN0X2xhdGlub1tuXTtcbiAgcmVhbCBwY3RfYXNpYW5bbl07XG4gIHJlYWwgcGN0X2JhY2hlbG9yc1tuXTtcbiAgcmVhbCBwY3RfY2xpbnRvbl8xNltuXTtcbiAgcmVhbCBwY3RfZGVtX2hvdXNlXzE2W25dO1xuICByZWFsIGxvZ19tZWRpYW5faGhfaW5jb21lW25dO1xuICByZWFsIGxvZ190b3Rfdm90aW5nX2FnZVtuXTtcbn1cblxucGFyYW1ldGVycyB7XG4gIHJlYWwgcDtcbiAgcmVhbCBwX25wO1xuICByZWFsIHBfbmV3O1xuICByZWFsPGxvd2VyID0gMD4gYTtcbiAgcmVhbCBiX25wO1xuICByZWFsIGJfbmV3O1xuICByZWFsIGJfd2hpdGU7XG4gIHJlYWwgYl9ibGFjaztcbiAgcmVhbCBiX2xhdGlubztcbiAgcmVhbCBiX2FzaWFuO1xuICByZWFsIGJfYmFjaDtcbiAgcmVhbCBiX21oaDtcbiAgcmVhbCBiX2NsaW50O1xuICByZWFsIGJfaG91c2U7XG4gIHJlYWwgYl92cG9wO1xuICByZWFsPGxvd2VyID0gMD4gc2NhbGU7XG59XG5cbm1vZGVsIHtcbiAgdmVjdG9yW25dIHByb2I7XG4gIHZlY3RvcltuXSBtdTtcbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgICAvLyBsaW5lYXIgbW9kZWxcbiAgICBwcm9iW2ldID0gcCArIHBfbnAgKiBub19jb3JwX3BhY3NbaV0gKyBwX25ldyAqIG5ld19tZW1iZXJbaV07IFxuICAgIHByb2JbaV0gPSBpbnZfbG9naXQocHJvYltpXSk7IC8vIGludmVyc2UgbGluayBmdW5jdGlvblxuICB9XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgLy8gbGluZWFyIG1vZGVsXG4gICAgbXVbaV0gPSBhICsgYl9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIGJfbmV3ICogbmV3X21lbWJlcltpXSArIFxuICAgICAgYl93aGl0ZSAqIHBjdF93aGl0ZVtpXSArIGJfYmxhY2sgKiBwY3RfYmxhY2tbaV0gKyBcbiAgICAgIGJfbGF0aW5vICogcGN0X2xhdGlub1tpXSArIGJfYXNpYW4gKiBwY3RfYXNpYW5baV0gKyBcbiAgICAgIGJfYmFjaCAqIHBjdF9iYWNoZWxvcnNbaV0gKyBiX2NsaW50ICogcGN0X2NsaW50b25fMTZbaV0gKyBcbiAgICAgIGJfaG91c2UgKiBwY3RfZGVtX2hvdXNlXzE2W2ldICsgYl9taGggKiBsb2dfbWVkaWFuX2hoX2luY29tZVtpXSArXG4gICAgICBiX3Zwb3AgKiBsb2dfdG90X3ZvdGluZ19hZ2VbaV07XG4gICAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxuICBcbiAgLy8gcHJpb3JzXG4gIFxuICAvLyBCaW5vbWlhbCBtb2RlbFxuICBwIH4gbm9ybWFsKDAsIDEpO1xuICBwX25wIH4gbm9ybWFsKDAsIDEpO1xuICBwX25ldyB+IG5vcm1hbCgwLCAxKTtcbiAgXG4gIC8vIEdhbW1hIG1vZGVsXG4gIGEgfiBub3JtYWwoMTIsIDE0KTtcbiAgYl9ucCB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfbmV3IH4gbm9ybWFsKDAsIDEzKTtcbiAgYl93aGl0ZSB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfYmxhY2sgfiBub3JtYWwoMCwgMTMpO1xuICBiX2xhdGlubyB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfYXNpYW4gfiBub3JtYWwoMCwgMTMpO1xuICBiX2JhY2ggfiBub3JtYWwoMCwgMTMpO1xuICBiX2NsaW50IH4gbm9ybWFsKDAsIDEzKTtcbiAgYl9ob3VzZSB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfdnBvcCB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfbWhoIH4gbm9ybWFsKDAsIDEzKTtcbiAgc2NhbGUgfiBleHBvbmVudGlhbCgyKTtcbiAgXG4gIC8vIGxpa2VsaWhvb2RcbiAgZm9yICggaSBpbiAxOm4pIHtcbiAgICBpZiAoZGlyX3BhY19wYXJ0eV9jb21taXR0ZWVbaV0gPT0gMClcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMXwgcHJvYltpXSkpO1xuICAgIGVsc2VcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMHwgcHJvYltpXSkgKyBnYW1tYV9scGRmKGRpcl9wYWNfcGFydHlfY29tbWl0dGVlW2ldfCBtdVtpXS9zY2FsZSwgMS9zY2FsZSkpO1xuICAgIH0vL2lcbn1cblxuZ2VuZXJhdGVkIHF1YW50aXRpZXMge1xuICB2ZWN0b3Jbbl0gcHJvYjtcbiAgdmVjdG9yW25dIG11O1xuICBcbiAgZm9yIChpIGluIDE6bikge1xuICAgIC8vIGxpbmVhciBtb2RlbFxuICAgIHByb2JbaV0gPSBwICsgcF9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIHBfbmV3ICogbmV3X21lbWJlcltpXTsgXG4gICAgcHJvYltpXSA9IGludl9sb2dpdChwcm9iW2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgbXVbaV0gPSBhICsgYl9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIGJfbmV3ICogbmV3X21lbWJlcltpXSArIFxuICAgICAgYl93aGl0ZSAqIHBjdF93aGl0ZVtpXSArIGJfYmxhY2sgKiBwY3RfYmxhY2tbaV0gKyBcbiAgICAgIGJfbGF0aW5vICogcGN0X2xhdGlub1tpXSArIGJfYXNpYW4gKiBwY3RfYXNpYW5baV0gKyBcbiAgICAgIGJfYmFjaCAqIHBjdF9iYWNoZWxvcnNbaV0gKyBiX2NsaW50ICogcGN0X2NsaW50b25fMTZbaV0gKyBcbiAgICAgIGJfaG91c2UgKiBwY3RfZGVtX2hvdXNlXzE2W2ldICsgYl9taGggKiBsb2dfbWVkaWFuX2hoX2luY29tZVtpXSArXG4gICAgICBiX3Zwb3AgKiBsb2dfdG90X3ZvdGluZ19hZ2VbaV07XG4gIG11W2ldID0gZXhwKG11W2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbn1cbmBgYCJ9 -->

```stan
data {
  int<lower = 1> n;
  real dir_pac_party_committee[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real p;
  real p_np;
  real p_new;
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  
  // Binomial model
  p ~ normal(0, 1);
  p_np ~ normal(0, 1);
  p_new ~ normal(0, 1);
  
  // Gamma model
  a ~ normal(12, 14);
  b_np ~ normal(0, 13);
  b_new ~ normal(0, 13);
  b_white ~ normal(0, 13);
  b_black ~ normal(0, 13);
  b_latino ~ normal(0, 13);
  b_asian ~ normal(0, 13);
  b_bach ~ normal(0, 13);
  b_clint ~ normal(0, 13);
  b_house ~ normal(0, 13);
  b_vpop ~ normal(0, 13);
  b_mhh ~ normal(0, 13);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    if (dir_pac_party_committee[i] == 0)
      target += (bernoulli_lpmf(1| prob[i]));
    else
      target += (bernoulli_lpmf(0| prob[i]) + gamma_lpdf(dir_pac_party_committee[i]| mu[i]/scale, 1/scale));
    }//i
}

generated quantities {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KGRpcl9wYWNfcGFydHlfY29tbWl0dGVlLCBub19jb3JwX3BhY3MsIG5ld19tZW1iZXIsIHBjdF93aGl0ZSwgcGN0X2JsYWNrLCBcbiAgICAgICAgICAgICAgICBwY3RfbGF0aW5vLCBwY3RfYXNpYW4sIHBjdF9iYWNoZWxvcnMsIHBjdF9jbGludG9uXzE2LFxuICAgICAgICAgICAgICAgIHBjdF9kZW1faG91c2VfMTYsIGxvZ190b3Rfdm90aW5nX2FnZSwgbG9nX21lZGlhbl9oaF9pbmNvbWUpICU+JVxuICBtdXRhdGUobm9fY29ycF9wYWNzID0gaWZlbHNlKG5vX2NvcnBfcGFjcyA9PSBcIllFU1wiLCAxLCAwKSxcbiAgICAgICAgIG5ld19tZW1iZXIgPSBpZmVsc2UobmV3X21lbWJlciA9PSBcIllFU1wiLCAxLCAwKSkgJT4lXG4gIGRyb3BfbmEoKSAlPiVcbiAgbXV0YXRlX2F0KC52YXJzID0gdmFycyhwY3Rfd2hpdGU6bG9nX21lZGlhbl9oaF9pbmNvbWUpLCAuZnVucyA9IGNlbnRlcikgJT4lXG4gIGNvbXBvc2VfZGF0YSgpXG5cbiMgbW9kZWwgZXN0aW1hdGlvbiAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cbiMjIEZyZXF1ZW50aXN0XG5wYXJ0eS5jb250cmlicy4wMSA8LSBnbG0oZGlyX3BhY19wYXJ0eV9jb21taXR0ZWUgfiBub19jb3JwX3BhY3MgKyBuZXdfbWVtYmVyICsgcGN0X3doaXRlICsgcGN0X2JsYWNrICsgcGN0X2xhdGlubyArIHBjdF9hc2lhbiArIHBjdF9iYWNoZWxvcnMgKyBwY3RfY2xpbnRvbl8xNiArIHBjdF9kZW1faG91c2VfMTYgKyBsb2dfbWVkaWFuX2hoX2luY29tZSArIGxvZ190b3Rfdm90aW5nX2FnZSxcbiAgICAgICAgICAgICAgICAgICAgIGZhbWlseSA9IEdhbW1hKGxpbmsgPSBcImxvZ1wiKSxcbiAgICAgICAgICAgICAgICAgICAgIGRhdGEgPSBhcy5kYXRhLmZyYW1lKHN0YW4uZGF0YSkgJT4lIGZpbHRlcihkaXJfcGFjX3BhcnR5X2NvbW1pdHRlZSAhPSAwKSAlPiUgbXV0YXRlX2F0KC52YXJzID0gdmFycyhwY3Rfd2hpdGU6bG9nX21lZGlhbl9oaF9pbmNvbWUpLCAuZnVucyA9IGNlbnRlcikpXG5cbiMjIEJheWVzaWFuXG5wcnR5cGFjLmZpdCA8LSBzYW1wbGluZyhwcnR5cGFjLm1vZGVsLFxuICAgICAgICAgICAgICAgICAgICAgICAgY2hhaW5zID0gNCxcbiAgICAgICAgICAgICAgICAgICAgICAgIGNvcmVzID0gNCxcbiAgICAgICAgICAgICAgICAgICAgICAgIGl0ZXIgPSAxMDAwMCxcbiAgICAgICAgICAgICAgICAgICAgICAgIHdhcm11cCA9IDUwMDAsXG4gICAgICAgICAgICAgICAgICAgICAgICBkYXRhID0gc3Rhbi5kYXRhKVxuXG50cmFjZXBsb3QocHJ0eXBhYy5maXQsIFxuICAgICAgICAgIGluY193YXJtdXAgPSBUUlVFLCBcbiAgICAgICAgICBwYXJzID0gYyhcImFcIiwgXCJiX25wXCIsIFwiYl9uZXdcIiwgXCJiX3doaXRlXCIsIFwiYl9ibGFja1wiLCBcImJfbGF0aW5vXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9hc2lhblwiLCBcImJfYmFjaFwiLCBcImJfY2xpbnRcIiwgXCJiX2hvdXNlXCIsIFwiYl92cG9wXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9taGhcIiwgXCJzY2FsZVwiKSlcblxudGlkeShwYXJ0eS5jb250cmlicy4wMSwgY29uZi5pbnQgPSBUUlVFLCBjb25mLmxldmVsID0gMC44OSwgZGlnaXRzID0gMylcbnByZWNpcyhwcnR5cGFjLmZpdCwgcHJvYiA9IDAuODksIGRpZ2l0cyA9IDMpXG5cbnBvc3QgPC0gc3ByZWFkX2RyYXdzKHBydHlwYWMuZml0LCBtdVtpXSkgJT4lIG1lZGlhbl9oZGkoKVxuZ2dwbG90KGRhdGEgPSBlc3RpbWF0aW9uLmRhdGEsIGFlcyh4ID0gZGlyX3BhY19wYXJ0eV9jb21taXR0ZWUpKSArXG4gIGdlb21fZGVuc2l0eShmaWxsID0gXCJncmF5XCIpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBwb3N0LCBhZXMoeCA9IG11KSwgZmlsbCA9IFwic2t5Ymx1ZVwiLCBhbHBoYSA9IDAuNylcbmBgYCJ9 -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(dir_pac_party_committee, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  drop_na() %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
## Frequentist
party.contribs.01 <- glm(dir_pac_party_committee ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + pct_clinton_16 + pct_dem_house_16 + log_median_hh_income + log_tot_voting_age,
                     family = Gamma(link = "log"),
                     data = as.data.frame(stan.data) %>% filter(dir_pac_party_committee != 0) %>% mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center))

## Bayesian
prtypac.fit <- sampling(prtypac.model,
                        chains = 4,
                        cores = 4,
                        iter = 10000,
                        warmup = 5000,
                        data = stan.data)

traceplot(prtypac.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh", "scale"))

tidy(party.contribs.01, conf.int = TRUE, conf.level = 0.89, digits = 3)
precis(prtypac.fit, prob = 0.89, digits = 3)

post <- spread_draws(prtypac.fit, mu[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = dir_pac_party_committee)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = mu), fill = "skyblue", alpha = 0.7)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# Individual Models

## Model of Small Individual Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDEwKSwgZXhwKDE0KSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiTm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgSW50ZXJjZXB0XCIpICtcbiAgc2NhbGVfeF9jb250aW51b3VzKGxhYmVscyA9IGRvbGxhcl9mb3JtYXQoKSkgK1xuICB0aGVtZShheGlzLnRleHQueCA9IGVsZW1lbnRfdGV4dChhbmdsZSA9IDQ1LCB2anVzdCA9IDEsIGhqdXN0ID0gMSkpXG5cbiMjIFNsb3BlIFBhcmFtZXRlcnNcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocm5vcm0oMWU0LCBleHAoMCksIGV4cCgxMykpKSxcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcIm5vcm1hbFwiKSkgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFNsb3BlIFBhcmFtZXRlcnNcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSBcblxuIyMgU2NhbGUgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAxKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0xXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0yXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2NhbGUgUGFyYW1ldGVyXCIpXG5cbiMjIFByb2JhYmlsaXR5IFBhcmFtZXRlclxuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShydW5pZigxZTQsIDAsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJ1bmlmb3JtXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMiwgMikpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMlwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNSkgK1xuICAgIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMS41LCAxLjUpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJiZXRhIDEuNVwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNykgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFByb2JhYmlsaXR5IFBhcmFtZXRlclwiKVxuYGBgIn0= -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(10), exp(14))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(13))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgc21hbGxfaW5kaXZbbl07XG4gIGludCBub19jb3JwX3BhY3Nbbl07XG4gIGludCBuZXdfbWVtYmVyW25dO1xuICByZWFsIHBjdF93aGl0ZVtuXTtcbiAgcmVhbCBwY3RfYmxhY2tbbl07XG4gIHJlYWwgcGN0X2xhdGlub1tuXTtcbiAgcmVhbCBwY3RfYXNpYW5bbl07XG4gIHJlYWwgcGN0X2JhY2hlbG9yc1tuXTtcbiAgcmVhbCBwY3RfY2xpbnRvbl8xNltuXTtcbiAgcmVhbCBwY3RfZGVtX2hvdXNlXzE2W25dO1xuICByZWFsIGxvZ19tZWRpYW5faGhfaW5jb21lW25dO1xuICByZWFsIGxvZ190b3Rfdm90aW5nX2FnZVtuXTtcbn1cblxucGFyYW1ldGVycyB7XG4gIHJlYWw8bG93ZXIgPSAwPiBhO1xuICByZWFsIGJfbnA7XG4gIHJlYWwgYl9uZXc7XG4gIHJlYWwgYl93aGl0ZTtcbiAgcmVhbCBiX2JsYWNrO1xuICByZWFsIGJfbGF0aW5vO1xuICByZWFsIGJfYXNpYW47XG4gIHJlYWwgYl9iYWNoO1xuICByZWFsIGJfbWhoO1xuICByZWFsIGJfY2xpbnQ7XG4gIHJlYWwgYl9ob3VzZTtcbiAgcmVhbCBiX3Zwb3A7XG4gIHJlYWw8bG93ZXIgPSAwPiBzY2FsZTtcbn1cblxubW9kZWwge1xuICB2ZWN0b3Jbbl0gbXU7XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgLy8gbGluZWFyIG1vZGVsXG4gICAgbXVbaV0gPSBhICsgYl9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIGJfbmV3ICogbmV3X21lbWJlcltpXSArIFxuICAgICAgYl93aGl0ZSAqIHBjdF93aGl0ZVtpXSArIGJfYmxhY2sgKiBwY3RfYmxhY2tbaV0gKyBcbiAgICAgIGJfbGF0aW5vICogcGN0X2xhdGlub1tpXSArIGJfYXNpYW4gKiBwY3RfYXNpYW5baV0gKyBcbiAgICAgIGJfYmFjaCAqIHBjdF9iYWNoZWxvcnNbaV0gKyBiX2NsaW50ICogcGN0X2NsaW50b25fMTZbaV0gKyBcbiAgICAgIGJfaG91c2UgKiBwY3RfZGVtX2hvdXNlXzE2W2ldICsgYl9taGggKiBsb2dfbWVkaWFuX2hoX2luY29tZVtpXSArXG4gICAgICBiX3Zwb3AgKiBsb2dfdG90X3ZvdGluZ19hZ2VbaV07XG4gICAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxuICBcbiAgLy8gcHJpb3JzXG4gIC8vIEdhbW1hIG1vZGVsXG4gIGEgfiBub3JtYWwoMTAsIDE0KTtcbiAgYl9ucCB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfbmV3IH4gbm9ybWFsKDAsIDEzKTtcbiAgYl93aGl0ZSB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfYmxhY2sgfiBub3JtYWwoMCwgMTMpO1xuICBiX2xhdGlubyB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfYXNpYW4gfiBub3JtYWwoMCwgMTMpO1xuICBiX2JhY2ggfiBub3JtYWwoMCwgMTMpO1xuICBiX2NsaW50IH4gbm9ybWFsKDAsIDEzKTtcbiAgYl9ob3VzZSB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfdnBvcCB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfbWhoIH4gbm9ybWFsKDAsIDEzKTtcbiAgc2NhbGUgfiBleHBvbmVudGlhbCgyKTtcbiAgXG4gIC8vIGxpa2VsaWhvb2RcbiAgZm9yICggaSBpbiAxOm4pIHtcbiAgICBzbWFsbF9pbmRpdltpXSB+IGdhbW1hKG11W2ldL3NjYWxlLCAxL3NjYWxlKTtcbiAgICB9XG59XG5cbmdlbmVyYXRlZCBxdWFudGl0aWVzIHtcbiAgdmVjdG9yW25dIG11O1xuICBcbiAgZm9yIChpIGluIDE6bikge1xuICBtdVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxufVxuYGBgIn0= -->

```stan
data {
  int<lower = 1> n;
  real small_indiv[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  // Gamma model
  a ~ normal(10, 14);
  b_np ~ normal(0, 13);
  b_new ~ normal(0, 13);
  b_white ~ normal(0, 13);
  b_black ~ normal(0, 13);
  b_latino ~ normal(0, 13);
  b_asian ~ normal(0, 13);
  b_bach ~ normal(0, 13);
  b_clint ~ normal(0, 13);
  b_house ~ normal(0, 13);
  b_vpop ~ normal(0, 13);
  b_mhh ~ normal(0, 13);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    small_indiv[i] ~ gamma(mu[i]/scale, 1/scale);
    }
}

generated quantities {
  vector[n] mu;
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KHNtYWxsX2luZGl2LCBub19jb3JwX3BhY3MsIG5ld19tZW1iZXIsIHBjdF93aGl0ZSwgcGN0X2JsYWNrLCBcbiAgICAgICAgICAgICAgICBwY3RfbGF0aW5vLCBwY3RfYXNpYW4sIHBjdF9iYWNoZWxvcnMsIHBjdF9jbGludG9uXzE2LFxuICAgICAgICAgICAgICAgIHBjdF9kZW1faG91c2VfMTYsIGxvZ190b3Rfdm90aW5nX2FnZSwgbG9nX21lZGlhbl9oaF9pbmNvbWUpICU+JVxuICBtdXRhdGUobm9fY29ycF9wYWNzID0gaWZlbHNlKG5vX2NvcnBfcGFjcyA9PSBcIllFU1wiLCAxLCAwKSxcbiAgICAgICAgIG5ld19tZW1iZXIgPSBpZmVsc2UobmV3X21lbWJlciA9PSBcIllFU1wiLCAxLCAwKSkgJT4lXG4gIGRyb3BfbmEoKSAlPiVcbiAgbXV0YXRlX2F0KC52YXJzID0gdmFycyhwY3Rfd2hpdGU6bG9nX21lZGlhbl9oaF9pbmNvbWUpLCAuZnVucyA9IGNlbnRlcikgJT4lXG4gIGNvbXBvc2VfZGF0YSgpXG5cbiMgbW9kZWwgZXN0aW1hdGlvbiAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cbiMjIEZyZXF1ZW50aXN0XG5zbWFsbC4wMSA8LSBnbG0oc21hbGxfaW5kaXYgfiBub19jb3JwX3BhY3MgKyBuZXdfbWVtYmVyICsgcGN0X3doaXRlICsgcGN0X2JsYWNrICsgcGN0X2xhdGlubyArIHBjdF9hc2lhbiArIHBjdF9iYWNoZWxvcnMgKyBwY3RfY2xpbnRvbl8xNiArIHBjdF9kZW1faG91c2VfMTYgKyBsb2dfbWVkaWFuX2hoX2luY29tZSArIGxvZ190b3Rfdm90aW5nX2FnZSxcbiAgICAgICAgICAgICAgICAgICAgIGZhbWlseSA9IEdhbW1hKGxpbmsgPSBcImxvZ1wiKSxcbiAgICAgICAgICAgICAgICAgICAgIGRhdGEgPSBhcy5kYXRhLmZyYW1lKHN0YW4uZGF0YSkgJT4lIGZpbHRlcihzbWFsbF9pbmRpdiAhPSAwKSAlPiUgbXV0YXRlX2F0KC52YXJzID0gdmFycyhwY3Rfd2hpdGU6bG9nX21lZGlhbl9oaF9pbmNvbWUpLCAuZnVucyA9IGNlbnRlcikpXG5cbiMjIEJheWVzaWFuXG5zbWxpbmR2LmZpdCA8LSBzYW1wbGluZyhzbWxpbmR2Lm1vZGVsLFxuICAgICAgICAgICAgICAgICAgICAgICAgY2hhaW5zID0gNCxcbiAgICAgICAgICAgICAgICAgICAgICAgIGNvcmVzID0gNCxcbiAgICAgICAgICAgICAgICAgICAgICAgIGl0ZXIgPSAxMDAwMCxcbiAgICAgICAgICAgICAgICAgICAgICAgIHdhcm11cCA9IDUwMDAsXG4gICAgICAgICAgICAgICAgICAgICAgICBkYXRhID0gc3Rhbi5kYXRhKVxuXG50cmFjZXBsb3Qoc21saW5kdi5maXQsIFxuICAgICAgICAgIGluY193YXJtdXAgPSBUUlVFLCBcbiAgICAgICAgICBwYXJzID0gYyhcImFcIiwgXCJiX25wXCIsIFwiYl9uZXdcIiwgXCJiX3doaXRlXCIsIFwiYl9ibGFja1wiLCBcImJfbGF0aW5vXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9hc2lhblwiLCBcImJfYmFjaFwiLCBcImJfY2xpbnRcIiwgXCJiX2hvdXNlXCIsIFwiYl92cG9wXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9taGhcIiwgXCJzY2FsZVwiKSlcblxudGlkeShzbWFsbC4wMSwgY29uZi5pbnQgPSBUUlVFLCBjb25mLmxldmVsID0gMC44OSwgZGlnaXRzID0gMylcbnByZWNpcyhzbWxpbmR2LmZpdCwgcHJvYiA9IDAuODksIGRpZ2l0cyA9IDMpXG5cbnBvc3QgPC0gc3ByZWFkX2RyYXdzKHNtbGluZHYuZml0LCBtdVtpXSkgJT4lIG1lZGlhbl9oZGkoKVxuZ2dwbG90KGRhdGEgPSBlc3RpbWF0aW9uLmRhdGEsIGFlcyh4ID0gc21hbGxfaW5kaXYpKSArXG4gIGdlb21fZGVuc2l0eShmaWxsID0gXCJncmF5XCIpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBwb3N0LCBhZXMoeCA9IG11KSwgZmlsbCA9IFwic2t5Ymx1ZVwiLCBhbHBoYSA9IDAuNylcbmBgYCJ9 -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(small_indiv, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  drop_na() %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
## Frequentist
small.01 <- glm(small_indiv ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + pct_clinton_16 + pct_dem_house_16 + log_median_hh_income + log_tot_voting_age,
                     family = Gamma(link = "log"),
                     data = as.data.frame(stan.data) %>% filter(small_indiv != 0) %>% mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center))

## Bayesian
smlindv.fit <- sampling(smlindv.model,
                        chains = 4,
                        cores = 4,
                        iter = 10000,
                        warmup = 5000,
                        data = stan.data)

traceplot(smlindv.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh", "scale"))

tidy(small.01, conf.int = TRUE, conf.level = 0.89, digits = 3)
precis(smlindv.fit, prob = 0.89, digits = 3)

post <- spread_draws(smlindv.fit, mu[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = small_indiv)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = mu), fill = "skyblue", alpha = 0.7)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Model of Large Individual Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDExKSwgZXhwKDE0KSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiTm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgSW50ZXJjZXB0XCIpICtcbiAgc2NhbGVfeF9jb250aW51b3VzKGxhYmVscyA9IGRvbGxhcl9mb3JtYXQoKSkgK1xuICB0aGVtZShheGlzLnRleHQueCA9IGVsZW1lbnRfdGV4dChhbmdsZSA9IDQ1LCB2anVzdCA9IDEsIGhqdXN0ID0gMSkpXG5cbiMjIFNsb3BlIFBhcmFtZXRlcnNcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocm5vcm0oMWU0LCBleHAoMCksIGV4cCgxMykpKSxcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcIm5vcm1hbFwiKSkgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFNsb3BlIFBhcmFtZXRlcnNcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSBcblxuIyMgU2NhbGUgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAxKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0xXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0yXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2NhbGUgUGFyYW1ldGVyXCIpXG5cbiMjIFByb2JhYmlsaXR5IFBhcmFtZXRlclxuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShydW5pZigxZTQsIDAsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJ1bmlmb3JtXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMiwgMikpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMlwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNSkgK1xuICAgIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMS41LCAxLjUpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJiZXRhIDEuNVwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNykgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFByb2JhYmlsaXR5IFBhcmFtZXRlclwiKVxuYGBgIn0= -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(11), exp(14))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(13))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgbGFyZ2VfaW5kaXZbbl07XG4gIGludCBub19jb3JwX3BhY3Nbbl07XG4gIGludCBuZXdfbWVtYmVyW25dO1xuICByZWFsIHBjdF93aGl0ZVtuXTtcbiAgcmVhbCBwY3RfYmxhY2tbbl07XG4gIHJlYWwgcGN0X2xhdGlub1tuXTtcbiAgcmVhbCBwY3RfYXNpYW5bbl07XG4gIHJlYWwgcGN0X2JhY2hlbG9yc1tuXTtcbiAgcmVhbCBwY3RfY2xpbnRvbl8xNltuXTtcbiAgcmVhbCBwY3RfZGVtX2hvdXNlXzE2W25dO1xuICByZWFsIGxvZ19tZWRpYW5faGhfaW5jb21lW25dO1xuICByZWFsIGxvZ190b3Rfdm90aW5nX2FnZVtuXTtcbn1cblxucGFyYW1ldGVycyB7XG4gIHJlYWw8bG93ZXIgPSAwPiBhO1xuICByZWFsIGJfbnA7XG4gIHJlYWwgYl9uZXc7XG4gIHJlYWwgYl93aGl0ZTtcbiAgcmVhbCBiX2JsYWNrO1xuICByZWFsIGJfbGF0aW5vO1xuICByZWFsIGJfYXNpYW47XG4gIHJlYWwgYl9iYWNoO1xuICByZWFsIGJfbWhoO1xuICByZWFsIGJfY2xpbnQ7XG4gIHJlYWwgYl9ob3VzZTtcbiAgcmVhbCBiX3Zwb3A7XG4gIHJlYWw8bG93ZXIgPSAwPiBzY2FsZTtcbn1cblxubW9kZWwge1xuICB2ZWN0b3Jbbl0gbXU7XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgLy8gbGluZWFyIG1vZGVsXG4gICAgbXVbaV0gPSBhICsgYl9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIGJfbmV3ICogbmV3X21lbWJlcltpXSArIFxuICAgICAgYl93aGl0ZSAqIHBjdF93aGl0ZVtpXSArIGJfYmxhY2sgKiBwY3RfYmxhY2tbaV0gKyBcbiAgICAgIGJfbGF0aW5vICogcGN0X2xhdGlub1tpXSArIGJfYXNpYW4gKiBwY3RfYXNpYW5baV0gKyBcbiAgICAgIGJfYmFjaCAqIHBjdF9iYWNoZWxvcnNbaV0gKyBiX2NsaW50ICogcGN0X2NsaW50b25fMTZbaV0gKyBcbiAgICAgIGJfaG91c2UgKiBwY3RfZGVtX2hvdXNlXzE2W2ldICsgYl9taGggKiBsb2dfbWVkaWFuX2hoX2luY29tZVtpXSArXG4gICAgICBiX3Zwb3AgKiBsb2dfdG90X3ZvdGluZ19hZ2VbaV07XG4gICAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxuICBcbiAgLy8gcHJpb3JzXG4gIC8vIEdhbW1hIG1vZGVsXG4gIGEgfiBub3JtYWwoMTEsIDE0KTtcbiAgYl9ucCB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfbmV3IH4gbm9ybWFsKDAsIDEzKTtcbiAgYl93aGl0ZSB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfYmxhY2sgfiBub3JtYWwoMCwgMTMpO1xuICBiX2xhdGlubyB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfYXNpYW4gfiBub3JtYWwoMCwgMTMpO1xuICBiX2JhY2ggfiBub3JtYWwoMCwgMTMpO1xuICBiX2NsaW50IH4gbm9ybWFsKDAsIDEzKTtcbiAgYl9ob3VzZSB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfdnBvcCB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfbWhoIH4gbm9ybWFsKDAsIDEzKTtcbiAgc2NhbGUgfiBleHBvbmVudGlhbCgyKTtcbiAgXG4gIC8vIGxpa2VsaWhvb2RcbiAgZm9yICggaSBpbiAxOm4pIHtcbiAgICBsYXJnZV9pbmRpdltpXSB+IGdhbW1hKG11W2ldL3NjYWxlLCAxL3NjYWxlKTtcbiAgICB9Ly9pXG59XG5cbmdlbmVyYXRlZCBxdWFudGl0aWVzIHtcbiAgdmVjdG9yW25dIG11O1xuICBcbiAgZm9yIChpIGluIDE6bikge1xuICBtdVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxufVxuYGBgIn0= -->

```stan
data {
  int<lower = 1> n;
  real large_indiv[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  // Gamma model
  a ~ normal(11, 14);
  b_np ~ normal(0, 13);
  b_new ~ normal(0, 13);
  b_white ~ normal(0, 13);
  b_black ~ normal(0, 13);
  b_latino ~ normal(0, 13);
  b_asian ~ normal(0, 13);
  b_bach ~ normal(0, 13);
  b_clint ~ normal(0, 13);
  b_house ~ normal(0, 13);
  b_vpop ~ normal(0, 13);
  b_mhh ~ normal(0, 13);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    large_indiv[i] ~ gamma(mu[i]/scale, 1/scale);
    }//i
}

generated quantities {
  vector[n] mu;
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KGxhcmdlX2luZGl2LCBub19jb3JwX3BhY3MsIG5ld19tZW1iZXIsIHBjdF93aGl0ZSwgcGN0X2JsYWNrLCBcbiAgICAgICAgICAgICAgICBwY3RfbGF0aW5vLCBwY3RfYXNpYW4sIHBjdF9iYWNoZWxvcnMsIHBjdF9jbGludG9uXzE2LFxuICAgICAgICAgICAgICAgIHBjdF9kZW1faG91c2VfMTYsIGxvZ190b3Rfdm90aW5nX2FnZSwgbG9nX21lZGlhbl9oaF9pbmNvbWUpICU+JVxuICBtdXRhdGUobm9fY29ycF9wYWNzID0gaWZlbHNlKG5vX2NvcnBfcGFjcyA9PSBcIllFU1wiLCAxLCAwKSxcbiAgICAgICAgIG5ld19tZW1iZXIgPSBpZmVsc2UobmV3X21lbWJlciA9PSBcIllFU1wiLCAxLCAwKSkgJT4lXG4gIGRyb3BfbmEoKSAlPiVcbiAgbXV0YXRlX2F0KC52YXJzID0gdmFycyhwY3Rfd2hpdGU6bG9nX21lZGlhbl9oaF9pbmNvbWUpLCAuZnVucyA9IGNlbnRlcikgJT4lXG4gIGNvbXBvc2VfZGF0YSgpXG5cbiMgbW9kZWwgZXN0aW1hdGlvbiAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cbiMjIEZyZXF1ZW50aXN0XG5sYXJnZS4wMSA8LSBnbG0obGFyZ2VfaW5kaXYgfiBub19jb3JwX3BhY3MgKyBuZXdfbWVtYmVyICsgcGN0X3doaXRlICsgcGN0X2JsYWNrICsgcGN0X2xhdGlubyArIHBjdF9hc2lhbiArIHBjdF9iYWNoZWxvcnMgKyBwY3RfY2xpbnRvbl8xNiArIHBjdF9kZW1faG91c2VfMTYgKyBsb2dfbWVkaWFuX2hoX2luY29tZSArIGxvZ190b3Rfdm90aW5nX2FnZSxcbiAgICAgICAgICAgICAgICAgICAgIGZhbWlseSA9IEdhbW1hKGxpbmsgPSBcImxvZ1wiKSxcbiAgICAgICAgICAgICAgICAgICAgIGRhdGEgPSBhcy5kYXRhLmZyYW1lKHN0YW4uZGF0YSkgJT4lIGZpbHRlcihsYXJnZV9pbmRpdiAhPSAwKSAlPiUgbXV0YXRlX2F0KC52YXJzID0gdmFycyhwY3Rfd2hpdGU6bG9nX21lZGlhbl9oaF9pbmNvbWUpLCAuZnVucyA9IGNlbnRlcikpXG5cbiMjIEJheWVzaWFuXG5scmdpbmR2LmZpdCA8LSBzYW1wbGluZyhscmdpbmR2Lm1vZGVsLFxuICAgICAgICAgICAgICAgICAgICAgICAgY2hhaW5zID0gNCxcbiAgICAgICAgICAgICAgICAgICAgICAgIGNvcmVzID0gNCxcbiAgICAgICAgICAgICAgICAgICAgICAgIGl0ZXIgPSAxMDAwMCxcbiAgICAgICAgICAgICAgICAgICAgICAgIHdhcm11cCA9IDUwMDAsXG4gICAgICAgICAgICAgICAgICAgICAgICBkYXRhID0gc3Rhbi5kYXRhKVxuXG50cmFjZXBsb3QobHJnaW5kdi5maXQsIFxuICAgICAgICAgIGluY193YXJtdXAgPSBUUlVFLCBcbiAgICAgICAgICBwYXJzID0gYyhcImFcIiwgXCJiX25wXCIsIFwiYl9uZXdcIiwgXCJiX3doaXRlXCIsIFwiYl9ibGFja1wiLCBcImJfbGF0aW5vXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9hc2lhblwiLCBcImJfYmFjaFwiLCBcImJfY2xpbnRcIiwgXCJiX2hvdXNlXCIsIFwiYl92cG9wXCIsIFxuICAgICAgICAgICAgICAgICAgIFwiYl9taGhcIiwgXCJzY2FsZVwiKSlcblxudGlkeShsYXJnZS4wMSwgY29uZi5pbnQgPSBUUlVFLCBjb25mLmxldmVsID0gMC44OSwgZGlnaXRzID0gMylcbnByZWNpcyhscmdpbmR2LmZpdCwgcHJvYiA9IDAuODksIGRpZ2l0cyA9IDMpXG5cbnBvc3QgPC0gc3ByZWFkX2RyYXdzKGxyZ2luZHYuZml0LCBtdVtpXSkgJT4lIG1lZGlhbl9oZGkoKVxuZ2dwbG90KGRhdGEgPSBlc3RpbWF0aW9uLmRhdGEsIGFlcyh4ID0gbGFyZ2VfaW5kaXYpKSArXG4gIGdlb21fZGVuc2l0eShmaWxsID0gXCJncmF5XCIpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBwb3N0LCBhZXMoeCA9IG11KSwgZmlsbCA9IFwic2t5Ymx1ZVwiLCBhbHBoYSA9IDAuNylcbmBgYCJ9 -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(large_indiv, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  drop_na() %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
## Frequentist
large.01 <- glm(large_indiv ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + pct_clinton_16 + pct_dem_house_16 + log_median_hh_income + log_tot_voting_age,
                     family = Gamma(link = "log"),
                     data = as.data.frame(stan.data) %>% filter(large_indiv != 0) %>% mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center))

## Bayesian
lrgindv.fit <- sampling(lrgindv.model,
                        chains = 4,
                        cores = 4,
                        iter = 10000,
                        warmup = 5000,
                        data = stan.data)

traceplot(lrgindv.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh", "scale"))

tidy(large.01, conf.int = TRUE, conf.level = 0.89, digits = 3)
precis(lrgindv.fit, prob = 0.89, digits = 3)

post <- spread_draws(lrgindv.fit, mu[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = large_indiv)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = mu), fill = "skyblue", alpha = 0.7)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Model of Total Individual Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDEyKSwgZXhwKDE0KSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiTm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgSW50ZXJjZXB0XCIpICtcbiAgc2NhbGVfeF9jb250aW51b3VzKGxhYmVscyA9IGRvbGxhcl9mb3JtYXQoKSkgK1xuICB0aGVtZShheGlzLnRleHQueCA9IGVsZW1lbnRfdGV4dChhbmdsZSA9IDQ1LCB2anVzdCA9IDEsIGhqdXN0ID0gMSkpXG5cbiMjIFNsb3BlIFBhcmFtZXRlcnNcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocm5vcm0oMWU0LCBleHAoMCksIGV4cCgxNCkpKSxcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcIm5vcm1hbFwiKSkgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFNsb3BlIFBhcmFtZXRlcnNcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSBcblxuIyMgU2NhbGUgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAxKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0xXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0yXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2NhbGUgUGFyYW1ldGVyXCIpXG5cbiMjIFByb2JhYmlsaXR5IFBhcmFtZXRlclxuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShydW5pZigxZTQsIDAsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJ1bmlmb3JtXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMiwgMikpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMlwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNSkgK1xuICAgIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMS41LCAxLjUpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJiZXRhIDEuNVwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNykgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFByb2JhYmlsaXR5IFBhcmFtZXRlclwiKVxuYGBgIn0= -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(12), exp(14))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(14))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgbmV0X2luZGl2X3RvdGFsW25dO1xuICBpbnQgbm9fY29ycF9wYWNzW25dO1xuICBpbnQgbmV3X21lbWJlcltuXTtcbiAgcmVhbCBwY3Rfd2hpdGVbbl07XG4gIHJlYWwgcGN0X2JsYWNrW25dO1xuICByZWFsIHBjdF9sYXRpbm9bbl07XG4gIHJlYWwgcGN0X2FzaWFuW25dO1xuICByZWFsIHBjdF9iYWNoZWxvcnNbbl07XG4gIHJlYWwgcGN0X2NsaW50b25fMTZbbl07XG4gIHJlYWwgcGN0X2RlbV9ob3VzZV8xNltuXTtcbiAgcmVhbCBsb2dfbWVkaWFuX2hoX2luY29tZVtuXTtcbiAgcmVhbCBsb2dfdG90X3ZvdGluZ19hZ2Vbbl07XG59XG5cbnBhcmFtZXRlcnMge1xuICByZWFsPGxvd2VyID0gMD4gYTtcbiAgcmVhbCBiX25wO1xuICByZWFsIGJfbmV3O1xuICByZWFsIGJfd2hpdGU7XG4gIHJlYWwgYl9ibGFjaztcbiAgcmVhbCBiX2xhdGlubztcbiAgcmVhbCBiX2FzaWFuO1xuICByZWFsIGJfYmFjaDtcbiAgcmVhbCBiX21oaDtcbiAgcmVhbCBiX2NsaW50O1xuICByZWFsIGJfaG91c2U7XG4gIHJlYWwgYl92cG9wO1xuICByZWFsPGxvd2VyID0gMD4gc2NhbGU7XG59XG5cbm1vZGVsIHtcbiAgdmVjdG9yW25dIG11O1xuICBcbiAgZm9yIChpIGluIDE6bikge1xuICAgIC8vIGxpbmVhciBtb2RlbFxuICAgIG11W2ldID0gYSArIGJfbnAgKiBub19jb3JwX3BhY3NbaV0gKyBiX25ldyAqIG5ld19tZW1iZXJbaV0gKyBcbiAgICAgIGJfd2hpdGUgKiBwY3Rfd2hpdGVbaV0gKyBiX2JsYWNrICogcGN0X2JsYWNrW2ldICsgXG4gICAgICBiX2xhdGlubyAqIHBjdF9sYXRpbm9baV0gKyBiX2FzaWFuICogcGN0X2FzaWFuW2ldICsgXG4gICAgICBiX2JhY2ggKiBwY3RfYmFjaGVsb3JzW2ldICsgYl9jbGludCAqIHBjdF9jbGludG9uXzE2W2ldICsgXG4gICAgICBiX2hvdXNlICogcGN0X2RlbV9ob3VzZV8xNltpXSArIGJfbWhoICogbG9nX21lZGlhbl9oaF9pbmNvbWVbaV0gK1xuICAgICAgYl92cG9wICogbG9nX3RvdF92b3RpbmdfYWdlW2ldO1xuICAgIG11W2ldID0gZXhwKG11W2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbiAgXG4gIC8vIHByaW9yc1xuICAvLyBHYW1tYSBtb2RlbFxuICBhIH4gbm9ybWFsKDEyLCAxNCk7XG4gIGJfbnAgfiBub3JtYWwoMCwgMTQpO1xuICBiX25ldyB+IG5vcm1hbCgwLCAxNCk7XG4gIGJfd2hpdGUgfiBub3JtYWwoMCwgMTQpO1xuICBiX2JsYWNrIH4gbm9ybWFsKDAsIDE0KTtcbiAgYl9sYXRpbm8gfiBub3JtYWwoMCwgMTQpO1xuICBiX2FzaWFuIH4gbm9ybWFsKDAsIDE0KTtcbiAgYl9iYWNoIH4gbm9ybWFsKDAsIDE0KTtcbiAgYl9jbGludCB+IG5vcm1hbCgwLCAxNCk7XG4gIGJfaG91c2UgfiBub3JtYWwoMCwgMTQpO1xuICBiX3Zwb3AgfiBub3JtYWwoMCwgMTQpO1xuICBiX21oaCB+IG5vcm1hbCgwLCAxNCk7XG4gIHNjYWxlIH4gZXhwb25lbnRpYWwoMik7XG4gIFxuICAvLyBsaWtlbGlob29kXG4gIGZvciAoIGkgaW4gMTpuKSB7XG4gICAgbmV0X2luZGl2X3RvdGFsW2ldIH4gZ2FtbWEobXVbaV0vc2NhbGUsIDEvc2NhbGUpO1xuICAgIH0vL2lcbn1cblxuZ2VuZXJhdGVkIHF1YW50aXRpZXMge1xuICB2ZWN0b3Jbbl0gbXU7XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gIG11W2ldID0gYSArIGJfbnAgKiBub19jb3JwX3BhY3NbaV0gKyBiX25ldyAqIG5ld19tZW1iZXJbaV0gKyBcbiAgICAgIGJfd2hpdGUgKiBwY3Rfd2hpdGVbaV0gKyBiX2JsYWNrICogcGN0X2JsYWNrW2ldICsgXG4gICAgICBiX2xhdGlubyAqIHBjdF9sYXRpbm9baV0gKyBiX2FzaWFuICogcGN0X2FzaWFuW2ldICsgXG4gICAgICBiX2JhY2ggKiBwY3RfYmFjaGVsb3JzW2ldICsgYl9jbGludCAqIHBjdF9jbGludG9uXzE2W2ldICsgXG4gICAgICBiX2hvdXNlICogcGN0X2RlbV9ob3VzZV8xNltpXSArIGJfbWhoICogbG9nX21lZGlhbl9oaF9pbmNvbWVbaV0gK1xuICAgICAgYl92cG9wICogbG9nX3RvdF92b3RpbmdfYWdlW2ldO1xuICBtdVtpXSA9IGV4cChtdVtpXSk7IC8vIGludmVyc2UgbGluayBmdW5jdGlvblxuICB9XG59XG5gYGAifQ== -->

```stan
data {
  int<lower = 1> n;
  real net_indiv_total[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  // Gamma model
  a ~ normal(12, 14);
  b_np ~ normal(0, 14);
  b_new ~ normal(0, 14);
  b_white ~ normal(0, 14);
  b_black ~ normal(0, 14);
  b_latino ~ normal(0, 14);
  b_asian ~ normal(0, 14);
  b_bach ~ normal(0, 14);
  b_clint ~ normal(0, 14);
  b_house ~ normal(0, 14);
  b_vpop ~ normal(0, 14);
  b_mhh ~ normal(0, 14);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    net_indiv_total[i] ~ gamma(mu[i]/scale, 1/scale);
    }//i
}

generated quantities {
  vector[n] mu;
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBjb21wb3NlIGRhdGFcbnN0YW4uZGF0YSA8LSBcbiAgZXN0aW1hdGlvbi5kYXRhICU+JVxuICBkcGx5cjo6c2VsZWN0KG5ldF9pbmRpdl90b3RhbCwgbm9fY29ycF9wYWNzLCBuZXdfbWVtYmVyLCBwY3Rfd2hpdGUsIHBjdF9ibGFjaywgXG4gICAgICAgICAgICAgICAgcGN0X2xhdGlubywgcGN0X2FzaWFuLCBwY3RfYmFjaGVsb3JzLCBwY3RfY2xpbnRvbl8xNixcbiAgICAgICAgICAgICAgICBwY3RfZGVtX2hvdXNlXzE2LCBsb2dfdG90X3ZvdGluZ19hZ2UsIGxvZ19tZWRpYW5faGhfaW5jb21lKSAlPiVcbiAgbXV0YXRlKG5vX2NvcnBfcGFjcyA9IGlmZWxzZShub19jb3JwX3BhY3MgPT0gXCJZRVNcIiwgMSwgMCksXG4gICAgICAgICBuZXdfbWVtYmVyID0gaWZlbHNlKG5ld19tZW1iZXIgPT0gXCJZRVNcIiwgMSwgMCkpICU+JVxuICBkcm9wX25hKCkgJT4lXG4gIG11dGF0ZV9hdCgudmFycyA9IHZhcnMocGN0X3doaXRlOmxvZ19tZWRpYW5faGhfaW5jb21lKSwgLmZ1bnMgPSBjZW50ZXIpICU+JVxuICBjb21wb3NlX2RhdGEoKVxuXG4jIG1vZGVsIGVzdGltYXRpb24gLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXG4jIyBGcmVxdWVudGlzdFxuaW5kaXYudG90LjAxIDwtIGdsbShuZXRfaW5kaXZfdG90YWwgfiBub19jb3JwX3BhY3MgKyBuZXdfbWVtYmVyICsgcGN0X3doaXRlICsgcGN0X2JsYWNrICsgcGN0X2xhdGlubyArIHBjdF9hc2lhbiArIHBjdF9iYWNoZWxvcnMgKyBwY3RfY2xpbnRvbl8xNiArIHBjdF9kZW1faG91c2VfMTYgKyBsb2dfbWVkaWFuX2hoX2luY29tZSArIGxvZ190b3Rfdm90aW5nX2FnZSxcbiAgICAgICAgICAgICAgICAgICAgIGZhbWlseSA9IEdhbW1hKGxpbmsgPSBcImxvZ1wiKSxcbiAgICAgICAgICAgICAgICAgICAgIGRhdGEgPSBhcy5kYXRhLmZyYW1lKHN0YW4uZGF0YSkgJT4lIGZpbHRlcihuZXRfaW5kaXZfdG90YWwgIT0gMCkgJT4lIG11dGF0ZV9hdCgudmFycyA9IHZhcnMocGN0X3doaXRlOmxvZ19tZWRpYW5faGhfaW5jb21lKSwgLmZ1bnMgPSBjZW50ZXIpKVxuXG4jIyBCYXllc2lhblxudG90aW5kdi5maXQgPC0gc2FtcGxpbmcodG90aW5kdi5tb2RlbCxcbiAgICAgICAgICAgICAgICAgICAgICAgIGNoYWlucyA9IDQsXG4gICAgICAgICAgICAgICAgICAgICAgICBjb3JlcyA9IDQsXG4gICAgICAgICAgICAgICAgICAgICAgICBpdGVyID0gMTAwMDAsXG4gICAgICAgICAgICAgICAgICAgICAgICB3YXJtdXAgPSA1MDAwLFxuICAgICAgICAgICAgICAgICAgICAgICAgZGF0YSA9IHN0YW4uZGF0YSlcblxudHJhY2VwbG90KHRvdGluZHYuZml0LCBcbiAgICAgICAgICBpbmNfd2FybXVwID0gVFJVRSwgXG4gICAgICAgICAgcGFycyA9IGMoXCJhXCIsIFwiYl9ucFwiLCBcImJfbmV3XCIsIFwiYl93aGl0ZVwiLCBcImJfYmxhY2tcIiwgXCJiX2xhdGlub1wiLCBcbiAgICAgICAgICAgICAgICAgICBcImJfYXNpYW5cIiwgXCJiX2JhY2hcIiwgXCJiX2NsaW50XCIsIFwiYl9ob3VzZVwiLCBcImJfdnBvcFwiLCBcbiAgICAgICAgICAgICAgICAgICBcImJfbWhoXCIsIFwic2NhbGVcIikpXG5cbnRpZHkoaW5kaXYudG90LjAxLCBjb25mLmludCA9IFRSVUUsIGNvbmYubGV2ZWwgPSAwLjg5LCBkaWdpdHMgPSAzKVxucHJlY2lzKHRvdGluZHYuZml0LCBwcm9iID0gMC44OSwgZGlnaXRzID0gMylcblxucG9zdCA8LSBzcHJlYWRfZHJhd3ModG90aW5kdi5maXQsIG11W2ldKSAlPiUgbWVkaWFuX2hkaSgpXG5nZ3Bsb3QoZGF0YSA9IGVzdGltYXRpb24uZGF0YSwgYWVzKHggPSBuZXRfaW5kaXZfdG90YWwpKSArXG4gIGdlb21fZGVuc2l0eShmaWxsID0gXCJncmF5XCIpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBwb3N0LCBhZXMoeCA9IG11KSwgZmlsbCA9IFwic2t5Ymx1ZVwiLCBhbHBoYSA9IDAuNylcbmBgYCJ9 -->

```r
# compose data
stan.data <- 
  estimation.data %>%
  dplyr::select(net_indiv_total, no_corp_pacs, new_member, pct_white, pct_black, 
                pct_latino, pct_asian, pct_bachelors, pct_clinton_16,
                pct_dem_house_16, log_tot_voting_age, log_median_hh_income) %>%
  mutate(no_corp_pacs = ifelse(no_corp_pacs == "YES", 1, 0),
         new_member = ifelse(new_member == "YES", 1, 0)) %>%
  drop_na() %>%
  mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center) %>%
  compose_data()

# model estimation ------------------------------------------------------------
## Frequentist
indiv.tot.01 <- glm(net_indiv_total ~ no_corp_pacs + new_member + pct_white + pct_black + pct_latino + pct_asian + pct_bachelors + pct_clinton_16 + pct_dem_house_16 + log_median_hh_income + log_tot_voting_age,
                     family = Gamma(link = "log"),
                     data = as.data.frame(stan.data) %>% filter(net_indiv_total != 0) %>% mutate_at(.vars = vars(pct_white:log_median_hh_income), .funs = center))

## Bayesian
totindv.fit <- sampling(totindv.model,
                        chains = 4,
                        cores = 4,
                        iter = 10000,
                        warmup = 5000,
                        data = stan.data)

traceplot(totindv.fit, 
          inc_warmup = TRUE, 
          pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                   "b_asian", "b_bach", "b_clint", "b_house", "b_vpop", 
                   "b_mhh", "scale"))

tidy(indiv.tot.01, conf.int = TRUE, conf.level = 0.89, digits = 3)
precis(totindv.fit, prob = 0.89, digits = 3)

post <- spread_draws(totindv.fit, mu[i]) %>% median_hdi()
ggplot(data = estimation.data, aes(x = net_indiv_total)) +
  geom_density(fill = "gray") +
  geom_density(data = post, aes(x = mu), fill = "skyblue", alpha = 0.7)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Model of Individual Ideological PAC Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDEwLjUpLCBleHAoMTMpKSksXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJOb3JtYWxcIikpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBJbnRlcmNlcHRcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSArXG4gIHRoZW1lKGF4aXMudGV4dC54ID0gZWxlbWVudF90ZXh0KGFuZ2xlID0gNDUsIHZqdXN0ID0gMSwgaGp1c3QgPSAxKSlcblxuIyMgU2xvcGUgUGFyYW1ldGVyc1xuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShybm9ybSgxZTQsIGV4cCgwKSwgZXhwKDEyKSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwibm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhsYWJlbHMgPSBkb2xsYXJfZm9ybWF0KCkpIFxuXG4jIyBTY2FsZSBQYXJhbWV0ZXJcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTFcIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmV4cCgxZTQsIDIpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJyYXRlPTJcIiksIFxuICAgICAgICAgICAgICAgYWxwaGEgPSAwLjUpICtcbiAgdGhlbWVfY2xhc3NpYygpICtcbiAgbGFicyh4ID0gXCJNZWFuIGFuZCBTRCBvZiBTY2FsZSBQYXJhbWV0ZXJcIilcblxuIyMgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJ1bmlmKDFlNCwgMCwgMSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcInVuaWZvcm1cIikpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAyLCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiYmV0YSAyXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gICAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmJldGEoMWU0LCAxLjUsIDEuNSkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMS41XCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC43KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgUHJvYmFiaWxpdHkgUGFyYW1ldGVyXCIpXG5gYGAifQ== -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(10.5), exp(13))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(12))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgaW5kaXZfaWRlb2xvZ2ljYWxbbl07XG4gIGludCBub19jb3JwX3BhY3Nbbl07XG4gIGludCBuZXdfbWVtYmVyW25dO1xuICByZWFsIHBjdF93aGl0ZVtuXTtcbiAgcmVhbCBwY3RfYmxhY2tbbl07XG4gIHJlYWwgcGN0X2xhdGlub1tuXTtcbiAgcmVhbCBwY3RfYXNpYW5bbl07XG4gIHJlYWwgcGN0X2JhY2hlbG9yc1tuXTtcbiAgcmVhbCBwY3RfY2xpbnRvbl8xNltuXTtcbiAgcmVhbCBwY3RfZGVtX2hvdXNlXzE2W25dO1xuICByZWFsIGxvZ19tZWRpYW5faGhfaW5jb21lW25dO1xuICByZWFsIGxvZ190b3Rfdm90aW5nX2FnZVtuXTtcbn1cblxucGFyYW1ldGVycyB7XG4gIHJlYWwgcDtcbiAgcmVhbCBwX25wO1xuICByZWFsIHBfbmV3O1xuICByZWFsPGxvd2VyID0gMD4gYTtcbiAgcmVhbCBiX25wO1xuICByZWFsIGJfbmV3O1xuICByZWFsIGJfd2hpdGU7XG4gIHJlYWwgYl9ibGFjaztcbiAgcmVhbCBiX2xhdGlubztcbiAgcmVhbCBiX2FzaWFuO1xuICByZWFsIGJfYmFjaDtcbiAgcmVhbCBiX21oaDtcbiAgcmVhbCBiX2NsaW50O1xuICByZWFsIGJfaG91c2U7XG4gIHJlYWwgYl92cG9wO1xuICByZWFsPGxvd2VyID0gMD4gc2NhbGU7XG59XG5cbm1vZGVsIHtcbiAgdmVjdG9yW25dIHByb2I7XG4gIHZlY3RvcltuXSBtdTtcbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgICAvLyBsaW5lYXIgbW9kZWxcbiAgICBwcm9iW2ldID0gcCArIHBfbnAgKiBub19jb3JwX3BhY3NbaV0gKyBwX25ldyAqIG5ld19tZW1iZXJbaV07IFxuICAgIHByb2JbaV0gPSBpbnZfbG9naXQocHJvYltpXSk7IC8vIGludmVyc2UgbGluayBmdW5jdGlvblxuICB9XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgLy8gbGluZWFyIG1vZGVsXG4gICAgbXVbaV0gPSBhICsgYl9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIGJfbmV3ICogbmV3X21lbWJlcltpXSArIFxuICAgICAgYl93aGl0ZSAqIHBjdF93aGl0ZVtpXSArIGJfYmxhY2sgKiBwY3RfYmxhY2tbaV0gKyBcbiAgICAgIGJfbGF0aW5vICogcGN0X2xhdGlub1tpXSArIGJfYXNpYW4gKiBwY3RfYXNpYW5baV0gKyBcbiAgICAgIGJfYmFjaCAqIHBjdF9iYWNoZWxvcnNbaV0gKyBiX2NsaW50ICogcGN0X2NsaW50b25fMTZbaV0gKyBcbiAgICAgIGJfaG91c2UgKiBwY3RfZGVtX2hvdXNlXzE2W2ldICsgYl9taGggKiBsb2dfbWVkaWFuX2hoX2luY29tZVtpXSArXG4gICAgICBiX3Zwb3AgKiBsb2dfdG90X3ZvdGluZ19hZ2VbaV07XG4gICAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxuICBcbiAgLy8gcHJpb3JzXG4gIFxuICAvLyBCaW5vbWlhbCBtb2RlbFxuICBwIH4gbm9ybWFsKDAsIDEpO1xuICBwX25wIH4gbm9ybWFsKDAsIDEpO1xuICBwX25ldyB+IG5vcm1hbCgwLCAxKTtcbiAgXG4gIC8vIEdhbW1hIG1vZGVsXG4gIGEgfiBub3JtYWwoMTAuNSwgMTMpO1xuICBiX25wIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9uZXcgfiBub3JtYWwoMCwgMTIpO1xuICBiX3doaXRlIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9ibGFjayB+IG5vcm1hbCgwLCAxMik7XG4gIGJfbGF0aW5vIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9hc2lhbiB+IG5vcm1hbCgwLCAxMik7XG4gIGJfYmFjaCB+IG5vcm1hbCgwLCAxMik7XG4gIGJfY2xpbnQgfiBub3JtYWwoMCwgMTIpO1xuICBiX2hvdXNlIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl92cG9wIH4gbm9ybWFsKDAsIDEyKTtcbiAgYl9taGggfiBub3JtYWwoMCwgMTIpO1xuICBzY2FsZSB+IGV4cG9uZW50aWFsKDIpO1xuICBcbiAgLy8gbGlrZWxpaG9vZFxuICBmb3IgKCBpIGluIDE6bikge1xuICAgIGlmIChpbmRpdl9pZGVvbG9naWNhbFtpXSA9PSAwKVxuICAgICAgdGFyZ2V0ICs9IChiZXJub3VsbGlfbHBtZigxfCBwcm9iW2ldKSk7XG4gICAgZWxzZVxuICAgICAgdGFyZ2V0ICs9IChiZXJub3VsbGlfbHBtZigwfCBwcm9iW2ldKSArIGdhbW1hX2xwZGYoaW5kaXZfaWRlb2xvZ2ljYWxbaV18IG11W2ldL3NjYWxlLCAxL3NjYWxlKSk7XG4gICAgfS8vaVxufVxuXG5nZW5lcmF0ZWQgcXVhbnRpdGllcyB7XG4gIHZlY3RvcltuXSBwcm9iO1xuICB2ZWN0b3Jbbl0gbXU7XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgLy8gbGluZWFyIG1vZGVsXG4gICAgcHJvYltpXSA9IHAgKyBwX25wICogbm9fY29ycF9wYWNzW2ldICsgcF9uZXcgKiBuZXdfbWVtYmVyW2ldOyBcbiAgICBwcm9iW2ldID0gaW52X2xvZ2l0KHByb2JbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxuICBcbiAgZm9yIChpIGluIDE6bikge1xuICBtdVtpXSA9IGEgKyBiX25wICogbm9fY29ycF9wYWNzW2ldICsgYl9uZXcgKiBuZXdfbWVtYmVyW2ldICsgXG4gICAgICBiX3doaXRlICogcGN0X3doaXRlW2ldICsgYl9ibGFjayAqIHBjdF9ibGFja1tpXSArIFxuICAgICAgYl9sYXRpbm8gKiBwY3RfbGF0aW5vW2ldICsgYl9hc2lhbiAqIHBjdF9hc2lhbltpXSArIFxuICAgICAgYl9iYWNoICogcGN0X2JhY2hlbG9yc1tpXSArIGJfY2xpbnQgKiBwY3RfY2xpbnRvbl8xNltpXSArIFxuICAgICAgYl9ob3VzZSAqIHBjdF9kZW1faG91c2VfMTZbaV0gKyBiX21oaCAqIGxvZ19tZWRpYW5faGhfaW5jb21lW2ldICtcbiAgICAgIGJfdnBvcCAqIGxvZ190b3Rfdm90aW5nX2FnZVtpXTtcbiAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxufVxuYGBgIn0= -->

```stan
data {
  int<lower = 1> n;
  real indiv_ideological[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real p;
  real p_np;
  real p_new;
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  
  // Binomial model
  p ~ normal(0, 1);
  p_np ~ normal(0, 1);
  p_new ~ normal(0, 1);
  
  // Gamma model
  a ~ normal(10.5, 13);
  b_np ~ normal(0, 12);
  b_new ~ normal(0, 12);
  b_white ~ normal(0, 12);
  b_black ~ normal(0, 12);
  b_latino ~ normal(0, 12);
  b_asian ~ normal(0, 12);
  b_bach ~ normal(0, 12);
  b_clint ~ normal(0, 12);
  b_house ~ normal(0, 12);
  b_vpop ~ normal(0, 12);
  b_mhh ~ normal(0, 12);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    if (indiv_ideological[i] == 0)
      target += (bernoulli_lpmf(1| prob[i]));
    else
      target += (bernoulli_lpmf(0| prob[i]) + gamma_lpdf(indiv_ideological[i]| mu[i]/scale, 1/scale));
    }//i
}

generated quantities {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-frame-begin eyJtZXRhZGF0YSI6eyJjbGFzc2VzIjpbImRhdGEuZnJhbWUiXSwibmNvbCI6NiwibnJvdyI6MTZ9LCJyZGYiOiJINHNJQUFBQUFBQUFBOFZVYTJ3TVVSU2UzZTV1MjhGcUVPK0lzRVFpdXlMVmxJamNrUlR4YUdqUmxJanQ3T3kwczh6T2JIYW50cWltalNBSWZpQmErcU1JVWhxUHBtbTg0bTZLYUFqcGc5UWpRdEZLRVNGZWFWSHVuVHQzZDYzNGJaUEp1ZWZNT2QvNTduZk9iRzVXZmpxYnp6SU1rOFFrMmN4TWtoVWRHZXZLRmZPZE14bkdZa2FPaWJFd3FkaVdvS1NoNkdCRGp4MDlhUkhydXJMZUp4TmE0UnRzV3JyZzgvM2ExWFZsZlp4alYyVHJFS1VTdE5vY1Q2ZU5yZ0d2WnVXbmViTGI0Ym5aM3RWajYxSmd3NmJPQldlSGJZWW5MU054QmJ3MGZNZFU4OEJGc0xvdVpmcmtubzNjb0p5MzI4WmR6QVhiY1hqaFpYaXdBZ09ONFJnTTMvdUVxK3pveDMwcEQvQ2dhZCthS1YrdWd4NlNCN3FiRzNCSGNPVEV5K1hmN2owQ1I2WGcrZkUzcXNFcGRrN2dlQWNMTk5ML0wxdEMrSU1DVDNiYng5b2ZBRjdiZ0pHQVpNU1hHZkcyc1RwUkxyMVp4NDNxa2RwVk1IZG41cEtJaWZTQmZmZC9mbXFjZDR4ekVON2dEaElKWllCT25kWUllS0gyKzJ0NWR4WnNOTzVaVzRWL2grRVZYYWJwc0liMDR3WmNYcHNCU3QrRFVxSVBQTFJuNlRNa0plaXRkOXJ6TEtPNHlsVmZNYzhvRHhQUkd6elcyMldDRi9oNm16bzVSdzN5RnQ4Qzk4bmNRRGVXcldrZlBFTjBndldLamd5UEdaYnlxaUs4T0RzZVM4NDdzUGVpUGlDNDM1aUhKUTBUUGMxVjVTRTVqcHlrUExoMmR3U3ZFL2RBM3lxR2F4ZUp2ZnV3WExldFc0aC8reWF4MEVsc3hHSDRudzI3ektpekU5dDhnTlMzRERYd3F6bUNWMkg0UDNRL3VoOGZpUE8vYk1LSFpGVjR2eGpDeEJqOE1aR2d4Uy95aW5FMmg3dzBtdUhLbUVUTFpzMkljeFMzV0ZoSXMzSWxYa3Rva2hwVXc2NzRSbW4wTXc3UXFvQmJvV2NyT290aG1zSFRERTljaGljdUk5bmpEa3MrVFl5NUhwa1gxaHR1aXNjdDg1cFBVV092K1pBdmVqc2J5dVlGS1licmw2UllwaUQ3RkMzbVNtcHhTSXdWYmdpb1VVSWhnWmZGUkdrRm1RL1JHOU1nNitVMTNsVVlSR0lncnoraEpGa05hRDVWUVVWbS9PZkdHdFprTEkrSnl5bVBHNkw1VndLNE5lenphaExCL2VPRmVlSkVBcWREbVJMZW1vSUpnY0hGQ3A2VzF5bEl4Y3A2NTR5NFd1WWZaL3dNaXNPbVoxc0NWWmF1Uks2TDN0VzR1cUQ2L1dKVWJvc29TSFJtTmxFcDhpbFVlYXZNZTBTWlZxR2RDdkZGMGVHSCthRGlVNG9NMTQ3MlRsODdWeUFZbXlTTG9pR1hwbW84UldFRlZhWVJJbDcvYjV2NXdNeU1CZ0FBIn0= -->

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["mean"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["sd"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["5.5%"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["94.5%"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["n_eff"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Rhat"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"-2.677","2":"0.340","3":"-3.237","4":"-2.153","5":"19839","6":"1","_rn_":"p"},{"1":"-0.828","2":"0.797","3":"-2.130","4":"0.419","5":"21524","6":"1","_rn_":"p_np"},{"1":"-0.518","2":"0.712","3":"-1.677","4":"0.590","5":"19860","6":"1","_rn_":"p_new"},{"1":"10.271","2":"0.027","3":"10.228","4":"10.313","5":"15281","6":"1","_rn_":"a"},{"1":"0.266","2":"0.034","3":"0.212","4":"0.319","5":"17908","6":"1","_rn_":"b_np"},{"1":"0.632","2":"0.045","3":"0.559","4":"0.705","5":"13706","6":"1","_rn_":"b_new"},{"1":"-0.061","2":"0.005","3":"-0.069","4":"-0.054","5":"7981","6":"1","_rn_":"b_white"},{"1":"-0.080","2":"0.005","3":"-0.088","4":"-0.072","5":"8264","6":"1","_rn_":"b_black"},{"1":"-0.043","2":"0.005","3":"-0.050","4":"-0.036","5":"8180","6":"1","_rn_":"b_latino"},{"1":"-0.098","2":"0.006","3":"-0.107","4":"-0.088","5":"8016","6":"1","_rn_":"b_asian"},{"1":"-0.028","2":"0.002","3":"-0.032","4":"-0.025","5":"14876","6":"1","_rn_":"b_bach"},{"1":"3.665","2":"0.124","3":"3.466","4":"3.865","5":"12585","6":"1","_rn_":"b_mhh"},{"1":"0.011","2":"0.003","3":"0.007","4":"0.015","5":"16463","6":"1","_rn_":"b_clint"},{"1":"-0.021","2":"0.001","3":"-0.023","4":"-0.019","5":"22129","6":"1","_rn_":"b_house"},{"1":"2.057","2":"0.283","3":"1.606","4":"2.508","5":"17927","6":"1","_rn_":"b_vpop"},{"1":"1589.999","2":"19.783","3":"1558.491","4":"1621.672","5":"22513","6":"1","_rn_":"scale"}],"options":{"columns":{"min":{},"max":[10],"total":[6]},"rows":{"min":[10],"max":[10],"total":[16]},"pages":{}}}
  </script>
</div>

<!-- rnb-frame-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Model of Business Individual Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDExKSwgZXhwKDE0KSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiTm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgSW50ZXJjZXB0XCIpICtcbiAgc2NhbGVfeF9jb250aW51b3VzKGxhYmVscyA9IGRvbGxhcl9mb3JtYXQoKSkgK1xuICB0aGVtZShheGlzLnRleHQueCA9IGVsZW1lbnRfdGV4dChhbmdsZSA9IDQ1LCB2anVzdCA9IDEsIGhqdXN0ID0gMSkpXG5cbiMjIFNsb3BlIFBhcmFtZXRlcnNcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocm5vcm0oMWU0LCBleHAoMCksIGV4cCgxMykpKSxcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcIm5vcm1hbFwiKSkgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFNsb3BlIFBhcmFtZXRlcnNcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSBcblxuIyMgU2NhbGUgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAxKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0xXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0yXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2NhbGUgUGFyYW1ldGVyXCIpXG5cbiMjIFByb2JhYmlsaXR5IFBhcmFtZXRlclxuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShydW5pZigxZTQsIDAsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJ1bmlmb3JtXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMiwgMikpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMlwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNSkgK1xuICAgIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMS41LCAxLjUpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJiZXRhIDEuNVwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNykgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFByb2JhYmlsaXR5IFBhcmFtZXRlclwiKVxuYGBgIn0= -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(11), exp(14))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(13))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgaW5kaXZfYml6W25dO1xuICBpbnQgbm9fY29ycF9wYWNzW25dO1xuICBpbnQgbmV3X21lbWJlcltuXTtcbiAgcmVhbCBwY3Rfd2hpdGVbbl07XG4gIHJlYWwgcGN0X2JsYWNrW25dO1xuICByZWFsIHBjdF9sYXRpbm9bbl07XG4gIHJlYWwgcGN0X2FzaWFuW25dO1xuICByZWFsIHBjdF9iYWNoZWxvcnNbbl07XG4gIHJlYWwgcGN0X2NsaW50b25fMTZbbl07XG4gIHJlYWwgcGN0X2RlbV9ob3VzZV8xNltuXTtcbiAgcmVhbCBsb2dfbWVkaWFuX2hoX2luY29tZVtuXTtcbiAgcmVhbCBsb2dfdG90X3ZvdGluZ19hZ2Vbbl07XG59XG5cbnBhcmFtZXRlcnMge1xuICByZWFsPGxvd2VyID0gMD4gYTtcbiAgcmVhbCBiX25wO1xuICByZWFsIGJfbmV3O1xuICByZWFsIGJfd2hpdGU7XG4gIHJlYWwgYl9ibGFjaztcbiAgcmVhbCBiX2xhdGlubztcbiAgcmVhbCBiX2FzaWFuO1xuICByZWFsIGJfYmFjaDtcbiAgcmVhbCBiX21oaDtcbiAgcmVhbCBiX2NsaW50O1xuICByZWFsIGJfaG91c2U7XG4gIHJlYWwgYl92cG9wO1xuICByZWFsPGxvd2VyID0gMD4gc2NhbGU7XG59XG5cbm1vZGVsIHtcbiAgdmVjdG9yW25dIG11O1xuICBcbiAgZm9yIChpIGluIDE6bikge1xuICAgIC8vIGxpbmVhciBtb2RlbFxuICAgIG11W2ldID0gYSArIGJfbnAgKiBub19jb3JwX3BhY3NbaV0gKyBiX25ldyAqIG5ld19tZW1iZXJbaV0gKyBcbiAgICAgIGJfd2hpdGUgKiBwY3Rfd2hpdGVbaV0gKyBiX2JsYWNrICogcGN0X2JsYWNrW2ldICsgXG4gICAgICBiX2xhdGlubyAqIHBjdF9sYXRpbm9baV0gKyBiX2FzaWFuICogcGN0X2FzaWFuW2ldICsgXG4gICAgICBiX2JhY2ggKiBwY3RfYmFjaGVsb3JzW2ldICsgYl9jbGludCAqIHBjdF9jbGludG9uXzE2W2ldICsgXG4gICAgICBiX2hvdXNlICogcGN0X2RlbV9ob3VzZV8xNltpXSArIGJfbWhoICogbG9nX21lZGlhbl9oaF9pbmNvbWVbaV0gK1xuICAgICAgYl92cG9wICogbG9nX3RvdF92b3RpbmdfYWdlW2ldO1xuICAgIG11W2ldID0gZXhwKG11W2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbiAgXG4gIC8vIHByaW9yc1xuICAvLyBHYW1tYSBtb2RlbFxuICBhIH4gbm9ybWFsKDExLCAxNCk7XG4gIGJfbnAgfiBub3JtYWwoMCwgMTMpO1xuICBiX25ldyB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfd2hpdGUgfiBub3JtYWwoMCwgMTMpO1xuICBiX2JsYWNrIH4gbm9ybWFsKDAsIDEzKTtcbiAgYl9sYXRpbm8gfiBub3JtYWwoMCwgMTMpO1xuICBiX2FzaWFuIH4gbm9ybWFsKDAsIDEzKTtcbiAgYl9iYWNoIH4gbm9ybWFsKDAsIDEzKTtcbiAgYl9jbGludCB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfaG91c2UgfiBub3JtYWwoMCwgMTMpO1xuICBiX3Zwb3AgfiBub3JtYWwoMCwgMTMpO1xuICBiX21oaCB+IG5vcm1hbCgwLCAxMyk7XG4gIHNjYWxlIH4gZXhwb25lbnRpYWwoMik7XG4gIFxuICAvLyBsaWtlbGlob29kXG4gIGZvciAoIGkgaW4gMTpuKSB7XG4gICAgaW5kaXZfYml6W2ldIH4gZ2FtbWEobXVbaV0vc2NhbGUsIDEvc2NhbGUpO1xuICAgIH1cbn1cblxuZ2VuZXJhdGVkIHF1YW50aXRpZXMge1xuICB2ZWN0b3Jbbl0gbXU7XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gIG11W2ldID0gYSArIGJfbnAgKiBub19jb3JwX3BhY3NbaV0gKyBiX25ldyAqIG5ld19tZW1iZXJbaV0gKyBcbiAgICAgIGJfd2hpdGUgKiBwY3Rfd2hpdGVbaV0gKyBiX2JsYWNrICogcGN0X2JsYWNrW2ldICsgXG4gICAgICBiX2xhdGlubyAqIHBjdF9sYXRpbm9baV0gKyBiX2FzaWFuICogcGN0X2FzaWFuW2ldICsgXG4gICAgICBiX2JhY2ggKiBwY3RfYmFjaGVsb3JzW2ldICsgYl9jbGludCAqIHBjdF9jbGludG9uXzE2W2ldICsgXG4gICAgICBiX2hvdXNlICogcGN0X2RlbV9ob3VzZV8xNltpXSArIGJfbWhoICogbG9nX21lZGlhbl9oaF9pbmNvbWVbaV0gK1xuICAgICAgYl92cG9wICogbG9nX3RvdF92b3RpbmdfYWdlW2ldO1xuICBtdVtpXSA9IGV4cChtdVtpXSk7IC8vIGludmVyc2UgbGluayBmdW5jdGlvblxuICB9XG59XG5gYGAifQ== -->

```stan
data {
  int<lower = 1> n;
  real indiv_biz[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  // Gamma model
  a ~ normal(11, 14);
  b_np ~ normal(0, 13);
  b_new ~ normal(0, 13);
  b_white ~ normal(0, 13);
  b_black ~ normal(0, 13);
  b_latino ~ normal(0, 13);
  b_asian ~ normal(0, 13);
  b_bach ~ normal(0, 13);
  b_clint ~ normal(0, 13);
  b_house ~ normal(0, 13);
  b_vpop ~ normal(0, 13);
  b_mhh ~ normal(0, 13);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    indiv_biz[i] ~ gamma(mu[i]/scale, 1/scale);
    }
}

generated quantities {
  vector[n] mu;
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-frame-begin eyJtZXRhZGF0YSI6eyJjbGFzc2VzIjpbImRhdGEuZnJhbWUiXSwibmNvbCI6NiwibnJvdyI6MTN9LCJyZGYiOiJINHNJQUFBQUFBQUFBN1ZVVzBnVVVSaWVuYjJvUTRwVWRIdUlRQW9oZGtGTXlIbzRBMW5SUTVSV0loRXVaMmRIWjNUMnpMWXp1bVpSMGhXU2lpaE1xSkJBQ2FOQ29zSWlIUzJKTU9oR1FkRkRTQThSRXBZaFhiSE96RGxueGdaODdHSDIrLzkvL3UrL3oxWlYxSlFLTlFMSGNVRXVHT0c1WUJpTFhIakg5ZzNSMVJ3WDRyRVM0RUpjbm8wdDJHa2VGaUw0S2NCUHZsajg5ZmI2UzZQRDROYjlVN3VLcHg2QTRkNWZIN1QyQ2l1ZDJQemlTKzl2NjJSLzVmaVJwZjJnamVxbmxVemZzcEVMREVWKzVIelArMjNmcmNPdFl4dXZ6OTlyN2J1YVc3TGk0eDR3V1IxYWRNYThKM2FQVEN6cEdzeXlmSUQ2Z2JNVU83RlR3LzZmUUprRmFSMWdLOFZyTjZJRjFhSEZyczV3bU5hekpvRER0bzU1L1kzemN6YmRyUVZYdWdhYkg5MHNCM2RxQ2pGajBqcUl6cDNZOG02dHhlcWcrU3pIbXR4cE9lcVB0eUx2dE5GZ0hTZDhpODRCZkNKekVydDNUOXA1dlh5MmVWMEhHS0J6ZlhoNTRpSXVtZFhKNWdSWXZLUEhWdG9Gc2ptTFFmeURKWmJIYXFGem1Cb2RPalFYZFlvOUM5OHN3QlEzMzZ0YTBWNjMrS3lsemNFbkhRUUgwaHpCZG9xUVlnN0JwOThJUHBZSnZqeEFlSzhCaWZlOGZZalkreHk3dTcvUFJQbmY2RHZjTUlJcDJjQkNvWE84eEJoS3lSQlJtVGVTekZvV0sxdk9hT1dyWmlnb0x0ZlZNYThxQlpxK0pIa1pQUnVibVNpZmZUYVFzUkp4bEdiaHNDeG5xWktUaUdjVjFaUTlOYUZCcVpHcXVZbTRCazBWNmQ1cmFLaHU3UkhzRFNYRmk1dFNGTTlUMGxSa2VxcWlOeG15UjJ4TzYyNUJoZ1ExMlQ4NFNZTUc2NGNaaFNRMFlhd3VnMXZGMnJTUGtxT25UVlZIbU1UYmZ4VUN4UUE5Z1lCWXlTN0NwdkYvZk1IRFdUVnBLaVR1UHkvNG9pSVN6Z2tWOEwwTlpIeUd3aVprN3lJWmxaUW0xQmd0S1oxQjVtYVJBODdTUEdSeXhGZXJ3RFplRldQTjB0NGxQWldTM1htSFpFbGhTNHZJcUY1RmJQUmhEU1pramJId3lSaXczdDErRm1hUWl1cXBXb0RQeXJtcVdEcmpyVkxBVmlObTZpWmtVUVJKMTVpRlRHLzZMekU5bk1QYkJRQUEifQ== -->

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["mean"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["sd"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["5.5%"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["94.5%"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["n_eff"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Rhat"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"12.476","2":"0.010","3":"12.459","4":"12.492","5":"21881","6":"1","_rn_":"a"},{"1":"0.085","2":"0.020","3":"0.052","4":"0.117","5":"16866","6":"1","_rn_":"b_np"},{"1":"0.138","2":"0.024","3":"0.099","4":"0.177","5":"15661","6":"1","_rn_":"b_new"},{"1":"-0.004","2":"0.003","3":"-0.009","4":"0.001","5":"7536","6":"1","_rn_":"b_white"},{"1":"-0.015","2":"0.003","3":"-0.020","4":"-0.010","5":"7563","6":"1","_rn_":"b_black"},{"1":"0.008","2":"0.003","3":"0.003","4":"0.013","5":"7521","6":"1","_rn_":"b_latino"},{"1":"-0.017","2":"0.004","3":"-0.023","4":"-0.011","5":"7431","6":"1","_rn_":"b_asian"},{"1":"-0.017","2":"0.001","3":"-0.019","4":"-0.015","5":"16366","6":"1","_rn_":"b_bach"},{"1":"2.346","2":"0.058","3":"2.252","4":"2.440","5":"14026","6":"1","_rn_":"b_mhh"},{"1":"-0.010","2":"0.001","3":"-0.013","4":"-0.008","5":"20990","6":"1","_rn_":"b_clint"},{"1":"-0.007","2":"0.001","3":"-0.008","4":"-0.006","5":"24829","6":"1","_rn_":"b_house"},{"1":"1.146","2":"0.136","3":"0.927","4":"1.362","5":"17967","6":"1","_rn_":"b_vpop"},{"1":"2658.467","2":"26.005","3":"2616.974","4":"2699.924","5":"21186","6":"1","_rn_":"scale"}],"options":{"columns":{"min":{},"max":[10],"total":[6]},"rows":{"min":[10],"max":[10],"total":[13]},"pages":{}}}
  </script>
</div>

<!-- rnb-frame-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Model of Labor Individual Contributions

### Simulating Priors


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBsaWtlbGlob29kIHNpbXVsYXRpb25cbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocmdhbW1hKDFlNCwgc2hhcGUgPSAyMCwgcmF0ZSA9IDEwMCkpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2xvcGUgUGFyYW1ldGVyc1wiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhicmVha3MgPSBzY2FsZXM6OnByZXR0eV9icmVha3MobiA9IDEwKSkgXG5cbiMgcHJpb3Igc2ltdWxhdGlvblxuIyMgSW50ZXJjZXB0XG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJub3JtKDFlNCwgZXhwKDExKSwgZXhwKDE0KSkpLFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwiTm9ybWFsXCIpKSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgSW50ZXJjZXB0XCIpICtcbiAgc2NhbGVfeF9jb250aW51b3VzKGxhYmVscyA9IGRvbGxhcl9mb3JtYXQoKSkgK1xuICB0aGVtZShheGlzLnRleHQueCA9IGVsZW1lbnRfdGV4dChhbmdsZSA9IDQ1LCB2anVzdCA9IDEsIGhqdXN0ID0gMSkpXG5cbiMjIFNsb3BlIFBhcmFtZXRlcnNcbmdncGxvdCgpICtcbiAgZ2VvbV9kZW5zaXR5KGRhdGEgPSBhc190aWJibGUocm5vcm0oMWU0LCBleHAoMCksIGV4cCgxMykpKSxcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcIm5vcm1hbFwiKSkgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFNsb3BlIFBhcmFtZXRlcnNcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gZG9sbGFyX2Zvcm1hdCgpKSBcblxuIyMgU2NhbGUgUGFyYW1ldGVyXG5nZ3Bsb3QoKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAxKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0xXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJleHAoMWU0LCAyKSksIFxuICAgICAgICAgICAgICAgYWVzKHggPSB2YWx1ZSwgZmlsbCA9IFwicmF0ZT0yXCIpLCBcbiAgICAgICAgICAgICAgIGFscGhhID0gMC41KSArXG4gIHRoZW1lX2NsYXNzaWMoKSArXG4gIGxhYnMoeCA9IFwiTWVhbiBhbmQgU0Qgb2YgU2NhbGUgUGFyYW1ldGVyXCIpXG5cbiMjIFByb2JhYmlsaXR5IFBhcmFtZXRlclxuZ2dwbG90KCkgK1xuICBnZW9tX2RlbnNpdHkoZGF0YSA9IGFzX3RpYmJsZShydW5pZigxZTQsIDAsIDEpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJ1bmlmb3JtXCIpKSArXG4gIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMiwgMikpLCBcbiAgICAgICAgICAgICAgIGFlcyh4ID0gdmFsdWUsIGZpbGwgPSBcImJldGEgMlwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNSkgK1xuICAgIGdlb21fZGVuc2l0eShkYXRhID0gYXNfdGliYmxlKHJiZXRhKDFlNCwgMS41LCAxLjUpKSwgXG4gICAgICAgICAgICAgICBhZXMoeCA9IHZhbHVlLCBmaWxsID0gXCJiZXRhIDEuNVwiKSwgXG4gICAgICAgICAgICAgICBhbHBoYSA9IDAuNykgK1xuICB0aGVtZV9jbGFzc2ljKCkgK1xuICBsYWJzKHggPSBcIk1lYW4gYW5kIFNEIG9mIFByb2JhYmlsaXR5IFBhcmFtZXRlclwiKVxuYGBgIn0= -->

```r
# likelihood simulation
ggplot() +
  geom_density(data = as_tibble(rgamma(1e4, shape = 20, rate = 100)), 
               aes(x = value)) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

# prior simulation
## Intercept
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(11), exp(14))),
               aes(x = value, fill = "Normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Intercept") +
  scale_x_continuous(labels = dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Slope Parameters
ggplot() +
  geom_density(data = as_tibble(rnorm(1e4, exp(0), exp(13))),
               aes(x = value, fill = "normal")) +
  theme_classic() +
  labs(x = "Mean and SD of Slope Parameters") +
  scale_x_continuous(labels = dollar_format()) 

## Scale Parameter
ggplot() +
  geom_density(data = as_tibble(rexp(1e4, 1)), 
               aes(x = value, fill = "rate=1")) +
  geom_density(data = as_tibble(rexp(1e4, 2)), 
               aes(x = value, fill = "rate=2"), 
               alpha = 0.5) +
  theme_classic() +
  labs(x = "Mean and SD of Scale Parameter")

## Probability Parameter
ggplot() +
  geom_density(data = as_tibble(runif(1e4, 0, 1)), 
               aes(x = value, fill = "uniform")) +
  geom_density(data = as_tibble(rbeta(1e4, 2, 2)), 
               aes(x = value, fill = "beta 2"), 
               alpha = 0.5) +
    geom_density(data = as_tibble(rbeta(1e4, 1.5, 1.5)), 
               aes(x = value, fill = "beta 1.5"), 
               alpha = 0.7) +
  theme_classic() +
  labs(x = "Mean and SD of Probability Parameter")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Model 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgc3RhblxuZGF0YSB7XG4gIGludDxsb3dlciA9IDE+IG47XG4gIHJlYWwgaW5kaXZfbGFib3Jbbl07XG4gIGludCBub19jb3JwX3BhY3Nbbl07XG4gIGludCBuZXdfbWVtYmVyW25dO1xuICByZWFsIHBjdF93aGl0ZVtuXTtcbiAgcmVhbCBwY3RfYmxhY2tbbl07XG4gIHJlYWwgcGN0X2xhdGlub1tuXTtcbiAgcmVhbCBwY3RfYXNpYW5bbl07XG4gIHJlYWwgcGN0X2JhY2hlbG9yc1tuXTtcbiAgcmVhbCBwY3RfY2xpbnRvbl8xNltuXTtcbiAgcmVhbCBwY3RfZGVtX2hvdXNlXzE2W25dO1xuICByZWFsIGxvZ19tZWRpYW5faGhfaW5jb21lW25dO1xuICByZWFsIGxvZ190b3Rfdm90aW5nX2FnZVtuXTtcbn1cblxucGFyYW1ldGVycyB7XG4gIHJlYWwgcDtcbiAgcmVhbCBwX25wO1xuICByZWFsIHBfbmV3O1xuICByZWFsPGxvd2VyID0gMD4gYTtcbiAgcmVhbCBiX25wO1xuICByZWFsIGJfbmV3O1xuICByZWFsIGJfd2hpdGU7XG4gIHJlYWwgYl9ibGFjaztcbiAgcmVhbCBiX2xhdGlubztcbiAgcmVhbCBiX2FzaWFuO1xuICByZWFsIGJfYmFjaDtcbiAgcmVhbCBiX21oaDtcbiAgcmVhbCBiX2NsaW50O1xuICByZWFsIGJfaG91c2U7XG4gIHJlYWwgYl92cG9wO1xuICByZWFsPGxvd2VyID0gMD4gc2NhbGU7XG59XG5cbm1vZGVsIHtcbiAgdmVjdG9yW25dIHByb2I7XG4gIHZlY3RvcltuXSBtdTtcbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgICAvLyBsaW5lYXIgbW9kZWxcbiAgICBwcm9iW2ldID0gcCArIHBfbnAgKiBub19jb3JwX3BhY3NbaV0gKyBwX25ldyAqIG5ld19tZW1iZXJbaV07IFxuICAgIHByb2JbaV0gPSBpbnZfbG9naXQocHJvYltpXSk7IC8vIGludmVyc2UgbGluayBmdW5jdGlvblxuICB9XG4gIFxuICBmb3IgKGkgaW4gMTpuKSB7XG4gICAgLy8gbGluZWFyIG1vZGVsXG4gICAgbXVbaV0gPSBhICsgYl9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIGJfbmV3ICogbmV3X21lbWJlcltpXSArIFxuICAgICAgYl93aGl0ZSAqIHBjdF93aGl0ZVtpXSArIGJfYmxhY2sgKiBwY3RfYmxhY2tbaV0gKyBcbiAgICAgIGJfbGF0aW5vICogcGN0X2xhdGlub1tpXSArIGJfYXNpYW4gKiBwY3RfYXNpYW5baV0gKyBcbiAgICAgIGJfYmFjaCAqIHBjdF9iYWNoZWxvcnNbaV0gKyBiX2NsaW50ICogcGN0X2NsaW50b25fMTZbaV0gKyBcbiAgICAgIGJfaG91c2UgKiBwY3RfZGVtX2hvdXNlXzE2W2ldICsgYl9taGggKiBsb2dfbWVkaWFuX2hoX2luY29tZVtpXSArXG4gICAgICBiX3Zwb3AgKiBsb2dfdG90X3ZvdGluZ19hZ2VbaV07XG4gICAgbXVbaV0gPSBleHAobXVbaV0pOyAvLyBpbnZlcnNlIGxpbmsgZnVuY3Rpb25cbiAgfVxuICBcbiAgLy8gcHJpb3JzXG4gIFxuICAvLyBCaW5vbWlhbCBtb2RlbFxuICBwIH4gbm9ybWFsKDAsIDEpO1xuICBwX25wIH4gbm9ybWFsKDAsIDEpO1xuICBwX25ldyB+IG5vcm1hbCgwLCAxKTtcbiAgXG4gIC8vIEdhbW1hIG1vZGVsXG4gIGEgfiBub3JtYWwoMTEsIDE0KTtcbiAgYl9ucCB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfbmV3IH4gbm9ybWFsKDAsIDEzKTtcbiAgYl93aGl0ZSB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfYmxhY2sgfiBub3JtYWwoMCwgMTMpO1xuICBiX2xhdGlubyB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfYXNpYW4gfiBub3JtYWwoMCwgMTMpO1xuICBiX2JhY2ggfiBub3JtYWwoMCwgMTMpO1xuICBiX2NsaW50IH4gbm9ybWFsKDAsIDEzKTtcbiAgYl9ob3VzZSB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfdnBvcCB+IG5vcm1hbCgwLCAxMyk7XG4gIGJfbWhoIH4gbm9ybWFsKDAsIDEzKTtcbiAgc2NhbGUgfiBleHBvbmVudGlhbCgyKTtcbiAgXG4gIC8vIGxpa2VsaWhvb2RcbiAgZm9yICggaSBpbiAxOm4pIHtcbiAgICBpZiAoaW5kaXZfbGFib3JbaV0gPT0gMClcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMXwgcHJvYltpXSkpO1xuICAgIGVsc2VcbiAgICAgIHRhcmdldCArPSAoYmVybm91bGxpX2xwbWYoMHwgcHJvYltpXSkgKyBnYW1tYV9scGRmKGluZGl2X2xhYm9yW2ldfCBtdVtpXS9zY2FsZSwgMS9zY2FsZSkpO1xuICAgIH0vL2lcbn1cblxuZ2VuZXJhdGVkIHF1YW50aXRpZXMge1xuICB2ZWN0b3Jbbl0gcHJvYjtcbiAgdmVjdG9yW25dIG11O1xuICBcbiAgZm9yIChpIGluIDE6bikge1xuICAgIC8vIGxpbmVhciBtb2RlbFxuICAgIHByb2JbaV0gPSBwICsgcF9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIHBfbmV3ICogbmV3X21lbWJlcltpXTsgXG4gICAgcHJvYltpXSA9IGludl9sb2dpdChwcm9iW2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbiAgXG4gIGZvciAoaSBpbiAxOm4pIHtcbiAgbXVbaV0gPSBhICsgYl9ucCAqIG5vX2NvcnBfcGFjc1tpXSArIGJfbmV3ICogbmV3X21lbWJlcltpXSArIFxuICAgICAgYl93aGl0ZSAqIHBjdF93aGl0ZVtpXSArIGJfYmxhY2sgKiBwY3RfYmxhY2tbaV0gKyBcbiAgICAgIGJfbGF0aW5vICogcGN0X2xhdGlub1tpXSArIGJfYXNpYW4gKiBwY3RfYXNpYW5baV0gKyBcbiAgICAgIGJfYmFjaCAqIHBjdF9iYWNoZWxvcnNbaV0gKyBiX2NsaW50ICogcGN0X2NsaW50b25fMTZbaV0gKyBcbiAgICAgIGJfaG91c2UgKiBwY3RfZGVtX2hvdXNlXzE2W2ldICsgYl9taGggKiBsb2dfbWVkaWFuX2hoX2luY29tZVtpXSArXG4gICAgICBiX3Zwb3AgKiBsb2dfdG90X3ZvdGluZ19hZ2VbaV07XG4gIG11W2ldID0gZXhwKG11W2ldKTsgLy8gaW52ZXJzZSBsaW5rIGZ1bmN0aW9uXG4gIH1cbn1cbmBgYCJ9 -->

```stan
data {
  int<lower = 1> n;
  real indiv_labor[n];
  int no_corp_pacs[n];
  int new_member[n];
  real pct_white[n];
  real pct_black[n];
  real pct_latino[n];
  real pct_asian[n];
  real pct_bachelors[n];
  real pct_clinton_16[n];
  real pct_dem_house_16[n];
  real log_median_hh_income[n];
  real log_tot_voting_age[n];
}

parameters {
  real p;
  real p_np;
  real p_new;
  real<lower = 0> a;
  real b_np;
  real b_new;
  real b_white;
  real b_black;
  real b_latino;
  real b_asian;
  real b_bach;
  real b_mhh;
  real b_clint;
  real b_house;
  real b_vpop;
  real<lower = 0> scale;
}

model {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
    // linear model
    mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
    mu[i] = exp(mu[i]); // inverse link function
  }
  
  // priors
  
  // Binomial model
  p ~ normal(0, 1);
  p_np ~ normal(0, 1);
  p_new ~ normal(0, 1);
  
  // Gamma model
  a ~ normal(11, 14);
  b_np ~ normal(0, 13);
  b_new ~ normal(0, 13);
  b_white ~ normal(0, 13);
  b_black ~ normal(0, 13);
  b_latino ~ normal(0, 13);
  b_asian ~ normal(0, 13);
  b_bach ~ normal(0, 13);
  b_clint ~ normal(0, 13);
  b_house ~ normal(0, 13);
  b_vpop ~ normal(0, 13);
  b_mhh ~ normal(0, 13);
  scale ~ exponential(2);
  
  // likelihood
  for ( i in 1:n) {
    if (indiv_labor[i] == 0)
      target += (bernoulli_lpmf(1| prob[i]));
    else
      target += (bernoulli_lpmf(0| prob[i]) + gamma_lpdf(indiv_labor[i]| mu[i]/scale, 1/scale));
    }//i
}

generated quantities {
  vector[n] prob;
  vector[n] mu;
  
  for (i in 1:n) {
    // linear model
    prob[i] = p + p_np * no_corp_pacs[i] + p_new * new_member[i]; 
    prob[i] = inv_logit(prob[i]); // inverse link function
  }
  
  for (i in 1:n) {
  mu[i] = a + b_np * no_corp_pacs[i] + b_new * new_member[i] + 
      b_white * pct_white[i] + b_black * pct_black[i] + 
      b_latino * pct_latino[i] + b_asian * pct_asian[i] + 
      b_bach * pct_bachelors[i] + b_clint * pct_clinton_16[i] + 
      b_house * pct_dem_house_16[i] + b_mhh * log_median_hh_income[i] +
      b_vpop * log_tot_voting_age[i];
  mu[i] = exp(mu[i]); // inverse link function
  }
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


### Estimation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-frame-begin eyJtZXRhZGF0YSI6eyJjbGFzc2VzIjpbImRhdGEuZnJhbWUiXSwibmNvbCI6NiwibnJvdyI6MTZ9LCJyZGYiOiJINHNJQUFBQUFBQUFBOFZVYTBnVVFSemZPKzlPWFVyRVNxUENENVgxUWU1RXlrb2lacU1YUlJIYWc2TVAyZDdlNnA3dTdSNTNxNmNWb2RDRGlENUVtQmhFVHlnam9pSjZxRFdXRnBwZ1pPS2pCeUlSMHR1ektLS3lablptN3E2RFByY3c5L3UvNS9mL3o4d1ZyM1F2NE4wOHgzRkpYSkxEeWlYWmtjalp0MnhlN1Z6Q2NUWXJVaXljalV2RldJMkNwaURCZ1ZZYVd1bmdiYVRweDZoNkNBN216M3RUM1RjQkg3emE5SzFuS0V2SVdtQis0UFM3ZmRtM2l0NkRTSWJXY0hqak1MeUpmcFo2dDBGNHQ2cnplcUViM25hbmV6YjBqc01XVkdSRmZSMW95anlRYTUyMERvNmZIZUNYQmM0QlkrZkltc3RUZDRGekJOdnNRMmFBc0RYU2lMN2pVUjZkUlhpall2RGkxK2NicTA1M2cyY2JlakV6Y0NkdnhzbTdWVjJnSGRQS1BBQzZtcmNYZ04wZndURmNaVzB6YURTLzQ2Qyt6akgzWmQ1TWNKTHdBdnVwZjlpazVRQjdLUS9HNXp1bWQzWkF5SkRNZnFNOFJtZy9FZHJuMjYyMjZVZU5WaUhUYUMzZjgvMDVmSVMzLzNBRWpHTHZyRXV3ellPSi9vUWRoRGU4aDhmNDZ3dnNRTTZVL1BuZ0ZQWC9wdk5qdktpOWJacEpMMGZZTWhjWE9CUGxNVTd6KzhuK1lKandFYkxvbkorWXg3WVNmTVZkOE12Z0pSUTk3MDBOdkdHR2w4TXJyM2NzUDdoNFBieDh5eHdzdUVwNURkQjVzSGxkd08xR0xzTHVoK2MvbmJqbUZOeTlxTXZaVHhnUG9YOU13TmRKNkh0S2NYNmJpVjFadFNhMnQzTW0zcjlKOEU0MXdSWklzRFdiMm5NSlBxNGgyR01qK2QycFJPOTFrcm85RFVRZnJESDE2RHpHaVBLL01PRWgyVFhSTDRjd01RNC9KbUswK1dWUm83STE1R1hXQWxkQkRrc3JYQmluYUNWeWFTbUxLbFpFSTJHVDFLQWVkc1Z2bE02ZWNZQmxCVW8wSnR1UkxJZFpoTWdpUEhFUm5yaUlaRTlKV1BFWmNrejFxS0pVUWRVVVQ0a3FHajVOajduRmtDL2FuUU5GaTVJU3ErdFhsRmlrcFBvMEk2WXFlbVZJamlWV0JmUW9vWkFrcW5MaWFDVlZETEdPbVpIM2lvYm9LZzJpWVNCdElpRWxXUThZUGwxRFNWYjg1OFpUdE5ETFl4R0thdU1PMGZvN29iZzk3UE1hQ3FuN2w4TTZadzRwWjVheUpIZ3R3UVJEZXFXR1Q4dnJsSlJLcmNLWnZ5Z3VtZnVIak5ma3VPSk1kaVJ3NWRtZEtIYXhabW52a3U3M3k5RjUyMlJKWVlmbWtMVXluOFpHYjFkRmo2eXlMSFNwUW1KWjlQVERZbER6YVdWVVRVTVh6N3gzcmtBd2RwUThzb1pjaG02SXJBb3Y2U3F6a09sTi9BR2cxUWltalFZQUFBPT0ifQ== -->

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["mean"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["sd"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["5.5%"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["94.5%"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["n_eff"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Rhat"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"0.842","2":"0.190","3":"0.542","4":"1.147","5":"23489","6":"1","_rn_":"p"},{"1":"-0.378","2":"0.453","3":"-1.097","4":"0.349","5":"21325","6":"1","_rn_":"p_np"},{"1":"-0.171","2":"0.411","3":"-0.823","4":"0.493","5":"20639","6":"1","_rn_":"p_new"},{"1":"5.800","2":"0.114","3":"5.614","4":"5.976","5":"12847","6":"1","_rn_":"a"},{"1":"0.035","2":"0.151","3":"-0.206","4":"0.276","5":"10118","6":"1","_rn_":"b_np"},{"1":"1.067","2":"0.201","3":"0.746","4":"1.390","5":"9582","6":"1","_rn_":"b_new"},{"1":"-0.092","2":"0.022","3":"-0.128","4":"-0.056","5":"7544","6":"1","_rn_":"b_white"},{"1":"-0.124","2":"0.025","3":"-0.164","4":"-0.086","5":"7103","6":"1","_rn_":"b_black"},{"1":"-0.099","2":"0.021","3":"-0.134","4":"-0.066","5":"7198","6":"1","_rn_":"b_latino"},{"1":"-0.109","2":"0.031","3":"-0.159","4":"-0.060","5":"7467","6":"1","_rn_":"b_asian"},{"1":"0.049","2":"0.011","3":"0.032","4":"0.067","5":"16114","6":"1","_rn_":"b_bach"},{"1":"-1.165","2":"0.491","3":"-1.942","4":"-0.366","5":"15369","6":"1","_rn_":"b_mhh"},{"1":"0.005","2":"0.010","3":"-0.011","4":"0.021","5":"13842","6":"1","_rn_":"b_clint"},{"1":"0.040","2":"0.005","3":"0.032","4":"0.047","5":"18615","6":"1","_rn_":"b_house"},{"1":"-2.731","2":"1.615","3":"-5.386","4":"-0.217","5":"15662","6":"1","_rn_":"b_vpop"},{"1":"91.775","2":"4.597","3":"84.578","4":"99.289","5":"25063","6":"1","_rn_":"scale"}],"options":{"columns":{"min":{},"max":[10],"total":[6]},"rows":{"min":[10],"max":[10],"total":[16]},"pages":{}}}
  </script>
</div>

<!-- rnb-frame-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# Results Summary

## Load Models and Extract Posteriors 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc2F2ZShiaXppbmR2LmZpdCwgZmlsZSA9IFwiRTovUHJvamVjdC9iaXppbmR2X2ZpdC5SZGFcIilcblxuYGBgIn0= -->

```r
save(bizindv.fit, file = "E:/Project/bizindv_fit.Rda")

```

<!-- rnb-source-end -->

<!-- rnb-output-begin eyJkYXRhIjoiRXJyb3IgaW4gZ3pmaWxlKGZpbGUsIFwid2JcIikgOiBjYW5ub3Qgb3BlbiB0aGUgY29ubmVjdGlvblxuIn0= -->

```
Error in gzfile(file, "wb") : cannot open the connection
```



<!-- rnb-output-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## PAC Results


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-plot-begin eyJjb25kaXRpb25zIjpbXSwiaGVpZ2h0Ijo0MzIuNjMyOSwic2l6ZV9iZWhhdmlvciI6MCwid2lkdGgiOjcwMH0= -->

<img src="data:image/png;base64,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rntuqfx7/Gr572eb5MOfdsoP/zsgi9elCAYlUey6zpgl7QngzcAHP+6UG19YKj8tSg7K6Z4DwsL7+eefS0REhBQVFWmr49dffy29e/eWNWvWSPv27e3vmod7zZo105bJ2rUDI2THnXfeKZdcconubXFxsaSkpAjE6oABAzQXiFXb9MUXX0jXrl1lypQpMmbMmHKtts8995y8/fbbctddd8nTTz8tjRo10pbad955R3AN3IeK0q233qpfPeLBYeDAgfLpp59q8Yt7dccdd1R0GvNJgARIIOQI5KsoCLDIwlILq+zhVBURQUVCgE9tslrjOFP1EshIXi0p23+XnNTdEhYRKbUbd5Ymna+W+PqnVO+F/bx2PGyN/2abTFETVyCiw9W9mkt8rYCQipWSDYheQEzBimmke++9V5o0aSI//fSTFmZGflXWnTt31mKwKnX48tzzzz9fbr/9drtLXnrppXL22Wfr14BPPfWU9diqVav0xA7Lli2Tnj17yq+//qqtwdYCamPixIkyduxYeffddwWTQRjpggsukMaNG8sjjzwit9xyixbUxjFjnZycLHv37pVx48ZpkYz8c845R15++WXJy+OgCIMT1yRAAsFNAK+HM7IKlb9sgZzIKpLjyu0gVcWtxTpFCYnjygXhmNqmJbZmPwcHVn0uRzdNF7NZPViEWV50F2QdkxN7l0rbCx+WBu371mwD/eDqEL7vTdshn83cLQMuaCaDlPAN9Bi+ASF4He99YmKiREdHa6uvcaxbt27y+OOPa1Fm5A0fPlzS0tLk448/1lkffPCBFnQHDx6UDh06CKykOAduAStXrpSHH35YZsyYoX1PIdbgx3r66afL+PHjtaDD6/oJEyboV/3GNTZu3ChPPvmkrF69WurXry833nijjBo1yirQ4WLwn//8R1tdMfCiT58+2hJbr149XUVlx43ruLJGW/FgsGvXLrvisO526tRJW1xhGYf1Fe4Ptunnn38WiFtbsWscf+ihh7R1HQ8Z5aW6devqe+HoDlKZ+0R5dTGPBEiABPyBAGYLQ8zZk8on9mR2oZ5c4aSaYCFDbVvW9vnpSugyRq0/3DnnbUjbs1iObPpZlLOkFrtmEwbzqb2IaCkxFcjeZR9KnLLyxtVt7byiEDmam2/SLg5wc8AUxZec10R6dWko7ZoH3nTFASF4YSU0mUz6tTpe3UOA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OX78uF+3k40jARIIDAIUvIFxn9hKEiABEiABEiABEiABDwlQ8HoIjqeRAAmQAAmQAAmQAAkEBgEK3sC4T2wlCZAACZAACZAACZCAhwQoeD0Ex9NIgARIgARIgARIgAQCgwCnFg6M+8RWkgAJkEBIEYiMjJThw4dLTExMSPWbnSUBEqgeAhS81cOVtZIACZAACVSRQEJCQhVr4OkkQAIkYCFAlwZ+EkiABEiABEiABEiABIKaAAVvUN9edo4ESIAESIAESIAESICCl58BEiABEiABEiABEiCBoCZAwRvUt5edIwESIIHAJICZ1g4cOCDZ2dmB2QG2mgRIwK8IUPD61e1gY0iABEiABEDAZDLJRx99JFu2bCEQEiABEqgyAQreKiNkBSRAAiRAAiRAAiRAAv5MgILXn+8O20YCJEACJEACJEACJFBlAhS8VUbICkiABEiABEiABEiABPyZAAWvP98dto0ESIAESIAESIAESKDKBCh4q4yQFZAACZAACXibQHh4uPTv319atmzp7apZHwmQQAgS4NTCIXjT2WUSIAES8HcCELx9+vTx92ayfSRAAgFCgBbeALlRbCYJkAAJkAAJkAAJkIBnBCh4PePGs0iABEiABEiABEiABAKEAAVvgNwoNpMESIAESIAESIAESMAzAhS8nnHjWSRAAiRAAiRAAiRAAgFCgII3QG4Um0kCJEACoUQAUwu/+eabsnHjxlDqNvtKAiRQTQQoeKsJLKslARIgARLwnIDZbJbU1FTJy8vzvBKeSQIkQAKlBCh4+VEgARIgARIgARIgARIIagIUvEF9e9k5EiABEiABEiABEiABCl5+BkiABEiABEiABEiABIKagEczrWEwwb59+yQyMlKaNm0qUVFREhYWFtSg2DkSIAESIAHfEcBvSoMGDaRWrVq+uyivRAIkELQE3LLwQui+8sorkpSUJO3bt5f3339fVqxYIRdddJHs2LEjaCGxYyRAAiRAAr4lEBERIcOGDZMzzzzTtxfm1UiABIKSgFsW3tGjR8vbb78tQ4YMkXXr1mkgrVu3lqysLOnRo4ccO3ZM8CXFRAIkQAIkQAIkQAIkQAL+QsBlCy9CxIwfP16mTJmi123atNF9gOBduXKlhIeHy/Lly/2lX2wHCZAACZAACZAACZAACWgCLgveI0eOSHZ2trbkOrKDD2/Xrl1l2bJl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aCR5Jvup7Kv3iva1l5SY5b+TNsrxkwVyq5qUgokESCBwCXjFhzdQu//SSy9JWFhYGTcNd/tTu3ZteeONN9w9zaPyR44c0W1Gu40FVvaGDRvKtddeK3/99Ve59b766qu6/DPPPFPucSPzzTfflF69eklSUpK0adNG4uPj5b777pPKHnZWrFghZ5xxhnTv3l22bt2qp5u+4oorpKSEAz8MtlyTgL8SKCwqkTeUNdNfxG5WylY5snmmzF+xS/YfOCBmc819j2AQ2/vTdsizE9ZLRnahv95CtosESKASAi6b7uCru3LlSoGwWb9+vbRr10569OihBQ7j71ZCuRoO33nnnXLJJZfomouLi/WgQYhVTA6yZs0aLVZtL/vFF19olxOElhszZky5Vlu4rLz99ts6xNzTTz8tjRo10pbad955R3CNzz//3LZKu+1bb71VWrVqJXPnzpWBAwdqFxeI36+//lruuOMOu7LcIQES8A8CWblFMv/vFPny972CmdRqOhXkpMruRW9I9rGt/4hc9XAfV6+ttO/zpMQmtayxJi5elyIbdqXL0Js6yYALmtZYO3hhEiABzwi4JHi3bNkil112mRw6dEhbCeGzi1fWY8eOlejoaPnuu+/kmmuu8awFPMsjAueff77cfvvtdudeeumlcvbZZ8u0adPkqaeesh5btWqVbNu2TU8Q0rNnT/n111+1NdhaQG1MnDhR3893331XHn30UeuhCy64QE8n/cgjj8gtt9yiBbX1YOlGcnKy7N27V8aNG6dFMrLPOeccefnll8v4fDuey30SIAHfEth5MEsWrkmRv7akydb9meotjO8mdnDWU1Nhrmz/fYTknTwoYeERqmi4/r1RfyQ3bY9s+/1F6XzVGxIdX99ZNdV6LCO7SF6atEl+UDOy3T+ovXTrXHNtqdaOsnISCEIClbo04BU5XnHXrVtX/ve//+lX2wfUK6acnBzZuXOn3HTTTXr59ttvgxCPSEFBgcDyeeaZZ+rX+x07dpTHHntMcnNz7fqblZWlLZmYce7//u//5JNPPrE7bjKZ5K233pLTTjtN4AIBwfrjjz9ayyxdulRbzOfMmaMHAWKK5hMnTliPu7Jx+umnC9wbdu2yDyUE626nTp20Nb53797a+upY388//ywQt7Zi1yjz0EMP6bYj1nJ5CZ+NiIgI2bNnj91huE888MADdnnc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fWTHSIAEfEeAgtd3rHklEiABEiABFwmYzWbJzMyUwsJCF89gMRIgARKomAAFb8VseIQESIAESIAESIAESCAICFDwBsFNZBdIgARIgARIgARIgAQqJkDBWzEbHiEBEiABEiABEiABEggCAhS8QXAT2QUSIAESCDYCYWFh0qxZM4mPjw+2rrE/JEACNUCAUwvXAHRekgRIgARIwDmBiIgIeeSRR5wX4lESIAEScJEALbwugmIxEiABEiABEiABEiCBwCRAwRuY942tJgESIAESIAESIAEScJEABa+LoFiMBEiABEiABEiABEggMAlQ8AbmfWOrSYAESIAESIAESIAEXCRAwesiKBYjARIgARLwHYGSkhKZO3euHDp0yHcX5ZVIgASClgAFb9DeWnaMBEiABAKXAATvggULKHgD9xay5STgVwQoeP3qdrAxJEACJEACJEACJEAC3iZAwettoqyPBEiABEiABEiABEjArwhQ8PrV7WBjSIAESIAESIAESIAEvE2AgtfbRFkfCZAACZAACZAACZCAXxHg1MJ+dTvYGBIgARIgARDA1MKPPvqo1KlTh0BIgARIoMoEKHirjJAVkAAJkAAJeJtAWFiYNG3a1NvVsj4SIIEQJUCXhhC98ew2CZAACZAACZAACYQKAQreULnT7CcJkAAJkAAJkAAJhCgBCt4QvfHsNgmQAAmQAAmQAAmECgEK3lC50+wnCZAACQQYgfT0dCkoKAiwVrO5JEAC/kiAgtcf7wrbRAIkQAIhTqC4uFhef/11Wb9+fYiTYPdJgAS8QYCC1xsUWQcJkAAJkAAJkAAJkIDfEqDg9dtbw4aRAAmQAAmQAAmQAAl4gwAFrzcosg4SIAESIAESIAESIAG/JUDB67e3hg0jARIgARIgARIgARLwBgEKXm9QZB0kQAIkQAJeJRAeHi49evSQJk2aeLVeVkYCJBCaBDi1cGjed/aaBEiABPyaAATvlVde6ddtZONIgAQChwAtvIFzr9hSEiABEiABEiABEiABDwhQ8HoAjaeQAAmQAAmQAAmQAAkEDgEK3sC5V2wpCZAACZAACZAACZCABwQoeD2AxlNIgARIgARIgARIgAQChwAFb+DcK7aUBEiABEKGgMlkkk8++US2bdsWMn1mR0mABKqPAAVv9bFlzSRAAiRAAh4SMJvNsnfvXsnMzPSwBp5GAiRA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wPvvvx9OO+208OCDD9ZJoc0iIAIiUJ2AFN7qrJRSBERABERABERABESgBwlI4e3BSlORRUAEREAEREAEREAEqhOQwludlVKKgAiIgAiIgAiIgAj0IAEpvD1YaSqyCIiACIiACIiACIhAdQJSeKuzUkoREAEREIFBIjDNNNOEddZZJyy44IKDdEadRgREYDgT0NTCw7l2dW0iIAIi0KMEUHjHjx/fo6VXsUVABLqNgCy83VYjKo8IiIAIiIAIiIAIiEBHCUjh7ShOZSYCIiACIiACIiACItBtBKTwdluNqDwiIAIiIAIiIAIiIAIdJSCFt6M4lZkIiIAIiIAIiIAIiEC3EZDC2201ovKIgAiIgAgEphY+6aSTwn333ScaIiACItA2ASm8bSNUBiIgAiIgAp0mEGMML7/8cnj77bc7nbXyEwERGIEEpPCOwErXJYuACIiACIiACIjASCIghXck1bauVQREQAREQAREQARGIAEpvCOw0nXJIiACIiACIiACIjCSCEjhHUm1rWsVAREQgR4hMGrUqDDHHHOE0aNH90iJVUwREIFuJqCphbu5dlQ2ERABERihBD7wgQ+E/fbbb4RevS5bBESg0wRk4e00UeUnAiIgAiIgAiIgAiLQVQSk8HZVdagwIiACIiACIiACIiACnSYghbfTRJWfCIiACIiACIiACIhAVxGQwttV1aHCiIAIiIAIiIAIiIAIdJqAFN5OE1V+IiACIiACbRP473//G6ZMmRJeeOGFtvNSBiIgAiIghVf3gAiIgAiIQNcRQOG9+uqrw7PPPtt1ZVOBREAEeo+AFN7eqzOVWAREQAREQAREQAREoAUCUnhbgKWkIiACIiACIiACIiACvUdACm/v1ZlKLAIiIAIiIAIiIAIi0AIBzbTWAiwlHRgCr776ajjmmGMGJnPlKgIi0JME3n///XDrrbcG2ofrr7++J69BhRYBERg8At/61rcCMzTWk1HRpN5ObReBgSbw5JNPhgkTJoSXXnqp6almm2228N5774U333yzaVolGBwCs8wyi5/otddeG5wT6ixNCcw000xh2mmnDf/85z+bpu32BOn1NGrUqG4vasPyzTjjjIG/V155pWE67Rw8AjPMMEP40Ic+5M8JAyQlQ0+Adot3yuuvv+7v+lZL9PLLL4fpp5++7mGy8NZFox2DQWCRRRYJjz76aKVTjRs3LvB3wgknVEqvRANPYOLEif5FfcEFFwz8yXSGSgT22WefcMcdd4Snn366UnolGngCJ554Yjj99NNDsxfywJdEZ0gELrzwwnDooYeGO++8M/Aekgw9galTp4YddtghXHLJJWHVVVfteIHkw9txpMpQBERABERABERABESgmwhI4e2m2lBZREAEREAEREAEREAEOk5APrwdR6oMB4rAAw88EGaeeeaw0EILDdQplG+LBB577LGAf+Viiy3W4pFKPlAEcGXAz33ppZceqFMo3xYJvPjiiz5OYezYsf68tHi4kg8AgX/84x/hr3/9a1hyySUD/rySoSfwxhtvhCeeeMJdTHjXd1qk8HaaqPITAREQAREQAREQARHoKgJyaeiq6lBh0qjsMhLvvvuuD/oo20foor/85S9lu7StRQJVRiw3qidGor/99tulZ6WONFK9FE3bG1UnbSNsKYNGvFNGjdKoPUuUOvfbbtulOulMXdRr/1PujZ6LKmn6+46Rwpvo6ndICfz0pz8N48ePDx/84AfDSiutFG6++eaa8rA+55xzhuWXXz5suummfRRfRnb+6le/qjlGK9UJPPjgg2G99dbzkDDUwQorrBCuu+66mgzobiLO4eKLLx4+/OEPh8022yzQLZgX9i+44ILh4x//eDjiiCPyu1zRpf74OJG0RgAXhUUXXTR87Wtf63Ngs2dHddIHWVsbiAm84oorepixVVZZJUyaNCkUX+DN6kTtWVtV0Odg2qpll13WXRPmmmuusNdee4W33nqrJp3qpAbHgK2cffbZ/q4unqDK+6NKmrbaM3tQJSIwpAR+97vfRYudF+3FEe+666642267xdGjR8d77rknK9eGG24YDz/88GjB6OM666wTf/zjH2f7OGbuueeO9rBk27RQnYAprXH++eePpozGn//85/Gaa66J66+/fpxuuumihezJMvr6178eTemK9nKJ1NkyyywT7SUTzariaSwWrx9jynP829/+Fs0vzn9TBgcddFDcdttt06p+WyCw++67Ey897rHHHjVHNXt2VCc1uNpeufHGG6MFto/24RFvv/12b5Nouy699NIs72Z1QkK1Zxmuthdob2hrNt5443jLLbfEU045JZr/Z9xpp52yvFUnGYoBXbBwYv4ut5jTfc7T7P3BAc3StNue8WUqEYEhJbDUUkvFrbbaqqYMn/jE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" />

<!-- rnb-plot-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Party Results


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-plot-begin eyJjb25kaXRpb25zIjpbXSwiaGVpZ2h0Ijo0MzIuNjMyOSwic2l6ZV9iZWhhdmlvciI6MCwid2lkdGgiOjcwMH0= -->

<img src="data:image/png;base64,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AggggAACCCCAAAIIIIAAAggggAACCCBAgJd7AAEEEEAAAQQQQAABBBBAAAEEEEAAAQQQyFMBArx5euHoNgIIIIAAAggggAACCCCAAAIIIIAAAgggQICXewABBBBAAAEEEEAAAQQQQAABBBBAAAEEEMhTAQK8eXrh6DYCCCCAAAIIIIAAAggggAACCCCAAAIIIECAl3sAAQQQQAABBBBAAAEEEEAAAQQQQAABBBDIUwECvHl64eg2AggggAACCCCAAAIIIIAAAggggAACCCBAgJd7AAEEEEAAAQQQQAABBBBAAAEEEEAAAQQQyFMBArx5euHoNgIIIIAAAggggAACCCCAAAIIIIAAAgggQICXewABBBBAAAEEEEAAAQQQQAABBBBAAAEEEMhTAQK8eXrh6DYCCCCAAAIIIIAAAggggAACCCCAAAIIIECAl3sAAQQQQAABBBBAAAEEEEAAAQQQQAABBBDIUwECvHl64eg2AggggAACCCCAAAIIIIAAAggggAACCCBAgJd7AAEEEEAAAQQQQAABBBBAAAEEEEAAAQQQyFMBArx5euHoNgIIIIAAAggggAACCCCAAAIIIIAAAgggQICXewABBBBAAAEEEEAAAQQQQAABBBBAAAEEEMhTAQK8eXrh6DYCCCCAAAIIIIAAAggggAACCCCAAAIIIECAl3sAAQQQQAABBBBAAAEEEEAAAQQQQAABBBDIUwECvHl64eg2AggggAACCCCAAAIIIIAAAggggAACCCBAgJd7AAEEEEAAAQQQQAABBBBAAAEEEEAAAQQQyFMBArx5euHoNgIIIIAAAggggAACCCCAAAIIIIAAAgggQICXewABBBBAAAEEEEAAAQQQQAABBBBAAAEEEMhTAQK8eXrh6DYCCCCAAAIIIIAAAggggAACCCCAAAIIIECAl3sAAQQQQAABBBBAAAEEEEAAAQQQQAABBBDIUwECvHl64eg2AggggAACCCCAAAIIIIAAAggggAACCCBAgJd7AAEEEEAAAQQQQAABBBBAAAEEEEAAAQQQyFMBArx5euHoNgIIIIAAAggggAACCCCAAAIIIIAAAgggUAIBAggggED+C6xfv146deoU90RCoZCUlZVJeXm5bLXVVtKvXz855ZRT5Fe/+lXc/errxgMOOEBmzpwpJSUl8u2334Z1s3fv3rJgwQJp0aKFfPnl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BBBAAAEEEEAAAQQQyJEAAd4cwXNYBBBAAAEEEEAAAQQQQAABBBBAAAEEEEAgXQECvOkKsj8CCCCAAAIIIIAAAggggAACCCCAAAIIIJAjAQK8OYLnsAgggAACCCCAAAIIIIAAAggggAACCCCAQLoCBHjTFWR/BBBAAAEEEEAAAQQQQAABBBBAAAEEEEAgRwIEeHMEz2ERQAABBBBAAAEEEEAAAQQQQAABBBBAAIF0BQjwpivI/ggggAACCCCAAAIIIIAAAggggAACCCCAQI4ECPDmCJ7DIoAAAggggAACCCCAAAIIIIAAAggggAAC6QoQ4E1XkP0RQAABBBBAAAEEEEAAAQQQQAABBBBAAIEcCRDgzRE8h0UAAQQQQAABBBBAAAEEEEAAAQQQQAABBNIVIMCbriD7I4AAAggggAACCCCAAAIIIIAAAggggAACORIgwJsjeA6LAAIIIIAAAggggAACCCCAAAIIIIAAAgikK0CAN11B9kcAAQQQQAABBBBAAAEEEEAAAQQQQAABBHIkQIA3R/AcFgEEEEAAAQQQQAABBBBAAAEEEEAAAQQQSFegQQZ4q6urpbKyMl079kcAAQQQQAABBBBAAAEEEEAAAQQQQAABBHIq0CADvKNHj5bS0lKZN29eTvE5OAIIIIAAAggggAACCCCAAAIIIIAAAgggkI5ASTo759O+OmJ38uTJ8u6778qzzz5ru37llVfKfvvtJwcccIC0b98+n06HviKAAAIIIIAAAggggAACCCCAAAIIIIAAAhLyTCl0h5deeklGjBghn3/+edRTLS8vlwsuuECuvvpqKSsri1on1sp33nlH9t57b2nVqpUsWbIkVrUa6zXI/Mgjj8isWbPsjx53hx12kB133FHOOecc6dmzZ419UllRVVUlDzzwgDz66KP2OCtWrJA+ffrIXnvtJYcccoj06tUroWYz1c7XX38tN9xwg3zwwQeiy9tss43079/f9mfIkCGy2WabJdQfKiGAAAIIIIAAAggggAACCCCAAAIIIICAFH6A96233pKBAweKBii1NGvWzAZj58+fL61btw4Lyp5++uly3333JXxfLFu2TPr16ydfffWVbLHFFmFtxWrkm2++kbPOOktefvnlWFWkuLhYzjvvPBkzZkxaAc+FCxfK4MGDZebMmVGPVVJSIuPHj5eTTjop6na3MlPt3HLLLTJq1CipqKhwTYc97rnnnnZ0tQbLKQgggAACCCCAAAIIIIAAAggggAACCCBQu0CxGbV6de3V8rOGjqg98MADZfny5RIKheSKK66QZ555Rn766SebruHDDz+ULl26yLRp02TdunUyffp06dSpk+y22261nrCOhD3ooIPkk08+sXWbNGkil1xySdz99Bj777+/TJ061dZr27atnHnmmaKBZU0Toc8//fRTG4zWPn377bfym9/8Jm6bsTZq/7RN1z8dEXzGGWfICSecIM2bN5fZs2fLhg0b5N///rcNdOuo3mglU+1oIPkPf/iD6AR3OmL6xBNPtCOVd911V9Fj/PDDD/Z8n3vuOTn66KOladOm0brDOgQQQAABBBBAAAEEEEAAAQQQQAABBBAICmiKhkItt956q6afsD/Dhw/3T/Oyyy6z6+bOnWvXmZGlfj0zItevF2vBpGXwTCoFfx89hhnBG6u6v96MyvX3GTRokGcCzf42tzBjxgxvyy239Os9+eSTblNSjxdddJHfhkl94K1fvz5sfzOy2WvRooWtY0byemaUbth29yQT7fz4449e48aN7bH0mK+//rpr3j6aQLN33HHH+f0999xzw7bzBAEEEEAAAQQQQAABBBBAAAEEEEAAAQSiCxQFg72FtqwjdF25+OKL3WKNx1NOOcWOKtUNOnJWR5NGK6tXr5aRI0fKgAEDYubzjbafrtNJ3jQXrhZNQfDggw/K5ptvbp8H/9ERrffee6+/6v777/eXE11YunSp3HPPPbZ6x44dZcKECTVyC+s5TJw40dbRvgWP6Y6TqXbGjh0ra9eutc3edNNNss8++7hD2MfS0lJ5+OGHpXfv3vb5Qw89JCtXrgyrwxMEEEAAAQQQQAABBBBAAAEEEEAAAQQQqClQ0AHejz/+2J6xTmDWoUOHmmf/yxrNnztp0iR5++23xYw2FTOCtkZdDfx2795dbrvtNptmoKioyOaTbdeuXY260Va8//77ogFiLUceeWTUY7j9DjvsMD/3bjBI7bbX9mhG/cqqVatstbPPPrtGcNftr8fRid20aIA3MjduptoZN26cPYbmPz711FPtcuQ/6nnhhRfa1dp3DfJSEEAAAQQQQAABBBBAAAEEEEAAAQQQQCC+QEEHeDt37mzPXnPNfv/993ElNFdv//79bT7aaBU1d++cOXPspq233tpOknb99deLTlSmRXP8xiuaZ1aPsfPOO8see+wRr6posFPz8WpZvHixmPQKcetHbpwyZYq/SvMExyuap1fLd999J6+99lpY1Uy0Y9Jg+PY6cleD7bGK5id2jjqiN51y1113yXXXXSd6jSgIIIAAAggggAACCCCAAAIIIIAAAggUqkBBB3j33ntv/7rdcccd/nKqCzrSVwOGn3/+uQwcODCpZjTQ+uKLL8rMmTPtZGPxdtZJ4TQwqkVH2DZq1MguJ/qPm8RNA8U9evSIu5umhHBF+xYsmWjHtaHt6kRv8UqbNm1kq622slUi+xJvv2jbNC2ETqp35ZVXRtvMOgQQQAABBBBAAAEEEEAAAQQQQAABBApCoKADvMERoTfccINce+21SY+GdVfZTFRmg66jRo2S5s2bu9VZedR0CSZlsm27T58+SR9j9uzZdp/27duL5reNVzRHrytffPGFW7SPmWhn1qxZfptuRLW/IsqC64/m4F20aFGUGqxCAAEEEEAAAQQQQAABBBBAAAEEEEAAASewMb+Ae1Zgj27CsjPPPNPmzdXRnLfffrvoSFEty5Ytk06dOiV01pp/ty6KpkpwaQV0BK72PZmieX6rqqrsLtFyCUe25Sx0vU6q5kqm2tHUFK6k0h9NhxGraH5iTZ0RqzRt2lTWrFkTazPrEUAAAQQQQAABBBBAAAEEEEAAAQQQyHuBgg7w6tUZNmyYvUjnn3++rFu3zua01by2WnbffXc7cdpRRx0lw4cPF03BkMuiqRkOPvhg0UctI0aMkL322iupLrl9dafGjRvXum+wTjAYWt/aiXYiOjo4Vq5ezeWr5xY8p2htsA4BBBBAAAEEEEAAAQQQQAABBBBAAIF8FijoFA3uwmiQV0fG3n333RJMeaBpED7++GO55pprZKeddpInn3zS7VLnj2vXrpUjjzxSPvroI3vsbt26+SN5k+mMpjZwpby83C3GfAzm9w0GQ+tbOzFPgA0IIIAAAggggAACCCCAAAIIIIAAAgg0YIGCH8Hrrm3Lli3l7LPPtj8jR46U2267TQYMGCDvvPOOTd/w448/iubZffDBB+Xkk092u9XJ45IlS+SII46QKVOm2ONpHtr//e9/CY3AjexgMOduZWVl5OYaz4N1ggHh+tZOjY6bFTvuuGPMFBb33HMPo3ejobEOAQQQQAABBBBAAAEEEEAAAQQQQKCgBBpMgDd41VxagokTJ9rJzDR9g8vlesEFF8igQYMkkXyxwTZTXdaJzDQtg5vQTCcie+WVVySRCcmiHVPzzrqiKSlqK8E6LVq08KvXt3b8jgUWevToIfoTrWiAV/MIax5jCgIIIIAAAggggAACCCCAAAIIIIAAAoUq0OCjX9tuu61MmjRJ+vXrZ6+xTjT26KOP1sn1njp1qvTv398P7u6xxx52FG+qwV3tdLNmzfy+Byc481dGLATrNG/e3N+aqXaCbQaP5R8oYiFYJ7hvRDWeIoAAAggggAACCCCAAAIIIIAAAggggIARaPABXr0LdEKu3/72t/4NoXl5s12efvpp2W+//eykb3qsQw45RN544420Rw7r6OStt97adn/BggW1nkawTrt27fz6mWpnu+2289sMHstfGbHg6pSUlOR80ruIrvEUAQQQQAABBBBAAAEEEEAAAQQQQACBeidQsAHeOXPmyOjRo+XQQw+V6667rlZ4TZPgyjfffOMWs/Ko6QOOOeYY0YnVtJxzzjl2FPFmm22WkePphHFadDTs4sWL47Y5a9Ysf3vv3r39ZV3IRDuuDW3PpaHQ5WiloqJC5s2bZzd1795dgjmBo9VnHQIIIIAAAggggAACCCCAAAIIIIAAAg1doGADvDpx2ZgxY+S5556zwdPaLvTy5cv9Kl26dPGXM73wwAMP2IBudXW1HTn817/+Ve666y4pLi7O2KH69u3rt/Xmm2/6y9EW3nrrLX91cD9dGXyeaju77rqrNGrUyB6jtjbeffddWb9+va0bPLZdwT8IIIAAAggggAACCCCAAAIIIIAAAgggUEOgYAO8O++8s5SVldkT/vDDD+XLL7+scfLBFa+++qr/dJdddvGXM7kwc+ZMOeOMM+zEbjr5lwZ7L7zwwkwewralo4NdmTBhglus8Th//nybFkI39OrVq0Z6iEy0o5O1DR482B5bz3/69Ok1+uFWPPTQQ25RDjvsMH+ZBQQQQAABBBBAAAEEEEAAAQQQQAABBBCILlCwAd4mTZrICSecYM+6qqpKhg8f7o8OjaTQlAw33XSTXa1pAY444ojIKhl5fu6550plZaVt66qrrpKTTz45pXa1v++//779+fbbb2u00bNnT9EJ27ToBHITJ06sUUfTQwwbNkw0LYKWSy+9tEadTLWjx3HlrLPOkp9//tk99R91pPX48ePt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b9++oV17zhcBBBBAAAEEEEAAAQQQQAABBBBAAAEE8lygQYzgfemll6RHjx4ycOBAufTSS+WTTz6xl+2hhx6SU045Rbp27SqjRo2SDRs2JH0533nnHSkuLpbWrVsnte/XX38tw4YNk912202aN28uO+20k30+fvx4Wb16dVJtxatcVVUl999/vw1id+rUSVq1aiWDBg2Sa665Rt5///14u4Zty1Q7dXXeYZ3nCQIIIIAAAggggAACCCCAAAIIIIAAAgUqEPJMKdBzs6f11ltv2cCuBii1NGvWzAY558+fb4OyS5Yssev1n9NPP13uu+8+/3ltC8uWLZN+/frJV199JVtssYUE24q37y233GIDyhUVFVGr7bnnnnaUsQZj0ykLFy6UwYMHy8yZM6M2U1JSIhpQPumkk6Judysz1U5dnbf2u1u3bjJr1iwpKioSd+3d+fCIAAIIIIAAAggggAACCCCAAAIIIIBAoQgU9AheDbgef/zxNsAXCoXkiiuusEHYE044wV4/Tdnwt7/9TVq2bGmf60hXDXgmUlasWGFHwmpwN5mi7f/xj38UDe6Wl5fL0KFD5R//+IdoXuBdd93VNjVlyhTZZ5995Icffkim6bC62r9DDjnED+727NnTnr8eS8+/cePGdqK5k08+We68886wfYNPMtVOXZ13sO8sI4AAAggggAACCCCAAAIIIIAAAgggUPACOoK3UMutt96qo5Ptz/Dhw/3TvOyyy+y6uXPn2nVmZKlfz4zI9evFWjBpGbwdd9zR30ePYUbwxqrur//xxx89E1i1+7Vo0cJ7/fXX/W26YFJEeMcdd5zf7rnnnhu2PZknF110kd/OkCFDvPXr14ftbkY2e9oH7bsZyeuZUbph292TTLRTl+ft+r399tvbczMjeN0qHhFAAAEEEEAAAQQQQAABBBBAAAEEECg4gYIewfvhhx/6AfqLL77YX45c0Dy8OppWy7Rp02KOnNXcuCNHjpQBAwbI559/HtlMrc/Hjh0ra9eutfVuuukmO0o3uFNpaak8/PDD0rt3b7tacwSvXLkyWCWh5aVLl8o999xj63bs2FEmTJggZWVlYfvqOUycONGu0wno7r333rDt+iRT7dTVedc4AVYggAACCCCAAAIIIIAAAggggAACCCBQ4AIFHeD9+OOP7eXT4GaHDh1iXkrNnztp0iR5++23xYw2lS233LJGXQ38du/eXW677Taprq62uV11YrZ27drVqBtrxbhx4+wmzQN86qmnRq2mOWMvvPBCu23VqlWiQd5ky5NPPim6r5azzz67RnDXtXfYYYfJDjvsYJ9qgDcyJ3Cm2qmr83bnxSMCCCCAAAIIIIAAAggggAACCCCAAAINRaCgA7ydO3e219GkPpDvv/8+7jU98MADpX///nbitWgVn3nmGZkzZ47dtPXWW8vLL78s119/vehEZVo0x2+8YtJB+H3Q/LqRI2qD++6///5+ezqiN9miOXxdOeigg9xi1McDDjjArv/uu+/ktddeC6uTiXbq8rzDOs8TBBBAAAEEEEAAAQQQQAABBBBAAAEEGoBAQQd49957b/8S3nHHHf5yqgs60leDupqeYeDAgUk1M3XqVL++TngWr7Rp00a22morW2XmzJnxqkbd5o6lo4F79OgRtY5b6SZ20+eRx8pEO64NbT/b563HoCCAAAIIIIAAAggggAACCCCAAAIIINCQBAo6wBscCXvDDTfItddeK2aysZSur5moTHQ0qqZlaN68edJtzJo1y9/HjSz2V0RZ0Ny5WjQH76JFi6LUiL1q9uzZdmP79u1F8/rGK+44WueLL74Iq5qJduryvMM6zxMEEEAAAQQQQAABBBBAAAEEEEAAAQQagMDG/AIFeqI6OlVzy5555pk2b+6VV14pt99+u+gIWS3Lli2TTp06JXT2mn83nbJixQp/92g5fv2Nvyy4PupTnexM00IkUnQiuKqqKls1leO4Y2SqnWye91dffSXBNBKu7+6xcePGKQf0XRs8IoAAAggggAACCCCAAAIIIIAAAgggUJ8FCjrAq/DDhg2z/ueff76sW7dOFi9ebH905e67724nTjvqqKNk+PDhoikYslWWL1/uN62Bx9pKsM6aNWtqq+5vz9Rx6ls7/gkGFqZPny5jxowJrNm0qDmRmzZtSoB3EwlLCCCAAAIIIIAAAggggAACCCCAAAIFKFDQKRrc9dIgr04idvfdd0ufPn3cavE8Tz7++GO55pprZKeddpInn3zS35bpBU214Ep5eblbjPnYqFEjf1syAd5MHae+teNjsIAAAggggAACCCCAAAIIIIAAAggggAACvkCDCPDq2bZs2VLOPvtsmTZtmowYMcICDBgwQHQiMi0//vijaJ7dhx56yD7P9D/BXLiVlZW1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jp70+Ek7ne5I5TbuYELGv4c7g+kggECRE/BLgLfIKeSy4NatWztXtR5ubk0zZv/8808zRDOX7exdPdGyZUvn1v/9z/N/OdNBOo8dDC1RooSpS+zc7EXn+uuvd0ZpBrKnpiUkfvnlF3O5bdu2OcpA+GIeravcq1cv84xVq1bJsmXLPC3HbFRnX+zbt6/d5RsBBBBAAAEEEEAAAQQQQAABBLIJRIaHSFTJ4oX+RFtz0BBAoGgLFEu3mq9fITEx0WwMVph5L774YtHPuW5aZqFevXpy+vRpCQoKEg3Oag1dzTTVjeG09vCECRPMMu+//3557733TH/gwIFm8zl7/cePHxctO7B161Zz6oknnsiyCZs9Tn8cugnbxIkTzanbb7/d6dtjdE2HDh0yh1o3t1q1avYl51sDtpp1rBvYTZo0SQYMGOBc005SUpJcffXV8sMPP5jzU6dOFdeArj3YF/N8++23cuWVV5optQyEGmYvdfHdd9/JNddcI1qmoWnTprJixQqzdnsd+f2uX7++KT+hPzOtY0xDAAEEEEAAAQQQQAABBBBAAAEEEEDgvBTQAK+v2549ezRoXKjPM8884+tlFXi+f/zjH867WBm16W+//Xb67t27zTkrwJtubaCW3r9/f2dMxYoV062auzmeZ2XLplsBR2fc3XffnW4FO9OtgLj5/Pbbb+ndunVzrtesWTPdKvmQY56bbrrJGTNs2LAc1/XEggUL0q3grhmnz3zhhRfSrcBw+smTJ9OtsgzpF110kTNHx44d060gqF/nsbJ4nedZ2czpP/30U7oVZE7fsWNH+ptvvplevHhxZ61WQNjtWvJz0grKO/Pl5z7GIoAAAggggAACCCCAAAIIIIAAAgggUJQE/JLBq5uCaWZpYZoV4JWnn366MFP47F7N1tUyA7rZmt10Q7XU1FSxApMm69Q+X758efn888/NZmv2Odfv999/X6ygrJw4ccI5rXNolqlmCdutTp06ovV/NYM2e7OCyeaante5Xn311exDzPGXX35psoE1o9pu2ddbt25dsYLBouv21Hwxj2Yc67pdy1NkX4s+/4033pAhQ4Z4WorX58ng9ZqKgQgggAACCCCAAAIIIIAAAggggAACRVjALwFeDYh+//33ubJYUXAT5LQyXUU315o2bZopGzB8+HB5/vnnTTkELS8QKE0DsK+//rpMmTLF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5Ig0a9ZMxowZIwsXLpSDBw9CjwACCCCAAAIIIIAAAggggAACCCCAAAIIIOADAb9ssnb06FEZOnSopKWlyfTp00WPXVt0dLRERka6nsrR1/v1Q0OgIAJsslYQNe5BAAEEEEAAAQQQQAABBBBAAAEEEChqAiH+WHBSUpJ89NFHHqfWurx51eY9duyYx/u5gAACCCCAAAIIIIAAAggggAACCCCAAAIIICDilwBvsWLFpHz58oXyjYiIKNT93IwAAggggAACCCCAAAIIIIAAAggggAACCJzvAn4J8FasWFH2799/vtvxfggggAACCCCAAAIIIIAAAggggAACCCCAwDkV8Msma+f0jXg4AggggAACCCCAAAIIIIAAAggggAACCCBwgQj4JYP31KlT8ttvvxnCzp07S4kSJbzmnDp1qqxZs0ZatGgh11xzjdf3MRABBBBAAAEEEEAAAQQQQAABBBBAAAEEELjQBIqlW83XL713716JiYkx0+7evdvpe/OcqKgo0Q3W7rnnHhk3bpw3tzAGgRwC9evXl7i4OAkKCpK0tLQc1zmBAAIIIIAAAggggAACCCCAAAIIIIDA+SAQUCUakpKSRD/aDh48eD748g4IIIAAAggggAACCCCAAAIIIIAAAggggIDfBApdomH69Omya9euLAvUDFy7TZgwQUqVKmUfevxOSUmRWbNmSWpqqhnTpEkTj2O5gAACCCCAAAIIIIAAAggggAACCCCAAAIIICBS6ACv/vX3Bx980KPliBEjPF7L7UKHDh1yu8w1BBBAAAEEEEAAAQQQQAABBBBAAAEEEEDgghcodImGG264QS677DKfQj7++ONyxRVX+HROJkMAAQQQQAABBBBAAAEEEEAAAQQQQAABBM43gUJn8CrIhx9+KHPmzHFsjh49KoMHDzbHY8aMkejoaOeau06xYsUkLCxMIiMjRUsz1KpVy90wziGAAAIIIIAAAggggAACCCCAAAIIIIAAAgi4CBRLt5rLsU+6e/fulZiYGDPX7t27nb5PJmcSBLwQqF+/vsTFxUlQUJBoGREaAggggAACCCCAAAIIIIAAAggggAAC56OATzJ4s8PopmqauastKioq+2WOEUAAAQQQQAABBBBAAAEEEEAAAQQQQAABBHwg4JcMXh+siykQKJQAGbyF4uNmBBBAAAEEEEAAAQQQQAABBBBAAIEi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7rrrigzY1qpVSz744AOCuxYe3wgggAACCCCAAAIIIIAAAggggAACCCCAQBECId3B631vTcfw/fffm12VurPywIED0rhxY9Gdlvrp16+fVKxY0XsKdQRKLMAO3hLTMREBBBBAAAEEEEAAAQQQQAABBBBA4BQSCFmAV/+z+Ojo6FOIgkcNJwECvOH0a/IuCCCAAAIIIIAAAggggAACCCCAAAL+BEKSokHT+nbs2FH+7//+Tz777DMJYZpff+9FOwIIIIAAAggggAACCCCAAAIIIIAAAgggEPYCIdnBu2DBAjnvvPMMXtOmTWXjxo1hD8kLli0BdvCWrd+Dp0EAAQQQQAABBBBAAAEEEEAAAQQQCI1ASHbwrl271n7avn372nUqCCCAAAIIIIAAAggggAACCCCAAAIIIIAAAsETCEmAt1WrVvYTpqam2nUqCCCAAAIIIIAAAggggAACCCCAAAIIIIAAAsETCEmAt3v37tK4cWPzlF999ZVs3749eE/MSggggAACCCCAAAIIIIAAAggggAACCCCAAAJGICQB3ujoaJk7d6506tRJjhw5Im3atJExY8bI4sWL5dChQ9AjgAACCCCAAAIIIIAAAggggAACCCCAAAIIBEEgJIesHT16VO677z5xOp0yZcoU0WvvUqlSJSlfvrx3U4G6ztcPBYGSCHDIWknUmIMAAggggAACCCCAAAIIIIAAAgggcKoJxITigTMyMuTdd9/1u7Tm5S0qN29aWprf+XQggAACCCCAAAIIIIAAAggggAACCCCAAAIIiIQkwOtwOKR69eql8k1KSirVfCYjgAACCCCAAAIIIIAAAggggAACCCCAAALhLhCSAG+NGjXkwIED4W7H+yGAAAIIIIAAAggggAACCCCAAAIIIIAAAn+qQEgOWftT34ibI4AAAggggAACCCCAAAIIIIAAAggggAACESJAgDdCfmheEwEEEEAAAQQQQAABBBBAAAEEEEAAAQTCTyBoKRpyc3Pls88+k++//14WL14su3btklatWknr1q1l6NCh0rx58/DT440QQAABBBBAAAEEEEAAAQQQQAABBBBAAIE/UcDh9pTS3v/w4cMyYMAAmTdvns+l4uPj5aGHHpIRI0ZIXFyczzE0IhBMgeTkZElJSZGoqChxOp3BXJq1EEAAAQQQQAABBBBAAAEEEEAAAQQQKDMCpd7B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IIAAAggggAACCCCAAAIIIBBJAgR4I+nX5l0RQAABBBBAAAEEEEAAAQQQQAABBBBAIKwECPCG1c/JyyCAAAIIIIAAAggggAACCCCAAAIIIIBAJAkQ4I2kX5t3RQABBBBAAAEEEEAAAQQQQAABBBBAAIGwEiDAG1Y/Jy+DAAIIIIAAAggggAACCCCAAAIIIIAAApEkQIA3kn5t3hUBBBBAAAEEEEAAAQQQQAABBBBAAAEEwkqAAG9Y/Zy8DAIIIIAAAggggAACCCCAAAIIIIAAAghEkgAB3lL82rm5ueJyuUqxAlMRQAABBBBAAAEEEEAAAQQQQAABBBBAAIGSCxDgLaFdVlaWxMbGypAhQ0q4AtMQQAABBBBAAAEEEEAAAQQQQAABBBBAAIHSCcSUbnrkzd6xY4csWLBAfvjhB/PyCxculKefflq6du0q559/vkRFETOPvL8VvDECCCCAAAIIIIAAAggggAACCCCAAAJ/joDD7Sl/zq1PrbumpqbKP//5T3n99ddFUzP4Kmeeeaa89tpr0qVLF1/dedqmT58uH3/8saSkpJhPXFycNG/eXFq2bCm33nqrtG/fPs/4kl44nU55//33ZeLEieY+R48elc6dO8s555wjffr0kbPOOiugpYO1zqZNm+TZZ5+V5cuXi9br1asn3bp1M88zcOBAKVeuXEDPU9Sg5ORk874acNdnpyCAAAIIIIAAAggggAACCCCAAAIIIBCOAgR4A/hVNQbeu3dv+e9//2uPbt26taxZs0aqVq0qR44csXPxVqhQQRYvXiytWrWyx3pXNm/eLLfccovMmTPHuzlPPTo6Wm6//XZ55plnShXw3LVrl1x88cXmOfPc4PeLmJgYGTdunPz973/31W23BWud559/XoYPHy45OTn22t4V3QWtge8qVap4N5eoToC3RGxMQgABBBBAAAEEEEAAAQQQQAABBBA4xQQI8Abwgz333HPyyCOPmJG6y/aTTz6RFi1aSEJCggwaNEjuvfdeefDBB2X27NlmTLNmzWTt2rWiu3K9S2Zmptk9u3r1atNco0YNufbaa00wOD093exq1V291g7h66+/Xj744APvJQKu607dv/zlL7Jq1SozR3cE/9///Z/UrVvXpJj44osvJCMjQxwOh/znP/8xAWVfiwdrHQ0k33DDDeYW6nbllVeatBaa8mLatGny888/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LwoJkAAJkAAJkAAJkAAJkAAJkAAJRBGggjeKDMNJgARIgARqNIFlk19y51e/835uO2zDW75AnP/cmZ/LCv3LJCtmfCL4gzTudYFT5pY1We1rt0wXV8skZQ1am8Pe8vmZovEYCZAACZAACZAACZAACZAACZBALSdABW8tbwA8fRIgARKorQSW/z7GnHqd+s2k/lp9s2Oo2yh7nIrFq+LUqasWweV/btd36coad3DbK2Z+IeXNu7j94EbFwikmqKzRWsFD3CcBEiABEiABEiABEiABEiABEiABR4AKXoeCGyRAAiRAArWFgFexVCrmfmNOt27LrllPu6zJ2tLq0K+zxpvz2IbqpqFCGnQZKI37XpMWv7zjHrL486vFW7FElk//QMrXPygtDgK8pXOda4Z6a24ZGoeBJEACJEACJEACJEACJEACJEACJAACXGSN7YAESIAESKDWEaiYPU5k5Qpz3mUtVClbIMHCaeVdDjWlLfvp37JsyquhJS8ed5vIn9bADdb7S2gcBpIACZAACZAACZAACZAACZAACZAACFDBy3ZAAiRAAiRQ6whUzP3OnXO9Ft3cdiE2Gm58sva+cNvgyaL3h8qySc+pRe8fpmhY9i769CpZ+v1DZr9B12OkXvvtC1EtlkECJEACJEACJEACJEACJEACJFCiBOiioUQvHKtNAiRAAiRQeQIrF01zieu2LKyCt6xxe2m20z9k4f+GqCuGWfLHe0ONwrdus85SsWCisywuX3eANO59qasnN0iABEiABEiABEiABEiABEiABEggjAAteMOoMIwESIAESKBGE/CWTHfnV7dl4Vw02ELrte0vzQe8IPXX3lnq1NPF21YuV5+741cpd3UxN1juNu5/s0avY5PwlwRIgARIgARIgARIgARIgARIgARCCdTxVEKPMJAESphA165dZfz48VJWViYVFRUlfCasOgmQQI0n4K2UivkT9G+ilDVaU2BRXKdekxp/2jxBEiABEiABEiABEiABEiABEiCBZAjQRUMyHJkLCZAACZBAgMCyn5+Xhe8MCoT6dr0K8VS5CalTRyeU1KnrO5i62bjvNdKw27GpgTVlT8+9bouu5q+mnBLPgwRIgARIgARIgARIgARIgARIoHAEqOAtHGuWRAIkQAJFT2DlSk9mzV+aSD3r/LFU6iybEyuvrFNJKpbEyoeRSIAESIAESIAESIAESIAESIAESKC2EaCCt7ZdcZ4vCZAACWQgAOVu/5NfyxAj/qE9ezaQK9ZbY3UCtdb1ls0X8VasDgvbUkveOuXNUy161S8thQRIgARIgARIgARIgARIgARIgARIIJ0AFbzpTBhCAiRAAiSQAIGyJu2l9ZFTTE7eisUyb/TOUjHri+w5q+uGOg1aS4v9xkhZg1bZ4zMGCZAACZAACZAACZAACZAACZAACdRiAlTw1uKLz1MnARIggSCB+vXKpG/3NsFgWb5ipUydtdi4b1i2fLVDhRZN6ssaLRtIm+blcKSbkm6DdZq5/cVfXBdPuftnipXzf5RFH18kTbcd7vLgBgmQAAmQAAmQAAmQAAmQAAmQAAmQQDqBOp5KejBDSKC0CXTt2lXGjx8vZWVlUlFRUdonw9qTQDUTeP5/v8oF934pi5dG30u9urWS4Wf1ljVbNkyrrbf8D5n9aEeRisVpxzIG6OJjrQ6bIGWN22eMxoMkQAIkQAIkQAIkQAIkQAIkQAIkUJsJ6LLlFBIgARIgARIIJ/D0W5Nl6O2fZVTuIuVn38+RgRe9K7NDFmhb/usbuSt3kan67F025WVsUUiABEiABEiABEiABEiABEiABEiABCIIUMEbAYbBJEACJFDbCfz020K5aMTY2Bh+mb5Izr/ny7T4FfN+SAuLG1CVtHHLYDwSIAESIAESIAESIAESIAESIAESKGUCVPCW8tVj3UmABEggjwRue/J7WVGRmxefNz6ZJp//MCelVt6KRSn7uezAvQOFBEiABEiABEiABEiABEiABEiABEggmgAVvNFseIQESIAEai2BxUtXyOsfT63U+Y9+79eUdFXxoVuVtCmV4A4JkAAJkAAJkAAJkAAJkAAJkAAJ1FACVPDW0AvL0yIBEiCBqhD48deFsnT5ykplMe6neSnp6rffPmU/l52qpM2lHMYlARIgARIgARIgARIgARIgARIggVIlQAVvqV451psESIAE8khg7sLllc597oJlKWnrtuwm9dbqnxIWZ6duC03XNvd0cfJmHBIgARIgARIgARIgARIgARIgARKoKQSo4K0pV5LnQQIkQAIJEmjZtH6lc2vVrDwtbeOtrhepUy8tPFNA462GSZ067KYyMeIxEiABEiABEiABEiABEiABEiABEsjtbZu8SIAESIAEagWBDdZpJg3Ly2TJstzdNGy6fgvDqGLeBFk25WXHq3y9v8iyn57S/ewLt9Vfdx+pmPudLNY/SP0OO0i91j3MNv+RAAmQAAmQAAmQAAmQAAmQAAmQAAmsJkAF72oW3CIBEiABEviTQMPyurJb3/by/P9SF0yLA2jfbdc20VbM+kIWffj3OEnS4iyfPFrwZ6VJ/1uo4LUw+EsCJEACJEACJEACJEACJEACJEACPgKc++qDwU0SIAESIIHVBE4f2FXq16uzOiDG1m5928lmG7SKEZNRSIAESIAESIAESIAESIAESIAESIAEkiBAC94kKDIPEiABEqiBBDq3bypXD9pMzhn+Rayz69SuiVx7ymYubr01ekmTrW93+1XZqNdum6okZ1oSIAESIAESIAESIAESIAESIAESqLEE6ngqNfbseGK1lkDXrl1l/PjxUlZWJhUVFbWWA0+8uAjMmLtUmjeuJw3U/UGusnKlJ9PnLpHWzRtIeb38Tb6o0HJ+mb4opXpvfz5dhj36rSxeFn0v9dygpVx63KbS0rfAWpOG9WSNlg1S8uIOCZAACZAACZAACZAACZAACZAACZBAsgSo4E2WJ3MrEgJU8BbJhWA1jA/btz6bLt/+PF/mLVxmiLRr3VD2334dOXzXTlJeP7OyFj5wn3xzskyetkiWr1ipHy3qyNprNJLd1RXC0Xutl1HZ++E3s+S+5ybEugoXq3IWFrgzVInc/+TXYqXJFmlA//Zy+9De2aLxOAmQAAmQAAmQAAmQAAmQAAmQAAmQQBUI0EVDFeAxKQmQAAlEEViq1q7XP/KtvPzh72lRps5eIvf+Z4K89P7vMuLcPtK8Sf20OEg/9PbP5Yvxc1KOwZJ3ilrYPjD6J3n1o6ly6+m9pL0qfMMEab+ZND/sUFrY4qXR1rlpkRlAAiRAAiRAAiRAAiRAAiRAAiRAAiRQNASo4C2aS8GKkAAJ1CQCfuXs7n3byx792kmvrq0Eyt0HX/hJXlPl7ORpf8gVI8fJsMFbpJ36nc+Md8rdbXquKYP230A6t28iM9XNwxNq0fvE6z8bRe9lD46Tu8/ubSx7g5n8MHmBCWrRtFwXPmsZPJyy36zxKiVzg/p1Zcdea6Ucq+zOJutlLrOy+TIdCZAACZAACZAACZAACZAACZAACZDAagJU8K5mwS0SIAESSITAf8b86pSzxw5YT05W5awVuEG4/IQeop4W5JUPp8p7X82U8VMWyIYdm9ko6iZhqTzz9hSz36tbK7nh1M2ljsaHtGvTUE4f2FXq6v6/XvtZvvpxrsAFxC69266K4Ps//pdVCt6tN20jcMEQR2BNfP95/eJEZRwSIAESIAESIAESIAESIAESIAESIIEiIJDZ+WMRVJBVIAESIIFSI/C4Kl4hXdZuKiftt1q56z+Po/dcz+1CyeuX79Rfr5WBO63rlLs2DL+H79bJ7X4zaZ7bthtzFiwz1r7Y36hTcxvMXxIgARIgARIgARIgARIgARIgARIggRpGgBa8NeyC8nRIgASqlwCUs3C9ADlqj/VClbM4tn6HpjJcXSs0V9cIa7ZsgCAnCxYtd9vNm6b758XBVs3Kpa6aAVeoT97Z85e6+HbjB7UKtkIFryXBXxIgARIgARIgARIgARIgARIgARKoeQSo4K1515RnRAIkUI0EvvQtitZn49YZa7LFhq1Cj/fr3saFj/txnoTFgwIXyl1I3+5ruPh2A24fIGWqBPa7f7DH+UsCJEACJEACJEACJEACJEACJEACJFAzCFDBWzOuI8+CBEigSAj8+Nsq6912rRtK6+blplbT5ywxPnm/nDBXmjaqb1wm9N6oldiFzYJVb9OigYkDa+CHX5ooW/dYw7h7sPFg4Xvjo9+aXVjx9lcfu0Gx/nc7q8/f+X8sl3uf+1G+mjBHfpmxWNpq3aD03a1PO/Erk4N5cJ8ESIAESIAESIAESIAESIAESIAESKD4CVDBW/zXiDUkARIoIQK/TF+l4G2pLhQgdzz9gzz2p09e/2lAiXvxsZuo9W26chbxLj1+Uxlyy6dmwbXjr/lQdtVF1Dq3byqz5i2V1z+Zqm4ZlpkF1y47voe0bLqqLH/+P0xeZcE7S+MdctG7smzFSncYCl9Y+L743m+y/3Zry5CB3aRRg7ruODdIgARIgARIgARIgARIgARIgARIgARKh0AdT6V0qsuakkA8Al27dpXx48fr9PQyqaioiJeIsUggAQJ/veoDozzdcqPWxlIWStQG5XWl27rNpLUqfX/8daFMmb7IlXTZCZvK7n3bu33/xsJFK+Ts4Z/LWLX8DQqsg+8/v6+0a90oeEiWLKuQXYa8Kfbp3qRhPemt7iJ6dW0l9euVyfeT58tLH/wuy5avUvruosrjK0/qmZYPA0iABEiABEiABEiABEiABEiABEiABIqfAC14i/8asYYkQAIlRGDRkhWmtp99P9soWHurovfi4zZNWUjtlQ+nyg2PfiOLl1bIrU/8YNwktAhY4X787Sy58h9fy8y5qxZQW6tVQ+nYtrFMm71Efp2xyFjwHnPFB3LqQV3lALXC9QuUyFa5CwXwrWdsIeu2beKPIoft2knOvetLsyDcG59MU3cN02X7zddKicMdEiABEiABEiABEiABEiABEiABEiCB4idABW/xXyPWkARIoIQIlNWpY2oLBWv7NRrJDX/bXBqqBa9f9ujXThYuXi43PfadzF24TB555Wf520Ebuiif60JtZ9/5hSxXtwrwl3v+0d1TXDn8PPUPuUqVv19PnCc3PPKNeLrY2oE7rOPSb9SpuTxx5Tby28zFsn6HpinKZRupk/rmPe/ojeXUYZ+YoH++MokKXguHvyRAAiRAAiRAAiRAAiRAAiRAAiRQQgTKSqiurCoJkAAJFD2BNVs1cHU8bsB6acpde/DA7ddxi7B9M2meDTa/w9VvL5S79erWkWGDt0hR7iIClLO3D93SKJCxf/ez41VhvMpyGPtYeK3jWo2NZfCaLVfXB8f8svmGrdR1RHMT9JNa/VJIgARIgARIgARIgARIgARIgARIgARKjwAVvKV3zVhjEiCBIiawVsuGrnZdOzZz28GNMlXCdm6/ym3CpN9XLcyGONPnLJFvJs030Xfv1166rN00mNTsY1G0k/fbwGxDuTvmyxmh8bIF2jrAXcTUWUuyRedxEiABEiABEiABEiABEiABEiABEiCBIiNABW+RXRBWhwRIoLQJrKm+cq1gcbVM0vJPv7t+69tf1a2ClW4ZFMSIs1Gn1QrkKdNWK4lt+ji/WHTNSnn91ds2jL8kQAIkQAIkQAIkQAIkQAIkQAIkQALFTYA+eIv7+rB2JEACJUagnfrMtfL1T/OMOwW7H/yFj1xImxbl7lATtcy1ssDndsGGRf02qL863aj//SpjJ8w1/n2vOrlnpJsI5AV/vpCmjes5lxEmgP9IgARIgARIgARIgARIgARIgARIgARKggDNtUriMhVXJVeuXCkrVqz291lctWNtSKB6CeywxVpiLXc/08XSomT+H8vFumbo0aWli7aeLooG37uQz3+ITo/j3/28ypUDtrvpwmpWZqibhxff/03e+2qmfPrdbBuc9jtj7lL5YcoCE77Rn7540yIxgARIgARIgARIgARIgARIgARIgARIoKgJUMFb1JenOCt34YUXSv369eXnn38uzgqyViRQjQRaNy+X/bZd29TgJVWyvvPF9NDaPDD6J1myrMIcG7BVBxcHLhOw+BnkC1Xw/vfz8PRIO2LUjyZe44Z1ZWOfu4a+3duYcPx7UMtZUeG5ff/G7U99L0v/rMPx+6zvP8RtEiABEiABEiABEiABEiABEiABEiCBEiFABW+JXKjqriYsdt9++2254YYb5IUXXjDVueSSS+Shhx6SX3/9tbqrx/JJoKgIHLl7J4Gi1lO96mUPjJNXP/pdFi1ZpcyFQvW2J7+Xp96cbOp88E4dpd8mqxWyCDz3qO7GrULFSk8uvm+swOUCLH6tjFer2/+78WP5/U8XD2ceupG0+NOfL+LAInjXPu1M9G/VyvfCe7+UmWqta2XWvKVy7l1fyBufTDNBsDq2SmUbh78kQAIkQAIkQAIkQAIkQAIkQAIkQAKlQaCOp1IaVWUtq4vAa6+9Jqeffrp8++23oVVo2LChDB06VC677DIpL1/tSzQs8o8//ijXXnutfPrpp4LtddZZR7beemvZZptt5JBDDpEmTZqEJcs5rGvXrjJ+/HgpKyuTiopVirWcM2ECEqgCAbhGuOT+r2TOgmUmFyh811mrkUyZtshZ1O68ZVu58qSeUmeVR4aU0l7/ZKpc89A3zsoXBzu3byJzNb+5C1crew/asaOcdfhGKWmxs3hphZyiSmAogyEoY501G8vyipUyddYSE4Z/A7buIOccubGU+xZbcwe5QQIkQAIkQAIkQAIkQAIkQAIkQAIkUPQEqOAt+ktUvRUcM2aM7LTTTk5J2qxZM2nVqpVMnjxZ1lhjDZk5c6ar4AknnCD333+/2w9uDBs2TC644AJZvny1csofp3///sY6GPlXVajgrSpBpk+CACxlr3vkW+MH17pjQL4Ny+vKPtt0kMEHd82oWJ06e7Hc+sQP8u7YGQJrXr90WbupnHnYRrJF1+j7Ba4Znn5rsjz4wk+ycFGq3+x12zaRgTt3FCiIKSRAAiRAAiRAAiRAAiRAAiRAAiRAAqVLgAre0r12ea85lLebb765ccFQR83/LrroIvN36aWXynXXXSffffedvPjii3LFFVfI3LlzTX0efPBBOe6449LqNnLkSDn++ONNOCx+Dz30UIFCd8qUKTJ69Gj58ssvzbEePXoILIbbtm2blkcuAVTw5kKLcfNNYKUqZydN/cNY77ZpUS7rd2gm8JsbV5avWClTpi+SX/SvTYsGxpK3ScN6cZMbVxFTZy2WyWo9DEvibuqvN5f0sQtiRBIgARIgARIgARIgARIgARIgARIggYIToIK34MhLp8Bbb73VuF5AjYcMGSK33Xabqfz5559vFLyTJk2STp06yU033SRnn322ObbVVlvJ+++/b7btvxkzZph4ixcvlhYtWshzzz0nO+ywgz1sLHqPOeYYefzxx03YqaeeKsOHD3fHK7NBBW9lqDENCZAACZAACZAACZAACZAACZAACZAACZBAqRHgImuldsUKWN/PPvvMlWYVuC7At3HssccKrHIhH374oUybtmrhJhsFimIodyHXX399inIXYfXr15dHH31U+vTpg115+OGHZcGCVX5DTQD/kQAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJhBKggjcUCwNBYOzYsQYEFk7r2DHaT2ebNm1k1KhR8u6778r06dPT3CvAbQME/nvD3DfgGBZDO/PMM7EpCxcuNEpes8N/JEACJEACJEACJEACJEACJEACJEACJEACJEACkQSo4I1EwwPrrbeegbBs2TKZOnVqRiC77babbL311mbhNX9EuHGwaeGWAcriKNlll10Evn4hsOilkAAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJZCZABW9mPrX66HbbbefO/84773TbuWx88MEHLjoWbMska665prRv395EGTduXKaoPEYCJEACJEACJEACJEACJEACJEACJEACJEACJKAEqOBlM4gk4Leovfbaa+XKK6+UpUuXRsYPOzB+/HgXbC2CXUDIxrrrrmtC4YP3t99+C4nBIBIgARIgARIgARIgARIgARIgARIgARIgARIgAUugnt3gLwkECWy22WYyYsQIOfnkk2XlypVyySWXyB133CGwtIXMmTNHOnXqFEyWsj9//ny337ZtW7cdtWHzxvHZs2dLhw4doqLKf//7X3nooYcij7do0YKLtUXS4QESIAESIAESIAESIAESIAESIAESIAESIIGaQIAK3ppwFfN4DieeeKLJ/bTTTpMlS5bIjBkzzB8Ce/XqJT169JADDjhAhgwZIlhsLSjz5s1zQY0aNXLbURv+OIsWLYqKZsLh2xcLu4UJfPlWVFQYxXSTJk3CojCMBEiABEiABEiABEiABEiABEiABEiABEiABKqdwO+//y7NmzevdD2o4K00utqTEEregw8+WB5//HEZOXKkfPTRR+bkPc+TsWPHmr977rlH4Kd34MCBKWDgasFKw4YN7Wbkb4MGDdyxbApeFzFiAy4hvvrqK6lqPhHZM5gESCADgdatW0tZ2SovQHPnzpUVK1ZkiM1DJEACJEACpUAAs6Pq169vqooxXq6uu0rhHFlHEiABEqhtBGAQZQ2t8O7M9+fa1gJ4vsVCADq2qggVvFWhV4vStmzZUk455RTzd8YZZ8htt90m2267rbz33nvGSnb69OlyyCGHGJcJxxxzjCNjXwIQEEfB44+TTSHct29fueaaa1xZwQ0s1AbFEoUESKDwBHD/wpIe0rhxY6lqZ1X4M2CJJEACJEACQQLl5eVSt25dE9y0aVMzWyoYh/skQAIkQAKlRQDv7PXqrVINNWvWLNZ7e2mdIWtLAqVBwBpIVba2VPBWllwtTme/7j3yyCNGaQP3DaNHjzZEhg4dKnvssYdYf7sY/FuBi4ds4o8DK5FMAgvdTAu3HXTQQXLppZdmyoLHSIAE8kRgu+22E3z4gTz//PPSs2fPPJXEbEmABEiABApF4Nhjj5X333/fFHfLLbcYN12FKpvlkAAJkAAJ5IfA5ZdfLv/6179M5oMHD5YzzzwzPwUxVxIggbwSWDV/Nq9FMPOaTKBz584yatQo2WqrrcxpYmG0xx57zJ2y33+If8E1FyGw4Y/jTxuIxl0SIAESIAESIAESIAESIAESIAESIAESIAESIAElQAUvm0GVCWAaNqxlrcAvr5UuXbrYTZkyZYrbjtqwcTBFJGzRtqh0DCcBEiABEiABEiABEiABEiABEiABEiABEiCB2kiACt7aeNVjnPPEiRPlwgsvlL333luuuuqqrCn22msvF+enn35y2927d3fbEyZMcNthG8uXL5eff/7ZHOrRo4dk88EblgfDSIAESIAESIAESIAESIAESIAESIAESIAESKA2EaCCtzZd7RzOdebMmWYBsxdffNG4YMiWdN68eS7K+uuv77Y322wzadCggdl/5513XHjYxkcffeRWY+7Xr19YFIaRAAmQAAmQAAmQAAmQAAmQAAmQAAmQAAmQAAn4CFDB64PBzdUENtlkE8FKyZDPPvtMvv/++9UHQ7befPNNF7rpppu6bSyytueee5r9cePGyeeff+6OBTcefvhhF7TPPvu4bW6QAAmQAAmQAAmQAAmQAAmQAAmQAAmQAAmQAAmEE6gXHszQ2k6gcePGcsQRR8g//vEPqaiokCFDhhhLXmuN6+cDlwzXX3+9CYJbhf32289/WE488UR57rnnTNigQYPk1VdflZYtW6bEgaXwyJEjTRgUxAMGDEg5zh0SIIHSInD66afLokWLTKU7dOhQWpVnbUmABEiABEIJHHnkkbLzzjubY3CnRSEBEiABEih9AjDIWm+99cyJ8Nle+teTZ1B7CdTxVGrv6fPMMxEYP3689OrVSxYuXGii7bDDDnLjjTfKM888YxS6X3/9tWBBtcGDB8usWbNMHPjrhe/eoMBH78svv2yCN998c7nppptk6623NumQ39lnny3wwVtWVmYUyfD9SyEBEiABEiABEiABEiABEiABEiABEiABEiABEshMgArezHxq/dFRo0bJIYcc4nzjAki9evVkxYoVUr9+faOUtZAGDhwojz76qAm3YfZ39uzZcvjhhxvrXRsWTI/wW265Rc444wwbhb8kQAIkQAIkQAIkQAIkQAIkQAIkQAIkQAIkQAIZCNAHbwY4PCTG3QIWP4PytlWrVgYJlLsQWNxC1lxzTXnooYfkySefDFXuIk7r1q3lpZdekgsuuMBsI8ymxzamgrzwwgtU7gIGhQRIgARIgARIgARIgARIgARIgARIgARIgARiEqAFb0xQjCaycuVK+fjjj+Xiiy+W1157TW699VbjK3fDDTfMGc/EiRPNgmuNGjWSrl27Gp8/cM9AIQESIAESKD0C+PCHZzif46V37VhjEiABEqgpBNgX1ZQryfMgARLIBwE+I/NBtbjypEatuK5HUdcGL+79+vWTLbfc0tTzgAMOkMood5EYTtz/8pe/CHzzdunShUqBor7yrBwJkAAJRBNYunSpmb1xwgknREfiERIgARIgARLIIwH2RXmEy6xJgARKngCfkSV/CWOdQN3LVGLFZCQS+JPAG2+8If/73/+MO4WWLVuSCwmQQDUSgKuTyZMnmwUPp0+fLg0bNpTGjRtXY41YdG0hMGXKFBk9erQ88MADZnbHggULBH+wDujUqZPUqVOntqDI23ny/s4bWmYcIMC2FgDC3ZIhwL6oZC4VK0oCJPAngTlz5sj3338vWLS+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EiABEiABEiABEiABEqjVBOiioVZffp48CZAACZAACZAACZAACZAACZAACZAACZAACZBAKROggreUrx7rTgIkQAIkQAIkQAIkQAIkQAIkQAIkQAIkQAIkUKsJUMFbqy8/T54ESIAESIAESIAESIAESIAESIAESIAESIAESKCUCVDBW8pXj3UnARIgARIgARIgARIgARIgARIgARIgARIgARKo1QSo4K3Vl58nTwIkQAIkQAIkQAIkQAIkQAIkQAIkQAIkQAIkUMoEqOAt5avHupMACZAACZAACZAACZAACZAACZAACZAACZAACdRqAvVq9dnz5EmABEiABEiABEiABPJCYP78+TJp0iSZPn261KtXTzp37izrrLOO2c5LgcyUBEiABEiABEiABEiABGopAVrw1tILz9MmARIgARIgARIggaQJLF26VO68807ZfvvtpXXr1rLZZpvJbrvtJjvttJOst9560rBhQ9l1113lP//5j1RUVCRdfNb8UOYvv/ySNR4jJEPg+OOPl3bt2pm/888/v1KZfv/99y4PtKeVK1dWKp8kEvXp08fUpVu3bklkF5pHz549TRn4razcfffdjtkzzzxT2WyYjgRIgARIgARIoIQIUMFbQheLVSUBEiABEiABEiCBYiXw8ssvy0YbbSSnnXaajBkzJlSBCwXrG2+8IQceeKBsuOGG8vHHHxfsdP73v/9Jr169jHK5YIXW8oL2339/mTZtmvkbOXJkaJvIhujhhx92eeyxxx5SVlZ9ry8zZ840dYFVer5kxowZpgz8VlYWLVrkmC1ZsqSy2TAdCZAACZAACZBACRGgi4YSulisKgmQAAmQAAmQAAkUI4Hhw4fL6aef7hR4devWlf79+xsLXlhdNmrUSCZOnGgUv6+99po5BezD0vfee++VY445Jq+nNWrUKIGykVJYAnvvvbe0bdvWKRtx7ffcc8/YlYC17j//+U8X/4QTTnDb3CABEiABEiABEiABElhNgAre1Sy4RQIkQAIkQAIkQAIkkCOBd955RwYPHuxSbbrppvLAAw9I3759XZh/Y9y4cSb+f//7X4F14V//+ldp2bKl7Lfffv5oiW7PmTPH5VenTh23zY38EoDvZVzfG264wRT0yCOP5KTgffPNN2XKlCkm7XbbbSf5dI0Qh8Rdd90lsI6tX79+nOiMQwIkQAIkQAIkQAIFI1B9c5wKdoosiARIgARIgARIgARIIB8EVqxYkaLcHTRokHz66aeRyl3UAQrgV199VY488khXpZNPPllmzZrl9rlRcwjAD68V+F7+448/7G7W34ceesjFOfHEE912dW3stddectBBB+X1Y0R1nRvLJQESIAESIAESKG0CVPCW9vVj7UmABEiABEiABEig2gjANcNXX31lyu/cubPceuutUl5enrU+iPPggw9K9+7dTVz4aT333HOzpmOE0iMAq9ttt93WVBzKXSh548iCBQvk3//+t4kKC++BAwfGScY4JEACJEACJEACJFArCdBFQ6287DxpEiABEiABEiABEqgaASw4demll7pMbr75ZmnYsKHbz7YBJS+mvO+4444m6hNPPCF33HGH8ddr077++uvy9NNPm11Ygka5fUCEq6++2k3nv+eee0waTPF/8skn5YcffjD7+Pf44487pfSZZ54pXbt2dcfsxocffij333+/sUaePHmyNGvWTDbYYAOBm4AzzjhDmjdvbqOG/n7wwQeCxcG++eYb+e6776RFixbSs2dP8wc/sh06dAhNh8Dzzz9f4FICPozh3uDXX381is63335b3n//fbM43U477SQDBgxI4TFhwgR55pln5K233jL17tixo/GBfMkll8h6660XWR4OLF++3CjcsejdF198IT///LNxh7D55pvLrrvuKgcccEDG9NkO4pyxyB3k0UcfTbHejkqL6w53CJAjjjgipV0E04Dzs88+a1iD908//SRrrbWWYYXF/A477DDp06dPMJnZB1e0Cch1110n8+bNM78vvPCCNGnSRHbffXc566yzZN1115ULL7zQWJo3btxY0N7DBAsJwhUFOKIu+MN5oP2grYHpKaecIg0aNAhLnhYGX9Voz2hT3377rbGAx32AhQr79euXFj/XANQTrPH75ZdfmnqhjviD1XS7du1yzZLxSYAESIAESIAEqoOARyEBEiABEiABEiABEiCBHAk8//zzno5dzV/v3r1zTL06epcuXVw+qoxdfUC3hg0b5o6p0izlWHBHFVIurj2mFsUuzNbV/6vKPRvV/M6fP9/7y1/+kjFN69atPbVCTUlnd6ZOneodffTRnvr5jcyjVatWni4cZpOk/a6zzjom7bHHHuupv2Kvffv2oXmpf1tP/Rib9O+9956nSuTQeKqM9F588cW0cmyA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tI7WTYPNNNviajYeftF23nrrrZTF1WCJDGtrVVAby2O0G7iigLWwFVjDJi2wPIaFNtohFuCzAhcI+tpnrI31I4SxCIcLE9vuf/31V2fRbdPYX5wD6goXCta/LqxrIbBIh/9r+Be2VtA2XdzfnXfe2bTVQYMGSatWrUwy1NW2V9xrxxxzjHHdAf/LFBIgARIgARIggeImUAe65uKuImtHAiRAAiRAAiRAAiRQagRmz55tFL6Yqo7hJlwSwM+uWu5W+lSw6BPcJiBvLK7Vpk2bnPPCNP4ffvjBpMU0/yh/r6gzlNBQ+GKxNyiT27dvH7s8pMe5I71aRZpzh4I4qrzYGecxInzCwqct3DTgOq2//vqiFsVFXWdwnjx5svH7PGfOHLMQHJTnYF4dAiUzXIjA1zSUr1CcqzV2laoCtxNoR2hPW2yxRai/6ioVoImhjMbHiT/++MNcd1x7LAJIIQESIAESIAESKA0CVPCWxnViLUmABEiABEiABEiABEiABEiABEiABEiABEiABEggjQBdNKQhYQAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJlAYBKnhL4zqxliRAAiRAAiRAAiRAAiRAAiRAAiRAAiRAAiRAAiSQRoAK3jQkDCABEiABEiABEiABEiABEiABEiABEiABEiABEiCB0iBABW9pXCfWkgRIgARIgARIgARIgARIgARIgARIgARIgARIgATSCFDBm4aEASRAAiRAAiRAAiRAAiRAAiRAAiRAAiRAAiRAAiRQGgSo4C2N68RakgAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJkEAaASp405AwgARIgARIgARIgARIgARIgARIgARIgARIgARIgARKgwAVvKVxnVhLEiABEiABEiABEiABEiABEiABEiABEiABEiABEkgjQAVvGhIGkAAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJkEBpEKCCtzSuE2tJAiRAAiRAAiRAAiRAAiRAAiRAAiRAAiRAAiRAAmkEqOBNQ8IAEiABEiABEiABEiABEiABEiABEiABEiABEiABEigNAlTwlsZ1Yi1JgARIgARIgARIgARIgARIgARIgARIgARIgARIII0AFbxpSBhAAiRAAiRAAiRAAiRAAiRAAiRAAiRAAiRAAiRAAqVBgAre0rhOrCUJkAAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJpBGggjcNCQNIgARIgARIgARIgARIgARIgARIgARIgARIgARIoDQIUMFbGteJtSQBEiABEiABEiABEiABEiABEiABEiABEiABEiCBNAJU8KYhYQAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJkAAJlAYBKnhL4zqxliRAAiRAAiRAAiRAAiRAAiRAAiRAAiRAAiRAAiSQRoAK3jQkDCABEiABEiABEiABEiABEiABEiABEiABEiABEiCB0iBABW9pXCfWkgRIgARIgARIgARIgARIgARIgARIgARIgARIgATSCPw/TbeVoAworBQAAAAASUVORK5CYII=" />

<!-- rnb-plot-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Individual Results 


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBleHRyYWN0IHBvc3RlcmlvciBwcmVkaWN0aW9uc1xuIyMgaW5kaXZpZHVhbCBjb250cmlidXRpb25zXG5pbmRpdi5tb2RlbHMgPC0gYyhzbWxpbmR2LmZpdCwgbHJnaW5kdi5maXQsIHRvdGluZHYuZml0LCBpbmRpdmlkZW9wYWMuZml0LFxuICAgICAgICAgICAgICAgICAgYml6aW5kdi5maXQpXG5uYW1lcyhpbmRpdi5tb2RlbHMpIDwtIGMoXCJzbWxpbmR2LmZpdFwiLCBcImxyZ2luZHYuZml0XCIsIFwidG90aW5kdi5maXRcIiwgXG4gICAgICAgICAgICAgICAgICAgICAgICAgXCJpbmRpdmlkZW9wYWMuZml0XCIsIFwiYml6aW5kdi5maXRcIilcbnBvc3QuaW5kaXYgPC0gXG4gIGluZGl2Lm1vZGVscyAlPiVcbiAgbWFwX2Rmcih+IHNwcmVhZF9kcmF3cyguLCBhLCBiX25wKSwgLmlkID0gXCJtb2RlbFwiKVxuXG4jIyBtYWluIGVmZmVjdHNcbnBvc3QuaW5kaXYgJT4lXG4gIG11dGF0ZShiX25wID0gZXhwKGJfbnApIC0gMSkgJT4lXG4gIGdncGxvdCgpICtcbiAgc3RhdF9oYWxmZXllaChhZXMoeSA9IGZjdF9yZW9yZGVyKG1vZGVsLCBiX25wKSwgeCA9IGJfbnApLCBcbiAgICAgICAgICAgICAgICAud2lkdGggPSBjKDAuODksIDAuNSksIFxuICAgICAgICAgICAgICAgIHBvaW50X2ludGVydmFsID0gbWVkaWFuX2hkaSxcbiAgICAgICAgICAgICAgICBmaWxsID0gXCIjMkQ2Q0MwXCIsXG4gICAgICAgICAgICAgICAgY29sb3IgPSBcImJsYWNrXCIsXG4gICAgICAgICAgICAgICAgYWxwaGEgPSAwLjkpICtcbiAgZ2VvbV92bGluZShhZXMoeGludGVyY2VwdCA9IDApLCBsaW5ldHlwZSA9IFwiZGFzaGVkXCIsIGNvbG9yID0gXCJncmV5NTVcIikgK1xuICBzY2FsZV94X2NvbnRpbnVvdXMobGFiZWxzID0gcGVyY2VudF9mb3JtYXQoKSxcbiAgICAgICAgICAgICAgICAgICAgIGJyZWFrcyA9IHByZXR0eV9icmVha3MobiA9IDYpKSArXG4gIHNjYWxlX3lfZGlzY3JldGUobGFiZWxzID0gYyhcIkluZGl2LiBCdXNpbmVzcyAkXCIsIFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJMYXJnZSBJbmRpdmlkdWFsICRcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiSW5kaXYuIElkZW9sb2dpY2FsICRcIixcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiVG90YWwgSW5kaXZpZHVhbCAkXCIsXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIlNtYWxsIEluZGl2aWR1YWwgJFwiKSkgK1xuICBsYWJzKHggPSBcIiUgQ2hhbmdlIGluIENvbnRyaWJ1dGlvbnMgKGV4cChDb2VmZmljaWVudCBFc3RpbWF0ZSkgLSAxKVwiLCBcbiAgICAgICB5ID0gXCJPdXRjb21lIFZhcmlhYmxlXCIpICtcbiAgdGhlbWVfcHVicigpXG5nZ3NhdmUoXCJGaWd1cmVzL1Jlc3VsdHMvaW5kdl9jb2VmLnBkZlwiLCBoZWlnaHQgPSA0LCB3aWR0aCA9IDcpXG5cbiMjIHByZWRpY3RlZCB2YWx1ZXNcbnByZWQuaW5kdiA8LSBcbiAgcG9zdC5pbmRpdiAlPiVcbiAgZ3JvdXBfYnkobW9kZWwpICU+JVxuICBtdXRhdGUobm9fcGFjX3ByZWQgPSBleHAoYSArIGJfbnApLFxuICAgICAgICAgcGFjX3ByZWQgPSBleHAoYSkpICU+JVxuICBkcGx5cjo6c2VsZWN0KG1vZGVsLCBub19wYWNfcHJlZCwgcGFjX3ByZWQpICU+JVxuICBnYXRoZXIobm9fcGFjX3ByZWQsIHBhY19wcmVkLCBcbiAgICAgICAgIGtleSA9IFwidHlwZVwiLCB2YWx1ZSA9IFwicHJlZGljdGlvblwiKSAlPiVcbiAgZ3JvdXBfYnkobW9kZWwsIHR5cGUpICU+JVxuICBtZWRpYW5faGRpKC53aWR0aCA9IDAuODkpXG5cbmdncGxvdChkYXRhID0gcHJlZC5pbmR2LCBcbiAgICAgICBhZXMoeCA9IGZjdF9yZW9yZGVyKG1vZGVsLCBwcmVkaWN0aW9uKSxcbiAgICAgICAgICAgeSA9IHByZWRpY3Rpb24sIFxuICAgICAgICAgICB5bWluID0gLmxvd2VyLCBcbiAgICAgICAgICAgeW1heCA9IC51cHBlcixcbiAgICAgICAgICAgY29sb3IgPSB0eXBlLFxuICAgICAgICAgICBsYWJlbCA9IGNvbW1hKHJvdW5kKHByZWRpY3Rpb24sIGRpZ2l0cyA9IDApKSkpICtcbiAgZ2VvbV9wb2ludChwb3NpdGlvbiA9IHBvc2l0aW9uX2RvZGdlKC4xKSwgc2l6ZSA9IDIpICtcbiAgZ2VvbV9lcnJvcmJhcihwb3NpdGlvbiA9IHBvc2l0aW9uX2RvZGdlKCksIHdpZHRoID0gMC4xKSArXG4gIGdlb21fdGV4dF9yZXBlbChtYXBwaW5nID0gYWVzKGdyb3VwID0gdHlwZSksXG4gICAgICAgICAgICAgICAgICBwb3NpdGlvbiA9IHBvc2l0aW9uX2RvZGdlKDAuMSksXG4gICAgICAgICAgICAgICAgICBhbHBoYSA9IDAuOSxcbiAgICAgICAgICAgICAgICAgIHNlZ21lbnQuc2l6ZSA9IDAsXG4gICAgICAgICAgICAgICAgICBzZWdtZW50LmFscGhhID0gMCxcbiAgICAgICAgICAgICAgICAgIHNob3cubGVnZW5kID0gRkFMU0UpICtcbiAgc2NhbGVfeV9jb250aW51b3VzKGxhYmVscyA9IGRvbGxhcl9mb3JtYXQoKSxcbiAgICAgICAgICAgICAgICAgICAgIGJyZWFrcyA9IHByZXR0eV9icmVha3MobiA9IDYpKSArXG4gIHNjYWxlX3hfZGlzY3JldGUobGFiZWxzID0gYyhcIkluZGl2LiBJZGVvbG9naWNhbCAkXCIsXCJTbWFsbCBJbmRpdmlkdWFsICRcIiwgXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICBcIkluZGl2LiBCdXNpbmVzcyAkXCIsIFwiTGFyZ2UgSW5kaXZpZHVhbCAkXCIsIFxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJUb3RhbCBJbmRpdmlkdWFsICRcIikpICtcbiAgbGFicyh4ID0gXCJPdXRjb21lIFZhcmlhYmxlXCIsIFxuICAgICAgIHkgPSBcIlByZWRpY3RlZCBDb250cmlidXRpb25zXCIsIFxuICAgICAgIGNvbG9yID0gXCJQcmVkaWN0aW9uOlwiKSArXG4gIHNjYWxlX2NvbG9yX21hbnVhbCh2YWx1ZXMgPSBjKFwiIzJENkNDMFwiLCBcIiNGMUFCMDBcIiksXG4gICAgICAgICAgICAgICAgICAgICBsYWJlbHMgPSBjKFwiTm8gdG8gQ29ycG9yYXRlIFBBQ3NcIiwgXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIFwiWWVzIHRvIENvcnBvcmF0ZSBQQUNzXCIpKSArXG4gIHRoZW1lX3B1YnIoKSArXG4gIHRoZW1lKGF4aXMudGV4dC54ID0gZWxlbWVudF90ZXh0KGFuZ2xlID0gNDUsIHZqdXN0ID0gMSwgaGp1c3QgPSAxKSlcbmdnc2F2ZShcIkZpZ3VyZXMvUmVzdWx0cy9pbmR2X3ByZWRzLnBkZlwiLCBoZWlnaHQgPSA0LCB3aWR0aCA9IDgpXG5cbiMgVGFibGVzIC0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS1cbnNpbmsoZmlsZSA9IFwiVGFibGVzL2luZGl2X3Jlc3VsdHMudGV4XCIpXG5tY21jUmVnKGxpc3Qoc21saW5kdi5maXQsIGxyZ2luZHYuZml0LCB0b3RpbmR2LmZpdCwgaW5kaXZpZGVvcGFjLmZpdCxcbiAgICAgICAgICAgICAgICAgIGJpemluZHYuZml0KSxcbiAgICAgICAgcG9pbnRlc3QgPSBcIm1lZGlhblwiLFxuICAgICAgICBjaSA9IDAuODksXG4gICAgICAgIGhwZGkgPSBUUlVFLFxuICAgICAgICBmb3JtYXQgPSBcImxhdGV4XCIsXG4gICAgICAgIHBhcnMgPSBjKFwiYVwiLCBcImJfbnBcIiwgXCJiX25ld1wiLCBcImJfd2hpdGVcIiwgXCJiX2JsYWNrXCIsIFwiYl9sYXRpbm9cIiwgXG4gICAgICAgICAgICAgICAgIFwiYl9hc2lhblwiLCBcImJfbWhoXCIsIFwiYl9jbGludFwiLCBcImJfaG91c2VcIiwgXCJiX3Zwb3BcIiwgXCJzY2FsZVwiLFxuICAgICAgICAgICAgICAgICBcInByb2JcIikpXG5zaW5rKClcbmBgYCJ9 -->

```r
# extract posterior predictions
## individual contributions
indiv.models <- c(smlindv.fit, lrgindv.fit, totindv.fit, individeopac.fit,
                  bizindv.fit)
names(indiv.models) <- c("smlindv.fit", "lrgindv.fit", "totindv.fit", 
                         "individeopac.fit", "bizindv.fit")
post.indiv <- 
  indiv.models %>%
  map_dfr(~ spread_draws(., a, b_np), .id = "model")

## main effects
post.indiv %>%
  mutate(b_np = exp(b_np) - 1) %>%
  ggplot() +
  stat_halfeyeh(aes(y = fct_reorder(model, b_np), x = b_np), 
                .width = c(0.89, 0.5), 
                point_interval = median_hdi,
                fill = "#2D6CC0",
                color = "black",
                alpha = 0.9) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey55") +
  scale_x_continuous(labels = percent_format(),
                     breaks = pretty_breaks(n = 6)) +
  scale_y_discrete(labels = c("Indiv. Business $", 
                              "Large Individual $",
                              "Indiv. Ideological $",
                              "Total Individual $",
                              "Small Individual $")) +
  labs(x = "% Change in Contributions (exp(Coefficient Estimate) - 1)", 
       y = "Outcome Variable") +
  theme_pubr()
ggsave("Figures/Results/indv_coef.pdf", height = 4, width = 7)

## predicted values
pred.indv <- 
  post.indiv %>%
  group_by(model) %>%
  mutate(no_pac_pred = exp(a + b_np),
         pac_pred = exp(a)) %>%
  dplyr::select(model, no_pac_pred, pac_pred) %>%
  gather(no_pac_pred, pac_pred, 
         key = "type", value = "prediction") %>%
  group_by(model, type) %>%
  median_hdi(.width = 0.89)

ggplot(data = pred.indv, 
       aes(x = fct_reorder(model, prediction),
           y = prediction, 
           ymin = .lower, 
           ymax = .upper,
           color = type,
           label = comma(round(prediction, digits = 0)))) +
  geom_point(position = position_dodge(.1), size = 2) +
  geom_errorbar(position = position_dodge(), width = 0.1) +
  geom_text_repel(mapping = aes(group = type),
                  position = position_dodge(0.1),
                  alpha = 0.9,
                  segment.size = 0,
                  segment.alpha = 0,
                  show.legend = FALSE) +
  scale_y_continuous(labels = dollar_format(),
                     breaks = pretty_breaks(n = 6)) +
  scale_x_discrete(labels = c("Indiv. Ideological $","Small Individual $", 
                              "Indiv. Business $", "Large Individual $", 
                              "Total Individual $")) +
  labs(x = "Outcome Variable", 
       y = "Predicted Contributions", 
       color = "Prediction:") +
  scale_color_manual(values = c("#2D6CC0", "#F1AB00"),
                     labels = c("No to Corporate PACs", 
                                "Yes to Corporate PACs")) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Figures/Results/indv_preds.pdf", height = 4, width = 8)

# Tables ----------------------------------------------------------------------
sink(file = "Tables/indiv_results.tex")
mcmcReg(list(smlindv.fit, lrgindv.fit, totindv.fit, individeopac.fit,
                  bizindv.fit),
        pointest = "median",
        ci = 0.89,
        hpdi = TRUE,
        format = "latex",
        pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                 "b_asian", "b_mhh", "b_clint", "b_house", "b_vpop", "scale",
                 "prob"))
sink()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Vote Results


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuIyBleHRyYWN0IHBvc3RlcmlvciBwcmVkaWN0aW9uc1xuIyMgdm90aW5nXG52b3RlLm1vZGVscyA8LSBjKHZvdGluZy5maXQsIHR1cm5vdXQuZml0KVxubmFtZXModm90ZS5tb2RlbHMpIDwtIGMoXCJ2b3RpbmcuZml0XCIsIFwidHVybm91dC5maXRcIilcbnBvc3Qudm90ZSA8LSBcbiAgdm90ZS5tb2RlbHMgJT4lXG4gIG1hcF9kZnIofiBzcHJlYWRfZHJhd3MoLiwgYSwgYl9ucCksIC5pZCA9IFwibW9kZWxcIilcblxuIyMgQ29lZmZpY2llbnQgcGxvdFxucG9zdC52b3RlICU+JVxuICBtdXRhdGUoYl9ucCA9IGV4cChiX25wKSAtIDEpICU+JVxuICBnZ3Bsb3QoKSArXG4gIHN0YXRfaGFsZmV5ZWgoYWVzKHkgPSBmY3RfcmVvcmRlcihtb2RlbCwgYl9ucCksIHggPSBiX25wKSwgXG4gICAgICAgICAgICAgICAgLndpZHRoID0gYygwLjg5LCAwLjUpLCBcbiAgICAgICAgICAgICAgICBwb2ludF9pbnRlcnZhbCA9IG1lZGlhbl9oZGksXG4gICAgICAgICAgICAgICAgZmlsbCA9IFwiIzJENkNDMFwiLFxuICAgICAgICAgICAgICAgIGNvbG9yID0gXCJibGFja1wiLFxuICAgICAgICAgICAgICAgIGFscGhhID0gMC45KSArXG4gIGdlb21fdmxpbmUoYWVzKHhpbnRlcmNlcHQgPSAwKSwgbGluZXR5cGUgPSBcImRhc2hlZFwiKSArXG4gIHNjYWxlX3hfY29udGludW91cyhsYWJlbHMgPSBwZXJjZW50X2Zvcm1hdCgpLFxuICAgICAgICAgICAgICAgICAgICAgYnJlYWtzID0gcHJldHR5X2JyZWFrcyhuID0gNikpICtcbiAgc2NhbGVfeV9kaXNjcmV0ZShsYWJlbHMgPSBjKFwiVi5BLlAuIFR1cm5vdXRcIiwgXCJWb3RlIFNoYXJlXCIpKSArXG4gIGxhYnMoeCA9IFwiJSBDaGFuZ2UgaW4gQ29udHJpYnV0aW9ucyAoZXhwKENvZWZmaWNpZW50IEVzdGltYXRlKSAtIDEpXCIsIFxuICAgICAgIHkgPSBcIk91dGNvbWUgVmFyaWFibGVcIikgK1xuICB0aGVtZV9wdWJyKClcbmdnc2F2ZShcIkZpZ3VyZXMvUmVzdWx0cy92b3RlX2NvZWYucGRmXCIsIGhlaWdodCA9IDQsIHdpZHRoID0gNylcblxuIyMgcHJlZGljdGVkIHZhbHVlc1xucHJlZC52b3RlIDwtIFxuICBwb3N0LnZvdGUgJT4lXG4gIGdyb3VwX2J5KG1vZGVsKSAlPiVcbiAgbXV0YXRlKG5vX3BhY19wcmVkID0gZXhwKGEgKyBiX25wKSxcbiAgICAgICAgIHBhY19wcmVkID0gZXhwKGEpKSAlPiVcbiAgZHBseXI6OnNlbGVjdChtb2RlbCwgbm9fcGFjX3ByZWQsIHBhY19wcmVkKSAlPiVcbiAgZ2F0aGVyKG5vX3BhY19wcmVkLCBwYWNfcHJlZCwgXG4gICAgICAgICBrZXkgPSBcInR5cGVcIiwgdmFsdWUgPSBcInByZWRpY3Rpb25cIikgJT4lXG4gIGdyb3VwX2J5KG1vZGVsLCB0eXBlKSAlPiVcbiAgbWVkaWFuX2hkaSgud2lkdGggPSAwLjg5KVxuXG4jIyBhbGwgcHJlZGljdGlvbnNcbmdncGxvdChkYXRhID0gcHJlZC52b3RlLCBcbiAgICAgICBhZXMoeCA9IGZjdF9yZW9yZGVyKG1vZGVsLCBwcmVkaWN0aW9uKSxcbiAgICAgICAgICAgeSA9IHByZWRpY3Rpb24sIFxuICAgICAgICAgICB5bWluID0gLmxvd2VyLCBcbiAgICAgICAgICAgeW1heCA9IC51cHBlcixcbiAgICAgICAgICAgY29sb3IgPSB0eXBlLFxuICAgICAgICAgICBsYWJlbCA9IGNvbW1hKHJvdW5kKHByZWRpY3Rpb24sIGRpZ2l0cyA9IDIpKSkpICtcbiAgZ2VvbV9wb2ludChwb3NpdGlvbiA9IHBvc2l0aW9uX2RvZGdlKC4xKSwgc2l6ZSA9IDIpICtcbiAgZ2VvbV9lcnJvcmJhcihwb3NpdGlvbiA9IHBvc2l0aW9uX2RvZGdlKCksIHdpZHRoID0gMC4xKSArXG4gIGdlb21fdGV4dF9yZXBlbChtYXBwaW5nID0gYWVzKGdyb3VwID0gdHlwZSksXG4gICAgICAgICAgICAgICAgICBwb3NpdGlvbiA9IHBvc2l0aW9uX2RvZGdlKDAuMSksXG4gICAgICAgICAgICAgICAgICBhbHBoYSA9IDAuOSxcbiAgICAgICAgICAgICAgICAgIHNlZ21lbnQuc2l6ZSA9IDAsXG4gICAgICAgICAgICAgICAgICBzZWdtZW50LmFscGhhID0gMCxcbiAgICAgICAgICAgICAgICAgIHNob3cubGVnZW5kID0gRkFMU0UpICtcbiAgc2NhbGVfeV9jb250aW51b3VzKGJyZWFrcyA9IHByZXR0eV9icmVha3MobiA9IDYpKSArXG4gIHNjYWxlX3hfZGlzY3JldGUobGFiZWxzID0gYyhcIlYuQS5QLiBUdXJub3V0XCIsIFwiVm90ZSBTaGFyZVwiKSkgK1xuICBsYWJzKHggPSBcIk91dGNvbWUgVmFyaWFibGVcIiwgXG4gICAgICAgeSA9IFwiUHJlZGljdGVkICVcIiwgXG4gICAgICAgY29sb3IgPSBcIlByZWRpY3Rpb246XCIpICtcbiAgc2NhbGVfY29sb3JfbWFudWFsKHZhbHVlcyA9IGMoXCIjMkQ2Q0MwXCIsIFwiI0YxQUIwMFwiKSxcbiAgICAgICAgICAgICAgICAgICAgIGxhYmVscyA9IGMoXCJObyB0byBDb3Jwb3JhdGUgUEFDc1wiLCBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgXCJZZXMgdG8gQ29ycG9yYXRlIFBBQ3NcIikpICtcbiAgdGhlbWVfcHVicigpXG5nZ3NhdmUoXCJGaWd1cmVzL1Jlc3VsdHMvdm90ZV9wcmVkcy5wZGZcIiwgaGVpZ2h0ID0gNCwgd2lkdGggPSA2KVxuXG4jIFRhYmxlcyAtLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tLS0tXG5zaW5rKGZpbGUgPSBcIlRhYmxlcy92b3RlX3Jlc3VsdHMudGV4XCIpXG5tY21jUmVnKGxpc3Qodm90aW5nLmZpdCwgdHVybm91dC5maXQpLFxuICAgICAgICBwb2ludGVzdCA9IFwibWVkaWFuXCIsXG4gICAgICAgIGNpID0gMC44OSxcbiAgICAgICAgaHBkaSA9IFRSVUUsXG4gICAgICAgIGZvcm1hdCA9IFwibGF0ZXhcIixcbiAgICAgICAgcGFycyA9IGMoXCJhXCIsIFwiYl9ucFwiLCBcImJfbmV3XCIsIFwiYl93aGl0ZVwiLCBcImJfYmxhY2tcIiwgXCJiX2xhdGlub1wiLCBcbiAgICAgICAgICAgICAgICAgXCJiX2FzaWFuXCIsIFwiYl9taGhcIiwgXCJiX2NsaW50XCIsIFwiYl9ob3VzZVwiLCBcImJfdnBvcFwiLCBcInNjYWxlXCIsXG4gICAgICAgICAgICAgICAgIFwicHJvYlwiKSlcbnNpbmsoKVxuYGBgIn0= -->

```r
# extract posterior predictions
## voting
vote.models <- c(voting.fit, turnout.fit)
names(vote.models) <- c("voting.fit", "turnout.fit")
post.vote <- 
  vote.models %>%
  map_dfr(~ spread_draws(., a, b_np), .id = "model")

## Coefficient plot
post.vote %>%
  mutate(b_np = exp(b_np) - 1) %>%
  ggplot() +
  stat_halfeyeh(aes(y = fct_reorder(model, b_np), x = b_np), 
                .width = c(0.89, 0.5), 
                point_interval = median_hdi,
                fill = "#2D6CC0",
                color = "black",
                alpha = 0.9) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous(labels = percent_format(),
                     breaks = pretty_breaks(n = 6)) +
  scale_y_discrete(labels = c("V.A.P. Turnout", "Vote Share")) +
  labs(x = "% Change in Contributions (exp(Coefficient Estimate) - 1)", 
       y = "Outcome Variable") +
  theme_pubr()
ggsave("Figures/Results/vote_coef.pdf", height = 4, width = 7)

## predicted values
pred.vote <- 
  post.vote %>%
  group_by(model) %>%
  mutate(no_pac_pred = exp(a + b_np),
         pac_pred = exp(a)) %>%
  dplyr::select(model, no_pac_pred, pac_pred) %>%
  gather(no_pac_pred, pac_pred, 
         key = "type", value = "prediction") %>%
  group_by(model, type) %>%
  median_hdi(.width = 0.89)

## all predictions
ggplot(data = pred.vote, 
       aes(x = fct_reorder(model, prediction),
           y = prediction, 
           ymin = .lower, 
           ymax = .upper,
           color = type,
           label = comma(round(prediction, digits = 2)))) +
  geom_point(position = position_dodge(.1), size = 2) +
  geom_errorbar(position = position_dodge(), width = 0.1) +
  geom_text_repel(mapping = aes(group = type),
                  position = position_dodge(0.1),
                  alpha = 0.9,
                  segment.size = 0,
                  segment.alpha = 0,
                  show.legend = FALSE) +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  scale_x_discrete(labels = c("V.A.P. Turnout", "Vote Share")) +
  labs(x = "Outcome Variable", 
       y = "Predicted %", 
       color = "Prediction:") +
  scale_color_manual(values = c("#2D6CC0", "#F1AB00"),
                     labels = c("No to Corporate PACs", 
                                "Yes to Corporate PACs")) +
  theme_pubr()
ggsave("Figures/Results/vote_preds.pdf", height = 4, width = 6)

# Tables ----------------------------------------------------------------------
sink(file = "Tables/vote_results.tex")
mcmcReg(list(voting.fit, turnout.fit),
        pointest = "median",
        ci = 0.89,
        hpdi = TRUE,
        format = "latex",
        pars = c("a", "b_np", "b_new", "b_white", "b_black", "b_latino", 
                 "b_asian", "b_mhh", "b_clint", "b_house", "b_vpop", "scale",
                 "prob"))
sink()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->

