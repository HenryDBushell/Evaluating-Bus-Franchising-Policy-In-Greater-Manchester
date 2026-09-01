# Evaluating-Bus-Franchising-Policy-In-Greater-Manchester

**This project is still a work in progress. The data-wrangling code is long and messy, as I was coding in R for the first time and getting used to how to wrangle data in R (whilst not using AI to write code or teach me how to code). I am going to refine my code and make it clearer later on. HOWEVER, this code runs cleanly and reasonably quickly, presenting results for the user without any manual data wrangling needed.**

This is a paper written in the second year of my BSc Economics course for the module ESPS0015 Political Economy. It uses DiD methods to evaluate the efficacy of Mayor Burnham's bus franchising policy on the number of bus journeys taken in Greater Manchester. 

Data is sourced from the Department for Transport and the Office for National Statistics (please see the references in the paper for the specific pieces of data used), automatically wrangled into a clean panel data format using R (tidyverse, dplyr), then 5 DiD models are run on this data (simple pooled OLS, pooled OLS with controls, TWFE with controls, first differencing with controls then one-way fixed effects with controls and a time trend). 

All you should need to replicate the results of this project is: R Studio, as well as the data sources used (downloaded straight from here or from the government websites as referenced properly in the paper, as long as you replicate the structure of the "Sources of data" file as it appears in this repository). 

The paper is more of an proof-of-concept, with sufficient data not yet being available to evaluate the policy properly. The main aim of the paper is to use R and GitHub in a reproducible fashion as well as apply serious undergraduate-level econometrics to a novel research question. **The limits of the approach are discussed to a great extent in the appendix**.
