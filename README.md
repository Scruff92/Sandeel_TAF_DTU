# 2019_san.sa.1r

2019 - Sandeel (*Ammodytes* spp.) in Divisions 4.b and 4.c, Sandeel Area 1r
(central and southern North Sea, Dogger Bank) - HAWG

## How to run

Install the icesTAF package from CRAN, then open R in the `2019_san.sa.1r`
directory and run:

```
library(icesTAF)
taf.bootstrap()
sourceAll()
```

Note that there are two user-defined inputs at the beginning of the **Forecast.R** script:
* fcap = the appropriate fcap value for this area
* previous_TAC = The TAC reported in the previous years advice

## Explore results

To view the results on the TAF server, browse the
[GitHub taf branch](https://github.com/ices-taf/2019_san.sa.1r/tree/taf).

Once published, assessments results can also be browsed on the
[ICES TAF website](https://taf.ices.dk/app/stock#!/2019/san.sa.1r)


## Data input

To run the sandeel TAF, the following **data** files must be supplied in the `bootstrap/intial/data` directory:

 * canum.in
 * effort.in
 * fleet_catch.in
 * natmor.in
 * propmat.in
 * proportion_m_and_f_before_spawning.in
 * weca.in
 * west.in
 
 
 The following **config** files are also required in the `bootstrap/intial/data/config` directory:
 
 * fleet_info.dat
 * fleet_names.in
 * just_one.in
 * recruitment_years.in
 * reference_points.in
 * sms.dat
 * species_names.in
 * zero_catch_season_ages.in
 * zero_catch_year_season.in

 
 **NB: Note that all filenames must be in lower case if** *Linux* **is used.**
 
 
