# DISCRETE EVENT SIMULATION MODELLING FOR INTER-DONATION INTERVALS IN BLOOD DONORS

---
## Lois Kim, November 2022

These files accompany the paper by Kim et al, "Impact of a post-donation haemoglobin testing strategy on efficiency and safety of whole blood donation in England: a modelling study". Models are based on data from the COMPARE study [doi: 10.1111/tme.12750], using donor characteristics and haemoglobin (Hb) from two repeat donations over a follow-up period of up to one year. 

---
## SIMULATION MODEL

```./blooddonor_des.R```	Discrete event simulation (DES) model for post-donation haemoglobin (Hb) recovery, allowing user-input parameters to determine inter-donation interval strategy


---
## POST-DONATION RECALL STRATEGIES


Five post-donation recall strategies are modelled:

1. Current UK blood donor return strategy ("current")
* All men are eligible to return after 12 weeks, all women after 16 weeks.
* Non-Hb deferrals occur before on-site Hb testing (e.g. for medical reasons, or recent travel).
* Non-Hb deferrals are eligible again after a deferral period of 4 weeks.
* Men <135g/dl and women <125g/dl using an onsite test are deferred. 
* Low Hb deferrals are eligible again after a deferral period of 12 weeks.
* Men <125g/dl and women <115g/dl are considered to have very low Hb and are deferred longer.
* Very low Hb deferrals are eligible again after a deferral period of 52 weeks.

2. Donors eligible to return at time when modelled Hb is estimated to have 70% probability of being over the threshold to donate ("medcert")
* Threshold defined as >=135g/dl for men, >=125g/dl for women
* No on-site testing; all attenders donate
* Post-donation (off-site) testing provides Hb to inform modelling of next return time

3. Donors eligible to return at time when modelled Hb is estimated to have 90% probability of being over the threshold to donate ("highcert")
* Threshold defined as >=135g/dl for men, >=125g/dl for women
* No on-site testing; all attenders donate
* Post-donation (off-site) testing provides Hb to inform modelling of next return time

4. Donors eligible to return at time when modelled Hb is estimated to have 70% probability of being over the threshold to donate, with onsite testing for those in the 70-89% range ("onsite")
* Threshold defined as >=135g/dl for men, >=125g/dl for women
* Attenders with predicted 70-89% probability of being over threshold have onsite Hb testing, with deferrals handled as per current strategy
* Attenders with predicted >=90% probability of being over threshold all donate (no onsite testing)
* Post-donation (off-site) testing for all attenders provides Hb to inform modelling of next return time

5. Donors eligible to return at time when modelled Hb is estimated to have 90% probability of being over the threshold to donate, with return permitted as early as 8 weeks for men / 12 weeks for women ("early")
* Threshold defined as >=135g/dl for men, >=125g/dl for women
* No on-site testing; all attenders donate
* Post-donation (off-site) testing provides Hb to inform modelling of next return time


---
## INPUT PARAMETERS

Modelling is stratified by sex, with input parameters (including inter-donation interval parameters) provided separately for men and women. Due to data permissions associated with use of the COMPARE study data, a few lines of sample data only are provided for files in this section.

### COMPARE cohort characteristics: used to draw a donor population (with replacement) for the DES
* ```./men/input/blups_men.dta```	Stata data file holding best linear unbiased predictors (BLUPs) from mixed modelling of post-donation Hb in men
* ```./men/input/covs_men.dta```	Stata data file holding baseline characteristics for men in COMPARE
* ```./men/input/stpm_men.dta```	Stata data file holding data relevant for flexible parametric modelling of return times in men

* ```./women/input/blups_women.dta```	Stata data file holding best linear unbiased predictors (BLUPs) from mixed modelling of post-donation Hb in women
* ```./women/input/covs_women.dta```	Stata data file holding baseline characteristics for women in COMPARE
* ```./women/input/stpm_women.dta```	Stata data file holding data relevant for flexible parametric modelling of return times in women

### The DES is informed by underlying models of: 
	(i) post-donation Hb recovery (mixed effects regression model)
	(ii) probability of deferral for low Hb, given under the threshold to donate
	(iii) probability of deferral for other reasons
	(iv) time from eligibility to return to return to donate (flexible parametric rate model)
	(v) probability of dropout, i.e. non-attendance at return donation

Parameters relating to these underlying models are held in files corresponding to each post-donation recall strategy:

* ```./men/input/blooddonor_params_men_current.R```		
* ```./men/input/blooddonor_params_men_medcert.R```
* ```./men/input/blooddonor_params_men_highcert.R```
* ```./men/input/blooddonor_params_men_onsite.R```
* ```./men/input/blooddonor_params_men_early.R```

* ```./women/input/blooddonor_params_women_current.R```
* ```./women/input/blooddonor_params_women_medcert.R```
* ```./women/input/blooddonor_params_women_highcert.R```
* ```./women/input/blooddonor_params_women_onsite.R```
* ```./women/input/blooddonor_params_women_early.R```

---
## RUN FILES

### Load parameters and run DES
Example files set up for n=10,000. Stores results in ./men/output or ./women/output.

* ```./men/run_files/run_blooddonor_men_current.R```
* ```./men/run_files/run_blooddonor_men_medcert.R```
* ```./men/run_files/run_blooddonor_men_highcert.R```
* ```./men/run_files/run_blooddonor_men_onsite.R```
* ```./men/run_files/run_blooddonor_men_early.R```

* ```./women/run_files/run_blooddonor_women_current.R```
* ```./women/run_files/run_blooddonor_women_medcert.R```
* ```./women/run_files/run_blooddonor_women_highcert.R```
* ```./women/run_files/run_blooddonor_women_onsite.R```
* ```./women/run_files/run_blooddonor_women_early.R```

### Load parameters and run DES with PSA 
Drawing parameter values from joint distribution. Example file set up for 5 bootstrap samples of n=1000. Stores results in ./men/output or ./women/output.

* ```./men/run_files/run_blooddonor_men_psa_current.R```
* ```./men/run_files/run_blooddonor_men_psa_medcert.R```
* ```./men/run_files/run_blooddonor_men_psa_highcert.R```
* ```./men/run_files/run_blooddonor_men_psa_onsite.R```
* ```./men/run_files/run_blooddonor_men_psa_early.R```

* ```./women/run_files/run_blooddonor_women_psa_current.R```
* ```./women/run_files/run_blooddonor_women_psa_medcert.R```
* ```./women/run_files/run_blooddonor_women_psa_highcert.R```
* ```./women/run_files/run_blooddonor_women_psa_onsite.R```
* ```./women/run_files/run_blooddonor_women_psa_early.R```


