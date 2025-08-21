-----
# this block contain the metadata of the .md document, you can add here:
#   - background keys: header, footer.
#   - bslib::card arguments.
# you can have more information how to use this section on the background
# vingette
header: "Abstract"
-----

#### Safety of COVID-19 mRNA vaccines administered during pregnancy: a target trial emulation and meta-analysis of 4 European countries.

#### Background
Initially, mRNA COVID-19 vaccines were recommended during pregnancy; however, pregnant individuals were excluded from early clinical trials, resulting in limited data on vaccine efficacy and safety for this group. Subsequent observational studies to date have indicated that mRNA vaccines are safe during pregnancy, showing no increased risk of complications such as miscarriage, preterm delivery, stillbirth, or birth defects. Nevertheless, these studies often involve small sample sizes, limiting the ability to evaluate rare adverse events and the effects of different vaccine doses. 

#### Objectives
To leverage 4 real-world-data sources to assess whether mRNA COVID-19 vaccines increase the risk of these adverse events. 

#### Methods 
##### Data Sources
The study will include participants registered in primary care data from the United Kingdom, primary care data linked with hospital discharge data from Catalonia (Spain), and nationwide linked health registries from Norway and Sweden. All databases will have been mapped to the OMOP common data model facilitating federated analysis pipeline.

##### Study design
Cohort study following the target trial emulation framework. We will compare pregnant women receiving a 1st, 2nd, or booster (3rd or subsequent doses) COVID-19 vaccine during pregnancy (with an mRNA vaccine), compared to pregnant women eligible for the same dose but not yet vaccinated with it.

##### Outcomes 
Outcomes of interest in the study are:

1) 15 Adverse Events of Special Interest (AESI): Deep vein thrombosis, Pulmonary embolism, Myocardial infarction, Ischaemic stroke, Bell’s palsy, Encephalitis, Guillain Barré Syndrome, Transverse Myelitis, Haemorrhagic stroke, Myocarditis or pericarditis, Thrombosis with Thrombocytopenia, Immune Thrombocytopenia, Anaphylaxis, Narcolepsy, Disseminated Intravascular Coagulation

2) 13 Maternal Adverse Events (MAE): Miscarriage, Stillbirth, Preterm labour, Eclampsia, HELLP syndrome, Antepartum Haemorrhage, Dysfunctional labour, Postpartum haemorrhage, Postpartum endometritis, Maternal death


Outcome assessment time-window for AESI is the 42 days after vaccination, or until the end of pregnancy, whichever comes first. For the MAE, outcome-time is given by their definition: up until week 19 for miscarriage, 6 weeks after pregnancy end for post-partum endometriosis and maternal death, 12 weeks after pregnancy end for postpartum haemorrhage, and up until the end of pregnancy for the rest. As a secondary analysis, we will look at AESI 42 days after index date regardless of pregnancy end.

##### Study population 
Women pregnant during the enrolment period, aged 12 to 55 years at pregnancy start date, and with 365 days or more of previous observation in the database will be eligible to enter the study. Women with an occurrence of any of the outcomes of interest during the outcome-specific washout window relative to pregnancy start will be excluded.
Enrolment period will be different for each exposure of interest. When assessing the 1st dose of an mRNA COVID-19 vaccine during pregnancy, the enrolment period will span from 01/04/2021 to 28/02/2022. When studying the 2nd dose during, the enrolment period runs from 01/05/2021 to 31/03/2022. Finally, when the exposure of interest is the booster dose during pregnancy, the enrolment period goes from 01/10/2021 (when 1st booster was approved) to 12 months before the data cut-off.
For each day within the enrolment period, pregnant women receiving the exposure (vaccine dose) of interest will enter the exposure cohort. Index date for exposed will be exposure date. Simultaneously, a sample of eligible comparators (pregnant women who have not yet received the exposure) will enter the comparator cohort if they match on gestational age (2-week band) and maternal age (2-year band). Index date for sampled matched comparators will be the same as that of the exposed counterpart.

##### Follow-up
Follow-up will start at 1 day after the index date until the earliest of the following events: 1) outcome of interest, 2) end of time-window to observe the outcome of interest, 3) death or end of available data, 4) occurrence of a truncating events that precludes the outcome of interest (e.g., miscarriage for postpartum haemorrhage), and 5) subsequent COVID-19 vaccination. 
In sensitivity analysis, follow-up will also be stopped at COVID-19 infection.

##### Data analysis
Large scale Standardised Mean Differences (SMDs) will be calculated between exposed and comparator cohorts. An SMD ≤0.1 will indicate adequate covariate balance. 68 Negative Control Outcomes (NCOs) will be used to detect residual confounding. 
If the sampled cohorts are not balanced, Propensity Score (PS) overlap weighting will be used to create comparable cohorts. The PS model will include variables on demographics (age, location, index date), pregnancy-related (gestational time at enrolment, previous pregnancies, complications during pregnancy, obesity, smoking status and alcohol use), healthcare utilisation (days in observation, healthcare visits…), and pre-existing conditions (all history) and medications (last 180 days) selected through LASSO regression.  SMD and NCO will be used to assess observed and unobserved confounding in the weighted population. If required, PS-modeling will be iterated until balance is achieved.
Incidence Rate Ratios will measure the relative incidence of the outcomes of interest between exposed and comparator cohorts. Empirical calibration will adjust estimates and confidence intervals if NCOs indicate residual confounding. 
Database-specific estimates will be pooled through random-effects meta-analysis for overall results.


