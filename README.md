# Legal institutions and oppressive violence

## Notes

- [x] Update data with CCP and V-Dem vars
- [ ] Simple bivariate relationships; for each of the 15 vars of interest, plot var vs outcome; appendix stuff if include at all
- [ ] Redo models; for each of the 15 vars of interest:
    - [ ] Bivariate model, with RE if possible
    - [ ] Bivariate model with control vars, and RE if possible
- [ ] Control models
    - [ ] Only random effects
    - [ ] Control variables: GDP, pop, itt restricted access (with RE? depends on models above)
    - [ ] xgboost on restricted var set (control + vars of interest)
    - [ ] for giggles, xgboost on full data set
- [ ] Prepare output:
    - [ ] coefficient plot for vars of interest
    - [ ] coefficient table for vars of interest
    - [ ] some kind of fit comparison table (or chart)
    - [ ] xgboost var importance plot
- Put some evidence that RE intercepts are related to democracy somewhere in appendix, might be handy at some point

Cool, I think imputing 0s for those countries will be okay. Here is a list of what we need from CCP: 
TORTURE - const prohibition on torture; p. 116; 1=always prohibited, 3=prohibited for extracting confession; 96/98=other/not specified
PREREL - pre-trial release; p. 97
HABCORP - habeas corpus; p. 97
DUEPROC - due process; p. 99
SPEEDTRI - speedy trial; p. 100

Why don't we stick with V-Dem for democracy/institutions indicators? I want to stay away from their aggregate democracy indicators, though. Reading the code book, it is pretty clear that some of the components measure government violence, which is not good for us. So let's pull these: 
v2x_elecoff - elected officials 
v2xel_frefair - clean elections 
v2asuffrage - suffrage 
v2x_jucon - judicial constraints 
v2xlg_legcon - legislative constraints 

We should also include indicators that measure the relative status of different social groups, as these are likely to predict torture of marginalized groups. V-Dem has some of these: 
v2clacjust - poor/rich have same civil libs
v2clsocgrp - social groups (ethnic) have same civil libs
v2clnlpct - % that live in geographic areas where civ lib protection is weaker
v2pepwrses - poor/rich have same power/influence
v2pepwrsoc - social groups have same power/influence

I think it also makes sense to include the "exclpop" variable from the EPR data, which measures % excluded population. 

We can ditch the binary democracy indicator from Vreeland et al (dd_democracy), as well as legal system type and the judicial independence variable from Linzer/Staton (LJI). Other than what is listed above, I think we will only need population and GDP/capita. I think it makes sense to estimate a count model for each non-control variable, controlling for pop and gdp/capita. And then include everything in xgboost. Does that sound good? I will be working on the draft and will put it on Github as I make progress.

