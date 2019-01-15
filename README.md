# Legal institutions and oppressive violence

## Notes

[] Ingest comparative constitutions 
[x] Add GTD events
[x] Add ACED civil war
[x] Add EPR 
[] Update/rename models: analyses for democracy, LJI, legal system, CCP
[] Add full control models with some combination of: regime, wealth, conflict, 
   population, ethnic stuff; see whether base2 needs to be revised
   [] base model 3 convergence issues
[ ] Finish LJI (democracy or controls + LJI?) and legal sys model updates
[x] Re-run xgboost with new variable set: only v2x_polyarchy (and libdem?); keep 
   GDP, per capita, pop, rents
[x] Figure 3: scatterplot matrix; show best fit lines instead, or some other way
   to make it less cluttered

Less certain tasks:

[x] xgboost on random intercepts model residuals
[] Add data for press freedom?
[] Check why democracy in bivariate has no relationship but in models does 
   (random intercepts); RIs are correlated with democracy and wealth.
[] Fix ITT aggregation (dissident no multi-events)


Include ITT restricted access

bivariate model for each, with RE if possible
model with control vars (GDP, pop), RE if possible

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

deliverables:

coef plot
coef table
fit table, xgboost and maybe a few other models that have best fit?
var importance plot

