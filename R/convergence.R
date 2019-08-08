
library("numDeriv")

?lme4::convergence

cy$SP.POP.TOTL_ln_norm <- scale(cy$SP.POP.TOTL_ln)
cy$gtd_events_log1p_norm <- scale(log1p(cy$gtd_events))

mdl <- mdl_base3[[2]]

dd <- update(mdl,devFunOnly=TRUE)
pars <- unlist(getME(mdl,c("theta","fixef")))
grad2 <- grad(dd,pars)
hess2 <- hessian(dd,pars)
sc_grad2 <- solve(hess2,grad2)
max(pmin(abs(sc_grad2),abs(grad2)))

source(system.file("utils", "allFit.R", package="lme4"))

mdl_all <- allFit(mdl)
summary(mdl_all)
