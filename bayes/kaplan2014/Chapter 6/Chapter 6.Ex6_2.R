#-----------------------------------------------------------------------------------------
# EXAMPLE 6.2: BAYESIAN MULTIPLE REGRESSION WITH INFORMATIVE PRIORS
#             Program Steps
# Same as Example 6.1 except now priors are specified for the regression coefficients 
# b0 and precisions B0.
#-----------------------------------------------------------------------------------------

FullModel_inf <- MCMCregress(rcomb1~gender+native+ slang +  ESCS+
       JOYREAD+ DIVREAD+ MEMOR+ ELAB+ CSTRAT,data=datafile9,
       marginal.likelihood="Chib95",mcmc=10000,
b0=c(491.2, 12.9, 3.4, 21.4, 32.6, 22.9, 0.01, -18.6, -11.1, 22.8),
B0=c(0.0173, 0.0932, 0.0141, 0.0216, 0.3354, 0.3210, 0.3245, 0.2101, 0.2111, 0.1707))

pdf('Chapter 6/FullModel_inf.trace.pdf')
plot(FullModel_inf) # Produces the convergence plots and the posterior densities
pdf('Chapter 6/FullModel_inf.acf.pdf')
autocorr.plot(FullModel_inf)
dev.off()
summary(FullModel_inf)

geweke.diag(FullModel_inf, frac1=0.1, frac2=0.5) 
heidel.diag(FullModel_inf,eps=0.1,pvalue=0.05)  
raftery.diag(FullModel_inf,q=0.5,r=0.05,s=0.95,converge.eps=0.001)  

