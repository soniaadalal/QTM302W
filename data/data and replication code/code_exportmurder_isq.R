# this code replicates output for the following paper
# Exporting Murder? US Deportations and the Spread of Violence
# Forthcoming International Studies Quarterly, C. Ambrosius and D. Leblang

# load libraries
	library(plm)
	library(lmtest)
	library(clubSandwich)
	library(AER)
	library (calibrate)

# ---------------------------
# Figure 1
# ---------------------------

#	setEPS()
#	postscript("figure1.eps", width = 5, height = 3) # store eps file
	dep_tot <- read.csv("data_USdeporttotal.csv")
	par(mar=c(4,6,2,2), mfrow=c(1,1)) 
	plot(dep_tot$YEAR, dep_tot$TOTAL, col="black", type="l", ylab= "deportations (in 1000s)", xlab = "", yaxt="n", lwd=2, cex.lab = .8, cex.axis = .7)
	points(dep_tot$YEAR, dep_tot$CONVICT, col="grey40", type="l", lwd=2)	
	axis(2, at= c(100000,200000,300000,400000), labels= c(100,200,300,400), cex.axis = .7)
	abline(h=c(200000, 400000), col = "grey60", lty=2, lwd=1)
	text(2000, 300000, "total", cex=.8)
	text(2010, 70000, "convicts", col="grey40", cex=.8)
#	dev.off()


# ---------------------------
# Figure 2
# ---------------------------

# show corrrelation between homicides and derpotations
# all refers to average values over the period 2000 to 2016, in logs

#	postscript("figure2.eps", width = 5, height = 3) # store eps file
	agg_mean_graph <- read.csv("data_figure2.csv")
	par(mar=c(5,6,2,2), mfrow=c(1,1)) #,oma=c(3,2,2,2),
	plot_y <- log(agg_mean_graph$homrates_pc_unodc_mn +1)
	plot_x <- log(agg_mean_graph$deport_convict_mn +1)	
	PL <- na.omit(data.frame(agg_mean_graph$iso3, plot_x, plot_y))
	ytext <- round(exp(seq(from = 0, to= max(plot_y, na.rm=T))), digits=0)-1
	xtext <- round(exp(seq(from = 0, to= max(plot_x, na.rm=T))), digits=0)-1
	ytext <- c(0,3,5,10,25,50)
	xtext <- c(0,3,5,10,25,50,100,250)
	
	plot(PL$plot_x, PL$plot_y, xaxt = "n", yaxt = "n", ylab="homicide rates \n (UNODC, average 2000-2015)", xlab="deportation rates (average 2000-2015)", ylim=c(min(PL$plot_y)-.5, max(PL$plot_y)+.5), xlim=c(-1,5.5), main="", cex.lab=.8)
	axis(2, at= log(ytext+1), labels= ytext, cex.axis = .7)
	axis(1, at= log(xtext+1), labels= xtext, cex.axis = .7) # add here the other axis too with value!
	textxy(PL$plot_x, PL$plot_y, PL$agg_mean_graph.iso3, cex = .6, col = "black", m = c(1, 1))
#	dev.off()

# ---------------------------
# Table 1: Effect of US Deportations of Convicts on Origin Countries’ 
# Homicide Rates
# ---------------------------

# laod data
	data <- read.csv("data_replic.csv")	
	data <- pdata.frame(data, index = c("iso3", "year"), drop.index = FALSE, row.names = TRUE)
	data$year <- as.integer(as.vector(data$year))
	head(data)


# (1) convicts'deportation rates, no controls, only country and year fixed effects
	form_i_plm <- as.formula(("homrates_unodc ~ deport_convict + as.factor(year) "))	
	plm.fe_plm <- plm(form_i_plm , data = data, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm,vcov=vcovHC(plm.fe_plm,type="HC1")) # robust

# (2) convicts'deportation rates, with time-varying controls and country fixed effects

# define control variables:
	Xnms <- " +  war_intrastate + corruption + crime_US_weight  + GDP_growth + pop_sh14  + GDPpercapita_const2010USD_log + gini_mkt + polity2 + population_log + remittances_GDP + yr_sch  +  urban_interp"

	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm,vcov=vcovHC(plm.fe_plm,type="HC1")) # robust
	
# (3) change in convicts'deportation rates, with time-varying controls and country fixed effects
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict_d + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data, model = "within"); summary(plm.fe_plm1)
	coeftest(plm.fe_plm,vcov=vcovHC(plm.fe_plm,type="HC1")) # robust

# (4) change in convicts'deportation rates and change in non-convicts'deportation rate, with time-varying controls and country fixed effects
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict_d + deport_nonconvict_d + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm,vcov=vcovHC(plm.fe_plm,type="HC1")) # robust

# (5) change in convicts'deportation rates and change in leading values of convicts'deportation rate, with time-varying controls and country fixed effects
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict_d + deport_convict_lead + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm,vcov=vcovHC(plm.fe_plm,type="HC1")) # robust


# ---------------------------
# Table 2: Effect of US Deportations of Convicts on Origin Countries’ 
# Homicide Rates (only Latin America and Caribbean Countries) 
# ---------------------------

# subset to Latin American and the Caribbean region
	data_LAC <- subset(data, data$region == "Latin America & Caribbean")
# here, I subset to those that have WHO data. Although I run the code below on UNODC data in part?	
#	data_LAC3 <- subset(data_LAC, !is.na(data_LAC$homrates_who))

	Xnms <- " +  corruption + crime_US_weight  + GDP_growth + pop_sh14  + GDPpercapita_const2010USD_log + gini_mkt + polity2 + population_log + remittances_GDP + yr_sch  +  urban_interp"


# (1) 
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_LAC, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm,vcov=vcovHC(plm.fe_plm,type="HC1")) # robust

# (2) 
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict_d + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_LAC, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm,vcov=vcovHC(plm.fe_plm,type="HC1")) # robust

# now, repeat those but use the WHO data and a smaller set of controls
# this allows us to go further back in time (until 1992)
	Xnms <- "  + corruption  + GDP_growth + pop_sh14  + GDPpercapita_const2010USD_log + gini_mkt + polity2 + population_log + remittances_GDP + yr_sch  +  urban_interp"

# (3) for deportation rates in levels
	form_i_plm <- as.formula(paste("homrates_who ~ deport_convict + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_LAC, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm)) # robust

# (4) for changes in deportaotin rates
	form_i_plm <- as.formula(paste("homrates_who ~ deport_convict_d + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_LAC, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm)) # robust

# ---------------------------
# Table 3: Two-Stage Least Squares Regression Results (1st and 2nd Step): 
# Effect of US Deportations of Convicts on Origin Countries’ Homicide Rates
# ---------------------------

# subset to the same set of data for the 1st and 2nd step regression
	data_2stp <- subset(data, !is.na(data$homrates_unodc) & !is.na(data$instr_enforce_everify_lag1) & !is.na(data$instr_benef_medicaidpregn_lag1) & !is.na(data$deport_convict))
# subset for Latin America and Caribbean:	
	data_LAC2stp <- subset(data_LAC, !is.na(data_LAC $homrates_unodc) & !is.na(data_LAC$instr_enforce_everify_lag1) & !is.na(data_LAC$instr_benef_medicaidpregn_lag1) & !is.na(data_LAC $deport_convict))

# (1) get 1st step tables for levels
	Xnms <- " +  remittances_GDP  +  pop_sh14  +  yr_sch  +  population_log  +  GDPpercapita_const2010USD_log  +  gini_mkt  +  war_intrastate  +  GDP_growth  +  crime_US_weight  +  urban_interp  +  polity2  +  corruption"
	form_i_plm <- as.formula(paste("deport_convict ~ instr_enforce_everify_lag1 + instr_benef_medicaidpregn_lag1 + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_2stp, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm)) # robust
	
# (2) get 1st step tables for LAC
	form_i_plm <- as.formula(paste("deport_convict ~ instr_enforce_everify_lag1 + instr_benef_medicaidpregn_lag1 + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_LAC2stp, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))
		
# (3) the 2nd step results
	form_iv <- as.formula(paste("homrates_unodc ~ deport_convict + as.factor(year) ", Xnms, "| . - deport_convict + as.factor(year)  + instr_enforce_everify_lag1 + instr_benef_medicaidpregn_lag1", Xnms))		
	plm.fe_iv1 <- plm(form_iv , data = data_2stp, model = "within"); summary(plm.fe_iv1); coeftest(plm.fe_iv1, vcov.=vcovHC(plm.fe_iv1))
	
# also run as ivreg to extract some stats
# then, use conutry fixed effects too in the formula
	form_iv <- as.formula(paste("homrates_unodc ~ deport_convict + as.factor(year) + as.factor(iso3) ", Xnms, "| . - deport_convict + as.factor(year) + as.factor(iso3)  + instr_enforce_everify_lag1 + instr_benef_medicaidpregn_lag1", Xnms))
	summary(fit_iv <- ivreg(form_iv, x=TRUE, data = data_2stp))
	summary(fit_iv, df = Inf, diagnostics = TRUE) #

# (4) the 2nd step results for LAC
# here, don't include civil wars as a predictor (no variation on this indicator)
	Xnms <- " +  remittances_GDP  +  pop_sh14  +  yr_sch  +  population_log  +  GDPpercapita_const2010USD_log  +  gini_mkt  +   GDP_growth  +  crime_US_weight  +  urban_interp  +  polity2  +  corruption"
	
	form_iv <- as.formula(paste("homrates_unodc ~ deport_convict + as.factor(year) ", Xnms, "| . - deport_convict + as.factor(year)  + instr_enforce_everify_lag1 + instr_benef_medicaidpregn_lag1", Xnms))		
	plm.fe_iv1 <- plm(form_iv , data = data_LAC2stp, model = "within"); summary(plm.fe_iv1); coeftest(plm.fe_iv1, vcov.=vcovHC(plm.fe_iv1))
# using ivreg 
	form_iv <- as.formula(paste("homrates_unodc ~ deport_convict + as.factor(year) + as.factor(iso3) ", Xnms, "| . - deport_convict + as.factor(year) + as.factor(iso3)  + instr_enforce_everify_lag1 + instr_benef_medicaidpregn_lag1", Xnms))
	summary(fit_iv <- ivreg(form_iv, x=TRUE, data = data_LAC2stp))
	summary(fit_iv, df = Inf, diagnostics = TRUE) #



# ---------------------------

# ANNEX TABLES

# ---------------------------
# Annex Table 2: Effect of US Deportations of Convicts on Origin Countries’ 
# Homicide Rates (Low and Middle Income Countries)
# ---------------------------

# same as Table 1 but only low and middle income countries
# subset to emerging markets and developing countries:
	data_emdc <- subset(data, data$income_group != "High income: OECD")

# run the same models
# (1) convicts'deportation rates, no controls, only country and year fixed effects
	form_i_plm <- as.formula(("homrates_unodc ~ deport_convict + as.factor(year) "))	
	plm.fe_plm <- plm(form_i_plm , data = data_emdc, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))
	
# (2) convicts'deportation rates, with time-varying controls and country fixed effects
	Xnms <- " +  remittances_GDP  +  pop_sh14  +  yr_sch  +  population_log  +  GDPpercapita_const2010USD_log  +  gini_mkt  +  war_intrastate  +  GDP_growth  +  crime_US_weight  +  urban_interp  +  polity2  +  corruption"
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_emdc, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))
	
# (3) change in convicts'deportation rates, with time-varying controls and country fixed effects
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict_d + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_emdc, model = "within"); summary(plm.fe_plm)
coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))
	
# (4) change in convicts'deportation rates and change in non-convicts'deportation rate, with time-varying controls and country fixed effects
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict_d + deport_nonconvict_d + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_emdc, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))
	
# (5) change in convicts'deportation rates and change in leading values of convicts'deportation rate, with time-varying controls and country fixed effects
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict_d + deport_convict_lead + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_emdc, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))

# ---------------------------
# Annex Table 3: Effect of US Deportations of Convicts on Origin Countries’ 
# Homicide Rates (WHO Data)
# ---------------------------

## same as Table 1 but use WHO instead
# (1) convicts'deportation rates, no controls, only country and year fixed effects
# use the same years as in the tables that use controls (that is, after 2000)
	form_i_plm <- as.formula(("homrates_who ~ deport_convict + as.factor(year) "))	
	plm.fe_plm <- plm(form_i_plm , data = data, subset=year>=2000, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))
	
# (2) convicts'deportation rates, with time-varying controls and country fixed effects
# define control variables:
	Xnms <- " +  remittances_GDP  +  pop_sh14  +  yr_sch  +  population_log  +  GDPpercapita_const2010USD_log  +  gini_mkt  +  war_intrastate  +  GDP_growth  +  crime_US_weight  +  urban_interp  +  polity2  +  corruption"
	form_i_plm <- as.formula(paste("homrates_who ~ deport_convict + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))
		
# (3) change in convicts'deportation rates, with time-varying controls and country fixed effects
	form_i_plm <- as.formula(paste("homrates_who ~ deport_convict_d + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))

# (4) change in convicts'deportation rates and change in non-convicts'deportation rate, with time-varying controls and country fixed effects
	form_i_plm <- as.formula(paste("homrates_who ~ deport_convict_d + deport_nonconvict_d + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))

# (5) change in convicts'deportation rates and change in leading values of convicts'deportation rate, with time-varying controls and country fixed effects
	form_i_plm <- as.formula(paste("homrates_who ~ deport_convict_d + deport_convict_lead + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))


# ---------------------------
# Annex Table 4: Effect of US Deportations of Convicts on Origin Countries’
# Homicide Rates (3-Year-Intervals)
# ---------------------------

# load the data that takes averages over three years:
	data_3y <- pdata.frame(read.csv("data_replic_3y.csv"), index = c("iso3", "year"), drop.index = FALSE, row.names = TRUE)	
# run models as in Table 1
	
	# run the same models
# (1) convicts'deportation rates, no controls, only country and year fixed effects
	form_i_plm <- as.formula(("homrates_unodc ~ deport_convict + as.factor(year) "))	
	plm.fe_plm <- plm(form_i_plm , data = data_3y, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))

# (2) convicts'deportation rates, with time-varying controls and country fixed effects
	Xnms <- " +  remittances_GDP  +  pop_sh14  +  yr_sch  +  population_log  +  GDPpercapita_const2010USD_log  +  gini_mkt  +  war_intrastate  +  GDP_growth  +  crime_US_weight  +  urban_interp  +  polity2  +  corruption"
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_3y, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))

# (3) change in convicts'deportation rates, with time-varying controls and country fixed effects
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict_d + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_3y, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))

# (4) change in convicts'deportation rates and change in non-convicts'deportation rate, with time-varying controls and country fixed effects
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict_d + deport_nonconvict_d + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_3y, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))

# (5) change in convicts'deportation rates and change in leading values of convicts'deportation rate, with time-varying controls and country fixed effects
	form_i_plm <- as.formula(paste("homrates_unodc ~ deport_convict_d + deport_convict_lead + as.factor(year) ", Xnms))	
	plm.fe_plm <- plm(form_i_plm , data = data_3y, model = "within"); summary(plm.fe_plm)
	coeftest(plm.fe_plm, vcov.=vcovHC(plm.fe_plm))
	
