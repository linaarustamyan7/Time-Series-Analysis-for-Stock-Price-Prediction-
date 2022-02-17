*Combining two datasets
import delimited "C:\Users\lina\Downloads\^DJI (2).csv"
drop high low close volume open
rename adjclose Adjclose_DJI
save "C:\Users\lina\Desktop\DJI1.dta"
clear
import delimited "C:\Users\lina\Downloads\JPM (3).csv"
drop high low close volume open
rename adjclose adjclose_JPM
merge m:1 date using "C:\Users\lina\Desktop\DJI1.dta"
drop _merge


*Seting the data as time-series data
gen mydate=date(date, "YMD")
 format mydate %td
 gen year_month=mofd(mydate)
 format year_month %tm
 drop date
 tsset year_month

 
 gen ln_jpm=ln( adjclose_JPM )
gen ln_dji=ln(Adjclose_DJI)
gen r=D1.ln_jpm
gen i=D1.ln_dji

qui tsline ln_jpm, name(ln_jpm, replace)
qui tsline ln_dji, name(ln_dji, replace)
graph combine ln_jpm ln_dji

qui tsline r, name(Dln_jpm, replace)
qui tsline i, name(Dln_dji, replace)
graph combine Dln_jpm Dln_dji


// My version of garch arima
mat table = 999, 999
arch r ,  arima(0,0,1) arch(1) garch(1)
mat b0 = e(b)				// this will hold the initial values

forvalues v = -50/-1 {
global aa = `v'-1
arch r  in 1/$aa,  arima(0,0,1) arch(1) garch(1) from(b0) // run this model excluding the last 50 time period 
predict y_hat
su y_hat 	in `v'/`v'
scalar a1 = r(mean) 		// store the predicted mean for the -50th period 
su r in `v'/`v'
scalar a2 = r(mean) 		// store the actual    mean for the -50th period
scalar a3 = a1*a2			// if positive, then the direction is predicted correctly
global bb = abs(`v')
mat a$bb = sqrt((a1-a2)^2)			// square error
mat b$bb = a3  
mat table = table \ a$bb, b$bb
drop y_hat
}

mat l table


// My version of  arima
mat table = 999, 999
arima r, arima (0,0,1)
mat b0 = e(b)				// this will hold the initial values

forvalues v = -50/-1 {
global aa = `v'-1
arima r in 1/$aa,  arima(1,0,1) from(b0) // run this model excluding the last 50 time period 
predict y_hat
su y_hat 	in `v'/`v'
scalar a1 = r(mean) 		// store the predicted mean for the -50th period 
su r in `v'/`v'
scalar a2 = r(mean) 		// store the actual    mean for the -50th period
scalar a3 = a1*a2			// if positive, then the direction is predicted correctly
global bb = abs(`v')
mat a$bb = sqrt((a1-a2)^2)			// square error
mat b$bb = a3  
mat table = table \ a$bb, b$bb
drop y_hat
}

mat l table


// My version of  arimax
mat table = 999, 999
arima r L.i, arima (1,0,1)
mat b0 = e(b)				

forvalues v = -50/-1 {
global aa = `v'-1
arima r L.i in 1/$aa,  arima(0,0,1) from(b0)
predict y_hat
su y_hat 	in `v'/`v'
scalar a1 = r(mean) 		
su r in `v'/`v'
scalar a2 = r(mean) 		
scalar a3 = a1*a2			
global bb = abs(`v')
mat a$bb = sqrt((a1-a2)^2)			
mat b$bb = a3  
mat table = table \ a$bb, b$bb
drop y_hat
}

mat l table


// My version of garch dl
mat table = 999, 999
arch r , arima(1,0,1) arch(1/2) 
mat b0 = e(b)				// this will hold the initial values

forvalues v = -50/-1 {
global aa = `v'-1
arch r  in 1/$aa,  arima(1,0,1) arch(1/2) from(b0) // run this model excluding the last 50 time period 
predict y_hat
su y_hat 	in `v'/`v'
scalar a1 = r(mean) 		// store the predicted mean for the -50th period 
su r in `v'/`v'
scalar a2 = r(mean) 		// store the actual    mean for the -50th period
scalar a3 = a1*a2			// if positive, then the direction is predicted correctly
global bb = abs(`v')
mat a$bb = sqrt((a1-a2)^2)			// square error
mat b$bb = a3  
mat table = table \ a$bb, b$bb
drop y_hat
}

mat l table

// My version of garch
mat table = 999, 999
arch r , arch(1) garch(1)
mat b0 = e(b)				// this will hold the initial values

forvalues v = -50/-1 {
global aa = `v'-1
arch r  in 1/$aa, arch(1) garch(1) from(b0) // run this model excluding the last 50 time period 
predict y_hat
su y_hat 	in `v'/`v'
scalar a1 = r(mean) 		// store the predicted mean for the -50th period 
su r in `v'/`v'
scalar a2 = r(mean) 		// store the actual    mean for the -50th period
scalar a3 = a1*a2			// if positive, then the direction is predicted correctly
global bb = abs(`v')
mat a$bb = sqrt((a1-a2)^2)			// square error
mat b$bb = a3  
mat table = table \ a$bb, b$bb
drop y_hat
}

mat l table

// My version of tgarch
mat table = 999, 999
arch r, arch(1) garch(1) tarch(1/2)
mat b0 = e(b)				// this will hold the initial values
 
forvalues v = -50/-1 {
global aa = `v'-1
arch r in 1/$aa, arch(1) garch(2) tarch(1) from(b0) // run this model excluding the last 50 time period 
predict y_hat
su y_hat 	in `v'/`v'
scalar a1 = r(mean) 		// store the predicted mean for the -50th period 
su r in `v'/`v'
scalar a2 = r(mean) 		// store the actual    mean for the -50th period
scalar a3 = a1*a2			// if positive, then the direction is predicted correctly
global bb = abs(`v')
mat a$bb = sqrt((a1-a2)^2)			// square error
mat b$bb = a3  
mat table = table \ a$bb, b$bb
drop y_hat
}

mat l table


// My version of tgarch + arima
mat table = 999, 999
arch r , arima(1,0,1) arch(1) garch(1) tarch(1)
mat b0 = e(b)				// this will hold the initial values
 
forvalues v = -50/-1 {
global aa = `v'-1
arch r   in 1/$aa,  arima(1,0,1) arch(1) garch(1) tarch(1) from(b0) // run this model excluding the last 50 time period 
predict y_hat
su y_hat 	in `v'/`v'
scalar a1 = r(mean) 		// store the predicted mean for the -50th period 
su r in `v'/`v'
scalar a2 = r(mean) 		// store the actual    mean for the -50th period
scalar a3 = a1*a2			// if positive, then the direction is predicted correctly
global bb = abs(`v')
mat a$bb = sqrt((a1-a2)^2)			// square error
mat b$bb = a3  
mat table = table \ a$bb, b$bb
drop y_hat
}

mat l table

// My version of garch arimax
mat table = 999, 999
arch r ,  arima(1,0,1) arch(1/2) 
mat b0 = e(b)				// this will hold the initial values

forvalues v = -50/-1 {
global aa = `v'-1
arch r  in 1/$aa,  arima(1,0,1) arch(1/2) from(b0)  // run this model excluding the last 50 time period 
predict y_hat
su y_hat 	in `v'/`v'
scalar a1 = r(mean) 		// store the predicted mean for the -50th period 
su r in `v'/`v'
scalar a2 = r(mean) 		// store the actual    mean for the -50th period
scalar a3 = a1*a2			// if positive, then the direction is predicted correctly
global bb = abs(`v')
mat a$bb = sqrt((a1-a2)^2)			// square error
mat b$bb = a3  
mat table = table \ a$bb, b$bb
drop y_hat
}

mat l table

//ARIMA + MARCH + TGARCH
mat table = 999, 999
arch r,arima(0,0,1) archm arch(1) garch(1) tarch(1)
mat b0 = e(b)				// this will hold the initial values

forvalues v = -50/-1 {
global aa = `v'-1
arch r  in 1/$aa,  arima(1,0,1) archm arch(1) garch(2)tarch(1) from(b0) // run this model excluding the last 50 time period 
predict y_hat
su y_hat 	in `v'/`v'
scalar a1 = r(mean) 		// store the predicted mean for the -50th period 
su r in `v'/`v'
scalar a2 = r(mean) 		// store the actual    mean for the -50th period
scalar a3 = a1*a2			// if positive, then the direction is predicted correctly
global bb = abs(`v')
mat a$bb = sqrt((a1-a2)^2)			// square error
mat b$bb = a3  
mat table = table \ a$bb, b$bb
drop y_hat
}

mat l table



//DL + MARCH + GARCH
mat table = 999, 999
arch r i,   archm arch(1/2) garch(2)
mat b0 = e(b)				// this will hold the initial values

forvalues v = -50/-1 {
global aa = `v'-1
arch r L.i in 1/$aa, archm arch(1/2)   // run this model excluding the last 50 time period 
predict y_hat
su y_hat 	in `v'/`v'
scalar a1 = r(mean) 		// store the predicted mean for the -50th period 
su r in `v'/`v'
scalar a2 = r(mean) 		// store the actual    mean for the -50th period
scalar a3 = a1*a2			// if positive, then the direction is predicted correctly
global bb = abs(`v')
mat a$bb = sqrt((a1-a2)^2)			// square error
mat b$bb = a3  
mat table = table \ a$bb, b$bb
drop y_hat
}

mat l table

//ARDL(1,0)
mat table = 999, 999
ardl r i, lags(1,0)
mat b0 = e(b)				// this will hold the initial values

forvalues v = -50/-1 {
global aa = `v'-1
ardl r i in 1/$aa, lags(1,0) // run this model excluding the last 50 time period 
predict y_hat
su y_hat 	in `v'/`v'
scalar a1 = r(mean) 		// store the predicted mean for the -50th period 
su r in `v'/`v'
scalar a2 = r(mean) 		// store the actual    mean for the -50th period
scalar a3 = a1*a2			// if positive, then the direction is predicted correctly
global bb = abs(`v')
mat a$bb = sqrt((a1-a2)^2)			// square error
mat b$bb = a3  
mat table = table \ a$bb, b$bb
drop y_hat
}

mat l table
//ARIMAX + MGARCH
mat table = 999, 999
arch r i,arima(0,0,1) archm arch(1) garch(2)
mat b0 = e(b)

forvalues v = -50/-1 {
global aa = `v'-1
arch r i in 1/$aa, arima(0,0,1) archm arch(1) garch(2) 
predict y_hat
su y_hat 	in `v'/`v'
scalar a1 = r(mean) 		// store the predicted mean for the -50th period 
su r in `v'/`v'
scalar a2 = r(mean) 		// store the actual    mean for the -50th period
scalar a3 = a1*a2			// if positive, then the direction is predicted correctly
global bb = abs(`v')
mat a$bb = sqrt((a1-a2)^2)			// square error
mat b$bb = a3  
mat table = table \ a$bb, b$bb
drop y_hat
}

mat l table
