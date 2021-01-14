// ---------------PROJET ECONOMÉTRIE DES VARIABLES QUALITATIVES ----------------

* Auteurs :    Grégoire Haniquaut 
* Date :       24 janvier 2021
* Enseignant : Idrissa Diagne 

clear all 
capture log close

// chemin relatif selon l'utilisateur

if c(username) == "gregoirehaniquaut" {
         	cd "/Users/gregoirehaniquaut/Dropbox/3 - ENSAE_Dakar/S1/Econometrie_qualitative/Projet"
}	
 * aremplir 
         else if c(username) == {
		 	
}

// permet d'extraire les documents directement sous la forme d'un word
//  ssc install asdoc 

* les chemins relatifs

global tableReg      "./Latex/100_tab_results"	
global graphics      "./Latex/101_graphics"	 
global data 		 "./data"
global results 	     "./results"

* ouverture du log
log using "$results/results.log", replace 





****************************** PARTIE 1 ****************************************

* lecture de la base
use "$data/mroz.dta",clear 

* changement de labels pour la présentation des esttab
label var nwifeinc "HouseInc-womanWage"
label var exper "Actual experience"
label var expersq "ExperienceSq"
label var kidsge6 "kids 6 - 18y"
label var kidslt6 "kids < 6y"


// Q1 - statistiques descriptives 

summarize inlf nwifeinc educ exper expersq age kidslt6 kidsge6

asdoc bysort inlf : tabstat nwifeinc educ exper expersq age kidslt6 kidsge6, stat(mean sd min max cv q) columns(statistics) format(%9.2f) 

bysort inlf : tabstat nwifeinc educ exper expersq age kidslt6 kidsge6, stat(mean sd min max cv q) columns(statistics) format(%9.2f) 

tabstat nwifeinc educ exper expersq age kidslt6 kidsge6, stat(mean sd min max cv q) columns(statistics) format(%9.2f)


// Q2 - effet a priori

pwcorr exper age educ , print(0.05) star(0.01)
graph box age, over(inlf)  scheme(s2manual) graphregion(color(white) fcolor(white) lcolor(white))
graph export "$graphics/boxplot.pdf", as(pdf) replace

ttest nwifeinc , by(inlf)
tab kidsge6 inlf,  chi2

//Q3 - estimer modèles Logit Probit ProbabilitéLinéaire

global y1list inlf
global x1list nwifeinc educ exper expersq age kidslt6 kidsge6


scalar pFemmesTravail = 0.5684

asdoc logit $y1list $x1list, append 
	est store logit1
	
estat classification, cutoff(pFemmesTravail)

asdoc probit $y1list $x1list, append
	est store probit1

estat classification, cutoff(0.5684)

asdoc regress $y1list $x1list, replace
	est store mco1

// Q4 - Utiliter du modèle de probabilité linéaire

capture drop y_hat
quietly regress $y1list $x1list
predict y_hat 
predict residu_hat, resid

twoway (histogram y_hat, bin(50) fcolor(white%60) lcolor(gs8)) (pcarrowi 0.2 0 0.2 -0.4 (6) "probabilité < 0" 0.2 1 0.2 1.3 (6) "probabilité > 1", lcolor(black) lwidth(medium) lpattern(dash) vertical mcolor(black) mlabcolor(black) mlabposition(9) mlabangle(default) mlabgap(zero) mlcolor(black) mlalign(outside)), legend(off) scheme(s1color)
graph export "$graphics/histogram_yhat.pdf", as(pdf) replace

jb residu_hat

//Q5 - estimer les effets marginaux sur la probabilité + odd ratio

quietly logit $y1list $x1list
	margins , dydx(*)
	est store mfx_logit

quietly probit $y1list $x1list 
	margins , dydx(*)
	est store mfx_probit
 
esttab mfx_logit mfx_probit using "$tableReg/MarginalEffectsLogitProbit.tex", mtitles("mfx Logit" "mfx Probit" "mfx OLS") gaps brackets title(Effets marginaux des modèles logit et probit) replace label

logit $y1list $x1list, or
	est store logitOR

logit $y1list $x1list

scalar ExperienceSeuil = -(e(b)[1,3]+e(b)[1,4])/(2*e(b)[1,4])
display ExperienceSeuil  // experienceSeuil = 32.135184

scalar proba = exp(e(b)[1,3] + e(b)[1,4]*ExperienceSeuil+e(b)[1,4]*(ExperienceSeuil*ExperienceSeuil))
display proba     // proba = 12.33595


* Q6 - représentation de la probabilité de travailler en fonction des annéess d'experience

// evolution de la probabiité 
twoway function y = exp(e(b)[1,8]+ e(b)[1,1] * 20.12896 + e(b)[1,2] * 12.28685 +e(b)[1,3]*x+e(b)[1,4]*(x*x)+e(b)[1,5]* 42.53785 + e(b)[1,6] * .2377158 +e(b)[1,7] * 1.353254) / (1+ exp(e(b)[1,8]+ e(b)[1,1] * 20.12896 + e(b)[1,2] * 12.28685 +e(b)[1,3]*x+e(b)[1,4]*(x*x)+e(b)[1,5]* 42.53785 + e(b)[1,6] * .2377158 +e(b)[1,7] * 1.353254)), range(0 50) note("La relation est estimé au point moyen.") xtitle("Années d'expérience", size(small)) ytitle("Évolution de la probabilité de travailler (en %)", size(small)) scheme(s2manual) ytitle("Évolution de la probabilité") droplines("32.135184") saving(probabilité1)  

// evolution de la cote
twoway function y = exp(e(b)[1,8]+ e(b)[1,1] * 20.12896 + e(b)[1,2] * 12.28685 +e(b)[1,3]*x+e(b)[1,4]*(x*x)+e(b)[1,5]* 42.53785 + e(b)[1,6] * .2377158 +e(b)[1,7] * 1.353254), range(0 50) droplines("32.135184") note("La relation est estimé au point moyen.") xtitle("Années d'expérience", size(small)) ytitle("Probabilité de travailler (en %)", size(small)) scheme(s2manual) ytitle("Évolution de la côte") saving(cote1) 

// evolution du odd ratio 
twoway function y = exp(e(b)[1,8]+ e(b)[1,1] * 20.12896 + e(b)[1,2] * 12.28685 +e(b)[1,3]*(x+1)+e(b)[1,4]*((x+1)*(x+1))+e(b)[1,5]* 42.53785 + e(b)[1,6] * .2377158 +e(b)[1,7] * 1.353254)/(exp(e(b)[1,8]+ e(b)[1,1] * 20.12896 + e(b)[1,2] * 12.28685 +e(b)[1,3]*(x)+e(b)[1,4]*((x)*(x))+e(b)[1,5]* 42.53785 + e(b)[1,6] * .2377158 +e(b)[1,7] * 1.353254)), range(0 50) droplines("32.135184") xtitle("Années d'expérience", size(small)) ytitle("Évolution du odd-ratio", size(small)) scheme(s2manual) saving(odd_ratio1) 

graph combine probabilité1.gph cote1.gph odd_ratio1.gph,  scheme(sj)
graph export "$graphics/EvoProbaExperience.pdf", as(pdf) replace
 
 
// Q7 - endogénéité de la variable kidslt6

pwcorr kidslt6 kidsge6 age educ exper hushrs , print(0.05) star(0.01)
regress kidslt6 kidsge6 age educ exper

// Q8 


capture drop educ_exper
generate educ_exper = c.educ*c.exper
label var educ_exper "Interaction educ exper"

global y2list hours
global x2list nwifeinc educ exper expersq age kidslt6 kidsge6 hushrs educ_exper


// Q8 1 

// Q8 2 - modele sur le sous-échantillon des travailleurs

preserve 
	keep if inlf == 1 
	asdoc regress $y2list $x2list
	est store MCO_Workers
restore

// Q8 3 - modele sur l'échantillon global

asdoc regress $y2list $x2list
	est store MCO_global

	* test de normalité des résidus
predict residu_MCOglobal, resid 
jb residu_MCOglobal

esttab MCO_Workers MCO_global, r2 mtitles("OLS_workers" "OLS_global") gaps brackets

// Q8 4 - tobit model 

sum hours
asdoc tobit $y2list $x2list, ll(0) append

capture drop yhat_tobit
capture drop residu_tobit
predict yhat_tobit
gen residu_tobit = hours - yhat_tobit

hist residu_tobit, normal lstyle(foreground)  xtitle("Résidu de la regression Tobit", size(small)) ytitle("Fréquence", size(small)) scheme(s2manual) graphregion(color(white) fcolor(white) lcolor(white))
graph export "$graphics/Tobitresidu.pdf", as(pdf) replace

	* test de normalité des résidus
jb residu_tobit
sktest residu_tobit

preserve 
	replace hours = . if hours == 0 
	asdoc heckman $y2list $x2list,   select($x2list) twostep append
restore 

* A8 - 5 modèle de comptage

asdoc poisson $y2list $x2list, append
	est store poisson_reg

asdoc nbreg $y2list $x2list, append
	est store nb_reg

asdoc zip $y2list $x2list, inflate($x2list) append
	est store zip_model 

asdoc zinb $y2list $x2list , inflate($x2list) append
	est store zinb_model

esttab poisson_reg nb_reg zip_model zinb_model using "$tableReg/CountModel.tex", mtitles("poisson reg" "nb reg" "zip reg" "zinb reg") pr2 aic bic scalar(p)  replace label title(Regressions : modèles de comptages variable endogène : heures travaillées.)  nogaps longtable

help esttab





* -------------------------------- PARTIE B ------------------------------------ 

use "$data/keane2.dta", replace

des 
summarize

global y3list status
global x3list exper expersq black

label define status_lbl 1 "school" 2 "home" 3 "work"
label values status status_lbl
//B1  - independance of irrelevant alternative


//B2 - logit multinomial

tab status

asdoc mlogit $y3list $x3list, baseoutcome(1) append
	* test IIA 
	asdoc mlogtest, iia append

//B3 - RRR

mlogit $y3list $x3list, baseoutcome(1) rrr

//B4 - modèle avec un effet d'interaction Exper Black

mlogit $y3list $x3list i.black#c.exper, baseoutcome(1) rrr

	* test IIA 
mlogtest, iia

//B5 - modèle séquentiel

// ce modèle met en évidence travailler (work) contre ne pas travailler (ecole ou bien maison )
seqlogit $y3list $x3list i.black#c.exper, tree(1 : 2 3,2 : 3)

// ce modèle met en évidence active (work et école) contre non actif (modèle présenté dans le rapport)
seqlogit $y3list $x3list i.black#c.exper, tree(2 : 1 3 ,1 : 3) or r



// ce modèle met en évidence travailler (work) contre ne pas travailler (ecole ou bien maison )
seqlogit $y3list $x3list i.black#c.exper, tree(1 : 2 3,2 : 3)

// pour la représentation
nlogitgen occupé = $y3list(actif : school | work, noactif : home)
gen n = _n 

nlogittree status occupé, choice(status) case(n)

