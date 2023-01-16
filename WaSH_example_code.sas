* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* PROJECT:		GTMP/TD;
* PROGRAM: 		WaSH_example_code.SAS;
* PURPOSE: 		'INTERVENTION' PIECE OF THE ANALYSIS;
* FILES IN:		'TEMP' FROM THE W5A_WASH_ANALYSIS_MAIN_DDMMMYYYY.SAS;
* FILES OUT:	'RESULTS' DATASET;
* PROGRAMMER:	KRISTIN SULLIVAN;
* DATE:			01/12/2021;
* VERSION:		SAS 9.4;
* LICENSE:      GPL-2
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* Code below is an example of 55% water coverage intervention and 0% sanitation coverage intervention for the;
* reaching elimination ('01') group.; 
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* EXISTING VARIABLES;
* elig = 01 ” indicates the reaching elimination group; 
* eid = eu id number;
* cid = cluster id number;
* hid = household id number; 
* ww_exp_eu_pct = washing water coverage at eu level;
* sani_exp_eu_pct = sanitation coverage at eu level ;
* ww_exp_hh = household washing water (1 = yes, 0 = no);
* sani_exp_hh = household sanitation (1 = yes, 0 = no); ;
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;

* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* 1. READ IN FILE;
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
data all;
	set temp;
run; 

* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =s = = = = = = = = = = = = = = = = = = = =;
* 2. ASSIGN INTERVENTIONS ; 
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
data full_01;
	set all; 
	if elig = "01"; 		* selects only the 'reaching elimination' group; 										
run; 

* select only one of each household; 
proc sort data = full_01; by hid; run;

data full_01_hh; 
	set full_01;
	by hid; 
	if first.hid; 
	* keep only household- or eu-level variables;
	keep eid cid hid ww_exp_eu_pct sani_exp_eu_pct ww_exp_hh sani_exp_hh; 
run;

proc sort data = full_01_hh; by eid hid; run; 

* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* 2A. GET LIST OF RANDOMLY ORDERED CIDS WITHIN EIDS;
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
proc sort data = full_01; by cid; run; 

* select only one of each cid; 
data ran_cid;									 
	set full_01;
	if first.cid;
	by cid; 
	keep eid cid; 
run; 

proc sort data = ran_cid; by eid; run; 

data seed;
	* &j and &run are macro variables assigned in a program that calls to this program;
	seed_num = (&j*58496) + (&run*45895) + 1922; 
	call symput('seed',seed_num);
run;

* select all cids in an eid without replacement (randomly ordered list); 
proc surveyselect data = ran_cid out = ran_cid2 noprint 
	outrandom 
	seed = &seed 
	method = srs 
	samprate = 1;
	strata eid; 
run;
	
data ran_cid2; 
	set ran_cid2;
	rand_order = _n_; 
	keep eid cid rand_order; 
run; 

proc sort data = ran_cid2; by eid rand_order; run;

data ran_cid3; 
	set ran_cid2;
	by eid rand_order;
	order + 1; 
	if first.eid then order = 1;
	keep cid order; 				* 'order' is the randomly assigned cid order; 
run; 

* merge back to household set by cid; 
proc sort data = full_01_hh; by cid; run;
proc sort data = ran_cid3; by cid; run;
	 
data full_01_hh;
	merge full_01_hh ran_cid3;
	by cid;
	ww_exp_hh_nc = ww_exp_hh; 		* keep natural course water responses; 
	ww_exp_eu_pct_nc = ww_exp_eu_pct;
run; 

data temp;					* copy of the household dataset called temp - it will update with each iteration; 
	set full_01_hh;
run; 

proc datasets nolist noprint;
	delete ran:;
run; quit; 

* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* 2B. ASSIGN WATER INTERVENTIONS TO CLUSTERS; 
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* This step loops through 30 times. If the eu washing water percent (ww_exp_eu_pct) is below the intervention;
* percent (55% in this example), it will select (within each eid) the first cid in in the randomly sorted    ;
* list and assign all households washing water in that cid. If the intervention percent was not met after the; 
* assigning all households in the first round, it will move to the second cid, etc. up to 30 cids. It will   ;
* repeat this 30 times to allow it to cycle through all possible cids surveyed in an eu.                     ; 
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;

%macro pick(howmany);												 
	* for cids 1 - 30 starting at 1; 
	%do z = 1 %to &howmany;
		proc sort data = temp; by eid order; run; 		* order by eid and then random cid assigned above (#1-30);

		data a; 				
			set temp;
			by eid order; 													
			if first.eid then eid_temp_num + 1; 		* create temp number for eids; 
			do i = 1 to 350; 							* do for all eus #1 - 350;
			if eid_temp_num = i then do;
				if ww_exp_eu_pct lt 55 then do; 
					if order = &z then do;				* take the 1st(or 2nd, etc) cid in the randomly sorted list; 
					ww_int_recvd_cid = 1; 				* intervention received in village; 
						if ww_exp_hh = 1 then ww_int_recvd_hid = 9; 	* 9 = no change, already had water exposure; 
							if ww_exp_hh = 0 then do; 	* no water exposure, so assign intervention; 
							ww_exp_hh = 1; 				* now have water exposure; 
							ww_int_recvd_hid = 1; 		* intervention received in household; 
							end; 
						end;
					end;
				end;
			end; 

		drop ww_exp_eu_pct; 							* drop previous coverage to calculate new coverage below; 
		run;

		* recalculate ww_exp_eu_pct for each eu after the first (or second, etc,) rounds of intervention;
		proc sort data = a; by eid; run;
		 
		%macro propcalc (var, newvar); 
			proc freq data = a;
				table &var / out = &var.count noprint; 
				by eid;
			run;

			data &var.yescount; set &var.count; if &var ne 1 then delete; &newvar._yes_n = count; keep eid &newvar._yes_n; run;
			data &var.nocount; set &var.count; if &var ne 0 then delete; &newvar._no_n = count; keep eid &newvar._no_n; run;
			data &var.misscount; set &var.count; if &var ne . then delete; &newvar._miss_n = count; keep eid &newvar._miss_n; run;

			proc sort data = &var.yescount; by eid; run;
			proc sort data = &var.nocount; by eid; run;
			proc sort data = &var.misscount; by eid; run;

			data eu_&var;
				merge &var.yescount &var.nocount &var.misscount;
				by eid;
				if &newvar._yes_n = . then &newvar._yes_n = 0;
				if &newvar._no_n = . then &newvar._no_n = 0;
				if &newvar._miss_n = . then &newvar._miss_n = 0;
				&newvar._total_n = sum(&newvar._yes_n, &newvar._no_n, &newvar._miss_n); 
				&newvar._pct = (&newvar._yes_n)/(&newvar._yes_n + &newvar._no_n)* 100; * note this has a * 100; 
			run;

		%mend propcalc;
		%propcalc(ww_exp_hh, ww_exp_eu); 

		proc sort data = eu_ww_exp_hh; by eid; run; 
		proc sort data = a; by eid; run; 

		data temp; 		* set now has the new water coverage after the intervention was delivered to village (if needed); 
			merge a eu_ww_exp_hh; 
			by eid; 
		run;

		proc datasets nolist noprint;
		delete a eu: ww:;
		run; quit; 

		* repeat this for all cids in the eu (1-30); 
	%end;
%mend pick;
%pick(30); 		* maximum number of clusters in any eid in the dataset; 

* if not created above, create indicator if intervention received in cluster and household; 
data temp2;
	set temp;
	* if the natural course (original) eu coverage was less than the intervention percent - no interventions were delivered;
	if ww_exp_eu_pct_nc ge 55 then do; 
		ww_int_recvd_eid = 0; * no intervention in eu; 
		ww_int_recvd_cid = 0; * no intervention in cluster;
		ww_int_recvd_hid = 0; * no intervention in household;
	end; 

	if ww_exp_eu_pct_nc lt 55 then do;
		ww_int_recvd_eid = 1; * yes, intervention in eu;
		if ww_int_recvd_cid = . then ww_int_recvd_cid = 0; * will be assigned 1 above if received, so all others will be no; 
		if ww_int_recvd_hid = . then ww_int_recvd_hid = 0; * will be assigned 1 or 9 above if received, so all others will be no;
	end; 
	drop ww_exp_eu: order i eid_temp_num; 
run; 

* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* 2C. ASSIGN SANITATION INTERVENTIONS TO HOUSEHOLDS;
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* This step assigns the sanitation intervention at the household level. If the eu percent is less than the 	 ;
* intervention percent (0 in this example), it will determine what proportion of households in the eu will   ; 
* need sanitation in order to reach the intervention level. It then randomly assigns the households          ;
* sanitation based on a bernoulli distribution with success = the proportion of households needing sanitation;
* to reach the intervention level.	                					 				                     ;
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
proc sort data = temp2; by eid; run; 

data full_01_hh_int; 													
	set temp2; 
	by eid; 									
	if first.eid then eid_temp_num + 1;		* create temp number for eids; 

	if sani_exp_eu_pct lt 0 then do; 		
		* bernoulli proportion needed to shift distribution to desired intervention level;					
		sani_bern_pct = ((0/100) - (sani_exp_eu_pct/100))/(1-(sani_exp_eu_pct/100)); 		 
	end; 

	sani_exp_hh_nc = sani_exp_hh; 			* keep original value; 
	sani_exp_eu_pct_nc = sani_exp_eu_pct; 	* keep original value; 

	sani_int_recvd_eid = 0; 
	do i = 1 to 350; 													
		if eid_temp_num = i then do;
			if sani_exp_eu_pct lt 0 then do; 				
				sani_int_recvd_eid = 1; 
				sani_int_recvd_hid = 0;
				if sani_exp_hh = 1 then sani_int_recvd_hid = 9; 			
				if sani_exp_hh = 0 then do;														
					sani_int_recvd_hid = rand("bernoulli", sani_bern_pct);	 
					if sani_int_recvd_hid = 1 then sani_exp_hh = 1; 
				end; 
			end; 
		end;
	end; 
	drop i sani_exp_eu_pct eid_temp_num; 
run;

* recalculate ww_sani_eu_pct for each eu after the intervention; 
proc sort data = full_01_hh_int; by eid; run; 

%macro propcalc (var, newvar); 
	proc freq data = full_01_hh_int;
		table &var / out = &var.count noprint; 
		by eid;
	run;

	data &var.yescount; set &var.count; if &var ne 1 then delete; &newvar._yes_n = count; keep eid &newvar._yes_n; run;
	data &var.nocount; set &var.count; if &var ne 0 then delete; &newvar._no_n = count; keep eid &newvar._no_n; run;
	data &var.misscount; set &var.count; if &var ne . then delete; &newvar._miss_n = count; keep eid &newvar._miss_n; run;

	proc sort data = &var.yescount; by eid; run;
	proc sort data = &var.nocount; by eid; run;
	proc sort data = &var.misscount; by eid; run;

	data eu_&var;
		merge &var.yescount &var.nocount &var.misscount;
		by eid;
		if &newvar._yes_n = . then &newvar._yes_n = 0;
		if &newvar._no_n = . then &newvar._no_n = 0;
		if &newvar._miss_n = . then &newvar._miss_n = 0;

		&newvar._total_n = sum(&newvar._yes_n, &newvar._no_n, &newvar._miss_n); 

		&newvar._pct = (&newvar._yes_n)/(&newvar._yes_n + &newvar._no_n)* 100; * note this has a * 100 to scale; 
	run;

%mend propcalc;
%propcalc(ww_exp_hh, ww_exp_eu); 
%propcalc(sani_exp_hh, sani_exp_eu);

* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* 3. CREATE FINAL INTERVENTION SET;
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
proc sort data = eu_ww_exp_hh; by eid; run; 		* eu coverages - water; 
proc sort data = eu_sani_exp_hh; by eid; run; 		* eu coverages - sanitation; 
proc sort data = full_01_hh_int; by eid; run;		* hh interventions; 

data full_01_hh_int2;
	merge full_01_hh_int eu_ww_exp_hh eu_sani_exp_hh;
	by eid; 
run;

*remove the natural course variables from the full, original dataset; 
data full_01_noexp; 
	set full_01;
drop ww: sani:; 
run;

proc sort data = full_01_noexp; by hid; run; 
proc sort data = full_01_hh_int2; by hid; run;

* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* 3A. MERGE WITH ORIGINAL INDIVIDUAL-LEVEL DATA TO GET THE FULL INTERVENTION DATASET;
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
data full_01_int_set;
	merge full_01_noexp full_01_hh_int2;
	by hid; 

	length int_set $50; 		* identifies which minimum intervention coverage percentages were used; 
	int_set = "01 w:55 s:0"; 	* w = water s = sanitation; 

	ww_int_level = 55;
	sani_int_level = 0;
		
	* community exposures excluding the child’s household; 
	if ww_exp_hh = 1 then ww_exp_eu_pct2 = (ww_exp_eu_yes_n - 1)/(ww_exp_eu_yes_n + ww_exp_eu_no_n - 1);
	if ww_exp_hh = 0 then ww_exp_eu_pct2 = (ww_exp_eu_yes_n - 0)/(ww_exp_eu_yes_n + ww_exp_eu_no_n - 1);

	if sani_exp_hh = 1 then sani_exp_eu_pct2 = (sani_exp_eu_yes_n - 1)/(sani_exp_eu_yes_n + sani_exp_eu_no_n - 1);
	if sani_exp_hh = 0 then sani_exp_eu_pct2 = (sani_exp_eu_yes_n - 0)/(sani_exp_eu_yes_n + sani_exp_eu_no_n - 1);

	ww_exp_eu_pct2 = ww_exp_eu_pct2* 100;		
	sani_exp_eu_pct2 = sani_exp_eu_pct2* 100;		
run;

* select only children 1 - 9 for models; 
data full_01_int_kids;
	set full_01_int_set;
	if (1 le age le 9) and examined = 1; 
run;
 
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* 3B. RECREATE SPLINES AT LOCATIONS DETERMINED EARLIER IN ANALYSIS; 
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
data z_01.6.; * ’6’ here is indicating the run number; 
	set full_01_int_kids;

	%macro splinecreate (grp, var, short, k1, k2, k3); 					
		if elig = "&grp" then do;				
			&short._knot1 = &k1;		
			&short._knot2 = &k2;		
			&short._knot3 = &k3;		
		end;					
		* splines;					
		_&var = (max(0,&var-&short._knot1)* * 2-max(0,&var-&short._knot3)* * 2)/(&short._knot3- &short._knot1);		
		__&var = (max(0,&var-&short._knot2)* * 2-max(0,&var-&short._knot3)* * 2)/(&short._knot3- &short._knot1);	
	%mend splinecreate;		
	* spline knots are located at the 5th, 50th and 95th percentiles of the case distributions; 
	%splinecreate(01, age, age, &age_01_sp);
	%splinecreate(01, pop_den_hh_log, pop, &pop_den_hh_log_01_sp);
	%splinecreate(01, n_light_hh_log, light, &n_light_hh_log_01_sp);
	%splinecreate(01, ww_exp_eu_pct2, ww, &ww_exp_eu_pct2_01_sp);
	%splinecreate(01, prior_ww_exp_eu_pct, pr_ww, &prior_ww_exp_eu_pct_01_sp);
	%splinecreate(01, sani_exp_eu_pct2, sani, &sani_exp_eu_pct2_01_sp);
	%splinecreate(01, prior_sani_exp_eu_pct, pr_sani, &prior_sani_exp_eu_pct_01_sp);
	%splinecreate(01, tf_eu_pct, tf, &tf_eu_pct_01_sp);
	%splinecreate(01, prior_tf_eu_pct, pr_tf, &prior_tf_eu_pct_01_sp);
	%splinecreate(01, surv_int_dur, surdur, &surv_int_dur_01_sp);

	* outcome gets set to missing in the interventions sets so that the intervention set will not be modeled,;
	* but the pred. probabilities will be calculated using both fixed and random effects; 
	tf = .; 
run; 

proc datasets nolist noprint;
	delete full: ww: sani: eu: eidcount temp:;
run; quit; 

%symdel seed;
 
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* 3C. SET NATURAL COURSE WITH INTERVENTION SET FOR MODEL;
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* note: the outcome on the natural course data set are complete; 
* 		the outcomes for the intervention sets are set to missing; 
data set_01_a;
	set elig_01 z_01.6;  *set natural course data set with all intervention sets; 
	if int_set = " " then int_set = "01 original";
	dataset = "01";
run;

data set_01;
	set set_01_a;
run; 

* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* 4. RUN MODEL; 
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
proc sort data = set_01; by eid cid hid; run; 

proc glimmix data = set_01 method = laplace noclprint; 				
	class country (ref = "ethiopia") eid cid; 								
	model tf(event = '1') = 	
			&var_01_list 
			/dist = binary link = logit cl solution oddsratio (diff = first label) ;
			random intercept / subject = eid type = vc ; 
			random intercept / subject = cid(eid) type = vc ;
			output out = set_01_pp pred(blup ilink) = predicted;	
			* blup - uses the predictors of the random effects in computing the statistic.;
			* ilink - computes the statistic on the scale of the data (rather than the link function scale); 
	covtest / wald;
run;

proc datasets nolist noprint;
	delete set_01 set_01_a;
run; quit; 

* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
* 5. CALCULATE EFFECT ESTIMATES ;
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =;
data predprob;
	set set_01_pp;
	keep dataset int_set ww_int_level sani_int_level predicted; 
run;

proc freq data = predprob; 
	table int_set;
run;

proc sort data = predprob; by dataset int_set ww_int_level sani_int_level; run; 

proc means data = predprob mean;
	var predicted; 
	by dataset int_set ww_int_level sani_int_level; 
	output out = mean_prev
	mean = mean;
run;

proc sort data = mean_prev; by ww_int_level sani_int_level; run; 

* get means from natural course data; 
data mean_prev01; set mean_prev; if dataset = "01"; run; 

data nc_mean01;
	set mean_prev01;
	if int_set in ("01 original");
	nc_mean = mean; 
	keep nc_mean; 
run; 

data mean_prev01b;
 	set mean_prev01;
	if dataset = "01";
 	if _n_ eq 1 then do;
  	set nc_mean01;
 	end;
run;

data mean_prev_final;
	set mean_prev01b;
	drop _type_; 
run;

proc sort data = mean_prev_final; by int_set; run; 
proc sort data = eu_mod_01; by int_set; run; 

data results_pre;
	merge mean_prev_final(rename = (_freq_ = n_kids));
	by int_set;
	pd = (mean-nc_mean)* 100;
run;
