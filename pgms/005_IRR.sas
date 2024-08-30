

dm "log; clear;";
dm "out; clear;";
proc datasets library=work kill nolist;
run;
quit;




proc import datafile = ".\output\CID_full_model_results\Table3_dat_sas_not_adj.xlsx"
	dbms = excel
	out = total
	replace;
	getnames = yes;
run;

%macro IRR(condition_list);
    %local i condition count_outcome1_hiv1 count_outcome1_hiv0;
    %let i = 1;

    %do %while (%scan(&condition_list, &i) ne );
        %let condition = %scan(&condition_list, &i);

        data _null_;
            set work.total;
            where genotype= &condition and outcome=1 and HIV_x= '1' and paramcd= 'inc_fl';
            if count = 0 then call symput('count_outcome1_hiv1', 'yes');
            else call symput('count_outcome1_hiv1', 'no');
        run;
        data _null_;
            set work.total;
            where genotype= &condition and outcome=1 and HIV_x= '0' and paramcd= 'inc_fl';
            if count = 0 then call symput('count_outcome1_hiv0', 'yes');
            else call symput('count_outcome1_hiv0', 'no');
        run;

        %if %upcase(%trim(&count_outcome1_hiv1)) eq NO and %upcase(%trim(&count_outcome1_hiv0)) eq NO %then %do;
            %put No Error;
            
                ods output LSMeans= LSMeans_&i diffs= diffs_&i;
            proc genmod data=work.total;
                where genotype= &condition and paramcd= 'inc_fl';
                class id HIV_x(ref="0");
                model outcome = HIV_x / dist=poisson link=log scale=deviance;
                freq count;
                repeated subject=id;
                lsmeans HIV_x / ilink diff exp cl;
            run;

            data diffs_&i;
                length genotype $35 paramcd $10 IRR $35 P $10;
                set diffs_&i (keep= ExpEstimate LowerExp UpperExp Probz);
                genotype = &condition;
                paramcd = 'inc_fl';
                IRR= put(ExpEstimate, 5.2)||" ("||put(LowerExp, 5.2)||"-"||put(UpperExp, 5.2)||")";
                P= put(Probz, 5.3);
                keep genotype paramcd IRR P;
            run;
            data LSMeans_&i;
                length genotype $35 paramcd $10 IRR $35 P $10;
                set LSMeans_&i (keep= HIV_x ExpEstimate LowerExp UpperExp);
                genotype = &condition;
                paramcd = 'inc_fl';
                IRR= put(ExpEstimate*100, 5.2)||" ("||put(LowerExp*100, 5.2)||"-"||put(UpperExp*100, 5.2)||")";
                keep genotype paramcd HIV_x IRR;
            run;
            proc transpose data=LSMeans_&i out=LSMeans_&i(drop=_NAME_) prefix=HIV_;
                by genotype paramcd;
                id HIV_x;
                var IRR;
            run;
            data out_dat_&i;
                merge diffs_&i LSMeans_&i;
                by genotype paramcd;
            run;
        %end;
        %else %if %upcase(%trim(&count_outcome1_hiv1)) eq YES and %upcase(%trim(&count_outcome1_hiv0)) eq NO %then %do;
                %put Error in HIV_x=1;
                
                ods output LSMeans= LSMeans_&i diffs= diffs_&i;
            proc genmod data=work.total;
                where genotype= &condition and paramcd= 'inc_fl';
                class id HIV_x(ref="0");
                model outcome = HIV_x / dist=poisson link=log scale=deviance;
                freq count;
                lsmeans HIV_x / ilink diff exp cl;
            run;

            data diffs_&i;
                length genotype $35 paramcd $10 IRR $35 P $10;
                set diffs_&i (keep= ExpEstimate LowerExp UpperExp Probz);
                genotype = &condition;
                paramcd = 'inc_fl';
                IRR= '/';
                P= put(Probz, 5.3);
                keep genotype paramcd IRR P;
            run;
            data LSMeans_&i;
                length genotype $35 paramcd $10 IRR $35 P $10;
                set LSMeans_&i (keep= HIV_x ExpEstimate LowerExp UpperExp);
                genotype = &condition;
                paramcd = 'inc_fl';
                if HIV_x=1 then IRR= '/';
                else IRR= put(ExpEstimate*100, 5.2)||" ("||put(LowerExp*100, 5.2)||"-"||put(UpperExp*100, 5.2)||")";
                keep genotype paramcd HIV_x IRR;
            run;
            proc transpose data=LSMeans_&i out=LSMeans_&i(drop=_NAME_) prefix=HIV_;
                by genotype paramcd;
                id HIV_x;
                var IRR;
            run;
            data out_dat_&i;
                merge diffs_&i LSMeans_&i;
                by genotype paramcd;
            run;
        %end;
        %else %if (%upcase(%trim(&count_outcome1_hiv1)) eq NO) and (%upcase(%trim(&count_outcome1_hiv0)) eq YES) %then %do;
            %put Error in HIV_x=0;
            
                ods output LSMeans= LSMeans_&i diffs= diffs_&i;
            proc genmod data=work.total;
                where genotype= &condition and paramcd= 'inc_fl';
                class id HIV_x(ref="0");
                model outcome = HIV_x / dist=poisson link=log scale=deviance;
                freq count;
                lsmeans HIV_x / ilink diff exp cl;
            run;

            data diffs_&i;
                length genotype $35 paramcd $10 IRR $35 P $10;
                set diffs_&i (keep= ExpEstimate LowerExp UpperExp Probz);
                genotype = &condition;
                paramcd = 'inc_fl';
                IRR= '/';
                P= '/';
                keep genotype paramcd IRR P;
            run;
            data LSMeans_&i;
                length genotype $35 paramcd $10 IRR $35 P $10;
                set LSMeans_&i (keep= HIV_x ExpEstimate LowerExp UpperExp);
                genotype = &condition;
                paramcd = 'inc_fl';
                if HIV_x=0 then IRR= '/';
                else IRR= put(ExpEstimate*100, 5.2)||" ("||put(LowerExp*100, 5.2)||"-"||put(UpperExp*100, 5.2)||")";
                keep genotype paramcd HIV_x IRR;
            run;
            proc transpose data=LSMeans_&i out=LSMeans_&i(drop=_NAME_) prefix=HIV_;
                by genotype paramcd;
                id HIV_x;
                var IRR;
            run;
            data out_dat_&i;
                merge diffs_&i LSMeans_&i;
                by genotype paramcd;
            run;
        %end;
        %else %if (%upcase(%trim(&count_outcome1_hiv1)) eq YES) and (%upcase(%trim(&count_outcome1_hiv0)) eq YES) %then %do;
            %put Error in both;
                data out_dat_&i;
                    length genotype $35 paramcd $10 IRR $35 P $10;
                    genotype = &condition;
                    paramcd = 'inc_fl';
                    HIV_0 = '/';
                    HIV_1 = '/';
                    IRR = '/';
                    P = '/';
                run;
        %end;
        %else %abort;
        %let i = %eval(&i + 1);
    %end;
%mend IRR;


%IRR(condition_list=%nrstr('HPV16' 'HPV18' 'HPV31' 'HPV33' 'HPV35' 'HPV39' 'HPV45' 'HPV51' 'HPV52' 'HPV56' 'HPV58' 'HPV59' 'HPV68'
    'HPV6' 'HPV11' 'HPV26' 'HPV34' 'HPV40' 'HPV42' 'HPV43' 'HPV44' 'HPV53' 'HPV54' 'HPV55' 'HPV57' 'HPV61'
    'HPV66' 'HPV67' 'HPV69' 'HPV70' 'HPV71' 'HPV72' 'HPV73' 'HPV81' 'HPV82' 'HPV83' 'HPV84'));

data out_dat;
    retain genotype paramcd HIV_0 HIV_1 IRR P;
    set out_dat_1 - out_dat_37;
run;
proc print data=out_dat;
run;


proc export data=out_dat
    outfile=".\output\CID_full_model_results\Table3_IRR.xlsx"
    dbms=excel replace;
run;

