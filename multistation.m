(* ::Package:: *)

(* Anish Tondwalkar, NRL *)
(*
Initial Release on:Tue Aug  2 15:45:31 EDT 2011
*)

dir= "/home/atondwal/Ionosonde data 2008";
TIMEDIR="/home/atondwal/Sami Data from Sarah/time/";
Dir="/home/atondwal/Sami Data from Sarah/nmf2hmf2-1d/";
year=2008;
imageSize=600;
SetOptions[Plot,Frame->True,Axes->False];
SetOptions[ListPlot,Frame->True,Axes->False,Joined->True,PlotStyle->Hue@.1];


(*Reading in the binary data from bin.m*)
SetDirectory@dir;
<<("empirical_data"<>ToString@year<>".up")
<<("samifoF2"<>ToString@year<>".up")
<<("samiNmF2"<>ToString@year<>".up")
<<("samihmF2"<>ToString@year<>".up")
<<("times"<>ToString@year<>".up")
<<("stations"<>ToString@year<>".up")


(*The data has a lot entries like:*)
(*	2008-03-03 15:00 5.15 "/" "/"*)
(*2008-03-03 15:00 3.85 "/" "/"*)
(*so we'll just throw away the duplicates*)
(*dropbads[list_]:=Fold[Drop,list,Reverse@{{11},{13},{15},{17}}]*)
dropbads:=#&
(*dropbads[list_]:=Drop[list,{2}]*)


(*Throw away the ones not in the interested interval*)
hmF2C=Cases[#,{t_,h_}/;62<t<107]&/@hmF2;
foF2C=Cases[#,{t_,h_}/;62<t<107]&/@foF2;

(*Takes a function and a (time,val) pair, and finds the (time, difference at time) *)
diffs[ifunc_,list_]:={#1,ifunc[#1]-#2}&@@@list
(*Maps (time, diff) to (time,%diff) *)
diffper[diffdata_]:=Transpose/@Transpose@{Transpose[#][[1]]&/@diffdata ,Transpose[#][[2]]&/@(100diffdata/hmF2)};
perplots[diffper_]=ListPlot[#,PlotRange->{{62,68},{-100,100}}]&/@diffper;
(*UTC to localtime*)
local[list_,lon_]:={#1-lon/(15*24),#2}&@@@list;
tolocal[diffdatac_]:=local@@@Transpose[dropbads/@{diffdatac,Transpose[stations][[3]]}];


(* ::Input:: *)
(*diffhmF2=diffs@@@Transpose@{dropbads[IhmF2],dropbads[hmF2]};*)
(*difffoF2=diffs@@@Transpose@{dropbads[IfoF2],dropbads[foF2]};*)
(*diffplots=ListPlot[#1,Frame -> True, Axes -> False, FrameLabel->{"Day of Year","hmF2 (km)",#2[[1]],None},PlotRange->{{62,68},{-300,300}},ImageSize->600] & @@@ Transpose@{diffhmF2~Join~difffoF2,*)
(*stations};*)
(*diffhmF2c=diffs@@@Transpose@{IhmF2,hmF2C};*)
(*difffoF2c=diffs@@@Transpose@{IfoF2,foF2C};*)
(*Dimensions[diffhmF2hr=GatherBy[#,IntegerPart[24FractionalPart[#[[1]]]]&]&/@tolocal[diffhmF2c]]*)
(*Dimensions[difffoF2hr=GatherBy[#,IntegerPart[24FractionalPart[#[[1]]]]&]&/@tolocal[difffoF2c]]*)
(*Dimensions[diffhmF2day=GatherBy[Join@@tolocal[diffhmF2c],IntegerPart[#[[1]]]&]]*)
(*Dimensions[difffoF2day=GatherBy[Join@@tolocal[difffoF2c],IntegerPart[#[[1]]]&]]*)
(*Dimensions[diffhmF2dayhr=GatherBy[#,8<IntegerPart[24FractionalPart[#[[1]]]]<17&]&/@diffhmF2day]*)
(*Dimensions[difffoF2dayhr=GatherBy[#,8<IntegerPart[24FractionalPart[#[[1]]]]<17&]&/@difffoF2day]*)


per[stationdata_,function_]:=#[[2]]/function[#[[1]]]&/@stationdata
applyhmF2[stat_]:=stat[Transpose[#][[2]]]&/@#&/@diffhmF2hr
applyfoF2[stat_]:=stat[Transpose[#][[2]]]&/@#&/@difffoF2hr
applyhmF22[stat_]:={#[[1,1]],stat[Transpose[#][[2]]]}&/@#&/@diffhmF2dayhr
applyfoF22[stat_]:={#[[1,1]],stat[Transpose[#][[2]]]}&/@#&/@difffoF2dayhr


(* ::Input:: *)
(*{meanshmF2,nshmF2,medianshmF2,stddevshmF2}=applyhmF2/@{Mean,Length,Median,StandardDeviation};*)
(*{meansfoF2,nsfoF2,mediansfoF2,stddevsfoF2}=applyfoF2/@{Mean,Length,Median,StandardDeviation};*)
(*{meanshmF22,nshmF22,medianshmF22,stddevshmF22}=applyhmF22/@{Mean,Length,Median,StandardDeviation};*)
(*{meansfoF22,nsfoF22,mediansfoF22,stddevsfoF22}=applyfoF22/@{Mean,Length,Median,StandardDeviation};*)


(*This function throws out outliers*)
norm[list_]:=(
stats={Abs@Median[vals=Transpose[list][[2]]],3 StandardDeviation@vals};
Cases[list,{t_,x_}/;stats[[1]]-x <= stats[[2]]]
)
hmF2I=Interpolation/@norm/@dropbads@hmF2; 
foF2I=Interpolation/@norm/@dropbads@foF2;


gaussianf[data_]:=GaussianFilter[data,5]
sample[sami_]:=Table[sami[x],{x,62,107,.01}]


(* ::Input:: *)
(*ListPlot@Abs@Fourier@Table[hmF2I[[20]][x],{x,62,107,.01}] *)
(*sami=IhmF2[[3]];sonde=hmF2I[[3]];*)
(*ListPlot[InverseFourier[Conjugate/@Fourier[(table=Table[sonde[x],{x,62,107,.01}])-(m1=Median[table])]Fourier[(table=Table[sami[x],{x,62,107,.01}])-(m2=Median[table])]],PlotRange->{{0,100},{-100000,100000}},Joined->False]*)


(*smooth, correlate, and find phase/median shifts*)
f[sami_,sonde_]:=(
list=InverseFourier[Conjugate/@Fourier[(table=gaussianf@sample@sonde)-(m1=Median[table])]Fourier[(table=sample@sami)-(m2=Median[table])]];
{Mod[N[Flatten[Position[list,Max[list[[1;;100]]]]][[1]]],100]/100,m1-m2}
)


correctionhmF2=f@@@Transpose@{dropbads@IhmF2,hmF2I}
correctionfoF2=f@@@Transpose@{dropbads@IfoF2,foF2I}


(* ::Input:: *)
(*Show[ListPlot[#1],Plot[#2[x],{x,62,107}, PlotStyle->Red], Plot[#2[x+#3[[1]]]+#3[[2]],{x,62,107}], PlotRange->{{62,68},Automatic}]&@@@ Transpose@{gaussianf/@sample/@dropbads@hmF2I,dropbads@IhmF2,correctionhmF2}*)
(*Show[ListPlot[#1],Plot[#2[x],{x,62,107}, PlotStyle->Red], Plot[#2[x+#3[[1]]]+#3[[2]],{x,62,107}], PlotRange->{{62,107},Automatic}]&@@@ Transpose@{gaussianf/@sample/@dropbads@hmF2I,dropbads@IhmF2,correctionhmF2}*)
(*Show[ListPlot[#1],Plot[#2[x],{x,62,107}, PlotStyle->Red], Plot[#2[x+#3[[1]]]+#3[[2]],{x,62,107}], PlotRange->{{62,68},Automatic}]&@@@ Transpose@{gaussianf/@sample/@dropbads@foF2I,dropbads@IfoF2,correctionfoF2}*)
(*Show[ListPlot[#1],Plot[#2[x],{x,62,107}, PlotStyle->Red], Plot[#2[x+#3[[1]]]+#3[[2]],{x,62,107}], PlotRange->{{62,107},Automatic}]&@@@ Transpose@{gaussianf/@sample/@dropbads@foF2I,dropbads@IfoF2,correctionfoF2}*)


(* ::Input:: *)
(*(*Lots of stats on the differences*)*)
(*Show/@hsmallshows*)
(*Show/@hbigshows*)
(*Show/@fsmallshows*)
(*Show/@fbigshows*)
(*Show@hsmallshows*)
(*Show@hbigshows*)
(*Show@fsmallshows*)
(*Show@fbigshows*)


allhmF2[n_]:=Show[ListPlot[
  Transpose[dropbads[Transpose[{uttimebig, SamihmF2big}]][[n]]], 
  PlotStyle -> Hue@.2, Joined -> False], 
 Plot[dropbads[IhmF2][[n]][x], {x, 62, 107}], 
 ListPlot[dropbads[hmF2][[n]],Joined -> False,PlotStyle -> Red], 
 ListPlot[Transpose@{sample[#&],#}&@gaussianf@sample@hmF2I[[n]],PlotStyle->{Orange,Thick}],
 Plot[dropbads[IhmF2][[n]][x + correctionhmF2[[n]][[1]]] + 
   correctionhmF2[[n]][[2]], {x, 62, 107}, PlotStyle -> {Hue@.8,Thick}], 
 PlotRange -> {{62, 68}, {100, 400}}, ImageSize -> 800]


allfoF2[n_]:=Show[ListPlot[
  Transpose[dropbads[Transpose[{uttimebig, SamifoF2big}]][[n]]], 
  PlotStyle -> Hue@.2, Joined -> False], 
 Plot[dropbads[IfoF2][[n]][x], {x, 62, 107}], 
 ListPlot[dropbads[foF2][[n]],Joined -> False,PlotStyle -> Red], 
 ListPlot[Transpose@{sample[#&],#}&@gaussianf@sample@foF2I[[n]],PlotStyle->{Orange,Thick}],
 Plot[dropbads[IfoF2][[n]][x + correctionfoF2[[n]][[1]]] + 
   correctionfoF2[[n]][[2]], {x, 62, 107}, PlotStyle -> {Hue@.8,Thick}], 
 PlotRange -> {{62, 68}, Automatic}, ImageSize -> 800]


allfoF2[1]
