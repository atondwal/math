(* ::Package:: *)

(*Anish Tondwalkar*)
(*NRL*)


$RecursionLimit=Infinity;
SetDirectory@dir;
fname[Station_]:=ToString[year]<>"/"<>Station[[2]]<>"_"<>ToString[year]<>".sanitized"
fhDATA=Get;


(*This commented line writes the sanzitized data as Mathematica ASCII*)
(*Put[#,#2]&@@@Transpose[{Transpose[{foF2,hmF2}],StringSplit[#,"."][[1]]<>".sanitized"&/@fname/@stations}];*)
(*Reads the sanitized ASCII*)
(*{foF2,hmF2}=Transpose[Import/@(StringSplit[#,"."][[1]]<>".sanitized"&/@fname/@stations)]*)
{foF2,hmF2}=Transpose[fhDATA/@fname/@stations];


(* ::Input:: *)
(*SetDirectory@Dir;*)
(*DumpSave["empirical_data"<>ToString@year<>".up",{foF2,hmF2}];*)


SetDirectory@Dir;
f[station_,DOY_]:={
Import["hmf2-1d_"<>ToString@year<>(DOYString=IntegerString[DOY,10,3])<>"_"<>station[[2]]<>".dat"],
Import["nmf2-1d_"<>ToString@year<>DOYString<>"_"<>station[[2]]<>".dat"],
If[tec,Import["tec-1d_"<>ToString@year<>DOYString<>"_"<>station[[2]]<>".dat"],],
(DOY+(#[[2]]+(#[[3]]+#[[4]]/60)/60)/24)&/@Import["time_"<>ToString@year<>DOYString<>".dat","Table"]
}
Dimensions[{SamihmF2data,SamiNmF2data,SamiTECdata,uttimedata}=Transpose[ f@@#&/@ Tuples[{stations,Range[62,107]} ]]]
Dimensions[{uttimebig,SamifoF2big,SamihmF2big,SamiTECbig,SamiNmF2big}=Join@@@#&/@(Partition[#,46]&/@(Flatten/@#&/@{uttimedata,SamifoF2data,SamihmF2data,SamiTECdata,SamiNmF2data}))]
SamifoF2data=(8980.*^-6) Sqrt[SamiNmF2data];
SamifoF2big=(8980.*^-6) Sqrt[SamiNmF2big];


IfoF2=Interpolation[Transpose[#]]&/@Transpose[{uttimebig,SamifoF2big}];
INmF2=Interpolation[Transpose[#]]&/@Transpose[{uttimebig,SamiNmF2big}];
IhmF2=Interpolation[Transpose[#]]&/@Transpose[{uttimebig,SamihmF2big}];
ITEC=Interpolation[Transpose[#]]&/@Transpose[{uttimebig,SamiTECbig}];


(* ::Input:: *)
(*DumpSave["samifoF2"<>ToString@year<>".up",IfoF2];*)
(*DumpSave["samiNmF2"<>ToString@year<>".up",INmF2];*)
(*DumpSave["samihmF2"<>ToString@year<>".up",IhmF2];*)
(*DumpSave["samiTEC"<>ToString@year<>".up",ITEC];*)


(* ::Input:: *)
(*DumpSave["times"<>ToString@year<>".up",uttimebig];*)


(* ::Input:: *)
(*DumpSave["stations"<>ToString@year<>".up",stations];*)
(*DumpSave["rawhmF2"<>ToString@year<>".up",SamihmF2big];*)
(*DumpSave["rawfoF2"<>ToString@year<>".up",SamifoF2big];*)


(*Reading in the binary data from bin.m*)
SetDirectory@Dir;

(*Uncomment these lines and change to the binaries output by bin.m
You shouldn't need to read in the binaries anymore, because bin.m
now reads in the data more quickly, so it's just as practical to 
read in all the data at runtime. 
<<("empirical_data"<>ToString@year<>".up")
<<("samifoF2"<>ToString@year<>".up")
<<("samiNmF2"<>ToString@year<>".up")
<<("samihmF2"<>ToString@year<>".up")
<<("times"<>ToString@year<>".up")
<<("stations"<>ToString@year<>".up")
<<("rawhmF2"<>ToString@year<>".up")
<<("rawfoF2"<>ToString@year<>".up")
*)


(*The data has a lot entries like:*)
(*	2008-03-03 15:00 5.15 "/" "/"*)
(*	2008-03-03 15:00 3.85 "/" "/"*)
(*so we'll just throw away the stations with duplicates*)
(*midlat*)
(*dropbads[list_]:=Fold[Drop,list,Reverse@{{11},{13},{15},{17}}]*)
(*no bads*)
dropbads:=#&
(*equatorial*)
(*dropbads[list_]:=Drop[list,{2}]*)


(*Throw away the ones that aren't in the WHI *)
hmF2C=Cases[#,{t_,h_}/;62<t<107]&/@hmF2;
foF2C=Cases[#,{t_,h_}/;62<t<107]&/@foF2;


(*Takes a function and a (time,val) pair, and finds the (time, difference at time) *)
diffs[ifunc_,list_]:={#1,ifunc[#1]-#2}&@@@list
(*Maps (time, diff) to (time,% diff) *)
diffper[diffdata_]:=Transpose/@Transpose@{Transpose[#][[1]]&/@diffdata ,Transpose[#][[2]]&/@(100 diffdata/hmF2)};
(*plots percentages*)
perplots[diffper_]=ListPlot[#,PlotRange->{{62,68},{-100,100}}]&/@diffper;
(*UTC to localtime*)
local[list_,lon_]:={#1-lon/(15*24),#2}&@@@list;
tolocal[diffdatac_]:=local@@@Transpose[dropbads/@{diffdatac,Transpose[stations][[3]]}];


(*This takes the difference between SAMI's output and the data, and splits in into lists*)
diffhmF2=diffs@@@Transpose@{dropbads[IhmF2],dropbads[hmF2]};
difffoF2=diffs@@@Transpose@{dropbads[IfoF2],dropbads[foF2]};
diffplots=ListPlot[#1,FrameLabel->{"Day of Year","hmF2 (km)",#2[[1]],None},PlotRange->{{62,68},{-300,300}},ImageSize->600]&@@@ 
 Transpose@{diffhmF2~Join~difffoF2,stations};
diffhmF2c=diffs@@@Transpose@{IhmF2,hmF2C};
difffoF2c=diffs@@@Transpose@{IfoF2,foF2C};
Dimensions[diffhmF2hr=GatherBy[#,IntegerPart[24FractionalPart[#[[1]]]]&]&/@tolocal[diffhmF2c]]
Dimensions[difffoF2hr=GatherBy[#,IntegerPart[24FractionalPart[#[[1]]]]&]&/@tolocal[difffoF2c]]
Dimensions[diffhmF2day=GatherBy[Join@@tolocal[diffhmF2c],IntegerPart[#[[1]]]&]]
Dimensions[difffoF2day=GatherBy[Join@@tolocal[difffoF2c],IntegerPart[#[[1]]]&]]
Dimensions[diffhmF2dayhr=GatherBy[#,8<IntegerPart[24FractionalPart[#[[1]]]]<17&]&/@diffhmF2day]
Dimensions[difffoF2dayhr=GatherBy[#,8<IntegerPart[24FractionalPart[#[[1]]]]<17&]&/@difffoF2day]


(*These are the functions that take statistics on the differences*)
per[stationdata_,function_]:=#[[2]]/function[#[[1]]]&/@stationdata
applyhmF2[stat_]:=stat[Transpose[#][[2]]]&/@#&/@diffhmF2hr
applyfoF2[stat_]:=stat[Transpose[#][[2]]]&/@#&/@difffoF2hr
applyhmF22[stat_]:={#[[1,1]],stat[Transpose[#][[2]]]}&/@#&/@diffhmF2dayhr
applyfoF22[stat_]:={#[[1,1]],stat[Transpose[#][[2]]]}&/@#&/@difffoF2dayhr


(*statistics on the differences are stored in these lists.
you can plot these using, for example
    BarChart/@stddevshmF2
which would show you the standard deviation of the differences between SAMI
and the ionosonde data for a given station for each hour.
*)
{meanshmF2,nshmF2,medianshmF2,stddevshmF2}=applyhmF2/@{Mean,Length,Median,StandardDeviation};
{meansfoF2,nsfoF2,mediansfoF2,stddevsfoF2}=applyfoF2/@{Mean,Length,Median,StandardDeviation};
{meanshmF22,nshmF22,medianshmF22,stddevshmF22}=applyhmF22/@{Mean,Length,Median,StandardDeviation};
{meansfoF22,nsfoF22,mediansfoF22,stddevsfoF22}=applyfoF22/@{Mean,Length,Median,StandardDeviation};


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
(*(*debug code*)*)
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


(*
/Tranp\[CapitalAHat]\[Euro]kbspose
f@lliTranspose@{sample[#&]&/@Range[19],\[RawEscape]f,i}j\[CapitalAHat]\[Euro]kb \[RawEscape]j
f]f]i, PlotStyle -> Orange \[RawEscape]jjjjj\[CapitalAHat]\[Euro]K1
/Auto
dwi {1,8}\[RawEscape]jjjjj\[CapitalAHat]\[Euro]K1
*)


(*This addes legends to prepare list of plots for output*)
font="Arial";
fontSize=12;
markerSize=12;
legend[names_, styles_] := (
 GraphicsGrid[Graphics/@{Text@Style[#1, FontSize -> fontSize, font],
  Flatten[{#2, Line[{{-1, 0}, {1, 0}}]}]}&@@@Transpose@{names,styles}]
);
out[hs_]:=Grid[{{GraphicsGrid[Partition[hs,4],ImageSize->1000],legend[{"Data","SAMI3","Shifted"},style]}}];


(*Generates the plots that I put in my talk, 16 per slide*)
hp=Plot[{#1[x],#2[x],#2[x + #3[[1]]] + #3[[2]]}, {x, 62, 107}] & @@@
 Transpose@{hmF2I, dropbads@IhmF2, correctionhmF2};
hs=Show[#,PlotRange->{{62,68},{100,400}}]&/@hp;
hf=Show[#,PlotRange->{{62,107},{100,400}}]&/@hp;
fp=Plot[{#1[x],#2[x],#2[x + #3[[1]]] + #3[[2]]}, {x, 62, 107}] & @@@
 Transpose@{foF2I, dropbads@IfoF2, correctionfoF2};
fs=Show[#,PlotRange->{{62,68},{1,8}}]&/@fp;
ff=Show[#,PlotRange->{{62,107},{1,8}}]&/@fp;


(*Writes out the plots of the data*)
Export["hs.pdf",out[Fold[Drop,hs,{{1},{-1},{-4}}]]];
Export["fs.pdf",out[Fold[Drop,fs,{{1},{-1},{-4}}]]];


(*Let's give us some fuctions to show everything for a station*)
allhmF2[n_]:=( allhmF2[n] = Show[ListPlot[
  Transpose[dropbads[Transpose[{uttimebig, SamihmF2big}]][[n]]], 
  PlotStyle -> Hue@.2, Joined -> False], 
 Plot[dropbads[IhmF2][[n]][x], {x, 62, 107}], 
 ListPlot[dropbads[hmF2][[n]],Joined -> False,PlotStyle -> Red], 
 ListPlot[Transpose@{sample[#&],#}&@gaussianf@sample@hmF2I[[n]],PlotStyle->{Orange,Thick}],
 Plot[dropbads[IhmF2][[n]][x + correctionhmF2[[n]][[1]]] + 
   correctionhmF2[[n]][[2]], {x, 62, 107}, PlotStyle -> {Hue@.8,Thick}], 
 PlotRange -> {{62, 68}, {100, 400}}, ImageSize -> 800])
allfoF2[n_]:=( allfoF2[n] = Show[ListPlot[
  Transpose[dropbads[Transpose[{uttimebig, SamifoF2big}]][[n]]], 
  PlotStyle -> Hue@.2, Joined -> False], 
 Plot[dropbads[IfoF2][[n]][x], {x, 62, 107}], 
 ListPlot[dropbads[foF2][[n]],Joined -> False,PlotStyle -> Red], 
 ListPlot[Transpose@{sample[#&],#}&@gaussianf@sample@foF2I[[n]],PlotStyle->{Orange,Thick}],
 Plot[dropbads[IfoF2][[n]][x + correctionfoF2[[n]][[1]]] + 
   correctionfoF2[[n]][[2]], {x, 62, 107}, PlotStyle -> {Hue@.8,Thick}], 
 PlotRange -> {{62, 68}, Automatic}, ImageSize -> 800] )


(* ::Input:: *)
(*allfoF2/@Range[1,24]*)


allfoF2[2]
