(* ::Package:: *)

(* Anish Tondwalkar, NRL *)
(*
Depends: bin.m

Gives:  Differences, statistics, and correlations
	And lots of plots.
*)
dir= "/home/atondwal/Ionosonde data 2008/bins/";
TIMEDIR="/home/atondwal/Sami Data from Sarah/time/";
Dir="/home/atondwal/Sami Data from Sarah/nmf2hmf2-1d/";
year=2008;
imageSize=600;
(*
style={Orange,Red,Blue};
SetOptions[Plot,Frame->True,Axes->False,PlotStyle->style];
*)
SetOptions[ListPlot,Frame->True,Axes->False,Joined->True,PlotStyle->Hue@.1];


(*Reading in the binary data from bin.m*)
SetDirectory@dir;
(*
(*Uncomment these lines and change to the binaries output by bin.m
You shouldn't need to read in the binaries anymore, because bin.m
now reads in the data more quickly, so it's just as practical to 
read in all the data at runtime. *)
<<("empirical_data"<>ToString@year<>".bin")
<<("samifoF2"<>ToString@year<>".bin")
<<("samiNmF2"<>ToString@year<>".bin")
<<("samihmF2"<>ToString@year<>".bin")
<<("times"<>ToString@year<>".bin")
<<("stations"<>ToString@year<>".bin")
*)


(*The data has a lot entries like:*)
(*	2008-03-03 15:00 5.15 "/" "/"*)
(*	2008-03-03 15:00 3.85 "/" "/"*)
(*so we'll just throw away the stations with duplicates*)
(*midlat*)
(*dropbads[list_]:=Fold[Drop,list,Reverse@{{11},{13},{15},{17}}]*)
(*no bads*)
(*dropbads:=#&*)
(*equatorial*)
(*dropbads[list_]:=Drop[list,{2}]*)


(*Throw away the ones that aren't in the WHI *)
hmF2C=Cases[#,{t_,h_}/;62<t<107]&/@hmF2;
foF2C=Cases[#,{t_,h_}/;62<t<107]&/@foF2;
NmF2C=Cases[#,{t_,h_}/;62<t<107]&/@NmF2;


(*Takes a function and a (time,val) pair, and finds the (time, difference at time) *)
diffs[ifunc_,list_]:={#1,ifunc[#1]-#2}&@@@list
(*Maps (time, diff) to (time,% diff) *)
diffsper[ifunc_,list_]:={#1,100*(ifunc[#1]-#2)/#2}&@@@list
(* Not used or working
diffper[diffdata_]:=Transpose/@Transpose@{Transpose[#][[1]]&/@diffdata ,Transpose[#][[2]]&/@(100 diffdata/hmF2)};
(*plots percentages*)
perplots[diffper_]=ListPlot[#,PlotRange->{{62,68},{-100,100}}]&/@diffper;
(*UTC to localtime*)
*)
local[list_,lon_]:={#1-lon/(15*24),#2}&@@@list;
tolocal[diffdatac_]:=local@@@Transpose[dropbads/@{diffdatac,Transpose[stations][[3]]}];


(*This takes the difference between SAMI's output and the data, and splits in into lists*)
(*bad but not used*)
(*
diffhmF2=diffs@@@Transpose@{dropbads[IhmF2],dropbads[hmF2]};
difffoF2=diffs@@@Transpose@{dropbads[IfoF2],dropbads[foF2]};
diffNmF2=diffs@@@Transpose@{dropbads[INmF2],dropbads[NmF2]};
diffNmF2per=diffsper@@@Transpose@{dropbads[INmF2],dropbads[NmF2]};
diffplots=ListPlot[#1,FrameLabel->{"Day of Year","hmF2 (km)",#2[[1]],None},PlotRange->{{62,68},{-300,300}},ImageSize->600]&@@@ 
 Transpose@{diffhmF2~Join~difffoF2,stations};
*)
diffhmF2c=diffs@@@Transpose@{IhmF2,hmF2C};
difffoF2c=diffs@@@Transpose@{IfoF2,foF2C};
diffNmF2c=diffs@@@Transpose@{INmF2,NmF2C};
diffNmF2perc=diffsper@@@Transpose@{INmF2,NmF2C};

Dimensions[diffhmF2hr=GatherBy[#,IntegerPart[24FractionalPart[#[[1]]]]&]&/@tolocal[diffhmF2c]]
Dimensions[difffoF2hr=GatherBy[#,IntegerPart[24FractionalPart[#[[1]]]]&]&/@tolocal[difffoF2c]]
Dimensions[diffNmF2hr=GatherBy[#,IntegerPart[24FractionalPart[#[[1]]]]&]&/@tolocal[diffNmF2c]]
Dimensions[diffNmF2perhr=GatherBy[#,IntegerPart[24FractionalPart[#[[1]]]]&]&/@tolocal[diffNmF2perc]]
Dimensions[diffhmF2day=GatherBy[Join@@tolocal[diffhmF2c],IntegerPart[#[[1]]]&]]
Dimensions[difffoF2day=GatherBy[Join@@tolocal[difffoF2c],IntegerPart[#[[1]]]&]]
Dimensions[diffNmF2day=GatherBy[Join@@tolocal[diffNmF2c],IntegerPart[#[[1]]]&]]
Dimensions[diffNmF2perday=GatherBy[Join@@tolocal[diffNmF2perc],IntegerPart[#[[1]]]&]]
Dimensions[diffhmF2dayhr=GatherBy[#,8<IntegerPart[24FractionalPart[#[[1]]]]<17&]&/@diffhmF2day]
Dimensions[difffoF2dayhr=GatherBy[#,8<IntegerPart[24FractionalPart[#[[1]]]]<17&]&/@difffoF2day]
Dimensions[diffNmF2dayhr=GatherBy[#,8<IntegerPart[24FractionalPart[#[[1]]]]<17&]&/@diffNmF2day]
Dimensions[diffNmF2perdayhr=GatherBy[#,8<IntegerPart[24FractionalPart[#[[1]]]]<17&]&/@diffNmF2perday]


(*These are the functions that take statistics on the differences*)
per[stationdata_,function_]:=#[[2]]/function[#[[1]]]&/@stationdata
applyhmF2[stat_]:=stat[Transpose[#][[2]]]&/@#&/@diffhmF2hr
applyfoF2[stat_]:=stat[Transpose[#][[2]]]&/@#&/@difffoF2hr
applyNmF2[stat_]:=stat[Transpose[#][[2]]]&/@#&/@diffNmF2hr
applyNmF2per[stat_]:=stat[Transpose[#][[2]]]&/@#&/@diffNmF2perhr
applyhmF22[stat_]:={#[[1,1]],stat[Transpose[#][[2]]]}&/@#&/@diffhmF2dayhr
applyfoF22[stat_]:={#[[1,1]],stat[Transpose[#][[2]]]}&/@#&/@difffoF2dayhr
applyNmF22[stat_]:={#[[1,1]],stat[Transpose[#][[2]]]}&/@#&/@diffNmF2dayhr
applyNmF2per2[stat_]:={#[[1,1]],stat[Transpose[#][[2]]]}&/@#&/@diffNmF2perdayhr


(*statistics on the differences are stored in these lists.
you can plot these using, for example
    BarChart/@stddevshmF2
which would show you the standard deviation of the differences between SAMI
and the ionosonde data for a given station for each hour.
*)
{meanshmF2,nshmF2,medianshmF2,stddevshmF2}=applyhmF2/@{Mean,Length,Median,StandardDeviation};
{meansfoF2,nsfoF2,mediansfoF2,stddevsfoF2}=applyfoF2/@{Mean,Length,Median,StandardDeviation};
{meansNmF2,nsfNmF2,mediansNmF2,stddevsNmF2}=applyNmF2/@{Mean,Length,Median,StandardDeviation};
{meansNmF2per,nsfNmF2per,mediansNmF2per,stddevsNmF2per}=applyNmF2per/@{Mean,Length,Median,StandardDeviation};
{meanshmF22,nshmF22,medianshmF22,stddevshmF22}=applyhmF22/@{Mean,Length,Median,StandardDeviation};
{meansfoF22,nsfoF22,mediansfoF22,stddevsfoF22}=applyfoF22/@{Mean,Length,Median,StandardDeviation};
{meansNmF22,nsNmF22,mediansNmF22,stddevsNmF22}=applyNmF22/@{Mean,Length,Median,StandardDeviation};
{meansNmF2per2,nsNmF2per2,mediansNmF2per2,stddevsNmF2per2}=applyNmF2per2/@{Mean,Length,Median,StandardDeviation};


(*This function throws out outliers*)
norm[list_]:=(
stats={Abs@Median[vals=Transpose[list][[2]]],3 StandardDeviation@vals};
Cases[list,{t_,x_}/;stats[[1]]-x <= stats[[2]]]
)
hmF2I=Interpolation/@norm/@hmF2; 
foF2I=Interpolation/@norm/@foF2; 
NmF2I=Interpolation/@norm/@NmF2;


gaussianf[data_]:=GaussianFilter[data,5]
sample[sami_]:=Table[sami[x],{x,62,107,.01}]


(* ::Input:: *)
(*debug code*)
(*ListPlot@Abs@Fourier@Table[hmF2I[[20]][x],{x,62,107,.01}] *)
(*sami=IhmF2[[3]];sonde=hmF2I[[3]];*)
(*ListPlot[InverseFourier[Conjugate/@Fourier[(table=Table[sonde[x],{x,62,107,.01}])-(m1=Median[table])]Fourier[(table=Table[sami[x],{x,62,107,.01}])-(m2=Median[table])]],PlotRange->{{0,100},{-100000,100000}},Joined->False]*)


(*smooth, correlate, and find phase/median shifts*)
f[sami_,sonde_]:=(
list=InverseFourier[Conjugate/@Fourier[(table=gaussianf@sample@sonde)-(m1=Median[table])]Fourier[(table=sample@sami)-(m2=Median[table])]];
{Mod[N[Flatten[Position[list,Max[list[[1;;100]]]]][[1]]],100]/100,m1-m2}
)


correctionhmF2=f@@@Transpose@{IhmF2,hmF2I}
correctionfoF2=f@@@Transpose@{IfoF2,foF2I}


(*
/Tranpkbsposef@lliTranspose@{sample[#&]&/@Range[19],f,i}jkbj
f]f]i, PlotStyle -> OrangejjjjjK1
/Autodwi{1,8}jjjjjK1
*)


(*This addes legends to prepare list of plots for output*)
font="Arial";
fontSize=12;
markerSize=12;
legend[names_, styles_] := (
 GraphicsGrid[Graphics/@{Text@Style[#1, FontSize -> fontSize, font],
  Flatten[{#2, Line[{{-1, 0}, {1, 0}}]}]}&@@@Transpose@{names,styles}]
);
(*Moved to export section
out[hs_]:=Grid[{{GraphicsGrid[Partition[hs,4],ImageSize->1200],legend[{"Data","SAMI3","Shifted"},style]}}];
*)


(*Generates the plots that I put in my talk, 16 per slide*)
{stationname,sid,slat,slong}=Transpose[stations];

hp=Plot[{#1[x],#2[x],#2[x + #3[[1]]] + #3[[2]]}, {x, 62, 107},PlotRange->{All,{150,450}},Axes->False,Frame->True,PlotStyle->style,FrameLabel->{"Day of Year","hmF2 (km)",#4}] & @@@
 Transpose@{hmF2I, IhmF2, correctionhmF2, stationname};
fp=Plot[{#1[x],#2[x],#2[x + #3[[1]]] + #3[[2]]}, {x, 62, 107},PlotRange->{All,{1,10}},Axes->False,Frame->True,PlotStyle->style, FrameLabel->{"Day of Year","foF2 (MHz)",#4}] & @@@
 Transpose@{foF2I, IfoF2, correctionfoF2, stationname};
Np=Plot[{#1[x],#2[x],#2[x + #3[[1]]] + #3[[2]]}, {x, 62, 107},Axes->False,Frame->True,PlotStyle->style,FrameLabel->{"Day of Year","NmF2 (\!\(\*SuperscriptBox[\"cm\", 
RowBox[{\"-\", \"3\"}]]\))",#4}] & @@@
 Transpose@{NmF2I, INmF2, correctionNmF2, stationname};

hp2=Plot[{#1[x],#2[x]}, {x, 62, 107},PlotRange->{All,{150,450}}, Axes->False,Frame->True,PlotStyle->stylefor2,FrameLabel->{"Day of Year","hmF2 (km)",#3}] & @@@
 Transpose@{hmF2I, IhmF2, stationname};
fp2=Plot[{#1[x],#2[x]}, {x, 62, 107},PlotRange->{All,{1,10}},Axes->False,Frame->True,PlotStyle->stylefor2,FrameLabel->{"Day of Year","foF2 (MHz)",#3}] & @@@
 Transpose@{foF2I, IfoF2, stationname};
Np2=Plot[{#1[x],#2[x]}, {x, 62, 107},Axes->False,Frame->True,PlotStyle->stylefor2,FrameLabel->{"Day of Year","NmF2 (\!\(\*SuperscriptBox[\"cm\", 
RowBox[{\"-\", \"3\"}]]\))",#3}] & @@@
 Transpose@{NmF2I, INmF2, stationname};

fs=Show[#,PlotRange->{{62,68},{1,10}}]&/@fp;
ff=Show[#,PlotRange->{{62,107},{1,10}}]&/@fp;
hs=Show[#,PlotRange->{{62,68},{150,450}}]&/@hp;
hf=Show[#,PlotRange->{{62,107},{150,450}}]&/@hp;
Ns=Show[#,PlotRange->{{62,68},{10^4,10^6}}]&/@Np;
Nf=Show[#,PlotRange->{{62,107},{10^4,10^6}}]&/@Np;
fs2=Show[#,PlotRange->{{62,68},{1,10}}]&/@fp2;
ff2=Show[#,PlotRange->{{62,107},{1,10}}]&/@fp2;
hs2=Show[#,PlotRange->{{62,68},{150,450}}]&/@hp2;
hf2=Show[#,PlotRange->{{62,107},{150,450}}]&/@hp2;
Ns2=Show[#,PlotRange->{{62,68},{10^4,10^6}}]&/@Np2;
Nf2=Show[#,PlotRange->{{62,107},{10^4,10^6}}]&/@Np2;


(*
(*Writes out the plots of the data*)
Export["hs.pdf",out[Fold[Drop,hs,{{1},{-1},{-4}}]]];
Export["fs.pdf",out[Fold[Drop,fs,{{1},{-1},{-4}}]]];
*)


(*Let's give us some fuctions to show everything for a station*)
allhmF2[n_]:=Show[ListPlot[
  Transpose[Identity[Transpose[{uttimebig, SamihmF2big}]][[n]]], 
  PlotStyle -> Hue@.2, Joined -> False], 
 Plot[Identity[IhmF2][[n]][x], {x, 62, 107}], 
 ListPlot[Identity[hmF2][[n]],Joined -> False,PlotStyle -> Red], 
 ListPlot[Transpose@{sample[#&],#}&@gaussianf@sample@hmF2I[[n]],PlotStyle->{Orange,Thick}],
 Plot[Identity[IhmF2][[n]][x + correctionhmF2[[n]][[1]]] + 
   correctionhmF2[[n]][[2]], {x, 62, 107}, PlotStyle -> {Hue@.8,Thick}], 
 PlotRange -> {{62, 68}, {100, 400}}, ImageSize -> 800, FrameLabel->{"Day of Year","hmF2 (km)",stationname[[n]]}]*)
allfoF2[n_]:=Show[ListPlot[
  Transpose[Identity[Transpose[{uttimebig, SamifoF2big}]][[n]]], 
  PlotStyle -> Hue@.2, Joined -> False], 
 Plot[Identity[IfoF2][[n]][x], {x, 62, 63}], 
 ListPlot[Identity[foF2][[n]],Joined -> False,PlotStyle -> Red], 
 ListPlot[Transpose@{sample[#&],#}&@gaussianf@sample@foF2I[[n]],PlotStyle->{Orange,Thick}],
 Plot[Identity[IfoF2][[n]][x + correctionfoF2[[n]][[1]]] + 
   correctionfoF2[[n]][[2]], {x, 62, 107}, PlotStyle -> {Hue@.8,Thick}], 
 PlotRange -> {{62, 68}, Automatic}, ImageSize -> 800]


(* ::Input:: *)
(*allfoF2[6]*)


(*this takes the statistics and displays mean differenec by hour, by day and night *)
distfoF2={IntegerPart[#1],If[8<IntegerPart[24FractionalPart[#1]]<17,1,0],#2}&@@@#&/@meansfoF22;
disthmF2={IntegerPart[#1],If[8<IntegerPart[24FractionalPart[#1]]<17,1,0],#2}&@@@#&/@meanshmF22;
distNmF2={IntegerPart[#1],If[8<IntegerPart[24FractionalPart[#1]]<17,1,0],#2}&@@@#&/@meansNmF22;
distNmF2per={IntegerPart[#1],If[8<IntegerPart[24FractionalPart[#1]]<17,1,0],#2}&@@@#&/@meansNmF2per2;
distsfoF2={#1,#3}&@@@#&/@GatherBy[Join@@distfoF2,#[[2]]&];
distshmF2={#1,#3}&@@@#&/@GatherBy[Join@@disthmF2,#[[2]]&];
distsNmF2={#1,#3}&@@@#&/@GatherBy[Join@@distNmF2,#[[2]]&];
distsNmF2per={#1,#3}&@@@#&/@GatherBy[Join@@distNmF2per,#[[2]]&];
(*
SetOptions[ListPlot,ImageSize->400,BaseStyle->{FontFamily->"Ariel",Bold,FontSize->12}];
ListPlot[distshmF2[[1]],Joined->False,PlotLabel->"Daytime hmF2",FrameLabel->{"Day of 2008","Difference in Altitude (km)"}]
ListPlot[distshmF2[[2]],Joined->False,PlotLabel->"Nighttime hmF2",FrameLabel->{"Day of 2008","Difference in Altitude (km)"}]
ListPlot[distsfoF2[[1]],Joined->False,PlotLabel->"Daytime foF2",FrameLabel->{"Day of 2008","Difference in Freq (MHz)"}]
ListPlot[distsfoF2[[2]],Joined->False,PlotLabel->"Nighttime foF2",FrameLabel->{"Day of 2008","Difference in Freq (MHz)"}]
ListPlot[distsNmF2[[1]],Joined->False,PlotLabel->"Daytime NmF2",FrameLabel->{"Day of 2008","Difference in Density (cm^-3)"}]
ListPlot[distsNmF2[[2]],Joined->False,PlotLabel->"Nighttime NmF2",FrameLabel->{"Day of 2008","Difference in Density (cm^-3)"}]
*)
ListPlot[distsNmF2per[[1]],Joined->False,PlotLabel->"Daytime NmF2",FrameLabel->{"Day of 2008","Difference in Density (%)"}]
ListPlot[distsNmF2per[[2]],Joined->False,PlotLabel->"Nighttime NmF2",FrameLabel->{"Day of 2008","Difference in Density (%)"}]

