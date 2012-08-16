(* ::Package:: *)

(*Anish Tondwalkar*)
(*NRL*)


$RecursionLimit=Infinity;
SetDirectory@dir;
fname[Station_]:=ToString[year]<>"/"<>Station[[2]]<>"_"<>ToString[year]<>".sanitized"
fhDATA=Cases[#,{a_,b_}/;b<900&&0<a<366]&/@Get[#]&;
Attributes[Table]={HoldFirst,Protected};
Attributes[Plot]={Protected};


(*This commented line writes the sanzitized data as Mathematica ASCII*)
(*Put[#,#2]&@@@Transpose[{Transpose[{foF2,hmF2}],StringSplit[#,"."][[1]]<>".sanitized"&/@fname/@stations}];*)
{foF2,hmF2}=Transpose[fhDATA/@fname/@stations];
NmF2=Transpose[{Transpose[#][[1]],(Transpose[#][[2]] 10^6/8980.)^2}]&/@foF2;


SetDirectory@Dir;
f[station_,DOY_]:={
Import["hmf2-1d_"<>ToString@year<>(DOYString=IntegerString[DOY,10,3])<>"_"<>station[[2]]<>".dat"],
Import["nmf2-1d_"<>ToString@year<>DOYString<>"_"<>station[[2]]<>".dat"],
If[tec,Import["tec-1d_"<>ToString@year<>DOYString<>"_"<>station[[2]]<>".dat"],],
(DOY+(#2+(#3+#4/60)/60)/24)&@@@Import["time_"<>ToString@year<>DOYString<>".dat","Table"]
}
Dimensions[{SamihmF2data,SamiNmF2data,SamiTECdata,uttimedata}=Transpose[ f@@@Tuples[{stations,Range@@days}] ]]
Dimensions[{uttimebig,SamifoF2big,SamihmF2big,SamiTECbig,SamiNmF2big}=
					Apply[Join,Map[Partition[#,days[[2]]-days[[1]]+1]&,
						Flatten/@#&/@{uttimedata,SamifoF2data,SamihmF2data,SamiTECdata,SamiNmF2data}
					],{2}]]
SamifoF2data=(8980.*^-6) Sqrt[SamiNmF2data];
SamifoF2big=(8980.*^-6) Sqrt[SamiNmF2big];


IfoF2=Interpolation[Transpose[#]]&/@Transpose[{uttimebig,SamifoF2big}];
INmF2=Interpolation[Transpose[#]]&/@Transpose[{uttimebig,SamiNmF2big}];
IhmF2=Interpolation[Transpose[#]]&/@Transpose[{uttimebig,SamihmF2big}];
ITEC=Interpolation[Transpose[#]]&/@Transpose[{uttimebig,SamiTECbig}];


(*Reading in the binary data from bin.m*)
SetDirectory@Dir;

(*Clean up the data; It's rather noisy*)
casefn=Transpose@MapAt[GaussianFilter[MedianFilter[#,5],5]&,Transpose@#,2]&;
hmF2C=casefn/@hmF2;
foF2C=casefn/@foF2;
NmF2C=casefn/@NmF2;


(*Takes a function and a (time,val) pair, and finds the (time, difference at time) *)
diffs[ifunc_,list_]:={#1,ifunc[#1]-#2}&@@@list
(*Maps (time, diff) to (time,% diff) *)
diffsper[ifunc_,list_]:={#1,100*(ifunc[#1]-#2)/#2}&@@@list
local[list_,lon_]:={#1-lon/(15*24),#2}&@@@list;
tolocal[diffdatac_]:=local@@@Transpose[{diffdatac,Transpose[stations][[3]]}];


(*This takes the difference between SAMI's output and the data, and splits in into lists*)
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
norm=casefn
hmF2I=Interpolation/@norm/@hmF2; 
foF2I=Interpolation/@norm/@foF2; 
NmF2I=Interpolation/@norm/@NmF2;


gaussianf[data_]:=GaussianFilter[data,5]
sample[sami_]:=Table[sami[x],{x}~Join~days~Join~{.01}]


(*smooth, correlate, and find phase/median shifts*)
f[sami_,sonde_]:=(
list=InverseFourier[Conjugate/@Fourier[(table=gaussianf@sample@sonde)-(m1=Median[table])]Fourier[(table=sample@sami)-(m2=Median[table])]];
{Mod[N[Flatten[Position[list,Max[list[[1;;100]]]]][[1]]],100]/100,m1-m2}
)


(*Unused, because it takes too long to run*)
(*
correctionhmF2=f@@@Transpose@{IhmF2,hmF2I}
correctionfoF2=f@@@Transpose@{IfoF2,foF2I}
correctionNmF2=f@@@Transpose@{INmF2,NmF2I}
*)

correctionNmF2=Table[{0,0},{thisVarIsNotUsed,Length[stations]}]
correctionfoF2=Table[{0,0},{thisVarIsNotUsed,Length[stations]}]
correctionhmF2=Table[{0,0},{thisVarIsNotUsed,Length[stations]}]


(*This adds legends to prepare list of plots for output*)
font="Arial";
fontSize=12;
markerSize=12;
legend[names_, styles_] := (
 GraphicsGrid[Graphics/@{Text@Style[#1, FontSize -> fontSize, font],
  Flatten[{#2, Line[{{-1, 0}, {1, 0}}]}]}&@@@Transpose@{names,styles}]
);


(*Generates the plots that I put in my talk, 16 per slide*)
{stationname,sid,slat,slong}=Transpose[stations];


hpn[f_]:=Plot[f@{#1[x],#2[x]}, {x}~Join~days,PlotRange->{All,{150,450}}, Axes->False,Frame->True,PlotStyle->stylefor2,FrameLabel->{"Day of Year","hmF2 (km)",#3}] & @@@
 Transpose@{hmF2I, IhmF2, stationname};
fpn[f_]:=Plot[f@{#1[x],#2[x]}, {x}~Join~days,PlotRange->{All,{1,15}},Axes->False,Frame->True,PlotStyle->stylefor2,FrameLabel->{"Day of Year","foF2 (MHz)",#3}] & @@@
 Transpose@{foF2I, IfoF2, stationname};
Npn[f_]:=Plot[f@{#1[x],#2[x]}, {x}~Join~days,Axes->False,Frame->True,PlotStyle->stylefor2,FrameLabel->{"Day of Year","NmF2 (\!\(\*SuperscriptBox[\"cm\", 
RowBox[{\"-\", \"3\"}]]\))",#3}] & @@@
 Transpose@{NmF2I, INmF2, stationname};

{hp2,fp2,Np2}=#[#&]&/@{hpn,fpn,Npn};
{hp3,fp3,Np3}=#[Reverse]&/@{hpn,fpn,Npn};
{hp,fp,Np}={hp2,fp2,Np2};


fs=Show[#,PlotRange->{{62,68},{1,10}}]&/@fp;
ff=Show[#,PlotRange->{days,{1,10}}]&/@fp;
hs=Show[#,PlotRange->{{62,68},{150,450}}]&/@hp;
hf=Show[#,PlotRange->{days,{150,450}}]&/@hp;
Ns=Show[#,PlotRange->{{62,68},{10^4,10^6}}]&/@Np;
Nf=Show[#,PlotRange->{days,{10^4,10^6}}]&/@Np;
fs2=Show[#,PlotRange->{{62,68},{1,10}}]&/@fp2;
ff2=Show[#,PlotRange->{days,{1,10}}]&/@fp2;
hs2=Show[#,PlotRange->{{62,68},{150,450}}]&/@hp2;
hf2=Show[#,PlotRange->{days,{150,450}}]&/@hp2;
Ns2=Show[#,PlotRange->{{62,68},{10^4,10^6}}]&/@Np2;
Nf2=Show[#,PlotRange->{days,{10^4,10^6}}]&/@Np2;
fs3=Show[#,PlotRange->{{62,68},{1,10}}]&/@fp3;
ff3=Show[#,PlotRange->{days,{1,10}}]&/@fp3;
hs3=Show[#,PlotRange->{{62,68},{150,450}}]&/@hp3;
hf3=Show[#,PlotRange->{days,{150,450}}]&/@hp3;
Ns3=Show[#,PlotRange->{{62,68},{10^4,10^6}}]&/@Np3;
Nf3=Show[#,PlotRange->{days,{10^4,10^6}}]&/@Np3;


(*Let's give us some fuctions to show everything for a station*)
allhmF2[n_]:=Show[ListPlot[
  Transpose[Identity[Transpose[{uttimebig, SamihmF2big}]][[n]]], 
  PlotStyle -> Hue@.2, Joined -> False], 
 Plot[Identity[IhmF2][[n]][x], {x}~Join~days], 
 ListPlot[Identity[hmF2][[n]],Joined -> False,PlotStyle -> Red], 
 ListPlot[Transpose@{sample[#&],#}&@gaussianf@sample@hmF2I[[n]],PlotStyle->{Orange,Thick}],
 Plot[Identity[IhmF2][[n]][x + correctionhmF2[[n]][[1]]] + 
   correctionhmF2[[n]][[2]], {x}~Join~days, PlotStyle -> {Hue@.8,Thick}], 
 PlotRange -> {{62, 68}, {150, 600}}, ImageSize -> 800, FrameLabel->{"Day of Year","hmF2 (km)",stationname[[n]]}]
allfoF2[n_]:=Show[ListPlot[
  Transpose[Identity[Transpose[{uttimebig, SamifoF2big}]][[n]]], 
  PlotStyle -> Hue@.2, Joined -> False], 
 Plot[Identity[IfoF2][[n]][x], {x, 62, 63}], 
 ListPlot[Identity[foF2][[n]],Joined -> False,PlotStyle -> Red], 
 ListPlot[Transpose@{sample[#&],#}&@gaussianf@sample@foF2I[[n]],PlotStyle->{Orange,Thick}],
 Plot[Identity[IfoF2][[n]][x + correctionfoF2[[n]][[1]]] + 
   correctionfoF2[[n]][[2]], {x}~Join~days, PlotStyle -> {Hue@.8,Thick}], 
 PlotRange -> {{62, 68}, {0,15}}, ImageSize -> 800, FrameLabel->{"Day of Year","foF2 (MHz)",stationname[[n]]}]


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
ListPlot[distsNmF2per[[1]],Joined->False,PlotLabel->"Daytime NmF2",FrameLabel->{"Day of 2008","Difference in Density (%)"}]
ListPlot[distsNmF2per[[2]],Joined->False,PlotLabel->"Nighttime NmF2",FrameLabel->{"Day of 2008","Difference in Density (%)"}]










