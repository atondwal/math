(* ::Package:: *)

(*Anish Tondwalkar*)
(*NRL*)


dir= "/home/atondwal/Ionosonde data 2008";
TIMEDIR="/home/atondwal/Sami Data from Sarah/time/";
Dir="/home/atondwal/Sami Data from Sarah/nmf2hmf2-1d/";
table="Data.csv";
lbound =25;
ubound =55;
year=2008;
SetDirectory@dir;
stations =Cases[Import@table, 
    {num_,code_,name_,lat_,lon_,mLat_,mLong_,date_,metadata_,_,_,_,_,_,_,_,_,"y",_,_}:>
    {name,code, mLat,lon}/;lbound<Abs[mLat]<ubound]; 
fname[Station_]:=ToString[year]<>"/"<>Station[[2]]<>"_"<>ToString[year]<>".txt.dated.new"
fhDATA[FileName_]:=(
DP1 = Transpose[Position[Dat1= Import[FileName,"Table"],"#>"]][[1]];
{Take[Dat1,Take[DP1,{2,3}]+{2,-1}] , Take[Dat1,Take[DP1,{-3,-2}]+{2,-1}]}
)


{foF2,hmF2}={#[[1]]+(#[[2]]/24.)+(#[[3]]/1440.),#[[4]]}&/@#&/@#&/@Transpose[fhDATA/@fname/@stations];


(* ::Input:: *)
(*DumpSave["empirical_data"<>ToString@year<>".bin",{foF2,hmF2}];*)


SetDirectory@Dir;
f[station_,DOY_]:={Import["hmf2-1d_"<>ToString@year<>(DOYString=IntegerString[DOY,10,3])<>"_"<>station[[2]]<>".dat"],
Import["nmf2-1d_"<>ToString@year<>DOYString<>"_"<>station[[2]]<>".dat"],
(DOY+(#[[2]]+(#[[3]]+#[[4]]/60)/60)/24)&/@Import[TIMEDIR<>"time_"<>ToString@year<>DOYString<>".dat","Table"]
}
Dimensions[{SamihmF2data,SamiNmF2data,uttimedata}=Transpose[ f@@#&/@ Tuples[{stations,Range[62,107]} ]]]
Dimensions[{uttimebig,SamifoF2big,SamihmF2big,SamiNmF2big}=Join@@@#&/@(Partition[#,46]&/@(Flatten/@#&/@{uttimedata,SamifoF2data,SamihmF2data,SamiNmF2data}))]
SamifoF2data=(8980.*^-6) Sqrt[SamiNmF2data];
SamifoF2big=(8980.*^-6) Sqrt[SamiNmF2big];


g[DOY_]:=(DOY+(#[[2]]+(#[[3]]+#[[4]]/60)/60)/24)&/@Import[TIMEDIR<>"time_"<>ToString@year<>IntegerString[DOY,10,3]<>".dat","Table"]
uttimedata=g/@Range[62,107];
uttimebig=Flatten[uttimedata]&/@Range[22];
Dimensions[g/@Range[62,107]//Flatten]


(*(Dimensions/@g/@Range[62,107]//Flatten) / (First/@Dimensions/@SamihmF2data[[46#+1;;46(#+1)]]) &/@Range[0,21]*)


SetDirectory@dir;
IfoF2=Interpolation[Transpose[#]]&/@Transpose[{uttimebig,SamifoF2big}];
INmF2=Interpolation[Transpose[#]]&/@Transpose[{uttimebig,SamiNmF2big}];
IhmF2=Interpolation[Transpose[#]]&/@Transpose[{uttimebig,SamihmF2big}];


(* ::Input:: *)
(*DumpSave["samifoF2"<>ToString@year<>".bin",IfoF2];*)
(*DumpSave["samiNmF2"<>ToString@year<>".bin",INmF2];*)
(*DumpSave["samihmF2"<>ToString@year<>".bin",IhmF2];*)


(* ::Input:: *)
(*DumpSave["times"<>ToString@year<>".bin",uttimebig];*)


(* ::Input:: *)
(*DumpSave["stations"<>ToString@year<>".bin",stations];*)
