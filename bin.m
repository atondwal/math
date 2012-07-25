(* ::Package:: *)

(*Anish Tondwalkar*)
(*NRL*)

(*
This takes data from dir and Dir and writes the important stuff into a 
mathematica binary file.

Depends: Ionosonde and sami data

Gives:
	foF2 and hmF2 are the empirics
	Ifof2, etc. are the interpolated SAMI data points

*)


dir= "C:\\Users\\Anish\\iono2008";
Dir="C:\\Users\\Anish\\sami";
$RecursionLimit=Infinity;
table="data.csv";
lbound =25;
ubound =60;
year=2008;
SetDirectory@dir;
stations =Cases[Import@table, 
    {num_,code_,name_,lat_,lon_,mLat_,mLong_,date_,metadata_,_,_,_,_,_,_,_,_,"y",_,_,_}:>
    {name,code, mLat,lon}/;lbound<Abs[mLat]<ubound]; 
fname[Station_]:=ToString[year]<>"/"<>Station[[2]]<>"_"<>ToString[year]<>".txt.dated.new"
fhDATA[FileName_]:=(
DP1 = Transpose[Position[Dat1= Import[FileName,"Table"],"#>"]][[1]];
{Take[Dat1,Take[DP1,{2,3}]+{2,-1}] , Take[Dat1,Take[DP1,{-3,-2}]+{2,-1}]}
)
unbad[x_]:=If[Dimensions[x][[1]]<2,x,{x[[1]]}~Join~unbad[x[[If[x[[2,1]]==x[[1,1]],3,2];;-1]]]]


(*This commented line writes the sanzitized data as Mathematica ASCII*)
(*Put[#,#2]&@@@Transpose[{Transpose[{foF2,hmF2}],StringSplit[#,"."][[1]]<>".sanitized"&/@fname/@stations}];*)
(*Reads the sanitized ASCII*)
(*{foF2,hmF2}=Transpose[Import/@(StringSplit[#,"."][[1]]<>".sanitized"&/@fname/@stations)]*)
{foF2,hmF2}=unbad/@#&/@({#[[1]]+(#[[2]]/24.)+(#[[3]]/1440.),#[[4]]}&/@#&/@#&/@Transpose[fhDATA/@fname/@stations]);


(* ::Input:: *)
(*SetDirectory@Dir;*)
(*DumpSave["empirical_data"<>ToString@year<>".up",{foF2,hmF2}];*)


SetDirectory@Dir;
f[station_,DOY_]:={
Import["hmf2-1d_"<>ToString@year<>(DOYString=IntegerString[DOY,10,3])<>"_"<>station[[2]]<>".dat"],
Import["nmf2-1d_"<>ToString@year<>DOYString<>"_"<>station[[2]]<>".dat"],
Import["tec-1d_"<>ToString@year<>DOYString<>"_"<>station[[2]]<>".dat"],
(DOY+(#[[2]]+(#[[3]]+#[[4]]/60)/60)/24)&/@Import["time_"<>ToString@year<>DOYString<>".dat","Table"]
}
Dimensions[{SamihmF2data,SamiNmF2data,SamiTECdata,uttimedata}=Transpose[ f@@#&/@ Tuples[{stations,Range[62,107]} ]]]
Dimensions[{uttimebig,SamifoF2big,SamihmF2big,SamiTECbig,SamiNmF2big}=Join@@@#&/@(Partition[#,46]&/@(Flatten/@#&/@{uttimedata,SamifoF2data,SamihmF2data,SamiTECdata,SamiNmF2data}))]
SamifoF2data=(8980.*^-6) Sqrt[SamiNmF2data];
SamifoF2big=(8980.*^-6) Sqrt[SamiNmF2big];


IfoF2=Interpolation[unbad[Transpose[#]]]&/@Transpose[{uttimebig,SamifoF2big}];
INmF2=Interpolation[unbad[Transpose[#]]]&/@Transpose[{uttimebig,SamiNmF2big}];
IhmF2=Interpolation[unbad[Transpose[#]]]&/@Transpose[{uttimebig,SamihmF2big}];
ITEC=Interpolation[unbad[Transpose[#]]]&/@Transpose[{uttimebig,SamiTECbig}];


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
