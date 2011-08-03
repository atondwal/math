(* ::Package:: *)

(*Anish Tondwalkar, NRL*)
(*
This file reads input from SAO files and extracts the profiles for plotting.
*)



station="WP937";
year=2003;
dir="/home/atondwal/ngdc.noaa.gov/ionosonde/data/"<>station<>"/individual/"<>ToString@year<>"/";
SetDirectory@dir;
data[field_,file_]:=(
(*lines = Ceiling[file[[2, 11 ;; 13]] {7, 120, 1, 8, 2, 7, 8, 8, 3, 1, 8}/120];
place=Plus@@lines[[1;;field-1]]+3;
Flatten[If[StringQ@#,ImportString[#,"Table"],#]&/@file[[place;;place-1+lines[[field]]]]]*)
lines= {0}~Join~Accumulate@Reverse@Ceiling[file[[2, 11 ;; 13]] {8,8,8}/120];
Flatten[If[StringQ@#,ToExpression/@StringReplace[StringJoin@@@Partition[Characters[#],8],"E"->" 10^"],#]&/@file[[-lines[[field+1]];;-lines[[field]]-1]]]
)
open[name_]:=MapAt[ToExpression/@StringJoin@@@Partition[Characters[#],3]&,Import[name,"List"],{{1},{2}}];


(*
http://ulcar.uml.edu/~iag/SAO-4.htm # TABLE1
The formatspec
7-hmF2 (km)
11-foF2 (MHz)
*)
profile[name_]:=(
file=open[name];
data[#,file]&/@{1,3}
)
hmf2[profile_]:=Interpolation[Transpose@profile]
max[profile_]:=Last/@profile
points[profile_]:=(
{ne,hm}=max[profile];
{hmf2[profile][.7 ne],.7 ne,hm,ne}
)
process[points_]:=MapAt[#^2/8980&,points,{{2},{4}}]
files[DOY_]:=FileNames[IntegerString[DOY, 10, 3] <> "/scaled/*.SAO"]
filesDay[DOY_]:=Cases[FileNames[IntegerString[DOY, 10, 3] <> "/scaled/*.SAO"],x_/; 8<ToExpression@StringTake[x,{25,26}]<17]
filesNight[DOY_]:=Cases[FileNames[IntegerString[DOY, 10, 3] <> "/scaled/*.SAO"], x_/; (hr=ToExpression@StringTake[x,{25,26}])<4||hr>20 ]


(* ::Input:: *)
(*file=open["101/scaled/WP937_20031010015.SAO"];*)
(*data[1,file]*)
(*data[3,file]*)


thedataDay=process/@points/@Cases[profile/@Flatten[filesDay/@Range[1,365]],Except[{}]];
badsDay=Reverse@Position[Transpose[thedataDay][[3]],Last[{}]];
{bsaDay,nbsaDay,hmF2Day,nmF2Day}=Transpose@Fold[Drop,thedataDay,badsDay];
thedataNight=process/@points/@Cases[profile/@Flatten[filesNight/@Range[1,365]],Except[{}]];
badsNight=Reverse@Position[Transpose[thedataNight][[3]],Last[{}]];
{bsaNight,nbsaNight,hmF2Night,nmF2Night}=Transpose@Fold[Drop,thedataNight,badsNight];
thedata=process/@points/@Cases[profile/@Flatten[files/@Range[1,365]],Except[{}]];
bads=Reverse@Position[Transpose[thedata][[3]],Last[{}]];
{bsa,nbsa,hmF2,nmF2}=Transpose@Fold[Drop,thedata,bads];


(* ::Input:: *)
(*DumpSave["processeddata.bin", {bsa, nbsa, hmF2,nmF2} ];*)
(* <<processeddata.bin *)


SetOptions[ListPlot,  ImageSize -> 700, AspectRatio -> 0.7, PlotStyle -> {Hue[0.8]},  
 BaseStyle -> {Thickness[0.004], FontFamily -> "Arial", FontSize -> 24}, Frame -> True, PlotRange -> All];

resolution = 5;
bsaPDF =ListPlot[Transpose[{Range[100, 800, resolution], 
   BinCounts[bsa, {100, 800 + resolution, resolution}]/Length[bsa]}], 
 FrameLabel -> {"Altitude (km)", 
   "Bottom Side Altitude Probability (\!\(\*SuperscriptBox[\"km\", 
RowBox[{\"-\", \"1\"}]]\))"}, Axes -> False, Joined -> True]

bsaCDF = ListPlot[
  Transpose[{Range[100, 800, resolution], 
    Accumulate@BinCounts[bsa, {100, 800 + resolution, resolution}]/
     Length[bsa]}], 
  FrameLabel -> {"Altitude (km)", 
    "Cumulative Bottom Side Altitude Probability"}, Axes -> False, 
  Joined -> True]

resolution = .015;
nbsaPDF = ListPlot[Transpose[{Range[0, 1, resolution], 
   BinCounts[nbsa 100, {0, 1 + resolution, resolution}]/
    Length[nbsa]}],
 FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \!\(\
\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))", 
   "Bottom Side Density Probability (\!\(\*SuperscriptBox[\"10\", 
RowBox[{\"-\", \"12\"}]]\) \!\(\*SuperscriptBox[\"m\", \"3\"]\))"}, 
 Axes -> False, Joined -> True]

resolution = .015;
nbsaCDF = 
 ListPlot[Transpose[{Range[0, 1, resolution], 
    Accumulate[
     BinCounts[nbsa 100, {0, 1 + resolution, resolution}]/
      Length[nbsa]]}],
  FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \
\!\(\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))", 
    "Cumulative Bottom Side Density Probability"}, Axes -> False, 
  Joined -> True]

resolution = 5;
hmF2PDF = ListPlot[Transpose[{Range[100, 800, resolution], 
   BinCounts[hmF2, {100, 800 + resolution, resolution}]/
    Length[hmF2]}],
 FrameLabel -> {"Altitude (km)", "Peak Height Probability"}, 
 Axes -> False, Joined -> True]

hmF2CDF = 
 ListPlot[Transpose[{Range[100, 800, resolution], 
    Accumulate@BinCounts[hmF2, {100, 800 + resolution, resolution}]/
     Length[hmF2]}],
  FrameLabel -> {"Altitude (km)", 
    "Cumulative Peak Height Probability"}, Axes -> False, 
  Joined -> True]

resolution = .015;
nmF2PDF = ListPlot[Transpose[{Range[0, 1, resolution], 
   BinCounts[nmF2 100, {0, 1 + resolution, resolution}]/
    Length[nmF2]}],
 FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \!\(\
\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))", 
   "Peak Density Probability (\!\(\*SuperscriptBox[\"10\", 
RowBox[{\"-\", \"12\"}]]\) \!\(\*SuperscriptBox[\"m\", \"3\"]\))"}, 
 Axes -> False, Joined -> True]

resolution = .015;
nmF2CDF = ListPlot[
  Transpose[{Range[0, 1, resolution], 
    Accumulate[
     BinCounts[nmF2 100, {0, 1 + resolution, resolution}]/
      Length[nmF2]]}], 
  FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \
\!\(\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))", "Cumulative Peak Density Probability"},   Axes -> False, Joined -> True]


SetOptions[ListPlot, PlotStyle -> {Hue[0.2]}];
resolution = 5;
bsaDayPDF =ListPlot[Transpose[{Range[100, 800, resolution], 
   BinCounts[bsaDay, {100, 800 + resolution, resolution}]/Length[bsaDay]}], 
 FrameLabel -> {"Altitude (km)", 
   "Bottom Side Altitude Probability (\!\(\*SuperscriptBox[\"km\", 
RowBox[{\"-\", \"1\"}]]\))"}, Axes -> False, Joined -> True]

bsaDayCDF = ListPlot[
  Transpose[{Range[100, 800, resolution], 
    Accumulate@BinCounts[bsaDay, {100, 800 + resolution, resolution}]/
     Length[bsaDay]}], 
  FrameLabel -> {"Altitude (km)", 
    "Cumulative Bottom Side Altitude Probability"}, Axes -> False, 
  Joined -> True]

resolution = .015;
nbsaDayPDF = ListPlot[Transpose[{Range[0, 1, resolution], 
   BinCounts[nbsaDay 100, {0, 1 + resolution, resolution}]/
    Length[nbsaDay]}], 
 FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \!\(\
\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))", 
   "Bottom Side Density Probability (\!\(\*SuperscriptBox[\"10\", 
RowBox[{\"-\", \"12\"}]]\) \!\(\*SuperscriptBox[\"m\", \"3\"]\))"}, 
 Axes -> False, Joined -> True]

resolution = .015;
nbsaDayCDF = 
 ListPlot[Transpose[{Range[0, 1, resolution], 
    Accumulate[
     BinCounts[nbsaDay 100, {0, 1 + resolution, resolution}]/
      Length[nbsaDay]]}], 
  FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \
\!\(\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))", 
    "Cumulative Bottom Side Density Probability"}, Axes -> False, 
  Joined -> True]

resolution = 5;
hmF2DayPDF = ListPlot[Transpose[{Range[100, 800, resolution], 
   BinCounts[hmF2Day, {100, 800 + resolution, resolution}]/
    Length[hmF2Day]}], 
 FrameLabel -> {"Altitude (km)", "Peak Height Probability"}, 
 Axes -> False, Joined -> True]

hmF2DayCDF = 
 ListPlot[Transpose[{Range[100, 800, resolution], 
    Accumulate@BinCounts[hmF2Day, {100, 800 + resolution, resolution}]/
     Length[hmF2Day]}], 
  FrameLabel -> {"Altitude (km)", 
    "Cumulative Peak Height Probability"}, Axes -> False, 
  Joined -> True]

resolution = .015;
nmF2DayPDF = ListPlot[Transpose[{Range[0, 1, resolution], 
   BinCounts[nmF2Day 100, {0, 1 + resolution, resolution}]/
    Length[nmF2Day]}], 
 FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \!\(\
\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))", 
   "Peak Density Probability (\!\(\*SuperscriptBox[\"10\", 
RowBox[{\"-\", \"12\"}]]\) \!\(\*SuperscriptBox[\"m\", \"3\"]\))"}, 
 Axes -> False, Joined -> True]

resolution = .015;
nmF2DayCDF = ListPlot[
  Transpose[{Range[0, 1, resolution], 
    Accumulate[
     BinCounts[nmF2Day 100, {0, 1 + resolution, resolution}]/
      Length[nmF2Day]]}], 
  FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \
\!\(\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))", "Cumulative Peak Density Probability"}, 
  Axes -> False, Joined -> True]


SetOptions[ListPlot, PlotStyle -> {Hue[0.2]}];
resolution = 5;
bsaNightPDF =ListPlot[Transpose[{Range[100, 800, resolution], 
   BinCounts[bsaNight, {100, 800 + resolution, resolution}]/Length[bsaNight]}], 
 FrameLabel -> {"Altitude (km)", 
   "Bottom Side Altitude Probability (\!\(\*SuperscriptBox[\"km\", 
RowBox[{\"-\", \"1\"}]]\))"}, Axes -> False, Joined -> True]

bsaNightCDF = ListPlot[
  Transpose[{Range[100, 800, resolution], 
    Accumulate@BinCounts[bsaNight, {100, 800 + resolution, resolution}]/
     Length[bsaNight]}], 
  FrameLabel -> {"Altitude (km)", 
    "Cumulative Bottom Side Altitude Probability"}, Axes -> False, 
  Joined -> True]

resolution = .015;
nbsaNightPDF = ListPlot[Transpose[{Range[0, 1, resolution], 
   BinCounts[nbsaNight 100, {0, 1 + resolution, resolution}]/
    Length[nbsaNight]}], 
 FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \!\(\
\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))", 
   "Bottom Side Density Probability (\!\(\*SuperscriptBox[\"10\", 
RowBox[{\"-\", \"12\"}]]\) \!\(\*SuperscriptBox[\"m\", \"3\"]\))"}, 
 Axes -> False, Joined -> True]

resolution = .015;
nbsaNightCDF = 
 ListPlot[Transpose[{Range[0, 1, resolution], 
    Accumulate[
     BinCounts[nbsaNight 100, {0, 1 + resolution, resolution}]/
      Length[nbsaNight]]}], 
  FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \
\!\(\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))", 
    "Cumulative Bottom Side Density Probability"}, Axes -> False, 
  Joined -> True]

resolution = 5;
hmF2NightPDF = ListPlot[Transpose[{Range[100, 800, resolution], 
   BinCounts[hmF2Night, {100, 800 + resolution, resolution}]/
    Length[hmF2Night]}], 
 FrameLabel -> {"Altitude (km)", "Peak Height Probability"}, 
 Axes -> False, Joined -> True]

hmF2NightCDF = 
 ListPlot[Transpose[{Range[100, 800, resolution], 
    Accumulate@BinCounts[hmF2Night, {100, 800 + resolution, resolution}]/
     Length[hmF2Night]}], 
  FrameLabel -> {"Altitude (km)", 
    "Cumulative Peak Height Probability"}, Axes -> False, 
  Joined -> True]

resolution = .015;
nmF2NightPDF = ListPlot[Transpose[{Range[0, 1, resolution], 
   BinCounts[nmF2Night 100, {0, 1 + resolution, resolution}]/
    Length[nmF2Night]}], 
 FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \!\(\
\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))", 
   "Peak Density Probability (\!\(\*SuperscriptBox[\"10\", 
RowBox[{\"-\", \"12\"}]]\) \!\(\*SuperscriptBox[\"m\", \"3\"]\))"}, 
 Axes -> False, Joined -> True]

resolution = .015;
nmF2NightCDF = ListPlot[
  Transpose[{Range[0, 1, resolution], 
    Accumulate[
     BinCounts[nmF2 100, {0, 1 + resolution, resolution}]/
      Length[nmF2]]}], 
  FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \
\!\(\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))", "Cumulative Peak Density Probability"}, 
  Axes -> False, Joined -> True]


nbsaAllCDF=Show[nbsaCDF,nbsaDayCDF,nbsaNightCDF]
bsaAllCDF=Show[bsaCDF,bsaDayCDF,bsaNightCDF]
hmF2AllCDF=Show[hmF2CDF,hmF2DayCDF,hmF2NightCDF]
nmF2AllCDF=Show[nmF2CDF,nmF2DayCDF,nmF2NightCDF]
nbsaAllPDF=Show[nbsaPDF,nbsaDayPDF,nbsaNightPDF]
bsaAllPDF=Show[bsaPDF,bsaDayPDF,bsaNightPDF]
hmF2AllPDF=Show[hmF2PDF,hmF2DayPDF,hmF2NightPDF]
nmF2AllPDF=Show[nmF2PDF,nmF2DayPDF,nmF2NightPDF]


Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nbsaCDF.jpg",nbsaCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"bsaCDF.jpg",bsaCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nmF2CDF.jpg",nmF2CDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"hmF2CDF.jpg",hmF2CDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nbsaPDF.jpg",nbsaPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"bsaPDF.jpg",bsaPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nmF2PDF.jpg",nmF2PDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"hmF2PDF.jpg",hmF2PDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nbsaNightCDF.jpg",nbsaNightCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"bsaNightCDF.jpg",bsaNightCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nmF2NightCDF.jpg",nmF2NightCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"hmF2NightCDF.jpg",hmF2NightCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nbsaNightPDF.jpg",nbsaNightPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"bsaNightPDF.jpg",bsaNightPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nmF2NightPDF.jpg",nmF2NightPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"hmF2NightPDF.jpg",hmF2NightPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nbsaDayCDF.jpg",nbsaDayCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"bsaDayCDF.jpg",bsaDayCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nmF2DayCDF.jpg",nmF2DayCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"hmF2DayCDF.jpg",hmF2DayCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nbsaDayPDF.jpg",nbsaDayPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"bsaDayPDF.jpg",bsaDayPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nmF2DayPDF.jpg",nmF2DayPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"hmF2DayPDF.jpg",hmF2DayPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nbsaAllCDF.jpg",nbsaAllCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"bsaAllCDF.jpg",bsaAllCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nmF2AllCDF.jpg",nmF2AllCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"hmF2AllCDF.jpg",hmF2AllCDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nbsaAllPDF.jpg",nbsaAllPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"bsaAllPDF.jpg",bsaAllPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"nmF2AllPDF.jpg",nmF2AllPDF];
Export["/home/atondwal/"<>station<>"/"<>ToString@year<>"hmF2AllPDF.jpg",hmF2AllPDF];
