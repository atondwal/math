(* ::Package:: *)

(*Anish Tondwalkar*)
(*NRL*)
(*
Inital Release on: Tue Aug  2 15:57:09 EDT 2011
This code reads in data from a run of SAMI2 and compares it to the binary data
output from bin.m
*)



(* ::Input:: *)
(*(*This is the directory where the files are located*)*)
(*dir="/home/atondwal/sami2-1.00/"*)


(* ::Input:: *)
(*dir="C:\\Users\\Anish\\Desktop\\sami\\"*)


SetDirectory@dir;


(* ::Input:: *)
(*(*We read in the SAMI2 run*)*)
(*sami2hmf2u = Partition[Import["hmf2u.datN","Real32"][[2;;-2]],100];*)
(*sami2nmf2u = Partition[Import["nmf2u.datN","Real32"][[2;;-2]],100];*)
(*sami2tecu = Partition[Import["tecu.datN","Real32"][[2;;-2]],100];*)


(* ::Input:: *)
(*SetDirectory@"/home/atondwal/sami/"*)
(*(*reading in data written by bin.*)*)
(*<<("empirical_data"<>ToString@year<>".up")*)
(*<<("samifoF2"<>ToString@year<>".up")*)
(*<<("samiNmF2"<>ToString@year<>".up")*)
(*<<("samihmF2"<>ToString@year<>".up")*)
(*<<("times"<>ToString@year<>".up")*)
(*<<("stations"<>ToString@year<>".up")*)


(* ::Input:: *)
(*(*We read in the SAMI2 run*)*)
(*sami2hmf2O = Partition[Import["hmf2u.datO","Real32"][[2;;-2]],100];*)
(*sami2nmf2O = Partition[Import["nmf2u.datO","Real32"][[2;;-2]],100];*)
(*sami2tecO = Partition[Import["tecu.datO","Real32"][[2;;-2]],100];*)
(*sami2hmf2T = Partition[Import["hmf2u.datT","Real32"][[2;;-2]],100];*)
(*sami2nmf2T = Partition[Import["nmf2u.datT","Real32"][[2;;-2]],100];*)
(*sami2tecT = Partition[Import["tecu.datT","Real32"][[2;;-2]],100];*)
(*sami2hmf2TT = Partition[Import["hmf2u.datTT","Real32"][[2;;-2]],100];*)
(*sami2nmf2TT = Partition[Import["nmf2u.datTT","Real32"][[2;;-2]],100];*)
(*sami2tecTT = Partition[Import["tecu.datTT","Real32"][[2;;-2]],100];*)
(*sami2hmf2TTT = Partition[Import["hmf2u.datTTT","Real32"][[2;;-2]],100];*)
(*sami2nmf2TTT = Partition[Import["nmf2u.datTTT","Real32"][[2;;-2]],100];*)
(*sami2tecTTT = Partition[Import["tecu.datTTT","Real32"][[2;;-2]],100];*)
(*sami2hmf2TTTT = Partition[Import["hmf2u.datTTTT","Real32"][[2;;-2]],100];*)
(*sami2nmf2TTTT = Partition[Import["nmf2u.datTTTT","Real32"][[2;;-2]],100];*)
(*sami2tecTTTT = Partition[Import["tecu.datTTTT","Real32"][[2;;-2]],100];*)
(*sami2hmf2TTTTT = Partition[Import["hmf2u.datTTTTT","Real32"][[2;;-2]],100];*)
(*sami2nmf2TTTTT = Partition[Import["nmf2u.datTTTTT","Real32"][[2;;-2]],100];*)
(*sami2tecTTTTT = Partition[Import["tecu.datTTTTT","Real32"][[2;;-2]],100];*)
(*sami2hmf2BAR = Partition[Import["hmf2u.datBAR","Real32"][[2;;-2]],100];*)
(*sami2nmf2BAR = Partition[Import["nmf2u.datBAR","Real32"][[2;;-2]],100];*)
(*sami2tecBAR = Partition[Import["tecu.datBAR","Real32"][[2;;-2]],100];*)


(*
3yy3P:.,.+3s/T\+/&T/g
*)


SetDirectory@"/home/atondwal/sami/";
<<("samiTEC"<>ToString@year<>".up")


(*generates plots of the SAMI2 run*)
SetOptions[ListPlot,ImageSize->100,PlotStyle->Green];
SetOptions[Show,ImageSize->700];
slabheights = sami2tecu/sami2nmf2u;
(*plots=ListPlot[Reverse/@Transpose@{# 1*^7,Range[62,63-2./24/4,1./24/4]}]&/@Transpose[slabheights];*)
plots=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]}]&/@Transpose[sami2hmf2u];


(*
f}i,PlotStyle->Red j OHf}i,PlotStyle->Blue jjjj OFhi,ImageSize->700jjj
yypiT
yyp:s/T\+[^r]/T&/g
yyp:s/T\+\([^r]\)/BAR\1/g
*)
plots=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]}]&/@Transpose[sami2hmf2u];
Oplots=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Red]&/@Transpose[sami2hmf2O];
Tplots=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2hmf2T];
TTplots=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2hmf2TT];
TTTplots=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2hmf2TTT];
TTTTplots=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2hmf2TTTT];
TTTTTplots=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2hmf2TTTTT];
BARplots=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2hmf2BAR];
Show[
 plots[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 Oplots[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 Tplots[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 TTplots[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 TTTplots[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 TTTTplots[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 TTTTTplots[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 BARplots[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 Plot[dropbads[IhmF2][[n]][x], {x, 62, 107}], 
 ListPlot[dropbads[hmF2][[n]],Joined->False,PlotStyle->Red], 
 PlotRange -> {{62, 63}, {100, 400}}, 
 ImageSize->700]


plotsn=ListPlot[Reverse/@Transpose@{Sqrt@# 8980*^-6,Range[62,63-2./24/4,1./24/4]}]&/@Transpose[sami2nmf2u];
Oplotsn=ListPlot[Reverse/@Transpose@{Sqrt@# 8980*^-6,Range[62,63-2./24/4,1./24/4]},PlotStyle->Red]&/@Transpose[sami2nmf2O];
Tplotsn=ListPlot[Reverse/@Transpose@{Sqrt@# 8980*^-6,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2nmf2T];
TTplotsn=ListPlot[Reverse/@Transpose@{Sqrt@# 8980*^-6,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2nmf2TT];
TTTplotsn=ListPlot[Reverse/@Transpose@{Sqrt@# 8980*^-6,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2nmf2TTT];
TTTTplotsn=ListPlot[Reverse/@Transpose@{Sqrt@# 8980*^-6,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2nmf2TTTT];
TTTTTplotsn=ListPlot[Reverse/@Transpose@{Sqrt@# 8980*^-6,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2nmf2TTTTT];
BARplotsn=ListPlot[Reverse/@Transpose@{Sqrt@# 8980*^-6,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2nmf2BAR];
Show[
  plotsn[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
  Oplotsn[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
  Tplotsn[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
  TTplotsn[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
  TTTplotsn[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
  TTTTplotsn[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
  TTTTTplotsn[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
  BARplotsn[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
   Plot[dropbads[IfoF2][[n]][x], {x, 62, 107}], 
 ListPlot[dropbads[foF2][[n]],Joined->False,PlotStyle->Red], 
PlotRange -> {{62, 63}, Automatic}, 
  ImageSize->700]


plotstec=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]}]&/@Transpose[sami2tecu];
Oplotstec=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Red]&/@Transpose[sami2tecO];
Tplotstec=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2tecT];
TTplotstec=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2tecTT];
TTTplotstec=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2tecTTT];
TTTTplotstec=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2tecTTTT];
TTTTTplotstec=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2tecTTTTT];
BARplotstec=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]},PlotStyle->Blue]&/@Transpose[sami2tecBAR];
Show[
 plotstec[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 Oplotstec[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 Tplotstec[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 TTplotstec[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 TTTplotstec[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 TTTTplotstec[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 TTTTTplotstec[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 BARplotstec[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
   rel=Take[Drop[Convert,61 12-3],All,{Floor[dropbads[stations][[n,4]]]},{Floor[dropbads[stations][[n,3]]]}];
 ListPlot[Transpose@{Range[62,62+(Length[rel]-1)/12,1/12] ,Flatten@rel},PlotStyle->Red],
 Plot[dropbads[ITEC][[n]][x], {x, 62, 107}], 
 PlotRange -> {{62, 63}, Automatic}, 
 ImageSize->700]


(* ::Input:: *)
(*plots[[#]]&/@Range[50,100]*)


(*This fucntion shows all the plots on top of each other, similar to that in 
all.m, all.nb and multistation.m*)
allhmF2[n_]:=Show[
 Plot[dropbads[IhmF2][[n]][x], {x, 62, 107}], 
 ListPlot[dropbads[hmF2][[n]],Joined->False,PlotStyle->Red], 
 plots[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 PlotRange -> {{62, 63}, {100, 400}}, ImageSize -> 800]


plotsn=ListPlot[Reverse/@Transpose@{Sqrt@# 8980*^-6,Range[62,63-2./24/4,1./24/4]}]&/@Transpose[sami2nmf2u];
allfoF2[n_]:=Show[
 Plot[dropbads[IfoF2][[n]][x], {x, 62, 107}], 
 ListPlot[dropbads[foF2][[n]],Joined->False,PlotStyle->Red], 
 plotsn[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 PlotRange -> {{62, 63}, Automatic}, ImageSize -> 800]


plotstec=ListPlot[Reverse/@Transpose@{#,Range[62,63-2./24/4,1./24/4]}]&/@Transpose[sami2tecu];
alltec[n_]:=Show[
   rel=Take[Drop[Convert,61 12-2],All,{Floor[dropbads[stations][[n,4]]]},{Floor[dropbads[stations][[n,3]]]}];
 ListPlot[Transpose@{Range[62,62+(Length[rel]-1)/12,1/12] ,Flatten@rel},PlotStyle->Red],
 Plot[dropbads[ITEC][[n]][x], {x, 62, 107}], 
 plotstec[[Ceiling[(dropbads[stations][[n,3]]+51.31454) 100/113]]],
 PlotRange -> {{62, 63}, Automatic}, ImageSize -> 800]


alltec/@{12,16,17}


allhmF2/@{12,16,17}


allfoF2/@{12,16,17}
