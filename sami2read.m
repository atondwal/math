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
(*sami2hmf2u = Partition[Import["hmf2u.dat","Real32"][[2;;-2]],100];*)
(*sami2nmf2u = Partition[Import["nmf2u.dat","Real32"][[2;;-2]],100];*)
(*sami2tecu = Partition[Import["tecu.dat","Real32"][[2;;-2]],100];*)


(* ::Input:: *)
(*(*reading in data written by bin.*)*)
(*<<("empirical_data"<>ToString@year<>".up")*)
(*<<("samifoF2"<>ToString@year<>".up")*)
(*<<("samiNmF2"<>ToString@year<>".up")*)
(*<<("samihmF2"<>ToString@year<>".up")*)
(*<<("times"<>ToString@year<>".up")*)
(*<<("stations"<>ToString@year<>".up")*)


(*generates plots of the SAMI2 run*)
SetOptions[ListPlot,ImageSize->500];
slabheights = sami2tecu/sami2nmf2u;
plots=ListPlot[Transpose[# 1*^7,Range[62,62+94]]]&/@Transpose[slabheights];


(* ::Input:: *)
(*plots[[{50,42,55}]]*)


(*This fucntion shows all the plots on top of each other, similar to that in 
all.m, all.nb and multistation.m*)
allhmF2[n_]:=Show[
 Plot[dropbads[IhmF2][[n]][x], {x, 62, 107}], 
 ListPlot[dropbads[hmF2][[n]],Joined -> False,PlotStyle -> Red], 
 plots[[n]],
 PlotRange -> {{62, 63}, {100, 400}}, ImageSize -> 800]


(* ::Input:: *)
(*allhmF2/@{50,42,55}*)
