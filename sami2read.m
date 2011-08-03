(* ::Package:: *)

sami2hmf2u = Partition[Import["/home/atondwal/sami2-1.00/hmf2u.dat","Real32"][[2;;-2]],100];
sami2nmf2u = Partition[Import["/home/atondwal/sami2-1.00/nmf2u.dat","Real32"][[2;;-2]],100];
sami2tecu = Partition[Import["/home/atondwal/sami2-1.00/tecu.dat","Real32"][[2;;-2]],100];
slabheights = sami2tecu/sami2nmf2u;
ListPlot[#[[30]]]&/@slabheights]

(* ::Input:: *)
(**)
