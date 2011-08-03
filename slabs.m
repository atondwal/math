(* ::Package:: *)

(* ::Input:: *)
(*BarChart/@stddevsfoF2*)


(* ::Input:: *)
(*BarChart/@meansfoF2*)


(* ::Input:: *)
(*dropbads@stations*)


(* ::Input:: *)
(*IfoF2[[3]][62.75]*)


(* ::Input:: *)
(* Plot[dropbads[IfoF2][[3]][x], {x, 62, 63}]*)


(* ::Input:: *)
(*Clear[dropbads]*)


(* ::Input:: *)
(*diffhmF2c//Dimensions*)


(* ::Input:: *)
(*n=4*)


(* ::Input:: *)
(*Plot[{ITEC[[n]][x],INmF2[[n]][x].25*^-4},{x,62,68}]*)


(* ::Input:: *)
(*Plot[INmF2[[n]][x],{x,62,68}]*)


(* ::Input:: *)
(*ITEC[[1]][64]*)


(* ::Input:: *)
(*INmF2[[1]][64] *)


(* ::Input:: *)
(*(ITEC[[n]][x]/INmF2[[n]][x] )1*^7*)


(* ::Input:: *)
(*SetOptions[ListPlot,Joined->False,ImageSize->300,PlotStyle->Hue@.8];*)


(* ::Input:: *)
(*slabthickness[n_]:=ListPlot[Table[(ITEC[[n]][x]/INmF2[[n]][x] )1*^7,{x,62,68,.02}]]*)


(* ::Input:: *)
(*Transpose@{slabthickness/@1~Range~23,Transpose[stations][[3]]}*)


(* ::Input:: *)
(*BarChart/@nsfoF2*)
