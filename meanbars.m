(* ::Package:: *)

(* Anish Tondwalkar, NRL *)
(*
This code shows some of the statistics from multistation.m with labels.
*)



(* ::Input:: *)
(*BarChart[#1,PlotLabel->#2[[1]], FrameLabel -> {"Hour of Day","Mean Difference Altitude (km)"} ,Frame->True]&@@@Fold[Drop,Transpose@{meanshmF2,dropbads@stations},Reverse@{{1},{4},{13},{19},{23}}]*)


(* ::Input:: *)
(*BarChart[#1,PlotLabel->#2[[1]], FrameLabel -> {"Hour of Day","Mean Difference Freq (MHz)"} ,Frame->True]&@@@Fold[Drop,Transpose@{meansfoF2,dropbads@stations},Reverse@{{1},{4},{13},{19},{23}}]*)


(* ::Input:: *)
(*BarChart[#1,PlotLabel->#2[[1]], FrameLabel -> {"Hour of Day","Mean Difference Altitude (km)"} ,Frame->True]&@@@Transpose@{meanshmF2,dropbads@stations}*)


(* ::Input:: *)
(*BarChart[#1,PlotLabel->#2[[1]], FrameLabel -> {"Hour of Day","Mean Difference Freq (MHz)"} ,Frame->True]&@@@Transpose@{meansfoF2,dropbads@stations}*)
