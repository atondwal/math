(* ::Package:: *)

(*Anish Tondwalkar*)
(*NRL*)
(*
This code is based on the statistics found in the input cells of multistation.m
It plots some of these stats, separated by Day/Night.
*)



(* ::Input:: *)
(*(*Thowaway code*)*)
(*goodmeans=Fold[Drop,meansfoF2,Reverse@{{1},{4},{13},{19},{23}}];*)
(*goodstations=Fold[Drop,stations,Reverse@{{1},{4},{13},{19},{23}}];*)
(*paired=Transpose[{Transpose[goodstations][[4]],goodmeans}];*)
(*x24[x_]:=x&/@1~Range~24;*)
(*dist=Transpose@{x24[#1],1~Range~24,#2}&@@@paired;*)
(*ListPointPlot3D[ dist, PlotRange->{{62,107},{0,1},Automatic},FaceGrids->{{0,1,0},{0,-1,0}}]*)

(*this takes the statistics from multistation.m and uses it to display mean differenec by hour, by day and night *)
distfoF2={IntegerPart[#1],If[8<IntegerPart[24FractionalPart[#1]]<17,1,0],#2}&@@@#&/@meansfoF22;
disthmF2={IntegerPart[#1],If[8<IntegerPart[24FractionalPart[#1]]<17,1,0],#2}&@@@#&/@meanshmF22;
distsfoF2={#1,#3}&@@@#&/@GatherBy[Join@@distfoF2,#[[2]]&]
distshmF2={#1,#3}&@@@#&/@GatherBy[Join@@disthmF2,#[[2]]&]
SetOptions[ListPlot,Joined->False,ImageSize->500,PlotStyle->Hue@.8];
ListPlot[distshmF2[[1]],PlotLabel->"Daytime \!\(\*SubscriptBox[\"h\", SubscriptBox[\"mF\", \"2\"]]\)",FrameLabel->{"Day of 2008","Difference in Altitude (km)"}]
ListPlot[distshmF2[[2]],PlotLabel->"Nighttime \!\(\*SubscriptBox[\"h\", SubscriptBox[\"mF\", \"2\"]]\)",FrameLabel->{"Day of 2008","Difference in Altitude (km)"}]
ListPlot[distsfoF2[[1]],PlotLabel->"Daytime \!\(\*SubscriptBox[SubscriptBox[\"f\", \"o\"], SubscriptBox[\"F\", \"2\"]]\)",FrameLabel->{"Day of 2008","Difference in Freq (MHz)"}]
ListPlot[distsfoF2[[2]],PlotLabel->"Nighttime \!\(\*SubscriptBox[SubscriptBox[\"f\", \"o\"], SubscriptBox[\"F\", \"2\"]]\)",FrameLabel->{"Day of 2008","Difference in Freq (MHz)"}]


(* ::Input::Plain:: *)
(*ListPlot3D[ dist]*)


(* ::Input:: *)
(*ListPointPlot3D[{#1,#2,Abs@#3}&@@@Reverse/@Transpose@Join[{Transpose[correctionfoF2][[2]]},Transpose[dropbads@stations][[3;;4]]]]*)


(* ::Input:: *)
(*ListPointPlot3D[{#1,#2,Abs@#3}&@@@Reverse/@Transpose@Join[{Transpose[correctionhmF2][[2]]},Transpose[dropbads@stations][[3;;4]]]]*)


(* ::Input:: *)
(*Position[hmF2I,_Interpolation]*)
(*Reverse/@Transpose@Join[{Transpose[correctionhmF2][[2]]},Transpose[dropbads@stations][[3;;4]]]*)


(* ::Input:: *)
(*Join@tolocal[diffhmF2c]*)
