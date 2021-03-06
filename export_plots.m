(* ::Package:: *)

year="2004"
runname="run_2004_4"
stylefor2={Red,Blue}
(*Suffix=runname<>".pdf"*)
Suffix2=runname<>".CSV"
SetDirectory@dir


fulls[Suffix_]:=(
	out2[hs_]:=Grid[{{GraphicsGrid[Partition[hs,4],ImageSize->1200],legend[{"Data","SAMI3"},stylefor2]}}];
	SetOptions[ListPlot,ImageSize->400,BaseStyle->{FontFamily->"Ariel",Bold,FontSize->12}];
	Export["hmF2Short_"<>Suffix,out2[hs2]]
	Export["hmF2Full_"<>Suffix,out2[hf2]]
	Export["foF2Short_"<>Suffix,out2[fs2]]
	Export["foF2Full_"<>Suffix,out2[ff2]]
	Export["NmF2Short_"<>Suffix,out2[Ns2]]
	Export["NmF2Full_"<>Suffix,out2[Nf2]]
	For [i=1,i<19,i++,
	Export["StationfoF2"<>ToString[i]<>Suffix,allfoF2[i]]];
	For [i=1,i<19,i++,
	Export["StationhmF2"<>ToString[i]<>Suffix,allhmF2[i]]];
)


bcs[Suffix_]:=(
	(*BC=BarChart/@Fold[Drop,meanshmF2,{{1},{-1},{-4}}]*)
	SetOptions[BarChart,BarSpacing->0.0,ImageSize->2100,BaseStyle->{FontFamily->"Ariel",Bold,FontSize->12}];
	BCmeanshmF2=BarChart[#1,PlotRange->{{0,24},{-100,100}},Frame->True,FrameLabel->{"LT (Hour)","Mean Diff. hmF2 (km)",#2},Epilog->{Inset[Text[Style["SAMI-Data",Smaller]],ImageScaled[{0.1,.98}]]}] & @@@
	 Transpose@{meanshmF2,stationname};
	BCstddevshmF2=BarChart[#1,PlotRange->{{0,24},{0,100}},Frame->True,FrameLabel->{"LT (Hour)","Residual Std. Dev. hmF2 (km)",#2}] & @@@
	 Transpose@{stddevshmF2,stationname};
	BCmeansfoF2=BarChart[#1,PlotRange->{{0,24},{-4,4}},Frame->True,FrameLabel->{"LT (Hour)","Mean Diff. foF2 (MHz)",#2},Epilog->{Inset[Text[Style["SAMI-Data",Smaller]],ImageScaled[{0.1,.98}]]}] & @@@
	 Transpose@{meansfoF2,stationname};
	BCstddevsfoF2=BarChart[#1,PlotRange->{{0,24},{0,4}},Frame->True,FrameLabel->{"LT (Hour)","Residual Std. Dev. foF2 (MHz)",#2}] & @@@
	 Transpose@{stddevsfoF2,stationname};
	BCmeansNmF2=BarChart[#1,PlotRange->{{0,24},{-8 10^5,8 10^5}},Frame->True,FrameLabel->{"LT (Hour)","Mean Diff. NmF2 (\!\(\*SuperscriptBox[\"cm\", 
	RowBox[{\"-\", \"3\"}]]\))",#2},Epilog->{Inset[Text[Style["SAMI-Data",Smaller]],ImageScaled[{0.1,.98}]]}] & @@@
	 Transpose@{meansNmF2,stationname};
	BCstddevsNmF2=BarChart[#1,PlotRange->{{0,24},{0,8 10^5}},Frame->True,FrameLabel->{"LT (Hour)","Residual Std. Dev. NmF2 (\!\(\*SuperscriptBox[\"cm\", 
	RowBox[{\"-\", \"3\"}]]\))",#2}] & @@@
	 Transpose@{stddevsNmF2,stationname};
	BCmeansNmF2per=BarChart[#1,PlotRange->{{0,24},{-300,300}},Frame->True,FrameLabel->{"LT (Hour)","Mean Diff. NmF2 (%)",#2},Epilog->{Inset[Text[Style["SAMI-Data",Smaller]],ImageScaled[{0.1,.98}]]}] & @@@
	 Transpose@{meansNmF2per,stationname};
	BCstddevsNmF2per=BarChart[#1,PlotRange->{{0,24},{0,300}},Frame->True,FrameLabel->{"LT (Hour)","Residual Std. Dev. NmF2 (%)",#2}] & @@@
	 Transpose@{stddevsNmF2per,stationname};

	out2[Statplot_]:=GraphicsGrid[Partition[Statplot,4],ImageSize->1200];
	Export["hmF2_LT_mean"<>Suffix,out2[BCmeanshmF2]]
	Export["hmF2_LT_sd"<>Suffix,out2[BCstddevshmF2]]
	Export["foF2_LT_mean"<>Suffix,out2[BCmeansfoF2]]
	Export["foF2_LT_sd"<>Suffix,out2[BCstddevsfoF2]]
	Export["NmF2_LT_mean"<>Suffix,out2[BCmeansNmF2]]
	Export["NmF2_LT_sd"<>Suffix,out2[BCstddevsNmF2]]
	Export["NmF2per_LT_mean"<>Suffix,out2[BCmeansNmF2per]]
	Export["NmF2per_LT_sd"<>Suffix,out2[BCstddevsNmF2per]]
)


daynights[Suffix_]:=(
	SetOptions[ListPlot,ImageSize->400,BaseStyle->{FontFamily->"Ariel",Bold,FontSize->12},Epilog->{Inset[Text[Style["SAMI-Data",Bold]],ImageScaled[{.9,.87}]]}];
	DayNight1=ListPlot[distshmF2[[1]],PlotRange->{All,{-100,100}},Joined->False,PlotLabel->"Daytime hmF2",FrameLabel->{"Day of "<>year,"Difference in Altitude (km)"}];
	DayNight2=ListPlot[distshmF2[[2]],PlotRange->{All,{-100,100}},Joined->False,PlotLabel->"Nighttime hmF2",FrameLabel->{"Day of "<>year,"Difference in Altitude (km)"}];
	DayNight3=ListPlot[distsfoF2[[1]],PlotRange->{All,{-6,6}},Joined->False,PlotLabel->"Daytime foF2",FrameLabel->{"Day of "<>year,"Difference in Freq (MHz)"}];
	DayNight4=ListPlot[distsfoF2[[2]],PlotRange->{All,{-6,6}},Joined->False,PlotLabel->"Nighttime foF2",FrameLabel->{"Day of "<>year,"Difference in Freq (MHz)"}];
	DayNight5=ListPlot[distsNmF2[[1]],PlotRange->{All,{-8 10^5,8 10^5}},Joined->False,PlotLabel->"Daytime NmF2",FrameLabel->{"Day of "<>year,"Difference in Density (\!\(\*SuperscriptBox[\"cm\", 
	RowBox[{\"-\", \"3\"}]]\))"}];
	DayNight6=ListPlot[distsNmF2[[2]],PlotRange->{All,{-8 10^5,8 10^5}},Joined->False,PlotLabel->"Nighttime NmF2",FrameLabel->{"Day of "<>year,"Difference in Density (\!\(\*SuperscriptBox[\"cm\", 
	RowBox[{\"-\", \"3\"}]]\))"}];
	DayNight7=ListPlot[distsNmF2per[[1]],PlotRange->{All,{-300,300}},Joined->False,PlotLabel->"Daytime NmF2",FrameLabel->{"Day of "<>year,"Difference in Density (%)"}];
	DayNight8=ListPlot[distsNmF2per[[2]],PlotRange->{All,{-300,300}},Joined->False,PlotLabel->"Nighttime NmF2",FrameLabel->{"Day of "<>year,"Difference in Density (%)"}];
	Export["DayNight Diffs"<>Suffix, GraphicsGrid[{{DayNight1,DayNight2},{DayNight3,DayNight4},{DayNight5,DayNight6},{DayNight7,DayNight8}}]]
)


#/@{".png",".pdf"}&/@{fulls,bcs,daynights}


Export["hmF2_byDOY_Daytime"<>Suffix2,distshmF2[[1]]]
Export["hmF2_byDOY_Nighttime"<>Suffix2,distshmF2[[2]]]
Export["NmF2_byDOY_Daytime"<>Suffix2,distsNmF2per[[1]]]
Export["NmF2_byDOY_Nighttime"<>Suffix2,distsNmF2per[[2]]]
