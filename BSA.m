(* ::Package:: *)

(* ::Input:: *)
(*Anish Tondwalkar*)
(*NRL*)
(*
Finalized on:Tue Aug  2 16:02:05 EDT 2011
*)


year="2010";
dir="/home/atondwal/ngdc.noaa.gov/ionosonde/data/WP937/individual/"<>year<>"/";
station="WP937";
SetDirectory@dir;
(*This reads in all the EDPs for the day of year provided*)
f[DOY_]:= Import[#,"Table"]&/@FileNames[(DOYString=IntegerString[DOY,10,3])<>"/scaled/*.EDP"];


profiles=Join[Cases[#,{h_,Except[-99.],Except[-99.],n:Except[-99.],Except[-99.],_,_,_,_}:>{h,n}]&/@#&/@f/@1~Range~365];
profilesbig=Cases[Join@@profiles,Except[{}]];

(* ::Input:: *)
(*DumpSave["profiles.bin",profiles];*)


(* ::Input:: *)
(*<<profiles.bin*)


(* ::Input:: *)
(*DumpSave["profilesbig.bin",profilesbig];*)


(* ::Input:: *)
(*<<profilesbig.bin*)


fot[profile_]:={Interpolation[Reverse/@#,#[[-1,2]]*.85],#[[-1,2]]*.85,#[[-1,1]],#[[-1,2]]}&@DeleteDuplicates[profile,#1[[2]]==#2[[2]]&]
{bsa,Nbsa,Hmf2,Nmf2} =Transpose [fot/@profilesbig];


(* ::Input:: *)
(*DumpSave["processeddata.bin",{bsa,Nbsa,Hmf2,Nmf2} ];*)


(* ::Input:: *)
(*<<processeddata.bin*)


resolution=.5;
ListPlot[Transpose[{Range[100,400,resolution],BinCounts[bsa, {100,400+resolution,resolution}]/Length[bsa]}],ImageSize -> 700, AspectRatio -> 0.7,PlotStyle ->{Hue[0.1]},  BaseStyle->{Thickness[0.004],FontFamily->"Arial",FontSize->24}, Frame -> True,PlotRange -> All,FrameLabel -> {"Altitude (km)","Bottom Side Altitude Probability (\!\(\*SuperscriptBox[\"km\", 
RowBox[{\"-\", \"1\"}]]\))"}, Axes -> False, Joined -> True]


bsaCDF=ListPlot[Transpose[{Range[100,600,resolution],Accumulate@BinCounts[bsa, {100,600+resolution,resolution}]/Length[bsa]}],ImageSize -> 700, AspectRatio -> 0.7,PlotStyle ->{Hue[0.1]},  BaseStyle->{Thickness[0.004],FontFamily->"Arial",FontSize->24}, Frame -> True,PlotRange -> All,FrameLabel -> {"Altitude (km)","Cumulative Bottom Side Altitude Probability"}, Axes -> False, Joined -> True]


resolution=.002;
ListPlot[Transpose[{Range[0,1,resolution],BinCounts[Nbsa/1*^12, {0,1+resolution,resolution}]/Length[Nbsa]}],ImageSize -> 700, AspectRatio -> 0.7,PlotStyle ->{Hue[0.1]},  BaseStyle->{Thickness[0.004],FontFamily->"Arial",FontSize->24}, Frame -> True,PlotRange -> All,FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \!\(\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))","Bottom Side Density Probability (\!\(\*SuperscriptBox[\"10\", 
RowBox[{\"-\", \"12\"}]]\) \!\(\*SuperscriptBox[\"m\", \"3\"]\))"}, Axes -> False, Joined -> True]


resolution=.002;
nbsaCDF=ListPlot[Transpose[{Range[0,1,resolution],Accumulate[BinCounts[Nbsa/1*^12, {0,1+resolution,resolution}]/Length[Nbsa]]}],ImageSize -> 700, AspectRatio -> 0.7,PlotStyle ->{Hue[0.1]},  BaseStyle->{Thickness[0.004],FontFamily->"Arial",FontSize->24}, Frame -> True,PlotRange -> All,FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \!\(\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))","Cumulative Bottom Side Density Probability"}, Axes -> False, Joined -> True]


resolution=1;
ListPlot[Transpose[{Range[100,600,resolution],BinCounts[Hmf2, {100,600+resolution,resolution}]/Length[Hmf2]}],ImageSize -> 700, AspectRatio -> 0.7,PlotStyle ->{Hue[0.8]},  BaseStyle->{Thickness[0.004],FontFamily->"Arial",FontSize->24}, Frame -> True,PlotRange -> All,FrameLabel -> {"Altitude (km)","Peak Height Probability"}, Axes -> False, Joined -> True]


hmF2CDF=ListPlot[Transpose[{Range[100,600,resolution],Accumulate@BinCounts[Hmf2, {100,600+resolution,resolution}]/Length[Hmf2]}],ImageSize -> 700, AspectRatio -> 0.7,PlotStyle ->{Hue[0.8]},  BaseStyle->{Thickness[0.004],FontFamily->"Arial",FontSize->24}, Frame -> True,PlotRange -> All,FrameLabel -> {"Altitude (km)","Cumulative Peak Height Probability"}, Axes -> False, Joined -> True]


resolution=.002;
ListPlot[Transpose[{Range[0,1,resolution],BinCounts[Nmf2/1*^12, {0,1+resolution,resolution}]/Length[Nmf2]}],ImageSize -> 700, AspectRatio -> 0.7,PlotStyle ->{Hue[0.8]},  BaseStyle->{Thickness[0.004],FontFamily->"Arial",FontSize->24}, Frame -> True,PlotRange -> All,FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \!\(\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))","Peak Density Probability (\!\(\*SuperscriptBox[\"10\", 
RowBox[{\"-\", \"12\"}]]\) \!\(\*SuperscriptBox[\"m\", \"3\"]\))"}, Axes -> False, Joined -> True]


Show[hmF2CDF,bsaCDF,FrameLabel -> {"Altitude (km)","Cumulative Probability"}]


resolution=.002;
nhm=ListPlot[Transpose[{Range[0,1,resolution],Accumulate[BinCounts[Nmf2/1*^12, {0,1+resolution,resolution}]/Length[Nmf2]]}],ImageSize -> 700, AspectRatio -> 0.7,PlotStyle ->{Hue[0.8]},  BaseStyle->{Thickness[0.004],FontFamily->"Arial",FontSize->24}, Frame -> True,PlotRange -> All,FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \!\(\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))","Cumulative Peak Density Probability"}, Axes -> False, Joined -> True]


Show[nbsaCDF,nhm,FrameLabel -> {"Density (\!\(\*SuperscriptBox[\"10\", \"12\"]\) \!\(\*SuperscriptBox[\"m\", 
RowBox[{\"-\", \"3\"}]]\))","Cumulative Probability"}]

(*
0i(*A*)
*)
