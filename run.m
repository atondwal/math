(* ::Package:: *)

dir= "C:\\Users\\Anish\\iono2008";
Dir="C:\\Users\\Anish\\run_2004_4";
year=2004;
(*spring, summer, fall, winter*)
seasonnum=2;

SetDirectory@dir;
table="data.csv";
lbound=25;
ubound=60;
stations=Cases[Import@table, 
    {num_,code_,name_,lat_,lon_,mLat_,mLong_,date_,metadata_,_,_,_,_,"y",_,_,_,_,_,_,_}:>
    {name,code, mLat,lon}/;lbound<Abs[mLat]<ubound]; 
days={1,365};
tec=True;

imageSize=600;
style={Orange,Red,Blue};
SetOptions[Plot,Frame->True,Axes->False,PlotStyle->style];
SetOptions[ListPlot,Frame->True,Axes->False,Joined->True,PlotStyle->Hue@.1];


<<"C:\\Users\\Anish\\math\\package.m"



