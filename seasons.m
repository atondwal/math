(* ::Package:: *)

(* This package should tell you when the seasons begin and end in the standard 
  time format *)
(* Right now, let's just add the solstice/equinox data for 2004. Optimally, we'd
  just pull this out of Scientific Astronomer *)

tostd[list_]:=(AbsoluteTime[list]-AbsoluteTime["2004"])/86400

points={
	summerS	={2004,6,21,00,51,0},
	fallE	={2004,9,22,16,30,0},
	winterS	={2004,12,21,18,35,0},
	springE	={2004,3,20,06,48,0}
};

seasons=Partition[tostd/@points, 2, 1,{2,2}];
