(* ::Package:: *)

(* ::Input:: *)
(*NotebookEvaluate[NotebookDirectory[]<>"functions.m"]*)


(* ::Input:: *)
(*\[Lambda]avg2b[\[Lambda]0_,ff\[Delta]_,\[Tau]_]:=Module[{\[Delta]max0,ff\[Delta]2,ff\[Delta]3,ff\[Delta]2a,ff\[Delta]2b,under,lenff\[Delta]},*)
(*\[Delta]max0=\[Delta]max[\[Tau]];*)
(*ff\[Delta]2=Transpose[{ff\[Delta],Thread[ff[\[Lambda]0,ff\[Delta],\[Tau]]]/.ff->\[Lambda]pol2}];*)
(*lenff\[Delta]=Length[Part[ff\[Delta]2,All,2]];*)
(*under = ff\[Delta]2[[1,1]]<\[Delta]max[\[Tau]];ff\[Delta]3=GatherBy[ff\[Delta]2,#[[1]]<\[Delta]max[\[Tau]]&];*)
(*ff\[Delta]2a=If[under,ff\[Delta]3[[1]],ff\[Delta]3[[2]]];*)
(*ff\[Delta]2b=If[under,ff\[Delta]3[[2]],ff\[Delta]3[[1]]];;*)
(*{*)
(*Part[ff\[Delta]2,All,2] // Mean,Part[ff\[Delta]2a,All,2] // Mean,Part[ff\[Delta]2b,All,2] // Mean,*)
(*(Count[Part[ff\[Delta]2a,All,2],_?(#<\[Lambda]0 &)])/ lenff\[Delta] // N,*)
(*(Count[Part[ff\[Delta]2,All,2],_?(#==\[Lambda]0 &)])/ lenff\[Delta] // N*)
(*}*)
(*]*)
(*plot\[Lambda]avg2[ff\[Delta]_,\[Tau]_,npoints_]:=Module[{points,all,low\[Delta],high\[Delta],nocomply,comply,remainder},*)
(*points=Table[{i,\[Lambda]avg2b[i,ff\[Delta],\[Tau]]},{i,Subdivide[.0001,1,npoints]}];*)
(*all=Transpose[{points[[All,1]],points[[All,2]][[All,1]]}];*)
(*low\[Delta]=Transpose[{points[[All,1]],points[[All,2]][[All,2]]}];*)
(*high\[Delta]=Transpose[{points[[All,1]],points[[All,2]][[All,3]]}];*)
(*nocomply=Transpose[{points[[All,1]],points[[All,2]][[All,4]]}];comply=Transpose[{points[[All,1]],points[[All,2]][[All,5]]}];*)
(*remainder = Transpose[{points[[All,1]],1-points[[All,2]][[All,4]]-points[[All,2]][[All,5]]}];*)
(*TableForm[{*)
(*Plot[Interpolation[all,\[Lambda]c],{\[Lambda]c,0.0001,1},ImageSize->Medium,AxesLabel->{\[Lambda],}],*)
(*Plot[Interpolation[low\[Delta],\[Lambda]c],{\[Lambda]c,0.0001,1},ImageSize->Medium,AxesLabel->{\[Lambda],}],*)
(*Plot[{Interpolation[nocomply,\[Lambda]c],Interpolation[comply,\[Lambda]c],Interpolation[remainder,\[Lambda]c]},{\[Lambda]c,0.0001,1},ImageSize->Medium,AxesLabel->{\[Lambda],}]*)
(*}]*)
(*]*)


(* ::Input:: *)
(*\[Lambda]avg3b[\[Lambda]0_,ff\[Delta]_,\[Tau]D_,\[Tau]F_,\[Kappa]_]:=Module[{\[Delta]max0,ffcComply,ff\[Lambda]Comply,ffcmin,ff\[Lambda]minD,ff\[Lambda]minF,ffcNComplyD,ffcNComplyF,ffComplyU,ffcAll,ff\[Lambda]All,ff\[Lambda],ffcLowest,ffcLowesti,lenff\[Delta]},*)
(*\[Delta]max0=\[Delta]max[\[Tau]D];*)
(*lenff\[Delta]=Length[ff\[Delta]];*)
(*ffcComply = c[\[Lambda]0,ff\[Delta]*\[Kappa]];*)
(*ff\[Lambda]Comply = Table[\[Lambda]0,lenff\[Delta]];*)
(*ffcmin = cmin[ff\[Delta]*\[Kappa]];*)
(*ff\[Lambda]minD = \[Lambda]min[ff\[Delta]*\[Kappa]];*)
(*ff\[Lambda]minF=  \[Lambda]min[ff\[Delta]/\[Kappa]];*)
(*ffComplyU = Position[ff\[Lambda]minD,_?(#>\[Lambda]0 &)] // Flatten; (* Unconstrained *)*)
(*ffcComply[[ffComplyU]] = ffcmin[[ffComplyU]];*)
(*ff\[Lambda]Comply[[ffComplyU]] = ff\[Lambda]minD[[ffComplyU]];*)
(*ffcNComplyD = ffcmin * \[Tau]D;*)
(*ffcNComplyF = \[Tau]F *\[Kappa]  * cmin[ff\[Delta]/\[Kappa]];*)
(*ffcAll=Transpose[{ffcComply,ffcNComplyD,ffcNComplyF}];ff\[Lambda]All=Transpose[{ff\[Lambda]Comply,ff\[Lambda]minD,ff\[Lambda]minF}];ffcLowest=(Ordering[#,1]&/@ffcAll)//Flatten;*)
(*ffcLowesti=Transpose[{Range[lenff\[Delta]],ffcLowest}];ff\[Lambda]=ff\[Lambda]All[[#[[1]],#[[2]]]]&/@ffcLowesti;*)
(*ff\[Lambda] // Mean*)
(*]*)


(* ::Input:: *)
(*plot\[Lambda]avg3[ff\[Delta]_,\[Tau]D_,\[Tau]F_,\[Kappa]_,npoints_]:=Module[{points,\[Lambda]rule,\[Lambda]avg},*)
(*\[Lambda]rule=Subdivide[.0001,1,npoints];*)
(*\[Lambda]avg = \[Lambda]avg3b[#,ff\[Delta],\[Tau]D,\[Tau]F,\[Kappa]] & /@ \[Lambda]rule;*)
(*points=Transpose[{\[Lambda]rule,\[Lambda]avg}];*)
(*points*)
(*]*)


(* ::Input:: *)
(*\[Zeta]=.25;\[Tau]D=1.1;\[Tau]F=1.2;\[Delta]\[Sigma]=.2;npoints=20;ndraws=5*10^4;*)


(* ::Input:: *)
(*ff\[Delta][\[Kappa]0_]:=RandomVariate[LogNormalDistribution[Log[1/\[Kappa]0],\[Delta]\[Sigma]],ndraws];*)


(* ::Input:: *)
(*points115=plot\[Lambda]avg3[ff\[Delta][1.15],\[Tau]D,\[Tau]F,1.15,npoints];*)
(*points120=plot\[Lambda]avg3[ff\[Delta][1.2],\[Tau]D,\[Tau]F,1.2,npoints];*)
(*points125=plot\[Lambda]avg3[ff\[Delta][1.25],\[Tau]D,\[Tau]F,1.25,npoints];*)
(*points130=plot\[Lambda]avg3[ff\[Delta][1.3],\[Tau]D,\[Tau]F,1.3,npoints];*)
(*pointsNoF=plot\[Lambda]avg3[ff\[Delta][1],\[Tau]D,\[Tau]F*1000,1,npoints];*)


(* ::Input:: *)
(*laffercurves=Plot[{Interpolation[pointsNoF,\[Lambda]c],Interpolation[points115,\[Lambda]c],Interpolation[points120,\[Lambda]c],Interpolation[points125,\[Lambda]c],Interpolation[points130,\[Lambda]c],pointsNoF[[1,2]]},{\[Lambda]c,0.0001,1},ImageSize->Large,Frame->True,*)
(*PlotStyle->{{Darker[RGBColor[0.368417, 0.506779, 0.709798]],Opacity[.6]},RGBColor[0.880722, 0.611041, 0.142051],RGBColor[0.560181, 0.691569, 0.194885],RGBColor[0.647624, 0.37816, 0.614037],RGBColor[0.922526, 0.385626, 0.209179],{Darker[RGBColor[0.368417, 0.506779, 0.709798]],Opacity[.6],Dashed}},*)
(*PlotLegends->Placed[{"No Assembly in F","\[Kappa]=1.15","\[Kappa]=1.2","\[Kappa]=1.25","\[Kappa]=1.3"},{.15,.85}],*)
(*FrameLabel->{"Regional content requirement (\!\(\*SubscriptBox[\(\[Chi]\), \(R\)]\))","Average regional content share (X)"},LabelStyle->Directive[Black],BaseStyle->{FontSize->9.9}];*)


(* ::Input:: *)
(*SetDirectory[ParentDirectory[NotebookDirectory[]]<>"/Plots_JIE_rev/Mathematica"];*)
(*Export["roo6v3_laffer_2.pdf",laffercurves];*)