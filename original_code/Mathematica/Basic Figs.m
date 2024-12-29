(* ::Package:: *)

(* ::Input:: *)
(*NotebookEvaluate[NotebookDirectory[]<>"functions.m"]*)
(*resetparams:=(ClearAll[\[Zeta]];\[Zeta]=.25;\[Tau]=1.1;)*)


(* ::Input:: *)
(*SetOptions[Plot,PlotStyle->Opacity[.75],ImageSize->Medium,Frame->True,LabelStyle->Directive[Black]];*)
(*dashedline[\[Lambda]0_,\[Tau]0_,color_,opacity_:1]:={color,Opacity[opacity],Dashed,Line[{{\[Lambda]0,0},{\[Lambda]0,\[Tau]0}}]}*)


(* ::Input:: *)
(*resetparams;partshares=Plot[{\[Lambda]pol2[\[Lambda],1.25,\[Tau]],\[Lambda]pol2[\[Lambda],.999,\[Tau]],\[Lambda]pol2[\[Lambda],.8,\[Tau]]},{\[Lambda],0,1},Exclusions->None,ImageSize->Medium,Frame->True,FrameLabel->{"Regional content requirement (\!\(\*SubscriptBox[\(\[Chi]\), \(R\)]\))","Firm domestic part share (\[Chi])"},Epilog-> {Text["\[Delta]=0.8",{0,.29},{-1,-1}],Text["\[Delta]=1",{0,.5},{-1,-1}],Text["\[Delta]=1.25",{0,.71},{-1,-1}]}];*)


(* ::Input:: *)
(*resetparams;costratio=Plot[{c1full[\[Lambda],1.25],c1full[\[Lambda],1],c1full[\[Lambda],.8]},{\[Lambda],0,1},Exclusions->None,ImageSize->Medium,Frame->True,FrameLabel->{"Regional content requirement (\!\(\*SubscriptBox[\(\[Chi]\), \(R\)]\))","Cost Penalty for ROO Compliance (\!\(\*OverscriptBox[\(C\), \(~\)]\))"},GridLines->{{},{1.1}},*)
(*GridLinesStyle->Directive[Gray,Thickness[0.003]],Epilog-> {Text["\[Tau]",{0,\[Tau]},{-1,-1}],Text["\[Delta]=0.8",{.77,1.195}],Text["\[Delta]=1",{.92,1.15}],Text["\[Delta]=1.25",{.79,1.02}],*)
(*dashedline[\[Lambda]star2[.8,\[Tau]],\[Tau],RGBColor[0.560181, 0.691569, 0.194885],.5],*)
(*dashedline[\[Lambda]star2[1,\[Tau]],\[Tau],RGBColor[0.880722, 0.611041, 0.142051],.5]*)
(*}];*)


(* ::Input:: *)
(*c1full7=Piecewise[{{1,\[Lambda]<= .5},{1.15,\[Lambda]>.5}}];c1full9=Piecewise[{{1,\[Lambda]<= .5},{1.05,\[Lambda]>.5}}];*)
(*c1full1=1;\[Lambda]pol9=Piecewise[{{.5,\[Lambda]<= .5},{1,\[Lambda]>.5}}];*)


(* ::Input:: *)
(*costratio1=Plot[{1,c1full9,c1full7},{\[Lambda],0,1},FrameLabel->{"Regional content requirement (\!\(\*SubscriptBox[\(\[Chi]\), \(R\)]\))","Cost Penalty for ROO Compliance (\!\(\*OverscriptBox[\(C\), \(~\)]\))"},Exclusions->None,GridLines->{{},{1.1}},*)
(*GridLinesStyle->Directive[Gray,Thickness[0.003]],Epilog-> {Text["\[Tau]",{0,\[Tau]},{-1,-1}],Text["\[Delta]=0.7",{1,1.15},{1,1}],Text["\[Delta]=0.9",{1,1.05},{1,-1}],Text["\[Delta]\[GreaterEqual]1",{1,1},{1,-1}]}];*)


(* ::Input:: *)
(*partshares1=Plot[{1,\[Lambda]pol9,.5},{\[Lambda],0,1},Exclusions->None,ImageSize->Medium,Frame->True,FrameLabel->{"Regional content requirement (\!\(\*SubscriptBox[\(\[Chi]\), \(R\)]\))","Firm domestic part share (\[Chi])"},Epilog-> {Text["0.8\[LessEqual]\[Delta]<1",{.51,.75},{-1,0}],Text["\[Delta]<0.8",{1,.5},{1,-1}],Text["\[Delta]\[GreaterEqual]1",{0,1},{-1,1}]}];*)


(* ::Input:: *)
(*SetDirectory[ParentDirectory[NotebookDirectory[]]<>"/Plots_JIE_rev/Mathematica"];*)
(*Export["CostPenalty.pdf",costratio];*)
(*Export["PartShares.pdf",partshares];*)
(*Export["CostPenalty_SinglePart.pdf",costratio1];*)
(*Export["PartShares_SinglePart.pdf",partshares1];*)
