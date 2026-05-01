(* ::Package:: *)

(* :Title: Gravity_FR_Try														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  The first case of putting together FeynArts and FeynGrav, the examples, and different things			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*The emission of a graviton from scalar particle interatcion*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El Ael -> El Ael, QED, matrix element squared, tree";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,1];
(*<<FeynArts`
$FAVerbose = 0;
<<FeynCalc`
FCCheckVersion[9,3,1];*)


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[p1,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(2\)]\)";
MakeBoxes[k0,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(0\)]\)";
MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


InitializeModel["QG", GenericModel -> "QG"];

top = CreateTopologies[0, 2 -> 3];

diags = InsertFields[
  top,
  {S[1], S[2]} -> {S[1], T[1], S[2]},
  InsertionLevel -> {Classes}, GenericModel -> "QG", Model -> "QG"
];
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


(*CreateFeynAmp[diags]*)
(*El FCFAConvert te un signe -i de diferencia respect si apliques directament les FR.*)
amp[0] = FCFAConvert[CreateFeynAmp[diags, PreFactor -> 1, Truncated -> True], IncomingMomenta->{p1, p2},
	OutgoingMomenta->{k1, k2}, ChangeDimension->4, List->True, SMP->True, Contract-> True];
amp[0][[2]]//Simplify
CreateFeynAmp[diags, Truncated -> False];


(* ::Section:: *)
(*Fix the kinematics*)


(* ::Section:: *)
(*Square the amplitude*)


(* ::Section:: *)
(*Check the final results*)
