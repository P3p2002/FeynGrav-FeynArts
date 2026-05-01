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


<<FeynGrav`


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(**)


FGtoFA[expr_] :=
  expr //. {
            Pair[LorentzIndex[mu_, ___], LorentzIndex[nu_, ___]] :> FAMetricTensor[mu, nu],
            Pair[Momentum[p_, ___], LorentzIndex[mu_, ___]] :> FAFourVector[p, mu],
            Pair[Momentum[p_, ___], Momentum[q_, ___]] :> FAScalarProduct[p, q]
  };
  ClearAll[CollectLorentzStructures];
SetAttributes[CollectLorentzStructures, HoldAll];

(*This function is greate as it will separate the different Lorentz Structures and return a list with them.
It should also put them together, for example a*g{\mu1 \nu1} + .... + bg{\mu1 \mu2} together.
It also returns a list with the coefficient of each Lorentz Structure.
*)
CollectLorentzStructures[expr_] := Module[
  {
    terms, grouped, getKey, getCoeff, key,
    normalizePair, flattenFactors, sortedKey
  },

  (* Normalize metric symmetry: g^{\[Mu]\[Nu]} = g^{\[Nu]\[Mu]} *)
  normalizePair[Pair[LorentzIndex[a_], LorentzIndex[b_]]] := 
    If[OrderedQ[{a, b}],
      Pair[LorentzIndex[a], LorentzIndex[b]],
      Pair[LorentzIndex[b], LorentzIndex[a]]
    ];
  normalizePair[p_] := p;

  (* Fully flatten all Times chains into a list *)
  flattenFactors[x_] := FixedPoint[(# /. Times -> List) &, x];

  (* Extract Lorentz structures *)
  getKey[term_] := Module[{factors, lorentzTerms},
    factors = flattenFactors[term];
    
    lorentzTerms = Select[factors,
      MatchQ[#, 
        Pair[LorentzIndex[_], Momentum[_]] |
        Pair[Momentum[_], LorentzIndex[_]] |
        Pair[LorentzIndex[_], LorentzIndex[_]] |
        Momentum[_, LorentzIndex[_]]
      ]&
    ];
    (* Normalize metric tensors only *)
    lorentzTerms /. p : Pair[LorentzIndex[_], LorentzIndex[_]] :> normalizePair[p]
  ];

  (* Sort the Lorentz structure *)
  sortedKey[key_List] := Sort[key, OrderedQ[{ToString[#1], ToString[#2]}] &];

  (* Extract coefficient by dividing out Lorentz factors *)
  getCoeff[term_, key_] := Module[{num, den, denKey},
  num = Numerator[term];
  den = Denominator[term];

   (* The momenta that I want to cancel *)
  denKey = Times @@ key;

  (* Now divide numerator by denominator key, and multiply back by remaining denominator *)
  (* To avoid mismatch, divide only if denKey divides den *)
  
  Cancel[num/(den*denKey)]
];


  (* Expand the input expression and break into terms *)
  terms = If[Head[expr] === Plus, List @@ Expand[expr], {Expand[expr]}];
	
  (* Group similar Lorentz structures *)
  grouped = <||>;
	Do[
	(*I put Numerator[term] because to get the momenta the denominator is not needed
	and it causes some errors because I have terms like a*b + c*d*e*)
	  key = sortedKey[getKey[Numerator[term]]];
	
	
	  If[KeyExistsQ[grouped, key],
	    grouped[key] += getCoeff[term, key],
	    grouped[key] = getCoeff[term, key]
	  ],
	  {term, terms}
	];
	
  (* Reconstruct expression from grouped terms 
  I think I do not need this function anymore*)
(*Table[
  Module[{factors = ReleaseHold[hk]},
    grouped[hk] *
      Which[
        ListQ[factors], Times @@ factors,
        Head[factors] === Times, factors,
        True, factors
      ]
  ],
  {hk, Keys[grouped]}
];*)
coefficients = Values[grouped];
lorentzStructures = Keys[grouped];
lorentzStructures = Times @@@ lorentzStructures;

{coefficients, lorentzStructures}

];


(* ::Subtitle:: *)
(*Put the number of gravitons, if we have scalars or not (0 or 2) and the different number of scalars*)


Numbergravis = 1; (*Only 1 and 2 gravis if we have scalars, and if we do not have them the only allowed number of gravitons 
is 3 and 4*)
Scalars = 2; (*This is if we want to generate the FR with scalars or without them
The only values that we are working with are either 0 or 2.*)
DifferentScalars = 1;(*This is if we have a theory with two distinguishable scalars, just one, or more than one*)


ClearAll[GenerateVertexData]

GenerateVertexData[Numbergravis_, Scalars_] := 
 Module[{gravitonLorentzIndices, gravitonmomenta, gravitonindicesmomenta, 
   scalarmomentum1, scalarmomentum2, VertexLS},

  (* Step 1: Create Lorentz index pairs for gravitons *)
  gravitonLorentzIndices = 
   Table[{Symbol["\[Mu]" <> ToString[i]], Symbol["\[Nu]" <> ToString[i]]}, 
    {i, Numbergravis}];

  (* Step 2: Depending on the number of scalars, build the appropriate vertex *)
  Which[
   Scalars > 0,
   Module[{},
    scalarmomentum1 = p1;
    scalarmomentum2 = p2;
    gravitonmomenta = 
     Table[Symbol["p" <> ToString[i + 2]], {i, Numbergravis}];
    VertexLS = 
     CollectLorentzStructures[
       GravitonScalarVertex[
         Flatten[gravitonLorentzIndices], scalarmomentum1, scalarmomentum2, 0] /. 
        D -> 4];
    ],
   
   Scalars == 0,
   Module[{},
    gravitonmomenta = Table[Symbol["p" <> ToString[i]], {i, Numbergravis}];
    gravitonindicesmomenta = 
     Flatten[MapThread[Join, {gravitonLorentzIndices, 
        List /@ gravitonmomenta}]];
    VertexLS = 
     CollectLorentzStructures[
       GravitonVertex[Sequence @@ gravitonindicesmomenta] /. D -> 4];
    ]
   ];

  (* Step 3: Return all relevant data *)
  <|
   "GravitonLorentzIndices" -> gravitonLorentzIndices,
   "GravitonMomenta" -> gravitonmomenta,
   "ScalarMomenta" -> If[Scalars > 0, {scalarmomentum1, scalarmomentum2}, {}],
   "VertexLS" -> VertexLS
   |>
  ]
Indices = GenerateVertexData[Numbergravis, Scalars][[1]];
MomentaGravi = GenerateVertexData[Numbergravis, Scalars][[2]];
MomentaScalar = GenerateVertexData[Numbergravis, Scalars][[3]];
VertexLS = GenerateVertexData[Numbergravis, Scalars][[4]];


(*This function is defined because we do not want to have any scalar product inside the couplings, we want them in the
Lorentz structures. The couplings will go to a file which does not have any information about the momenta of the 
particles, and thus it will not make sense it put it there*)
ClearAll[SolveSP];
(*I should be carefull with this one in the case I have something like (p1*p2)*(p2*p3)*)
SolveSP[VertexList_] := Module[{spList, LorStrfg, CoupConstfg, expandedCouplings},
  
  (* 1. Expand the coupling constant in case it\[CloseCurlyQuote]s a sum of SP[...] terms *)
  expandedCouplings = Expand[VertexList[[1]]];
  
  (* 2. Make sure it\[CloseCurlyQuote]s treated as a list of separate terms *)
  expandedCouplings = List @@ expandedCouplings;
  
  (* 3. Extract SPs (or 1 if none) from each term *)
  spList = Table[
    Module[{pairs = Cases[term, Pair[Momentum[__], Momentum[__]], {0, Infinity}]},
      If[pairs === {}, 1, Plus @@ pairs]
    ],
    {term, expandedCouplings}
  ];
  
  (* 4. Element-wise multiplication with Lorentz structures (also expanded if needed) *)
  LorStrfg = MapThread[#1*#2 &, {VertexList[[2]], spList}];
  CoupConstfg = MapThread[Simplify[#1/#2] &, {expandedCouplings, spList}];
  
  (* 5. Return both outputs *)
  <|"LorentzStructure" -> LorStrfg, "CouplingConstants" -> CoupConstfg|>
]

VertexLSSP = SolveSP[VertexLS//Simplify];


FAFG = FGtoFA @ VertexLSSP;
LorStr = FAFG[[1]];
CoupConst = FAFG[[2]];


(*This will generate what will be copy pasted to the .gen document*)
SentenceGenericCouplings[ngravis_, scalars_] := Module[{gravitonParts, scalarParts, 
FirstPart, finalPart0, finalPart1, FinalPart, FullSentence,FirstSen},
	Which[ scalars > 0,
			Module[{},
					gravitonParts = StringJoin[Table[
                    ",s" <> ToString[i + 2] <> " T[j" <> ToString[i +2] <> "," <> 
                    ToString[MomentaGravi[[i]], InputForm] <> "," <> ToString[Indices[[i]]] <> "]",
                    {i, Numbergravis}
                     ]];
                     scalarParts = "s" <> ToString[1] <> " S[j" <> ToString[1] <> "," <> ToString[MomentaScalar[[1]], InputForm] <> "]," <>
                        "s" <> ToString[2] <> " S[j" <> ToString[2] <> "," <> ToString[MomentaScalar[[2]],InputForm] <> "]";
			         FirstPart = "[" <> scalarParts <> gravitonParts <> "]";
					 finalPart0 = "G[1][s1 S[j1], s2 S[j2]" ;
					 finalPart1 = StringJoin[Table[",s" <> ToString[i + 2] <> "T[j" <> ToString[i + 2] <> "]", {i, Numbergravis}]];

					 FinalPart = finalPart0 <> finalPart1 <> "].";
					 FullSentence = "AnalyticalCoupling" <> FirstPart <> "==" <>FinalPart;];
					 <|"Full Sentence" -> FullSentence|>,
		scalars == 0, 
			Module[{},
					gravitonParts = StringRiffle[Table[
                    "s" <> ToString[i] <> " T[j" <> ToString[i] <> "," <> 
                    ToString[MomentaGravi[[i]], InputForm] <> "," <> ToString[Indices[[i]]] <> "]",
                    {i, Numbergravis}
                     ], ","];
			         FirstPart = "["  <> gravitonParts <> "]";
					 finalPart0 = "G[1][" ;
					 finalPart1 = StringRiffle[Table["s" <> ToString[i ] <> "T[j" <> ToString[i] <> "]", {i, Numbergravis}], ","];

					 FinalPart = finalPart0 <> finalPart1 <> "].";
					 FullSentence = "AnalyticalCoupling" <> FirstPart <> "==" <>FinalPart;];
					 <|"Full Sentence" -> FullSentence|>
						]

]
FullSentence = SentenceGenericCouplings[Numbergravis, Scalars];


outputDir = NotebookDirectory[];
file = FileNameJoin[{outputDir, "Generic_Couplings_QG.txt"}];
strm = OpenWrite[file];
data = Table[ToString[LorStr[[i]], InputForm], 
             {i, Length[LorStr]}];
WriteLine[strm, FullSentence[[1]]];
WriteLine[strm, data];
Close[strm];
(*This is what should be copy pasted to the .gen document*)


(*This will generate what will be copy pasted to the .mod document*)
file2 = FileNameJoin[{outputDir, "Coupling_Matrices.txt"}];
strm2 = OpenWrite[file2];
data = Table[0, {i, 2}];
helper = Join[{1}, Table[0, {i, 2-1}]];
data1 = ToString[Table[ data + helper*i, {i, CoupConst}], InputForm];
data2 = ToString[#, InputForm] & /@ CoupConst;
SentenceMatrices[ngravis_, scalars_, j_] := Module[{FirstSen},
	Which[ Scalars > 0,
		FirstSen = StringJoin["C[S[" , ToString[j] , "], S[" ,
		ToString[j], "],", StringRiffle[Table["T[1]", {i, Numbergravis}], ","], "]"],
		Scalars == 0, 
		FirstSen = StringJoin["C[", StringRiffle[Table["T[1]", {i, Numbergravis}], ","], "]"]
		]

]
Do[
  FirstSentence = SentenceMatrices[Numbergravis, Scalars, j];
  FullSentenceMatrix = FirstSentence <> "==" <> data1;
  WriteLine[strm2, FullSentenceMatrix];
,
  {j, 1, DifferentScalars}   (* loop j = 1 to n *)
];

Close[strm2];

(*This is what should be copy pasted to the .mod document*)
