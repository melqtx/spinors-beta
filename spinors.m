CheckMetric[metric_,tbundle_]:=
Which[
!MetricQ[metric],Throw@Message[DefSpinStructure::unknown,"metric",metric],xAct`xTensor`Private`FrozenMetricQ[metric],Throw@Message[DefSpinStructure::error,"A frozen metric cannot be used."],
TangentBundleOfManifold@BaseOfVBundle@tbundle=!=tbundle,
Throw@Message[DefSpinStructure::error,"The metric must be defined on a tangent bundle."],
DimOfVBundle[tbundle]=!=4,
Throw@Message[DefSpinStructure::notyet,"non 4d manifolds"],
SignDetOfMetric[metric]=!=-1,
Throw@Message[DefSpinStructure::invalid,metric,"Lorentzian metric"],
SignatureOfMetric[metric]=!={1,3,0},
Throw@Message[DefSpinStructure::invalid,metric,"metric of signature +,-,-,-."]
];


SetAttributes[{isetdelayed,itagset,itagsetdelayed},HoldAll];
isetdelayed=SetDelayed;
itagset=TagSet;
itagsetdelayed=TagSetDelayed;


$SolderingForms={};


Sigma[sigma_Symbol]:=GiveSymbol[Sigma,sigma];
GiveOutputString[Sigma,sigma_Symbol]:=StringJoin["\[CapitalSigma]",PrintAs[sigma]];
SetNumberOfArguments[Sigma,1];
Protect[Sigma];
