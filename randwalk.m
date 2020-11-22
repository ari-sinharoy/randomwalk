(* ::Package:: *)

(* ::Text:: *)
(*Author: Aritro Sinha Roy*)
(*Date: 22 November, 2020*)


(* ::Text:: *)
(*mathematica code for infinite 3d random walk and 3d random walk within a sphere of given radius. the DeleteDuplicates command remove repeats (if any), also implies that the total number of points might be smaller than the number of steps provided by the user. *)
(**)
(*note: it is possible to randomize the stepsize by feeding in a range of stepsize and picking the stepsize randomly at each iterations (not implemented here). *)


(* ::Subsection:: *)
(*infinite 3d random walk*)


randWalk3D[steps_,stepsize_]:=
Module[{space,\[Theta],\[Phi]},
space = {{0,0,0}};
For[i=1,i<steps,i++,
\[Theta] = RandomReal[1.*\[Pi]];
\[Phi] = RandomReal[2.*\[Pi]];
AppendTo[space,space[[-1]]+{stepsize*Cos[\[Phi]]*Sin[\[Theta]],stepsize*Sin[\[Phi]]*Sin[\[Theta]],stepsize*Cos[\[Theta]]}]];
Return[DeleteDuplicates[space]]
]


(* ::Subsection:: *)
(*random walk within a sphere of given radius*)


randWalkSphere[radius_,steps_,stepsize_]:=
Module[{space,\[Theta],\[Phi],range,jump},
space = {{0,0,0}};
For[i=1,i<steps,i++,
\[Theta] = RandomReal[1.*\[Pi]];
\[Phi] = RandomReal[2.*\[Pi]];
range = Round[radius/stepsize];
jump = RandomChoice[Range[range]];
AppendTo[space,space[[1]]+{jump*stepsize*Cos[\[Phi]]*Sin[\[Theta]],jump*stepsize*Sin[\[Phi]]*Sin[\[Theta]],jump*stepsize*Cos[\[Theta]]}]];
Return[DeleteDuplicates[space]]
]


Print["Loaded randWalk3D[steps, stepsize] and randWalkSphere[radius, steps, stepsize]"]
