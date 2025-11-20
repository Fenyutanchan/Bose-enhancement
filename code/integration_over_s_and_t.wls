#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory @ DirectoryName @ If[$FrontEnd === Null, $InputFileName, NotebookFileName[]];


integrand = \[Pi] / (2 MPl^4 s^2) (8 A^4 + 3 B^4 + 24 A^2 B^2 + 8 s A (2 A^2 + 3 B^2) + 4 s^2 (2 A^2 + B^2));
integrand == Integrate[(2 t^2 (s + t)^2) / (MPl^4 s^2) /. t -> A + B Cos[\[CurlyPhi]], {\[CurlyPhi], 0, 2 \[Pi]}] // FullSimplify


integral = Integrate[integrand / q //. {A -> -2 p1 k1 (1 - cosqp1 cosqk1), B -> 2 p1 k1 Sqrt[(1 - cosqp1^2)(1 - cosqk1^2)], cosqk1 -> (2 k1 Eq - s)/(2 k1 q), cosqp1 -> (2 p1 Eq - s)/(2 p1 q),q -> Sqrt[Eq^2 - s]}, {s, 0, smax},
    Assumptions -> Eq > p1 > 0 && Eq > k1 > 0 && 0 < smax < 4 k1 (Eq - k1) && 0 < smax < 4 p1 (Eq - p1)
] // FullSimplify;
Export["integral_over_s_and_t.m", %];
integral //. {
    p1 -> Eq / 2,
    p2 -> Eq / 2,
    smax -> 4 k1 (Eq - k1),
    Eq -> m\[Phi] aini
} // FullSimplify[#, Assumptions -> k1 < m\[Phi] aini]&
