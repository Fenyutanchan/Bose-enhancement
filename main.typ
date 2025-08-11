// Copyright (c) 2025 Quan-feng WU <wuquanfeng@ihep.ac.cn>
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

#import "FytcNote.typ": *

#show: doc => FytcNote(
  title: "Quick Note",
  abstract: text(red)[
    TBA.
  ],
  authors: (
    (
      name: "Quan-feng WU",
      email: "wuquanfeng@ihep.ac.cn",
      affiliation: [Institute of High Energy Physics,\
        Chinese Academy of Sciences,\
        Beijing 100049, CHINA],
    ),
  ),
  doc
)

#let inflaton = math.phi.alt

= Preliminaries

Boltzmann transport equation (BTE) for inflatons reads
$
  (partial f_inflaton) / (partial t) - H "p" (partial f_inflaton) / (partial "p") = Gamma_inflaton [f_inflaton] (t, "p").
$ <eq:BTE-inflaton>
and for reheatons reads
$
  (partial f_phi) / (partial t) - H "k" (partial f_phi) / (partial "k") = Gamma_phi [f_phi] (t, "k").
$ <eq:BTE-reheaton>
Consider the process of the inter-conversion between inflaton and two reheatons as
$
  inflaton(p) <-> phi(k_1) + phi(k_2),
$ <eq:inflaton-two-reheatons>
where $p = k_1 + k_2$ is required for four-momentum conservation.
The corresponding amplitude is given by
$
  cal(M)_(inflaton <-> phi phi) = "i" lambda,
$
with $lambda$ is the coupling constant.


The initial number density of inflaton is given by
$
  n_inflaton (t_I) = (3 M_"Pl"^2 H_I^2) / m_inflaton,
$
where $t_I$ is the initial time of reheating, $H_I$ is the corresponding Hubble parameter, $M_"Pl" := lr(1 mid(\/) sqrt(8 pi G_"N"))$ is the Planck mass, and $m_inflaton$ is the mass of inflaton.

We also take the ansatz as
$
  f_inflaton (t, "p") = (2 pi^2 n_inflaton (t)) / ("p"^2 epsilon) tilde(Theta)(0 <= "p" <= epsilon),
$ <eq:ansatz-f_inflaton>
where $epsilon -> 0^+$ is an infinitesimal variable.

We also present the decay width of the channel of $inflaton -> phi phi$ here as
$
  Gamma_inflaton = & 1 / (2 m_inflaton) integral tilde("d" k_1) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) 1/ 2 abs(cal(M)_(inflaton -> phi phi))^2 \
  = & lambda^2 / (4 m_inflaton) integral tilde("d" k_1) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) \
  = & lambda^2 / (4 m_inflaton) integral ("d"^3 arrow(k)_1) / ((2 pi)^3 2 E_1) lr(1 / (2 E_2) (2 pi) delta(m_inflaton - E_1 - E_2)|)_(E_2 = E_1) \
  = & lambda^2 / (4 m_inflaton) integral_0^oo ("k"^2 "d" "k") / (2 pi^2) (2 pi) / (4 E^2) delta(m_inflaton - 2 E) \
  = & lambda^2 / (4 m_inflaton) integral_(m_phi)^oo (E "d" E) / (4 pi E^2)  sqrt(E^2 - m_phi^2) delta(m_inflaton - 2 E) \
  = & lambda^2 / (4 m_inflaton) integral_(m_phi)^oo ("d" E) / (4 pi E) sqrt(E^2 - m_phi^2) / 2 delta(m_inflaton / 2 - E) \
  = & lambda^2 / (32 pi m_inflaton^2) sqrt(m_inflaton^2 - 4 m_phi^2)
  attach(=, bl: #text(purple, $(m_phi -> 0)$)) lambda^2 / (32 pi m_inflaton).
$

= Without Back-reaction

== For inflaton

Without the back-reaction, the collision term for the BTE of Eq.~@eq:BTE-inflaton for the process in Eq.~@eq:inflaton-two-reheatons is given by
$
  #text(blue)[$Gamma_inflaton [f_inflaton]$] (t, "p") = & -1 / (2 E_inflaton) integral tilde("d" k_1) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) abs(cal(M)_(inflaton <-> phi phi))^2 / 2 \
  & times f_inflaton (t, "p") [1 + f_phi (t, "k"_1)] [1 + f_phi (t, "k"_2)] \
  equiv & #text(blue)[$Gamma_inflaton^((inflaton))$] [f_inflaton] (t, "p") + 2 #text(blue)[$Gamma_inflaton^((phi))$] [f_inflaton] (t, "p") + #text(blue)[$Gamma_inflaton^((phi phi))$] [f_inflaton] (t, "p"),
$
where we have defined three terms as
$
  #text(blue)[$Gamma_inflaton^((inflaton))$] [f_inflaton] (t, "p") := & -1 / (2 E_inflaton) lambda^2 / 2 integral tilde("d" k_1) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) f_inflaton (t, "p"), \
  #text(blue)[$Gamma_inflaton^((phi))$] [f_inflaton] (t, "p") := & -1 / (2 E_inflaton) lambda^2 / 2 integral tilde("d" k_1) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) f_inflaton (t, "p") f_phi (t, "k"_1), \
  #text(blue)[$Gamma_inflaton^((phi phi))$] [f_inflaton] (t, "p") := & -1 / (2 E_inflaton) lambda^2 / 2 integral tilde("d" k_1) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) f_inflaton (t, "p") f_phi (t, "k"_1) f_phi (t, "k"_2).
$ <eq:Gamma-inflaton>
Then we can calculate these terms one by one.

For #text(blue)[$Gamma_inflaton^((inflaton))$], we have
$
  #text(blue)[$Gamma_inflaton^((inflaton))$] [f_inflaton] (t, "p") = & -1 / (2 E_inflaton) lambda^2 / 2 integral tilde("d" k_1) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) f_inflaton (t, "p") \
  = & -(lambda^2 f_inflaton (t, "p")) / (4 E_inflaton) integral ("d"^3 arrow(k)_1) / ((2 pi)^3 2 E_1) ("d"^3 arrow(k)_2) / ((2 pi)^3 2 E_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) \
  = & -(lambda^2 f_inflaton (t, "p")) / (4 E_inflaton) integral ("d"^3 arrow(k)_1) / ((2 pi)^3 2 E_1) lr((2 pi) / (2 E_2) delta(E_inflaton - E_1 - E_2)|)_(E_2 = sqrt("p"^2 + E_1^2 - 2 arrow(p)_inflaton dot arrow(k)_1)) \
  = & -(lambda^2 f_inflaton (t, "p")) / (4 E_inflaton) 1 / (2 pi) integral_(-1)^(+1) "d" cos theta integral_0^(+oo) ("k"_1^2 "d" "k"_1) / (2 E_1) lr(delta(E_inflaton - E_1 - E_2) / (2 E_2)|)_(E_2 = sqrt("p"^2 + E_1^2 - 2 "p" "k"_1 cos theta)) \
  = & -(lambda^2 f_inflaton (t, "p")) / (8 pi E_inflaton) integral_0^(+oo) ("k"_1^2 "d" "k"_1) / (2 E_1) 1 / (2 "p" "k"_1) \
  & #h(2em) times tilde(Theta)[abs("p" / 2 - E_inflaton / (2 m_inflaton) sqrt(m_inflaton^2 - 4 m_phi^2)) <= "k"_1 <= "p" / 2 + E_inflaton / (2 m_inflaton) sqrt(m_inflaton^2 - 4 m_phi^2)] \
  = & -(lambda^2 f_inflaton (t, "p")) / (32 pi E_inflaton "p") integral_abs("p" / 2 - E_inflaton / (2 m_inflaton) sqrt(m_inflaton^2 - 4 m_phi^2))^("p" / 2 + E_inflaton / (2 m_inflaton) sqrt(m_inflaton^2 - 4 m_phi^2)) ("k"_1 "d" "k"_1) / (E_1) \
  = & -(lambda^2 f_inflaton (t, "p")) / (32 pi E_inflaton "p") [E_1^((max)) - E_1^((min))],
$
where
$
  E_1^((min)) = & sqrt(abs("p" / 2 - E_inflaton / (2 m_inflaton) sqrt(m_inflaton^2 - 4 m_phi^2))^2 + m_phi^2), \
  E_1^((max)) = & sqrt(("p" / 2 + E_inflaton / (2 m_inflaton) sqrt(m_inflaton^2 - 4 m_phi^2))^2 + m_phi^2).
$ <eq:E_1-min-max>
If $m_phi = 0$, the above expression reduces to
$
  #text(blue)[$Gamma_inflaton^((inflaton))$] [f_inflaton] (t, "p") = -(lambda^2) / (32 pi E_inflaton) f_inflaton (t, "p").
$ \

For #text(blue)[$Gamma_inflaton^((phi))$] and #text(blue)[$Gamma_inflaton^((phi phi))$], we have
$
  #text(blue)[$Gamma_inflaton^((phi))$] [f_inflaton] (t, "p") & = -(lambda^2) / (32 pi E_inflaton "p") f_inflaton (t, "p") integral_(E_1^((min)))^(E_1^((max))) f_phi (t, sqrt(E_1^2 - m_phi^2)) "d" E_1 \
  & = (Gamma_inflaton^((inflaton)) [f_inflaton] (t, "p")) / "p" integral_(E_1^((min)))^(E_1^((max))) f_phi (t, sqrt(E_1^2 - m_phi^2)) "d" E_1
$
and
$
  #text(blue)[$Gamma_inflaton^((phi phi))$] [f_inflaton] (t, "p") = & -(lambda^2) / (32 pi E_inflaton "p") f_inflaton (t, "p") \
  & #h(2em) times integral_(E_1^((min)))^(E_1^((max))) f_phi (t, sqrt(E_1^2 - m_phi^2)) f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2)) "d" E_1 \
  = & (Gamma_inflaton^((inflaton)) [f_inflaton] (t, "p")) / "p" integral_(E_1^((min)))^(E_1^((max))) f_phi (t, sqrt(E_1^2 - m_phi^2)) f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2)) "d" E_1.
$
If $m_phi = 0$, the above expressions reduce to
$
  #text(blue)[$Gamma_inflaton^((phi))$] [f_inflaton] (t, "p") = (Gamma_inflaton^((inflaton)) [f_inflaton] (t, "p")) / "p" integral_(E_inflaton / 2 - "p" / 2)^(E_inflaton / 2 + "p" / 2) f_phi (t, E_1) "d" E_1
$
and
$
  #text(blue)[$Gamma_inflaton^((phi phi))$] [f_inflaton] (t, "p") = (Gamma_inflaton^((inflaton)) [f_inflaton] (t, "p")) / "p" integral_(E_inflaton / 2 - "p" / 2)^(E_inflaton / 2 + "p" / 2) f_phi (t, E_1) f_phi (t, E_inflaton - E_1) "d" E_1.
$
Therefore,
$
  #text(blue)[$Gamma_inflaton$] [f_inflaton] (t, "p") = & -(lambda^2) / (32 pi E_inflaton) f_inflaton (t, "p") \
  & times (1 + 1 / "p" integral_(E_1^((min)))^(E_1^((max))) "d" E_1 f_phi (t, sqrt(E_1^2 - m_phi^2)) [2 + f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2))]) \
  attach(=, bl: #text(purple, $(m_phi -> 0)$)) & -(lambda^2) / (32 pi E_inflaton) f_inflaton (t, "p") (1 + integral_(E_inflaton / 2 - "p" / 2)^(E_inflaton / 2 + "p" / 2) ("d" E_1) / "p" f_phi (t, E_1) [2 + f_phi (t, E_inflaton - E_1)]).
$

// Now consider
// $
//   f_inflaton (t, "p") & = (2 pi^2) / ("p"^2 epsilon) tilde(Theta)(0 <= "p" <= epsilon)  \
//   f_inflaton (t, "p") & = (2 pi)^3 n_inflaton (t) delta^((3))(arrow(p)) = (2 pi^2 n_inflaton (t)) / ("p"^2) delta("p").
// $ <eq:f_inflaton>
// To avoid the divergence at $"p" = 0$, we introduce a infinitesimal variable $epsilon -> 0^+$ to modify its behavior as
// $
//   f_inflaton (t, "p") = (2 pi^2 n_inflaton (t)) / ("p"^2) delta("p" - epsilon).
// $
// Therefore,
// $
//   #text(blue)[$Gamma_inflaton$] [f_inflaton] (t, "p") = & -(lambda^2) / (32 pi E_inflaton) f_inflaton (t, "p") (1 + integral_(E_inflaton / 2 - "p" / 2)^(E_inflaton / 2 + "p" / 2) ("d" E_1) / "p" f_phi (t, E_1) [2 + f_phi (t, E_inflaton - E_1)]) \
//   = & -(lambda^2) / (32 pi E_inflaton) (2 pi^2 n_inflaton (t)) / ("p"^2) delta("p" - epsilon) \
//   & #h(2em) times (1 + integral_(E_inflaton / 2 - "p" / 2)^(E_inflaton / 2 + "p" / 2) ("d" E_1) / "p" f_phi (t, E_1) [2 + f_phi (t, E_inflaton - E_1)]) \
//   tilde.eq & -(pi lambda^2) / (16 m_inflaton) (n_inflaton (t)) / ("p"^2) delta("p" - epsilon) [1 + f_phi (t, m_inflaton / 2)]^2.
// $

== For reheaton

Then, for reheaton, we have
$
  #text(blue)[$Gamma_phi$] [f_phi] (t, "k"_1) = & 1 / (2 E_1) integral tilde("d" p) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) abs(cal(M)_(inflaton <-> phi phi))^2 \
  & #h(4em) times f_inflaton (t, "p") [1 + f_phi (t, "k"_1)] [1 + f_phi (t, "k"_2)] \
  equiv & Gamma_phi^((inflaton phi)) [f_phi] (t, "k"_1) + Gamma_phi^((inflaton phi phi)) [f_phi] (t, "k"_1),
$
where we have defined three terms as
$
  #text(blue)[$Gamma_phi^((inflaton phi))$] [f_phi] (t, "k"_1) := & lambda^2 / (2 E_1) integral tilde("d" p) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) f_inflaton (t, "p") [1 + f_phi (t, "k"_1)], \
  #text(blue)[$Gamma_phi^((inflaton phi phi))$] [f_phi] (t, "k"_1) := & lambda^2 / (2 E_1) integral tilde("d" p) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) f_inflaton (t, "p") [1 + f_phi (t, "k"_1)] f_phi (t, "k"_2).
$
Then we can calculate these terms one by one.

For #text(blue)[$Gamma_phi^((inflaton phi))$], we have
$
  #text(blue)[$Gamma_phi^((inflaton phi))$] [f_phi] (t, "k"_1) = & lambda^2 / (2 E_1) integral tilde("d" p) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) f_inflaton (t, "p") [1 + f_phi (t, "k"_1)] \
  = & lambda^2 / (2 E_1) [1 + f_phi (t, "k"_1)] integral_(-1)^(+1) "d" cos theta \
  & #h(2em) times integral_0^oo lr(("p"^2 "d" "p") / ((2 pi)^2 2 E_inflaton) (f_inflaton (t, "p")) / (2 E_2) 2 pi delta(E_inflaton - E_1 - E_2)|)_(E_2 = sqrt("p"^2 + E_1^2 - 2 "p" "k"_1 cos theta)) \
  = & lambda^2 / (2 E_1) [1 + f_phi (t, "k"_1)] integral_0^oo ("p"^2 "d" "p") / (4 pi E_inflaton) (f_inflaton (t, "p")) / (2 E_2) (2 E_2) / (2 "p" "k"_1) \
  & #h(2em) times tilde(Theta)(m_inflaton / (2 m_phi^2) abs("k"_1 m_inflaton - E_1 sqrt(m_inflaton^2 - 4 m_phi^2)) <= "p" <= m_inflaton / (2 m_phi^2) ("k"_1 m_inflaton + E_1 sqrt(m_inflaton^2 - 4 m_phi^2))) \
  = & lambda^2 / (16 pi E_1 "k"_1) [1 + f_phi (t, "k"_1)] integral_(E_inflaton^((min)))^(E_inflaton^((max))) f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) "d" E_inflaton
$
with
$
  E_inflaton^((min)) = & sqrt(m_inflaton^2 / (4 m_phi^2) abs("k"_1 m_inflaton - E_1 sqrt(m_inflaton^2 - 4 m_phi^2))^2 + m_inflaton^2), \
  E_inflaton^((max)) = & sqrt(m_inflaton^2 / (4 m_phi^2) ("k"_1 m_inflaton + E_1 sqrt(m_inflaton^2 - 4 m_phi^2))^2 + m_inflaton^2).
$ <eq:E_inflaton-min-max>
If $m_phi -> 0$, the above expression reduces to
$
  #text(blue)[$Gamma_phi^((inflaton phi))$] [f_phi] (t, "k"_1) = lambda^2 / (16 pi E_1^2) [1 + f_phi (t, "k"_1)] integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) "d" E_inflaton.
$ \

For #text(blue)[$Gamma_phi^((inflaton phi phi))$], we have
$
  #text(blue)[$Gamma_phi^((inflaton phi phi))$] [f_phi] (t, "k"_1) = & lambda^2 / (2 E_1) integral tilde("d" p) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) f_inflaton (t, "p") [1 + f_phi (t, "k"_1)] f_phi (t, "k"_2) \
  = & lambda^2 / (2 E_1) [1 + f_phi (t, "k"_1)] integral_(-1)^(+1) "d" cos theta integral_0^oo ("p"^2 "d" "p") / ((2 pi)^2 2 E_inflaton) f_inflaton (t, "p") \
  & #h(2em) times lr((f_phi (t, sqrt(E_2^2 - m_phi^2))) / (2 E_2) 2 pi delta(E_inflaton - E_1 - E_2)|)_(E_2 = sqrt("p"^2 + E_1^2 - 2 "p" "k"_1 cos theta)) \
  = & lambda^2 / (2 E_1) [1 + f_phi (t, "k"_1)] integral_0^oo ("p"^2 "d" "p") / (4 pi E_inflaton) f_inflaton (t, "p") (f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2))) / (2 E_2) (2 E_2) / (2 "p" "k"_1) \
  & #h(2em) times tilde(Theta)(m_inflaton / (2 m_phi^2) abs("k"_1 m_inflaton - E_1 sqrt(m_inflaton^2 - 4 m_phi^2)) <= "p" <= m_inflaton / (2 m_phi^2) ("k"_1 m_inflaton + E_1 sqrt(m_inflaton^2 - 4 m_phi^2))) \
  = & lambda^2 / (16 pi E_1 "k"_1) [1 + f_phi (t, "k"_1)] integral_(E_inflaton^((min)))^(E_inflaton^((max))) "d" E_inflaton f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2)).
$
In the limit of $m_phi -> 0$, it reduces to
$
  #text(blue)[$Gamma_phi^((phi))$] [f_phi] (t, "k"_1) = & (lambda^2) / (16 pi E_1^2) [1 + f_phi (t, E_1)] \
  & #h(2em) times integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo "d" E_inflaton f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) f_phi (t, E_inflaton - E_1). \
$ \

Finally, we have the total collision term for the BTE of Eq.~@eq:BTE-reheaton as
$
  #text(blue)[$Gamma_phi$] [f_phi] (t, "k"_1) = & lambda^2 / (16 pi E_1 "k"_1) [1 + f_phi (t, "k"_1)] \
  & #h(2em) times integral_(E_inflaton^((min)))^(E_inflaton^((max))) f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) [1 + f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2))] "d" E_inflaton \
  attach(=, bl: #text(purple, $(m_phi -> 0)$)) & lambda^2 / (16 pi E_1^2) [1 + f_phi (t, E_1)] integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) [1 + f_phi (t, E_inflaton - E_1)] "d" E_inflaton.
$ \

// Also considering
// $
//   f_inflaton (t, "p") = (2 pi^2 n_inflaton (t)) / ("p"^2) delta("p" - epsilon),
// $
// we have
// $
//   #text(blue)[$Gamma_phi$] [f_phi] (t, "k"_1) = & lambda^2 / (16 pi E_1^2) [1 + f_phi (t, E_1)] integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) [1 + f_phi (t, E_inflaton - E_1)] "d" E_inflaton \
//   = & lambda^2 / (16 pi E_1^2) [1 + f_phi (t, E_1)] \
//   & #h(2em) times integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo (2 pi^2 n_inflaton (t)) / (E_inflaton^2 - m_inflaton^2) delta(sqrt(E_inflaton^2 - m_inflaton^2) - epsilon) [1 + f_phi (t, E_inflaton - E_1)] "d" E_inflaton \
//   = & (pi lambda^2) / (8 E_1^2) n_inflaton (t) [1 + f_phi (t, E_1)] tilde(Theta)(E_1^((min)) <= E_1 <= E_1^((max))) \
//   & #h(2em) times lr(1 / (E_inflaton^2 - m_inflaton^2) (2 sqrt(E_inflaton^2 - m_inflaton^2)) / (2 E_inflaton) [1 + f_phi (t, E_inflaton - E_1)] |)_(E_inflaton = sqrt(m_inflaton^2 + epsilon^2)) \
//   attach(=, bl: #text(purple, $(epsilon -> 0)$)) & (pi lambda^2) / (2 m_phi^3) n_inflaton (t) [1 + f_phi (t, m_inflaton / 2)]^2 delta(E_1 - m_inflaton / 2),
// $
// where
// $
//   E_1^((min)) = & 1 / 2 (sqrt(m_inflaton^2 + epsilon^2) - epsilon), \
//   E_1^((max)) = & 1 / 2 (sqrt(m_inflaton^2 + epsilon^2) + epsilon).
// $ <eq:E_1-min-max-new>

= With Back-reaction

== For Inflaton

Here we only consider the back-reaction term, which reads
$
  #text(red)[$Gamma_inflaton$] [f_inflaton] (t, "p") = & 1 / (2 E_inflaton) integral tilde("d" k_1) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) abs(cal(M)_(inflaton <-> phi phi))^2 / 2  [1 + f_inflaton (t, "p")] f_phi (t, "k"_1) f_phi (t, "k"_2) \
  equiv & #text(red)[$Gamma_inflaton^((phi phi))$] [f_inflaton] (t, "p") - #text(blue)[$Gamma_inflaton^((phi phi))$] [f_inflaton] (t, "p"),
$
where $Gamma_inflaton^((phi phi)) [f_inflaton]$ is calculated in Eq.~@eq:Gamma-inflaton, and
$
  #text(red)[$Gamma_inflaton^((phi phi))$] [f_inflaton] (t, "p") := & 1 / (2 E_inflaton) lambda^2 / 2 integral tilde("d" k_1) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) f_phi (t, "k"_1) f_phi (t, "k"_2) \
  = & (lambda^2) / (32 pi E_inflaton "p") integral_(E_1^((min)))^(E_1^((max))) f_phi (t, sqrt(E_1^2 - m_phi^2)) f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2)) "d" E_1,
$
where $E_1^((min))$ and $E_2^((max))$ are also shown in Eq.~@eq:E_1-min-max.

Along with the collision term without back-reaction, we have the total collision term for the BTE of Eq.~@eq:BTE-inflaton as
$
  & Gamma_inflaton [f_inflaton] (t, "p") \
  := & #text(blue)[$Gamma_inflaton$] [f_inflaton] (t, "p") + #text(red)[$Gamma_inflaton$] [f_inflaton] (t, "p") \
  = & #text(blue)[$Gamma_inflaton^((inflaton))$] [f_inflaton] (t, "p") + 2 #text(blue)[$Gamma_inflaton^((phi))$] [f_inflaton] (t, "p") + #text(red)[$Gamma_inflaton^((phi phi))$] [f_inflaton] (t, "p") \
  = & -(lambda^2) / (32 pi E_inflaton) [f_inflaton (t, "p") (1 + 2 / "p" integral_(E_phi^((min)))^(E_phi^((max))) f_phi (t, sqrt(E_1^2 - m_phi^2)) "d" E_1) \
  & #h(6em) - 1 / "p" integral_(E_phi^((min)))^(E_phi^((max))) f_phi (t, sqrt(E_1^2 - m_phi^2)) f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2)) "d" E_1] \
  attach(=, bl: #text(purple, $(m_phi -> 0)$)) & (lambda^2) / (32 pi E_inflaton) [1 / "p" integral_(E_inflaton / 2 - "p" / 2)^(E_inflaton / 2 + "p" / 2) f_phi (t, E_1) f_phi (t, E_inflaton - E_1) "d" E_1 - f_inflaton (t, "p") (1 + 2 / "p" integral_(E_inflaton / 2 - "p" / 2)^(E_inflaton / 2 + "p" / 2) f_phi (t, E_1) "d" E_1)].
$ <eq:full-collision-term-for-inflaton>

== For Reheaton

In this case, the back-reaction collision term for the BTE of Eq.~@eq:BTE-reheaton reads
$
  #text(red)[$Gamma_phi$] [f_phi] (t, "k"_1) = & -1 / (2 E_1) integral tilde("d" p) tilde("d" k_2) (2 pi)^4 delta^((4))(p - k_1 - k_2) abs(cal(M)_(inflaton <-> phi phi))^2 [1 + f_inflaton (t, "p")] f_phi (t, "k"_1) f_phi (t, "k"_2) \
  = & -lambda^2 / (16 pi E_1 "k"_1) f_phi (t, "k"_1) integral_(E_inflaton^((min)))^(E_inflaton^((max))) [1 + f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2))] f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2)) "d" E_inflaton,
$
where $E_inflaton^((min))$ and $E_inflaton^((max))$ are also shown in Eq.~@eq:E_inflaton-min-max.
Therefore,
$
  Gamma_phi [f_phi] (t, "k"_1) = & #text(blue)[$Gamma_phi$] [f_phi] (t, "k"_1) + #text(red)[$Gamma_phi$] [f_phi] (t, "k"_1) \
  = & lambda^2 / (16 pi E_1 "k"_1) ( [1 + f_phi (t, "k"_1)] \
    & #h(2em) times integral_(E_inflaton^((min)))^(E_inflaton^((max))) f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) [1 + f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2))] "d" E_inflaton \
    & #h(1em) - f_phi (t, "k"_1) integral_(E_inflaton^((min)))^(E_inflaton^((max))) [1 + f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2))] f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2)) "d" E_inflaton ) \
  = & lambda^2 / (16 pi E_1 "k"_1) (integral_(E_inflaton^((min)))^(E_inflaton^((max))) f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) [1 + f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2))] "d" E_inflaton \
    & #h(2em) + f_phi (t, "k"_1) integral_(E_inflaton^((min)))^(E_inflaton^((max))) [f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) - f_phi (t, sqrt((E_inflaton - E_1)^2 - m_phi^2))] "d" E_1 ) \
  attach(=, bl: #text(purple)[$(m_phi -> 0)$]) & lambda^2 / (16 pi E_1^2) (integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) [1 + f_phi (t, E_1) + f_phi (t, E_inflaton - E_1)] "d" E_inflaton \
    & #h(6em) - f_phi (t, E_1) integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo f_phi (t, E_inflaton - E_1) "d" E_inflaton).
$ <eq:full-collision-term-for-reheaton>

// Also considering that
// $
//   f_inflaton (t, "p") = (2 pi^2 n_inflaton (t)) / ("p"^2) delta("p" - epsilon),
// $
// we then have
// $
//   Gamma_phi [f_phi] (t, "k"_1) = & lambda^2 / (16 pi E_1^2) (integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) [1 + f_phi (t, E_inflaton - E_1)] "d" E_inflaton \
//     & #h(2em) + f_phi (t, E_1) integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo [f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) - f_phi (t, E_inflaton - E_1)] "d" E_inflaton) \
//   = & lambda^2 / (16 pi E_1^2) [integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo (2 pi^2 n_inflaton (t)) / (E_inflaton^2 - m_inflaton^2) delta(sqrt(E_inflaton^2 - m_inflaton^2) - epsilon) [1 + f_phi (t, E_inflaton - E_1)] "d" E_inflaton \
//     & #h(2em) + f_phi (t, E_1) integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo (2 pi^2 n_inflaton (t)) / (E_inflaton^2 - m_inflaton^2) delta(sqrt(E_inflaton^2 - m_inflaton^2) - epsilon) "d" E_inflaton \
//     & #h(2em) - f_phi (t, E_1) integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo f_phi (t, E_inflaton - E_1) "d" E_inflaton] \
//     = & lambda^2 / (16 pi E_1^2) [(2 pi^2 n_inflaton (t)) / (m_inflaton epsilon) [1 + f_phi (t, E_1) + f_phi (t, sqrt(m_inflaton^2 + epsilon^2) - E_1)] tilde(Theta)(E_1^((min)) <= E_1 <= E_1^((max))) \
//       & #h(2em) - f_phi (t, E_1) integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo f_phi (t, E_inflaton - E_1) "d" E_inflaton],
// $
// where $E_1^((min))$ and $E_1^((max))$ are also given in Eq.~@eq:E_1-min-max-new.
// Notice that with $epsilon -> 0^+$, we have $E_i = "k"_i tilde m_phi \/ 2$ for $i = 1, 2$, which leads to
// $
//   integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo f_phi (t, E_inflaton - E_1) "d" E_inflaton tilde.eq 0.
// $ <eq:amazing-assumption>
// Therefore,
// $
//   Gamma_phi [f_phi] (t, "k"_1) = (pi lambda^2) / (2 m_inflaton^3) n_inflaton (t) [1 + 2 f_phi (t, m_inflaton / 2)] delta(E_1 - m_inflaton / 2).
// $ <eq:full-collision-term-for-reheaton>

== Solution

=== For Reheaton

Taking Eq.~@eq:full-collision-term-for-reheaton into Eq.~@eq:BTE-reheaton, we have
$
  (partial f_phi) / (partial t) - H "k"_1 (partial f_phi) / (partial "k"_1) = & lambda^2 / (16 pi E_1^2) (integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo f_inflaton (t, sqrt(E_inflaton^2 - m_inflaton^2)) [1 + f_phi (t, E_1) + f_phi (t, E_inflaton - E_1)] "d" E_inflaton \
    & #h(6em) - f_phi (t, E_1) integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo f_phi (t, E_inflaton - E_1) "d" E_inflaton).
$ <eq:BTE-reheaton-full-1>
Here, we first consider the ansatz that last term of the RHS vanishes, and will check its validity later.
Then, applying the ansatz of Eq.~@eq:ansatz-f_inflaton, the RHS becomes
$
  & #text[RHS of Eq.~@eq:BTE-reheaton-full-1] \
  = & lambda^2 / (16 pi E_1^2) integral_(abs(m_inflaton^2 - 4 E_1^2) \/ (4 E_1))^epsilon ("p" "d" "p") / E_inflaton  (2 pi^2 n_inflaton (t)) / ("p"^2 epsilon) tilde(Theta)(abs(m_inflaton^2 - 4 E_1^2) / (4 E_1) <= epsilon) \
    & #h(4em) times [1 + f_phi (t, E_1) + f_phi (t, sqrt(m_inflaton^2 + "p"^2) - E_1)] \
  attach(=, bl: #text(purple, $(epsilon -> 0)$)) & lambda^2 / (16 pi E_1^2 m_inflaton) integral_(abs(m_inflaton^2 - 4 E_1^2) \/ (4 E_1))^epsilon ("d" "p") / "p" (2 pi^2 n_inflaton (t)) / epsilon tilde(Theta)(abs(m_inflaton^2 - 4 E_1^2) / (4 E_1) <= epsilon) \
    & #h(4em) times [1 + f_phi (t, E_1) + f_phi (t, m_inflaton - E_1) + cal(O)(epsilon)] \
  = & (pi lambda^2) / (8 E_1^2 m_inflaton) (n_inflaton (t)) / epsilon ln abs((4 E_1 epsilon) / (m_inflaton^2 - 4 E_1^2)) tilde(Theta)(abs(2 E_1 - sqrt(m_inflaton^2 + epsilon^2)) <= epsilon) \
    & #h(4em) times [1 + f_phi (t, E_1) + f_phi (t, m_inflaton - E_1) + cal(O)(epsilon)] \
  attach(=, bl: #text(purple, $(epsilon -> 0)$)) & (pi lambda^2) / (2 m_inflaton^3) n_inflaton (t) delta(E_1 - m_inflaton / 2) [1 + f_phi (t, m_inflaton / 2) + cal(O)(epsilon)],
$ <eq:RHS-of-BTE-reheaton-full-1-without-last-term>
where the last equality is based on Eq.~@eq:tilde-f_reheaton-out-of-range.
With the change of variables as $t -> a(t)$ and $tilde("k")_1 := "k"_1 a(t)$, _i.e._, $f_phi (t, "k"_1) = tilde(f)_phi (a, tilde("k")_1)$, the above equation can be rewritten as
$
  (partial f_phi) / (partial t) - H "k"_1 (partial f_phi) / (partial "k"_1) = & (partial a) / (partial t) (partial tilde(f)_phi) / (partial a) + (partial tilde("k")) / (partial t) (partial tilde(f)_phi) / (partial tilde("k")_1) - H "k"_1 ((partial a) / (partial "k"_1) (partial tilde(f)_phi) / (partial a) + (partial tilde("k")_1) / (partial "k"_1) (partial tilde(f)_phi) / (partial tilde("k")_1)) \
  = & H a (partial tilde(f)_phi) / (partial a) \
  = & (pi lambda^2) / (2 m_inflaton^3) n_inflaton (t) delta(E_1 - m_inflaton / 2) [1 + f_phi (t, m_inflaton / 2) + cal(O)(epsilon)] \
  = & (pi lambda^2) / (2 m_inflaton^3) n_inflaton (t) delta("k"_1 - m_inflaton / 2) [1 + tilde(f)_phi (a, m_inflaton / 2 a)] \
  = & (pi lambda^2) / (2 m_inflaton^3) n_inflaton (t) a delta(tilde("k")_1 - m_inflaton / 2 a) [1 + tilde(f)_phi (a, m_inflaton / 2 a)].
$
Or equivalently,
$
  (partial tilde(f)_phi) / (partial a) = (pi lambda^2) / (2 m_inflaton^3 H) n_inflaton (a) delta(tilde("k")_1 - m_inflaton / 2 a) [1 + tilde(f)_phi (a, m_inflaton / 2 a)],
$ <eq:tilde-f_reheaton-evolution>
whose solution is given by
$
  tilde(f)_phi (a, tilde("k")_1) = & exp[integral_(a_I)^a (pi lambda^2 n(a')) / (2 m_inflaton^3 H(a')) delta(tilde("k")_1 - m_inflaton / 2 a') "d" a'] - 1 \
  = & exp[(pi lambda^2 n_inflaton (a')) / (m_inflaton^4 H(a')) tilde(Theta)((m_inflaton a_I) / 2 <= tilde("k")_1 <= (m_inflaton a) / 2)]_(a' = 2 tilde("k")_1 \/ m_inflaton) - 1.
$ <eq:full-solution-to-f_reheaton-1>
Returning to $f_phi (t, "k"_1)$, we have
$
  f_phi (t, "k"_1) = exp[(pi lambda^2 n_inflaton (t')) / (m_inflaton^4 H(t')) tilde(Theta)(m_inflaton / 2 a(t_I) / a(t) <= "k"_1 <= m_inflaton / 2)]_(a(t') = 2 "k"_1 a(t) \/ m_inflaton) - 1.
$ <eq:full-solution-to-f_reheaton-2> \

Now let us consider the validity of the assumption that the last term of the RHS of Eq.~@eq:BTE-reheaton-full-1 vanishes.
According to Eq.~@eq:RHS-of-BTE-reheaton-full-1-without-last-term, we have
$
  E_1^((max)) = E_2^((max)) = 1 / 2 (sqrt(m_inflaton^2 + epsilon^2) + epsilon)
$
if we do not take the limit as $epsilon -> 0$.
So the last term of the RHS of Eq.~@eq:BTE-reheaton-full-1 reads
$
  & integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^oo f_phi (t, E_1) f_phi (t, E_inflaton - E_1) "d" E_inflaton \
  = & integral_((m_inflaton^2 + 4 E_1^2) \/ (4 E_1))^(sqrt(m_inflaton^2 + epsilon^2) + epsilon) f_phi (t, E_1) f_phi (t, E_inflaton - E_1) "d" E_inflaton \
  <= & integral_(m_inflaton)^(sqrt(m_inflaton^2 + epsilon^2) + epsilon) f_phi (t, E_1) f_phi (t, E_inflaton - E_1) "d" E_inflaton \
  = & cal(O)(epsilon)
$
where we have applied the fact that $f_phi (t, "k") >= 0$ and
$
  (m_inflaton^2 + 4 E_1^2) / (4 E_1) = m_inflaton^2 / (4 E_1) + E_1 >= 2 sqrt(m_inflaton^2 / (4 E_1) times E_1) = m_inflaton.
$
Therefore, it is safe to assume that the last term of the RHS of Eq.~@eq:BTE-reheaton-full-1 vanishes (up to the order of $epsilon^0$).

In Eq.~@eq:RHS-of-BTE-reheaton-full-1-without-last-term, we finally take the limit as $epsilon -> 0$ and get the Dirac-$delta$ function.
However, if we do not take the limit, $f(t, "k")$ for $m_inflaton \/ 2 < "k" < (sqrt(m_inflaton^2 + epsilon^2) + epsilon) \/ 2$ are non-zero.
To evaluate it, we take $0 <= epsilon.alt <= (sqrt(m_inflaton^2 + epsilon^2) + epsilon) \/ 2 - m_inflaton \/ 2$, and then we have
$
  & (partial tilde(f)) / (partial a') (a', (m_inflaton / 2 + epsilon.alt) a) \
  = & (pi lambda^2) / (8 E_1^2 m_inflaton) (n_inflaton (a')) / epsilon ln abs((4 E_1 epsilon) / (m_inflaton^2 - 4 E_1^2)) tilde(Theta)(abs(2 E_1 - sqrt(m_inflaton^2 + epsilon^2)) <= epsilon) \
  & #h(4em) times[1 + f_phi (t', E_1) + f_phi (t', m_inflaton - E_1) + cal(O)(epsilon)] \
  lt.tilde & (pi lambda^2) / (8 E_1^2 m_inflaton) (n_inflaton (a')) / epsilon ln abs((4 E_1 epsilon) / (m_inflaton^2 - 4 E_1^2)) tilde(Theta)(abs(2 E_1 - sqrt(m_inflaton^2 + epsilon^2)) <= epsilon) cal(O)(epsilon^0) \
$
with
$
  E_1 = (m_inflaton / 2 + epsilon.alt) a / a'.
$
In the last step, we do not care about the details of $f_phi (t', E_1)$ and $f_phi (t', m_inflaton - E_1)$ and combine them with $1 + cal(O)(epsilon)$ into a single term $cal(O)(epsilon^0)$.
For $tilde(Theta)$ condition, we have
$
  & abs((m_inflaton + 2 epsilon.alt) a / a' - sqrt(m_inflaton^2 + epsilon^2)) <= epsilon \
  <=> & (sqrt(m_inflaton^2 + epsilon^2) - epsilon) / (m_inflaton + 2 epsilon.alt) <= a / a' <= (sqrt(m_inflaton^2 + epsilon^2) + epsilon) / (m_inflaton + 2 epsilon.alt).
$
Also notice that
$
  0 <= epsilon.alt <= (sqrt(m_inflaton^2 + epsilon^2) + epsilon) / 2 - m_inflaton / 2 => epsilon.alt lt.tilde epsilon / 2.
$
Finally, the corresponding solution is given by `Mathematica`
$
  tilde(f)_phi (a, (m_inflaton / 2 + epsilon.alt) a) ~ cal(O)(epsilon^2),
$ <eq:tilde-f_reheaton-out-of-range>
or
$
  f_phi (t, m_inflaton / 2 + epsilon) ~ cal(O)(epsilon^2).
$ <eq:f_reheaton-out-of-range>

=== For Inflaton

Taking Eq.~@eq:full-collision-term-for-inflaton into Eq.~@eq:BTE-inflaton, we have
$
  & (partial f_inflaton) / (partial t) - H "p" (partial f_inflaton) / (partial "p") \
  = & (lambda^2) / (32 pi E_inflaton) [1 / "p" integral_(E_inflaton / 2 - "p" / 2)^(E_inflaton / 2 + "p" / 2) f_phi (t, E_1) f_phi (t, E_inflaton - E_1) "d" E_1 - f_inflaton (t, "p") (1 + 2 / "p" integral_(E_inflaton / 2 - "p" / 2)^(E_inflaton / 2 + "p" / 2) f_phi (t, E_1) "d" E_1)].
$
Since we have solved $f_phi (t, "k")$ in Eq.~@eq:full-solution-to-f_reheaton-2, we can analyze the collision term in detail.
According to Eq.~@eq:RHS-of-BTE-reheaton-full-1-without-last-term, we also have
$
  E_inflaton^((min)) & = m_inflaton, \
  E_inflaton^((max)) & = sqrt(m_inflaton^2 + epsilon^2) + epsilon,
$
which leads to
$
  "p"_min & = 0 \
  "p"_max & tilde.eq sqrt(2 m_inflaton epsilon) + cal(O)(epsilon^(3\/2)).
$
Therefore, we can simply take $"p" = epsilon.alt tilde sqrt(epsilon) -> 0^+$, which leads to
$
  & 1 / "p" integral_(E_inflaton / 2 - "p" / 2)^(E_inflaton / 2 + "p" / 2) f_phi (t, E_1) f_phi (t, E_inflaton - E_1) "d" E_1 \
  = & 1 / epsilon.alt integral_(1 / 2 (sqrt(m_inflaton^2 + epsilon.alt^2) - epsilon.alt))^(1 / 2 (sqrt(m_inflaton^2 + epsilon.alt^2) + epsilon.alt)) f_phi (t, E_1) f_phi (t, sqrt(m_inflaton^2 + epsilon.alt^2) - E_1) "d" E_1 \
  ~ & cal(O)(epsilon.alt^2) ~ cal(O)(epsilon).
$
because of Eq.~@eq:f_reheaton-out-of-range.
Similarly,
$
  & 2 / "p" integral_(E_inflaton / 2 - "p" / 2)^(E_inflaton / 2 + "p" / 2) f_phi (t, E_1) "d" E_1 \
  = & 2 / "p" (integral_(E_inflaton / 2 - "p" / 2)^(m_inflaton / 2) + integral_(m_inflaton / 2)^(E_inflaton / 2 + "p" / 2)) f_phi (t, E_1) "d" E_1 \
  = & 2 / epsilon.alt integral_(1 / 2 (sqrt(m_inflaton^2 + epsilon.alt^2) - epsilon.alt))^(m_inflaton / 2) f_phi (t, E_1) "d" E_1 + cal(O)(epsilon.alt^2) \
  ~ & f_phi (t, m_inflaton / 2) + cal(O)(epsilon.alt) \
$ \

Hence, the BTE becomes
$
  (partial f_inflaton) / (partial t) - H "p" (partial f_inflaton) / (partial "p") = -lambda^2 / (32 pi E_inflaton) f_inflaton (t, "p") [1 + f_phi (t, m_inflaton / 2)].
$
With the change of variables as $t -> a(t)$ and $tilde("p") := "p" a(t)$, _i.e._, $f_inflaton (t, "p") = tilde(f)_inflaton (a, tilde("p"))$, the above equation can be rewritten as
$
  H a (partial tilde(f)_inflaton) / (partial a) = -lambda^2 / (32 pi E_inflaton) tilde(f)_inflaton (a, tilde("p")) [1 + f_phi (t, m_inflaton / 2)].
$ \
With the ansatz that the solution to the BTE is taking the form of Eq.~@eq:ansatz-f_inflaton, _i.e._,
$
  tilde(f)_inflaton (a, tilde("p")) = & (2 pi^2 n_inflaton (t)) / ("p"^2 epsilon) tilde(Theta)(0 <= "p" <= epsilon) \
  = & (2 pi^2 n_inflaton (a)) / (tilde("p")^2 tilde(epsilon)) a^3 tilde(Theta)(0 <= tilde("p") <= tilde(epsilon)),
$
where we have defined $tilde(epsilon) := epsilon a$ for preserving the infinitesimal nature of $epsilon$, we have
$
  H a (partial tilde(f)_inflaton) / (partial a) = & (2 pi^2 H a) / (tilde("p")^2 tilde(epsilon)) (("d" n_inflaton (a)) / ("d" a) a^3 + 3 a^2 n_inflaton (a)) tilde(Theta)(0 <= tilde("p") <= tilde(epsilon)) \
  = & -lambda^2 / (32 pi E_inflaton) (2 pi^2 n_inflaton (a)) / (tilde("p")^2 tilde(epsilon)) a^3 tilde(Theta)(0 <= tilde("p") <= epsilon a) [1 + f_phi (t, m_inflaton / 2)].
$
For $epsilon -> 0$, it reads
$
  ("d" n_inflaton) / ("d" a) + (3 + (lambda^2) / (32 pi H m_inflaton) [1 + f_phi (t, m_inflaton / 2)]) (n_inflaton (a)) / a = 0.
$ <eq:number-density-evolution-inflaton>

= Analysis of Solution

In this section, we will analyze the details of the evolutions of the inflaton and reheaton.
First, we notice that Eq.~@eq:tilde-f_reheaton-evolution can be integrated in the phase space of $tilde("k")_1$, which leads to
$
  integral ("d"^3 arrow(k)_1) / (2 pi)^3 E_1 (partial tilde(f)_phi) / (partial a) & = integral ("d"^3 arrow(k)_1) / (2 pi)^3 E_1 (pi lambda^2) / (2 m_inflaton^3 H) n_inflaton (a) delta(tilde("k")_1 - m_inflaton / 2 a) [1 + tilde(f)_phi (a, m_inflaton / 2 a)] \
  integral_0^oo ("k"_1^3 "d" "k"_1) / (2 pi^2) (partial tilde(f)_phi) / (partial a) & = integral_0^oo ("k"_1^3 "d" "k"_1) / (2 pi^2) (pi lambda^2) / (2 m_inflaton^3 H) n_inflaton (a) delta(tilde("k")_1 - m_inflaton / 2 a) [1 + tilde(f)_phi (a, m_inflaton / 2 a)] \
  integral_0^oo 1 / a^4 (tilde("k")_1^3 "d" tilde("k")_1) / (2 pi^2) (partial tilde(f)_phi) / (partial a) & = integral_0^oo 1 / a^4 (tilde("k")_1^3 "d" tilde("k")_1) / (2 pi^2) (pi lambda^2) / (2 m_inflaton^3 H) n_inflaton (a) delta(tilde("k")_1 - m_inflaton / 2 a) [1 + tilde(f)_phi (a, m_inflaton / 2 a)] \
  "d" / ("d" a) integral_0^oo (tilde("k")_1^3 "d" tilde("k")_1) / (2 pi^2) tilde(f)_phi (a, tilde("k")_1) & = integral_0^oo (tilde("k")_1^3 "d" tilde("k")_1) / (2 pi^2) (pi lambda^2) / (2 m_inflaton^3 H) n_inflaton (a) delta(tilde("k")_1 - m_inflaton / 2 a) [1 + tilde(f)_phi (a, m_inflaton / 2 a)] \
  "d" / ("d" a) (rho_phi a^4) & = 1 / (2 pi^2) (m_inflaton / 2 a)^3 (pi lambda^2) / (2 m_inflaton^3 H) n_inflaton (a) [1 + tilde(f)_phi (a, m_inflaton / 2 a)] \
  & = (lambda^2) / (32 pi H) (n_inflaton a^3) [1 + tilde(f)_phi (a, m_inflaton / 2 a)].
$
According to Eq.~@eq:full-solution-to-f_reheaton-1, we have
$
  tilde(f)_phi (a, m_inflaton / 2 a) = exp[(pi lambda^2 n_inflaton (a)) / (m_inflaton^4 H(a))] - 1.
$
Therefore,
$
  "d" / ("d" a) (rho_phi a^4) = (lambda^2) / (32 pi H) (n_inflaton a^3) exp[(pi lambda^2 n_inflaton (a)) / (m_inflaton^4 H(a))].
$
Also, we have that
$
  "d" / ("d" a) (rho_inflaton a^3) = & -lambda^2 / (32 pi H) (n_inflaton a^3) [1 + f_phi (t, m_inflaton / 2)] \
  = & -lambda^2 / (32 pi H) (n_inflaton a^3) exp[(pi lambda^2 n_inflaton (a)) / (m_inflaton^4 H(a))],
$
which is derived from Eq.~@eq:number-density-evolution-inflaton.
Thus, we have the continuity equation as
$
  "d" / ("d" a) (rho_inflaton a^3 + rho_phi a^4) = 0.
$ \

If there is no Bose-Einstein enhancement, we have $1 + f_phi -> 1$ in Eqs.~@eq:full-solution-to-f_reheaton-1 and Eq.~@eq:number-density-evolution-inflaton, which leads to
