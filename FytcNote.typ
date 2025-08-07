// Copyright (c) 2025 Quan-feng WU <wuquanfeng@ihep.ac.cn>
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

#let FytcNote(
  title: "",
  abstract: [],
  authors: (),
  body,
) = {
  // Set the document's basic properties.
  set document(author: authors.map(a => a.name), title: title)
  set page(numbering: "1", number-align: center, margin: 2cm)
  set math.equation(numbering: "(1)", number-align: horizon,)
  set heading(numbering: "1.1",)
  set outline.entry(fill: none)
  show ref: it => {
    let eq = math.equation
    let el = it.element
    if el != none and el.func() == eq {
      // Override equation references.
      link(el.location(),
        numbering(el.numbering, ..counter(eq).at(el.location()))
      )
    } else {
      // Other references as usual.
      it
    }
  }

  // Title row.
  align(center)[
    #block(text(weight: 700, 1.75em, title))
    #v(1em, weak: true)
  ]

  // Author information.
  pad(
    top: 0.5em,
    bottom: 0.5em,
    x: 2em,
    grid(
      columns: (1fr,) * calc.min(3, authors.len()),
      gutter: 1em,
      ..authors.map(author => align(center)[
        *#author.name* \
        #link("mailto:" + str(author.email))
        #v(0.5em)
        #author.affiliation
      ]),
    ),
  )

  // Main body.
  show math.equation: set block(breakable: true)
  set par(justify: true, first-line-indent: 2em)

  align(center)[
    #heading(outlined: false, numbering: none, text(0.85em, smallcaps[Abstract]))
  ]

  abstract

  line(stroke: 0.5pt, length: 100%)
  outline()
  line(stroke: 0.5pt, length: 100%)
  pagebreak()

  body
}
