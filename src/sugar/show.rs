use crate::*;

// List
// -----

impl List {
  pub fn format(&self) -> Box<Show> {
    if self.vals.len() == 0 {
      return Show::text("[]");
    } else {
      return Show::call("", vec![
        Show::text("["),
        Show::pile(", ", self.vals.iter().map(|x| x.format_go()).collect()),
        Show::text("]"),
      ]);
    }
  }
}

// Equal
// -----

impl Equal {
  pub fn format(&self) -> Box<Show> {
    Show::glue(" ", vec![self.a.format_go(), Show::text("=="), self.b.format_go()])
  }
}

// ADT
// -----

impl ADT {
  pub fn format(&self) -> Box<Show> {
    // ADT head: `data Name <params> <indices>`
    let mut adt_head = vec![];
    adt_head.push(Show::text("data"));
    adt_head.push(Show::text(&self.name));
    for par in self.pars.iter() {
      adt_head.push(Show::text(par));
    }
    for (nam, typ) in self.idxs.iter() {
      adt_head.push(Show::call("", vec![
        Show::glue("", vec![Show::text("("), Show::text(nam), Show::text(": ")]),
        typ.format_go(),
        Show::text(")"),
      ]));
    }

    // ADT tail: constructors
    let mut adt_tail = vec![];
    for ctr in &self.ctrs {
      let mut adt_ctr = vec![];
      // Constructor head: name
      adt_ctr.push(Show::glue("", vec![Show::line(), Show::text("| "), Show::text(&ctr.name)]));
      // Constructor body: fields
      for (nam, typ) in ctr.flds.iter() {
        adt_ctr.push(Show::call("", vec![
          Show::glue("", vec![Show::text("("), Show::text(nam), Show::text(": ")]),
          typ.format_go(),
          Show::text(")"),
        ]));
      }
      // Constructor tail: return
      adt_ctr.push(Show::glue(" ", vec![
        Show::text(":"),
        Show::call(" ", {
          let mut ret_typ = vec![];
          ret_typ.push(Show::text(&format!("({}", &self.name)));
          for par in &self.pars {
            ret_typ.push(Show::text(par));
          }
          for idx in &ctr.idxs {
            ret_typ.push(idx.format_go());
          }
          ret_typ.push(Show::text(")"));
          ret_typ
        }),
      ]));
      adt_tail.push(Show::call(" ", adt_ctr));
    }

    return Show::glue(" ", vec![Show::glue(" ", adt_head), Show::glue(" ", adt_tail)]);
  }
}

// Match
// -----

impl Match {
  pub fn format(&self) -> Box<Show> {
    Show::pile(" ", vec![
      Show::glue(" ", vec![
        Show::text(if self.fold { "fold" } else { "match" }),
        Show::text(&self.name),
        Show::text("="),
        self.expr.format_go(),
      ]),
      Show::glue(" ", vec![
        Show::text("{"),
        Show::pile(
          "; ",
          self
            .cses
            .iter()
            .map(|(name, term)| Show::glue(" ", vec![Show::text(name), Show::text(":"), term.format_go()]))
            .collect(),
        ),
      ]),
      if let Some(moti) = &self.moti {
        Show::glue(" ", vec![Show::text(":"), moti.format_go()])
      } else {
        Show::text("")
      },
    ])
  }
}
