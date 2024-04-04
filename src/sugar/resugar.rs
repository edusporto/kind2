use crate::*;

// Nat
// ---

impl Term {
  // Interprets as a Nat:
  // - (Nat.succ (Nat.succ ... Nat.zero))
  // Patterns:
  // - (SUCC pred) ::= (App (Var "Nat.succ") pred)
  // - ZERO        ::= (Var "Nat.zero")
  pub fn as_nat(&self) -> Option<u64> {
    let mut nat = 0;
    let mut term = self;
    loop {
      match term {
        Term::App { era: _, fun, arg } => {
          if let Term::Var { nam } = &**fun {
            if nam == "Nat.succ" {
              nat += 1;
              term = arg;
              continue;
            }
          }
          return None;
        }
        Term::Var { nam } if nam == "Nat.zero" => {
          return Some(nat);
        }
        _ => {
          return None;
        }
      }
    }
  }
}

// List
// ----

impl Term {
  // Interprets as a list:
  // - (((List/cons _) x) (((List/cons _) y) ... (List/nil _)))
  // Patterns:
  // - (CONS head tail) ::= (App (App (App (Var "List/cons") Met) head) tail)
  // - NIL              ::= (App (Var "List/nil") Met)
  pub fn as_list(&self) -> Option<List> {
    let mut vals = Vec::new();
    let mut term = self;
    loop {
      match term {
        Term::App { era: _, fun, arg } => {
          if let Term::App { era: _, fun: cons, arg: head } = &**fun {
            if let Term::App { era: _, fun: cons_fun, arg: _ } = &**cons {
              if let Term::Var { nam } = &**cons_fun {
                if nam == "List/cons" {
                  vals.push(head.clone());
                  term = arg;
                  continue;
                }
              }
            }
          }
          if let Term::Var { nam } = &**fun {
            if nam == "List/nil" {
              return Some(List { vals });
            }
          }
          return None;
        }
        _ => return None,
      }
    }
  }
}

// Equal
// -----

impl Term {
  // Interprets as an Equality:
  // - (((Equal _) a) b)
  // Patterns:
  // - (EQUAL a b) ::= (App (App (App (Var "Equal") _) a) b)
  pub fn as_equal(&self) -> Option<Equal> {
    match self {
      Term::App { era: _, fun, arg: rhs } => {
        if let Term::App { era: _, fun: eq_fun, arg: lhs } = &**fun {
          if let Term::App { era: _, fun: eq_fun, arg: _ } = &**eq_fun {
            if let Term::Var { nam } = &**eq_fun {
              if nam == "Equal" {
                return Some(Equal { a: (**lhs).clone(), b: (**rhs).clone() });
              }
            }
          }
        }
        return None;
      }
      _ => return None,
    }
  }
}

// ADT
// ---

impl Term {
  // Interprets a λ-encoded Algebraic Data Type definition as an ADT struct.
  pub fn as_adt(&self) -> Option<ADT> {
    let name: String;
    let pvar: String;

    let mut pars: Vec<String> = Vec::new();
    let mut idxs: Vec<(String, Term)> = Vec::new();
    let mut ctrs: Vec<Constructor> = Vec::new();
    let mut term = self;

    // Skips the Slf: `$(self: (TY P0 P1 ... I0 I1 ...))`
    if let Term::Slf { nam: _, typ: _, bod } = term {
      term = bod;
    } else {
      return None;
    }

    // Reads the motive: `∀(P: ∀(I0: I0.TY) ∀(I1: I1.TY) ... ∀(x: (TY P0 P1 ... I0 I1 ...)) *).`
    if let Term::All { era: _, nam, inp, bod } = term {
      // Gets the motive name
      pvar = nam.clone();

      // Gets the motive type
      let mut moti = &**inp;

      // Reads each motive index
      while let Term::All { era: _, nam, inp: idx_inp, bod: idx_bod } = moti {
        if let Term::All { .. } = &**idx_bod {
          idxs.push((nam.clone(), *idx_inp.clone()));
          moti = idx_bod;
        } else {
          break;
        }
      }

      // Skips the witness
      if let Term::All { era: _, nam: _, inp: wit_inp, bod: wit_bod } = moti {
        // Here, the witness has form '(TY P0 P1 ... I0 I1 ...)'
        let mut wit_inp = wit_inp;

        // Skips the wit's indices (outermost Apps)
        for _ in 0 .. idxs.len() {
          if let Term::App { era: _, fun: wit_inp_fun, arg: _ } = &**wit_inp {
            wit_inp = wit_inp_fun;
          } else {
            return None;
          }
        }

        // Collects the wit's parameters (remaining Apps)
        while let Term::App { era: _, fun: wit_inp_fun, arg: wit_inp_arg } = &**wit_inp {
          if let Term::Var { nam: parameter } = &**wit_inp_arg {
            pars.push(parameter.to_string());
            wit_inp = wit_inp_fun;
          } else {
            return None;
          }
        }

        // Extracts the type name
        if let Term::Var { nam } = &**wit_inp {
          name = nam.clone();
        } else {
          return None;
        }
        moti = &wit_bod;
      } else {
        return None;
      }

      // Checks if the motive ends in Set
      if let Term::Set = moti {
        // Correct.
      } else {
        return None;
      }

      term = bod;
    } else {
      return None;
    }

    // Reads each constructor: `∀(C0: ∀(C0_F0: C0_F0.TY) ∀(C0_F1: C0_F1.TY) ... (P I0 I1 ... (TY.C0 P0 P1 ... C0_F0 C0_F1 ...)))`
    while let Term::All { era: _, nam, inp, bod } = term {
      let mut flds: Vec<(String, Term)> = Vec::new();
      let mut ctyp: &Term = &**inp;

      // Reads each field
      while let Term::All { era: _, nam, inp, bod } = ctyp {
        flds.push((nam.clone(), *inp.clone()));
        ctyp = bod;
      }

      // Now, the ctyp will be in the form (P I0 I1 ... (Ctr P0 P1 ... F0 F1 ...))

      // Skips the outermost application
      if let Term::App { era: _, fun: ctyp_fun, arg: _ } = ctyp {
        ctyp = ctyp_fun;
      } else {
        return None;
      }

      // Collects constructor indices until we reach the pattern head P
      let mut ctr_idxs: Vec<Term> = Vec::new();
      while let Term::App { era: _, fun: fun_app, arg: arg_app } = ctyp {
        ctr_idxs.push(*arg_app.clone());
        ctyp = fun_app;
      }

      // Checks if the pattern fun is `pvar`
      if let Term::Var { nam } = ctyp {
        if nam != &pvar {
          return None;
        }
      } else {
        return None;
      }

      ctr_idxs.reverse();
      ctrs.push(Constructor { name: nam.clone(), flds, idxs: ctr_idxs });

      term = bod;
    }

    return Some(ADT { name, pars, idxs, ctrs });
  }
}
