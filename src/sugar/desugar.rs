use crate::*;

// Nat
// -----

impl Term {
  // Nats have a dedicated term, for type-checking efficiency
  pub fn new_nat(nat: u64) -> Term {
    Term::Nat { nat }
  }
}

// List
// -----

impl Term {
  // Builds a chain of applications of List/cons and List/nil from a Vec<Box<Term>>
  pub fn new_list(list: &List) -> Term {
    let mut term = Term::Var { nam: "List/nil".to_string() };
    for item in (&list.vals).into_iter().rev() {
      term = Term::App {
        era: false,
        fun: Box::new(Term::App {
          era: false,
          fun: Box::new(Term::Var { nam: "List/cons".to_string() }),
          arg: item.clone(),
        }),
        arg: Box::new(term),
      };
    }
    term
  }
}

// Equal
// -----

impl Term {
  // Builds an equal chain
  pub fn new_equal(eq: &Equal) -> Term {
    Term::App {
      era: false,
      fun: Box::new(Term::App {
        era: false,
        fun: Box::new(Term::App {
          era: true,
          fun: Box::new(Term::Var { nam: "Equal".to_string() }),
          arg: Box::new(Term::Met {}),
        }),
        arg: Box::new(eq.a.clone()),
      }),
      arg: Box::new(eq.b.clone()),
    }
  }
}

// ADT
// -----

impl Term {
  // Builds a λ-encoded Algebraic Data Type definition from an ADT struct.
  pub fn new_adt(adt: &ADT) -> Term {
    // 1. Builds the self type: (Type P0 P1 ... I0 I1 ...)

    // Starts with the type name
    let mut self_type = Term::Var { nam: adt.name.clone() };

    // Then appends each type parameter
    for par in adt.pars.iter() {
      self_type =
        Term::App { era: false, fun: Box::new(self_type), arg: Box::new(Term::Var { nam: par.clone() }) };
    }

    // And finally appends each index
    for (idx_name, _) in adt.idxs.iter() {
      self_type = Term::App {
        era: false,
        fun: Box::new(self_type),
        arg: Box::new(Term::Var { nam: idx_name.clone() }),
      };
    }

    // 2. Builds the motive type: ∀(I0: I0.TY) ∀(I1: I1.TY) ... ∀(x: (Type P0 P1 ... I0 I1 ...)) *

    // Starts with the witness type: ∀(x: (Type P0 P1 ... I0 I1 ...)) *
    let mut motive_type = Term::All {
      era: false,
      nam: "x".to_string(),
      inp: Box::new(self_type.clone()),
      bod: Box::new(Term::Set),
    };

    // Then prepends each index type
    for (idx_name, idx_type) in adt.idxs.iter().rev() {
      motive_type = Term::All {
        era: false,
        nam: idx_name.clone(),
        inp: Box::new(idx_type.clone()),
        bod: Box::new(motive_type),
      };
    }

    // 3. Builds the final term, starting with the self application: (P I0 I1 ... self)
    let mut term = Term::Var { nam: "P".to_string() };

    // Applies the motive to each index
    for (idx_name, _) in adt.idxs.iter() {
      term =
        Term::App { era: false, fun: Box::new(term), arg: Box::new(Term::Var { nam: idx_name.clone() }) };
    }

    // And applies it to the witness (self)
    term =
      Term::App { era: false, fun: Box::new(term), arg: Box::new(Term::Var { nam: "self".to_string() }) };

    // 4. Prepends each constructor: ∀(C0: ∀(C0_F0: C0_F0.TY) ∀(C0_F1: C0_F1.TY) ... (P C0_I0 C0_I1 ... (Type.C0 P0 P1 ... C0_F0 C0_F1 ...)))
    for ctr in adt.ctrs.iter().rev() {
      // Builds the constructor application: (P C0_I0 C0_I1 ... (Type.C0 P0 P1 ... C0_F0 C0_F1 ...))
      let mut ctr_term = Term::Var { nam: "P".to_string() };

      // Applies the motive to each constructor index
      for idx in ctr.idxs.iter().rev() {
        ctr_term = Term::App { era: false, fun: Box::new(ctr_term), arg: Box::new(idx.clone()) };
      }

      // Builds the constructor type: (Type.C0 P0 P1 ... C0_F0 C0_F1 ...)
      let mut ctr_type = Term::Var { nam: format!("{}/{}/", adt.name, ctr.name) };

      // For each type parameter
      // NOTE: Not necessary anymore due to auto implicit arguments
      for par in adt.pars.iter() {
        ctr_type =
          Term::App { era: true, fun: Box::new(ctr_type), arg: Box::new(Term::Var { nam: par.clone() }) };
      }

      // And for each field
      for (fld_name, _fld_type) in ctr.flds.iter() {
        ctr_type = Term::App {
          era: false,
          fun: Box::new(ctr_type),
          arg: Box::new(Term::Var { nam: fld_name.clone() }),
        };
      }

      // Wraps the constructor type with the application
      ctr_term = Term::App { era: false, fun: Box::new(ctr_term), arg: Box::new(ctr_type) };

      // Finally, quantifies each field
      for (fld_name, fld_type) in ctr.flds.iter().rev() {
        ctr_term = Term::All {
          era: false,
          nam: fld_name.clone(),
          inp: Box::new(fld_type.clone()),
          bod: Box::new(ctr_term),
        };
      }

      // And quantifies the constructor
      term = Term::All { era: false, nam: ctr.name.clone(), inp: Box::new(ctr_term), bod: Box::new(term) };
    }

    // 5 Quantifies the motive
    term = Term::All { era: false, nam: "P".to_string(), inp: Box::new(motive_type), bod: Box::new(term) };

    // 6. Wraps everything with a self annotation
    term = Term::Slf { nam: "self".to_string(), typ: Box::new(self_type), bod: Box::new(term) };

    //println!("RESULT:\n{:#?}", term.format());
    //println!("PARSED:\n{}", term.format().flatten(Some(80)));

    return term;
  }

  pub fn constructor_code((adt_name, adt_term): (&str, &Term), ctr_ref: &str) -> Option<Box<Show>> {
    // Check if `adt_name` really is an ADT
    let adt = match &adt_term {
      // Type variables wrap ADTs in Lams
      Term::Lam { bod, .. } => return Term::constructor_code((adt_name, bod), ctr_ref),
      // Skip `Ann` and `Src`
      Term::Ann { val, .. } => return Term::constructor_code((adt_name, val), ctr_ref),
      Term::Src { val, .. } => return Term::constructor_code((adt_name, val), ctr_ref),
      // Found an ADT
      Term::Slf { .. } => adt_term.as_adt()?,
      _ => return None,
    };

    let ctrs = adt.ctrs.clone();

    // Search for constructor in ADT
    let ctr_ref = ctr_ref.trim_end_matches('/'); // last "/" is not part of name
    let ctr = ctrs.iter().find(|ctr| format!("{adt_name}/{}", ctr.name) == ctr_ref)?.clone();

    // Generate constructor code:

    // Constructor name.
    let ctr_name = &ctr.name;

    // Type parameters.
    // Format: <Par_1> .. <Par_n>
    let format_param = |param| Show::text(&format!("<{}>", param));
    let params = Show::glue(" ", adt.pars.iter().map(format_param).rev().collect());

    // Constructor fields.
    // Format: (f_1: T_1) .. (f_n: T_n)
    let format_field = |(name, typ): &(String, Term)| {
      Show::glue("", vec![
        Show::text("("),
        Show::glue("", vec![Show::text(name), Show::text(": "), typ.format()]),
        Show::text(")"),
      ])
    };
    let fields = Show::glue(" ", ctr.flds.iter().map(format_field).collect());

    // Constructor return type with type parameters and type indices.
    // Format: (adt_name Par_1 .. Par_n Idx_1 .. Idx_n)
    let mut ctr_typ = vec![Show::text(&adt.name)];
    adt.pars.iter().rev().for_each(|par| ctr_typ.push(Show::text(par)));
    ctr.idxs.iter().rev().for_each(|idx| ctr_typ.push(idx.format()));
    let ctr_typ = Show::glue("", vec![Show::text("("), Show::glue(" ", ctr_typ), Show::text(")")]);

    // Constructor names into Scott-encoding.
    // Format: λctr_name_1 .. λctr_name_n
    let format_ctr_lam_name = |ctr_name| Show::text(&format!("λ{ctr_name}"));
    let names = ctrs.into_iter().map(|ctr| ctr.name);
    let ctr_lam_names = Show::glue(" ", names.map(format_ctr_lam_name).collect());

    // Fields without their types.
    // Format: f_1 .. f_n
    let field_names = Show::glue(" ", ctr.flds.iter().map(|(name, _)| Show::text(name)).collect());

    // Applies constructor and fields.
    // Format: (ctr_name f_1 .. f_n)
    let final_app = Show::glue("", vec![
      Show::text("("),
      Show::glue(" ", vec![Show::text(ctr_name), field_names]),
      Show::text(")"),
    ]);

    // The result should be in the following format:
    // ctr_name <Par_1> .. <Par_n> (f_1: T_1) .. (f_n: T_n): (adt_name Par_1 .. Par_n Idx_1 .. Idx_n) =
    //   ~λP λctr_name_1 .. λctr_name_n (ctr_name f_1 .. f_n)
    let ctr_text = Show::glue(" ", vec![
      Show::text(ctr_name),
      params,
      fields,
      Show::text(":"),
      ctr_typ,
      Show::text("="),
      Show::line(),
      Show::text("~λP"),
      ctr_lam_names,
      final_app,
    ]);

    Some(ctr_text)
  }
}

// Match
// -----

impl Term {
  // Builds a λ-encoded pattern-match.
  pub fn new_match(mat: &Match) -> Term {
    let adt = ADT::load(&mat.adt).expect(&format!("Cannot load datatype '{}'", &mat.adt));

    let mut term: Term;

    // 1. Create the motive's term
    let mut motive;
    if let Some(moti) = &mat.moti {
      // Creates the first lambda: 'λx <motive>'
      motive = Term::Lam { era: false, nam: mat.name.clone(), bod: Box::new(moti.clone()) };
      // Creates a lambda for each index: 'λindices ... λx <motive>'
      for (idx_name, _) in adt.idxs.iter().rev() {
        motive = Term::Lam { era: false, nam: idx_name.clone(), bod: Box::new(motive) };
      }
      // Creates a forall for each moved value: 'λindices ... λx ∀(a: A) ... <motive>'
      for (nam, typ) in mat.with.iter().rev() {
        motive =
          Term::All { era: false, nam: nam.clone(), inp: Box::new(typ.clone()), bod: Box::new(motive) };
      }
    } else {
      // If there is no explicit motive, default to a metavar
      motive = Term::Met {};
    }

    // 2. Create each constructor's term
    let mut ctrs: Vec<Term> = Vec::new();
    for ctr in &adt.ctrs {
      // Find this constructor's case
      let found = mat.cses.iter().find(|(case_name, _)| {
        return case_name == &format!("{}/{}", adt.name, ctr.name);
      });
      if let Some((_, case_term)) = found {
        // If it is present...
        let mut ctr_term = case_term.clone();
        // Adds moved value lambdas
        for (nam, _) in mat.with.iter().rev() {
          ctr_term = Term::Lam { era: false, nam: nam.clone(), bod: Box::new(ctr_term) };
        }
        // Adds field lambdas
        for (fld_name, _) in ctr.flds.iter().rev() {
          ctr_term = Term::Lam {
            era: false,
            nam: format!("{}.{}", mat.name, fld_name.clone()),
            bod: Box::new(ctr_term),
          };
        }
        ctrs.push(ctr_term);
      } else {
        // Otherwise, show an error. TODO: improve the error reporting here
        println!("Missing case for constructor '{}' on {} match.", ctr.name, mat.adt);
        std::process::exit(0);
      }
    }

    // 3. Create either `(~x <motive>)` or `(Type/fold/ _ <motive> x)`
    if mat.fold {
      term = Term::Let {
        nam: mat.name.clone(),
        val: Box::new(mat.expr.clone()),
        bod: Box::new(Term::App {
          era: false,
          fun: Box::new(Term::App {
            era: true,
            fun: Box::new(Term::App {
              era: true,
              fun: Box::new(Term::Var { nam: format!("{}/fold/", adt.name) }),
              arg: Box::new(Term::Met {}),
            }),
            arg: Box::new(Term::Var { nam: format!("{}.P", mat.name) }),
          }),
          arg: Box::new(Term::Var { nam: mat.name.clone() }),
        }),
      };
    } else {
      term = Term::Let {
        nam: mat.name.clone(),
        val: Box::new(mat.expr.clone()),
        bod: Box::new(Term::App {
          era: false,
          fun: Box::new(Term::Ins { val: Box::new(Term::Var { nam: mat.name.clone() }) }),
          arg: Box::new(Term::Var { nam: format!("{}.P", mat.name) }),
        }),
      };
    }

    // 4. Apply each constructor (by name)
    for ctr in &adt.ctrs {
      term = Term::App {
        era: false,
        fun: Box::new(term),
        arg: Box::new(Term::Var { nam: format!("{}.{}", mat.name, ctr.name) }),
      };
    }

    // 5. Annotates with the moved var foralls
    if mat.with.len() > 0 {
      let mut ann_type = Term::Met {};
      for (nam, typ) in mat.with.iter().rev() {
        ann_type =
          Term::All { era: false, nam: nam.clone(), inp: Box::new(typ.clone()), bod: Box::new(ann_type) };
      }
      term = Term::Ann { val: Box::new(term), typ: Box::new(ann_type), chk: true };
    }

    // 6. Applies each moved var
    for (nam, _) in mat.with.iter() {
      term = Term::App { era: false, fun: Box::new(term), arg: Box::new(Term::Var { nam: nam.clone() }) };
    }

    // 7. Create the local 'use' definition for each term
    for (i, ctr) in adt.ctrs.iter().enumerate().rev() {
      term = Term::Use {
        nam: format!("{}.{}", mat.name, ctr.name),
        val: Box::new(ctrs[i].clone()),
        bod: Box::new(term),
      };
    }

    // 8. Create the local 'use' definition for the motive
    term = Term::Use { nam: format!("{}.P", mat.name), val: Box::new(motive), bod: Box::new(term) };

    // 9. Return 'term'
    return term;
  }
}
