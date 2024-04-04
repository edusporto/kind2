//! # Syntax sugar module.
//!
//! ## Examples
//! 
//! #### Nat
//! 
//! The Nat sugar is merely a shortcut. For example:
//!
//! ```text
//! (Nat.succ (Nat.succ (Nat.succ Nat.zero)))
//! ```
//!
//! Is converted to and from `3`.
//!
//! #### List
//! 
//! The List sugar is merely a shortcut. For example:
//!
//! ```text
//! (List/cons _ a (List/cons _ b (List/cons _ c (List/nil _))))
//! ```
//!
//! Is converted to and from `[a, b, c]`.
//!
//! #### Equal
//! 
//! The Equal sugar is also a shortcut. For example:
//!
//! ```text
//! (Equal _ a b)
//! ```
//!
//! Is converted to and from:
//!
//! ```text
//! {a = b}
//! ```
//!
//! #### Vector
//! 
//! The Vector type:
//!
//! ```text
//! data Vector A (len: Nat)
//! | nil : (Vector A zero)
//! | cons (len: Nat) (head: A) (tail: (Vector A len)) : (Vector A (succ len))
//! ```
//!
//! Has the following λ-encoding:
//!
//! ```text
//! Vector
//! : ∀(A: *) ∀(len: Nat) *
//! = λA λlen
//!   $(self: (Vector A len))
//!   ∀(P: ∀(len: Nat) ∀(x: (Vector A len)) *)
//!   ∀(cons: ∀(len: Nat) ∀(head: A) ∀(tail: (Vector A len)) (P (Nat.succ len) (Vector.cons A len head tail)))
//!   ∀(nil: (P 0 (Vector.nil A)))
//!   (P len self)
//! ```
//!
//! And is represented in Rust as:
//!
//! ```rust
//! let vector = ADT {
//!   name: "Vector".to_string(),
//!   pars: vec!["A".to_string()],
//!   idxs: vec![("len".to_string(), Term::Var { nam: "Nat".to_string() })],
//!   ctrs: vec![
//!     Constructor {
//!       name: "nil".to_string(),
//!       flds: vec![],
//!       idxs: vec![
//!         Term::Var { nam: "A".to_string() },
//!         Term::Var { nam: "zero".to_string() },
//!       ],
//!     },
//!     Constructor { 
//!       name: "cons".to_string(),
//!       flds: vec![
//!         ("len".to_string(), Term::Var { nam: "Nat".to_string() }),
//!         ("head".to_string(), Term::Var { nam: "A".to_string() }),
//!         ("tail".to_string(), Term::App {
//!           fun: Box::new(Term::App {
//!             fun: Box::new(Term::Var { nam: "Vector".to_string() }),
//!             arg: Box::new(Term::Var { nam: "A".to_string() }),
//!           }),
//!           arg: Box::new(Term::Var { nam: "len".to_string() }),
//!         }),        
//!       ],
//!       idxs: vec![
//!         Term::Var { nam: "A".to_string() },
//!         Term::App {
//!           fun: Box::new(Term::Var { nam: "succ".to_string() }),
//!           arg: Box::new(Term::Var { nam: "len".to_string() }),
//!         },
//!       ],
//!     },
//!   ],
//! };
//! ```
//!
//! #### Pattern matching
//! 
//! A pattern-match is represented as: 
//!
//! ```text
//! match name = expr with (a: A) (b: B) ... {
//!   ADT.foo: ...
//!   ADT.bar: ...
//! }: motive
//! ```
//!
//! Note:
//! 1. The `= expr` can be omitted. Will default to `Var { name }`.
//! 2. The `: motive` can be omitted. Will default to `Met {}`.
//! 3. The ADT is obtained from the 'ADT.ctr' cases.
//! 4. If there are no cases, ADT is defaulted to 'Empty'.
//! 5. The 'with' clause is optional.
//!
//! The 'with' clause moves outer vars inwards, linearizing them. For example:
//!
//! ```text
//! let a = (f y)
//! match x {
//!   tic: a
//!   tac: a
//! }
//! ```
//!
//! Would, normally, cause 'a' to be *cloned*. This has two problems:
//!
//! 1. Cloning can have large efficiency costs in some runtimes, such as the HVM.
//! 2. If 'x' occurs in the type of 'a', it won't get specialized, blocking proofs.
//!
//! The 'with' clause allows us to solve both issues. For example:
//!
//! ```text
//! let a = (f y)
//! match x with a {
//!   tic: a
//!   tac: a
//! }
//! ```
//!
//! This expression is desugared as:
//! 
//! ```text
//! let a = (f y)
//! let b = (g y)
//! ({(match x with (a: A) (b: B) ... {
//!   tic: λa a
//!   tac: λa a
//! }): ∀(a: A) ∀(b: B) _} a b)
//! ```
//!
//! Or, in raw terms, the 'match_expr' is changed to:
//!
//! ```text
//! (APP (APP (ANN match_expr (ALL "a" A (ALL "b" B ... MET))) a) b)
//! ```
//!
//! For a full example, the expression:
//!
//! ```text
//! match x = (f arg) with (a: A) (b: B) {
//!   Vector.cons: (U60.add x.head (sum x.tail))
//!   Vector.nil: #0
//! }: #U60
//! ```
//!
//! Is converted to:
//!
//! ```text
//! use x.P = λx.len λx ∀(a: A) ∀(b: B) #U60
//! use x.cons = λx.head λx.tail λa λb ((U60.add x.head) (sum x.tail))
//! use x.nil = λx.len λa λb #0
//! (({(((~(f arg) x.P) x.cons) x.nil): ∀(a: A) ∀(b: B) _} a) b)
//! ```

use crate::{*};

mod desugar;
mod resugar;
mod parse;
mod show;

// A List.
#[derive(Debug, Clone)]
pub struct List {
  pub vals: Vec<Box<Term>>,
}

// A Propositional Equality.
#[derive(Debug, Clone)]
pub struct Equal {
  pub a: Term,
  pub b: Term,
}

// An Algebraic Data Type (ADT).
#[derive(Debug, Clone)]
pub struct ADT {
  pub name: String,
  pub pars: Vec<String>, // parameters
  pub idxs: Vec<(String,Term)>, // indices
  pub ctrs: Vec<Constructor>, // constructors
}

#[derive(Debug, Clone)]
pub struct Constructor {
  pub name: String, // constructor name
  pub flds: Vec<(String,Term)>, // constructor fields
  pub idxs: Vec<Term>, // constructor type indices
}

// NOTE: we've just added the 'with' field to the 'match' sugar.
// We must now refactor some functions to enable it.
#[derive(Debug, Clone)]
pub struct Match {
  pub adt: String, // datatype
  pub fold: bool, // is this a fold?
  pub name: String, // scrutinee name
  pub expr: Term, // structinee expression
  pub with: Vec<(String,Term)>, // terms to move in
  pub cses: Vec<(String,Term)>, // matched cases
  pub moti: Option<Term>, // motive
}

impl ADT {

  // Loads an ADT from its λ-encoded file.
  pub fn load(name: &str) -> Result<ADT, String> {
    let book = Book::boot(".", name)?;
    if let Some(term) = book.defs.get(name) {
      let mut term = term.clone();
      // Skips Anns, Lams, Srcs
      loop {
        match term {
          Term::Ann { val, .. } => { term = *val; }
          Term::Lam { bod, .. } => { term = *bod; }
          Term::Src { val, .. } => { term = *val; }
          _ => { break; }
        }
      }
      //println!("{}", term.format().flatten(Some(800)));
      return term.as_adt().ok_or_else(|| format!("Failed to interpret '{}' as an ADT.", name))
    } else {
      Err(format!("Cannot find definition for type '{}'.", name))
    }
  }
}
