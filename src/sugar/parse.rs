use crate::*;

// List
// -----

impl<'i> KindParser<'i> {
  pub fn parse_list(&mut self, fid: u64, uses: &Uses) -> Result<crate::sugar::List, String> {
    self.consume("[")?;
    let mut vals = Vec::new();
    while self.peek_one() != Some(']') {
      vals.push(Box::new(self.parse_term(fid, uses)?));
      self.skip_trivia();
      if self.peek_one() == Some(',') {
        self.consume(",")?;
      }
    }
    self.consume("]")?;
    return Ok(crate::sugar::List { vals });
  }
}

// Equal
// -----

impl<'i> KindParser<'i> {
  pub fn parse_equal(&mut self, fid: u64, uses: &Uses) -> Result<crate::sugar::Equal, String> {
    self.consume("{")?;
    let a = self.parse_term(fid, uses)?;
    self.consume("=")?;
    let b = self.parse_term(fid, uses)?;
    self.consume("}")?;
    return Ok(crate::sugar::Equal { a, b });
  }
}

// ADT
// -----

impl<'i> KindParser<'i> {
  pub fn parse_adt(&mut self, fid: u64, uses: &Uses) -> Result<crate::sugar::ADT, String> {
    self.consume("data ")?;
    let name = self.parse_name()?;
    let mut pars = Vec::new();
    let mut idxs = Vec::new();
    let mut uses = uses.clone();
    // Parses ADT parameters (if any)
    self.skip_trivia();
    while self.peek_one().map_or(false, |c| c.is_ascii_alphabetic()) {
      let par = self.parse_name()?;
      self.skip_trivia();
      uses = shadow(&par, &uses);
      pars.push(par);
    }
    // Parses ADT fields
    while self.peek_one() == Some('(') {
      self.consume("(")?;
      let idx_name = self.parse_name()?;
      self.consume(":")?;
      let idx_type = self.parse_term(fid, &uses)?;
      self.consume(")")?;
      uses = shadow(&idx_name, &uses);
      idxs.push((idx_name, idx_type));
      self.skip_trivia();
    }
    // Parses ADT constructors
    let mut ctrs = Vec::new();
    self.skip_trivia();
    while self.peek_one() == Some('|') {
      self.consume("|")?;
      let ctr_name = self.parse_name()?;
      let mut uses = uses.clone();
      let mut flds = Vec::new();
      // Parses constructor fields
      self.skip_trivia();
      while self.peek_one() == Some('(') {
        self.consume("(")?;
        let fld_name = self.parse_name()?;
        self.consume(":")?;
        let fld_type = self.parse_term(fid, &uses)?;
        self.consume(")")?;
        uses = shadow(&fld_name, &uses);
        flds.push((fld_name, fld_type));
        self.skip_trivia();
      }
      // Parses constructor return
      self.skip_trivia();
      let mut ctr_indices = Vec::new();
      // Annotated
      if self.peek_one() == Some(':') {
        self.consume(":")?;
        self.skip_trivia();
        // Call
        if self.peek_one() == Some('(') {
          self.consume("(")?;
          // Parses the type (must be fixed, equal to 'name')
          self.consume(&name)?;
          // Parses each parameter (must be fixed, equal to 'pars')
          for par in &pars {
            self.consume(par)?;
          }
          // Parses the indices
          while self.peek_one() != Some(')') {
            let ctr_index = self.parse_term(fid, &uses)?;
            ctr_indices.push(ctr_index);
            self.skip_trivia();
          }
          self.consume(")")?;
        // Non-call
        } else {
          // Parses the type (must be fixed, equal to 'name')
          self.consume(&name)?;
        }
      // Non-annotated
      } else {
        if idxs.len() > 0 {
          return self.expected("annotation for indexed type");
        }
      }
      ctrs.push(sugar::Constructor { name: ctr_name, flds, idxs: ctr_indices });
      self.skip_trivia();
    }
    return Ok(sugar::ADT { name, pars, idxs, ctrs });
  }
}

// Match
// -----

impl<'i> KindParser<'i> {
  // MAT ::= match <name> = <term> { <name> : <term> <...> }: <term>
  pub fn parse_match(&mut self, fid: u64, uses: &Uses, fold: bool) -> Result<Match, String> {
    // Parses the header: 'match <name> = <expr>'
    self.consume(if fold { "fold " } else { "match " })?;
    let name = self.parse_name()?;
    self.skip_trivia();
    let expr = if self.peek_one() == Some('=') {
      self.consume("=")?;
      self.parse_term(fid, uses)?
    } else {
      Term::Var { nam: name.clone() }
    };
    // Parses the with clause: 'with (a: A) (b: B) ...'
    let mut with = Vec::new();
    self.skip_trivia();
    if self.peek_many(5) == Some("with ") {
      self.consume("with")?;
      self.skip_trivia();
      while self.peek_one() == Some('(') {
        self.consume("(")?;
        let mov_name = self.parse_name()?;
        self.consume(":")?;
        let mov_type = self.parse_term(fid, uses)?;
        self.consume(")")?;
        with.push((mov_name, mov_type));
        self.skip_trivia();
      }
    }
    self.skip_trivia();
    // Parses the cases: '{ Type.constructor: value ... }'
    self.consume("{")?;
    let mut adt = "Empty".to_string();
    let mut cses = Vec::new();
    while self.peek_one() != Some('}') {
      self.skip_trivia();
      let cse_name = if self.peek_many(2) == Some("++") {
        self.consume("++")?;
        "List/cons".to_string()
      } else if self.peek_many(2) == Some("[]") {
        self.consume("[]")?;
        "List/nil".to_string()
      } else {
        self.parse_name()?
      };
      let cse_name = uses.get(&cse_name).unwrap_or(&cse_name).to_string();
      // Infers the local ADT name
      let adt_name = {
        let pts = cse_name.split('/').collect::<Vec<&str>>();
        if pts.len() < 2 {
          return self
            .expected(&format!("valid constructor (did you forget 'TypeName/' before '{}'?)", cse_name));
        } else {
          pts[.. pts.len() - 1].join("/")
        }
      };
      // Sets the global ADT name
      if adt == "Empty" {
        adt = adt_name.clone();
      } else if adt != adt_name {
        return self.expected(&format!("{}/constructor", adt));
      }
      // Finds this case's constructor
      let cnm = cse_name.split('/').last().unwrap().to_string();
      let ctr = ADT::load(&adt).ok().and_then(|adt| adt.ctrs.iter().find(|ctr| ctr.name == cnm).cloned());
      if ctr.is_none() {
        return self.expected(&format!("a valid constructor ({}/{} doesn't exit)", adt_name, cnm));
      }
      // Shadows this constructor's field variables
      let mut uses = uses.clone();
      for (fld_name, _) in &ctr.unwrap().flds {
        uses = shadow(&format!("{}.{}", name, fld_name), &uses);
      }
      // Parses the return value
      self.consume(":")?;
      let cse_body = self.parse_term(fid, &uses)?;
      cses.push((cse_name, cse_body));
      self.skip_trivia();
    }
    self.consume("}")?;
    // Parses the motive: ': return_type'
    let moti = if self.peek_one() == Some(':') {
      self.consume(":")?;
      Some(self.parse_term(fid, uses)?)
    } else {
      None
    };
    return Ok(Match { adt, fold, name, expr, with, cses, moti });
  }
}
