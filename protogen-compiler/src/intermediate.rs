// use petgraph::Graph;

use crate::ast;
use crate::ast::{BinOp, DataType, Field, Message, Protocol, UnaryOp, Value};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::fmt::Formatter;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Protocol;
    use crate::parser::grammar::{ExpressionParser, ProtocolParser};

    #[test]
    fn solve_for() {
        let pexpr = ExpressionParser::new().parse("(@var - 5) * 10").unwrap();
        let lh = IRExpression::from_ast("message", &pexpr);
        let rh = IRExpression::Value(Value::Number(20));

        let solved = Equation { lh, rh }
            .solve_for(&Ref::new("message".to_string(), "var".to_string()))
            .unwrap();
        assert_eq!("(20 / 10) + 5", solved.to_string());
        assert_eq!("7", solved.simplify().unwrap().to_string());
    }

    #[test]
    fn test_graph() {
        let protocol: Protocol = ProtocolParser::new()
            .parse(
                r#"
wave = {
  chunk_id: [u8; 4] | [b"RIFF"];
  @chunk_size: u32;
  public wave_id: [u8; 4] | [b"WAVE"];

  // first we have the format subchunk
  fmt_id: [u8; 4] | [b"fmt "];
  @fmt_size: u32;

  public audio_format: u16;
  public num_channels: u16;
  public sample_rate: u32;
  public byte_rate: u32;
  public block_align: u16;
  public bits_per_sample: u16;
  public format_rest: [u8; @fmt_size - 16];

  @data_size: u32 = (@chunk_size - @fmt_size) - 12;
  // then the remaining chunks
  @data: [u8; @data_size];
  public chunks: apply @data many!(subchunk(@bits_per_sample));
}

subchunk($bits_per_sample: u16) = {
  @id: [u8; 4];
  @size: u32;
  @data: [u8; @size];
  public subchunk: apply @data choose {
    U8DataSubchunk = u8_data_subchunk(@id) |
    U16DataSubchunk = u16_data_subchunk(@id) |
    OtherSubchunk = other_subchunk(@id)
  };
}

u8_data_subchunk ($id: [u8; 4] = b"data") = {
  public data: rest!();
}

u16_data_subchunk ($id: [u8; 4] = b"data") = {
  public data: many!(u16);
}

other_subchunk (public $id: [u8; 4]) = {
  public data: rest!();
}
"#,
            )
            .unwrap();

        let constraints = find_constraints(&protocol.messages).unwrap();

        for c in &constraints {
            println!("{:15} | {}", c.0.to_string(), c.1);
        }

        let mut c_map = HashMap::new();
        for (r, c) in &constraints {
            c_map.entry(r.clone()).or_insert(vec![]).push(c.clone());
        }

        let complete: HashSet<Ref> = protocol
            .messages
            .iter()
            .flat_map(|m| {
                let name = m.name.clone();
                m.fields
                    .iter()
                    .filter(|f| !f.variable)
                    .map(move |f| ref_field(&name, f))
            })
            .collect();

        let _result = expr_for_field(
            &Ref {
                message: "wave".to_string(),
                field: "chunk_size".to_string(),
            },
            &c_map,
            &complete,
        )
        .unwrap();
    }

    #[test]
    fn test_expr_for_field() {
        let a = Ref {
            message: "msg".to_string(),
            field: "a".to_string(),
        };
        let b = Ref {
            message: "msg".to_string(),
            field: "b".to_string(),
        };
        let c = Ref {
            message: "msg".to_string(),
            field: "c".to_string(),
        };

        let mut constraints = HashMap::new();
        let mut complete = HashSet::new();
        complete.insert(a.clone());

        constraints.insert(
            b.clone(),
            vec![IRExpression::Binary(
                BinOp::Plus,
                Box::new(IRExpression::Variable(a.clone())),
                Box::new(IRExpression::Value(Value::Number(15))),
            )],
        );

        constraints.insert(
            c.clone(),
            vec![IRExpression::Binary(
                BinOp::Minus,
                Box::new(IRExpression::Variable(b.clone())),
                Box::new(IRExpression::Value(Value::Number(12))),
            )],
        );

        let _result = expr_for_field(&c, &constraints, &complete).unwrap();
    }

    #[test]
    fn test_multi_arg() {
        let protocol: Protocol = ProtocolParser::new()
            .parse(
                r#"vars3 = {
  @len: u8;
  @type: u16;
  @data: [u8; @len];
  public sub: apply @data choose {
    Submessage1 = submessage_2ary_1(@len, @type) |
    Submessage2 = submessage_2ary_2(@len, @type)
  };
}

submessage_2ary_1($a1: u8, $a2: u16 = 0x1) = {
  public f1: [u8; $a1];
}

submessage_2ary_2($b1: u8, $b2: u16) = {
  public f1: [u8; $b1];
  public f2: u16 = $b2 + 5;
}"#,
            )
            .unwrap();

        /*
        len(data) = @len
        serialize(sub) = @data
        @type = (if let Submessage1(s) = sub { submessage_2ary_1.a2 })
        @type = (if let Submessage2(s) = sub { submessage_2ary_2.b2 })

        type = if let Submessage1(s) = sub {
          s.a2
        }  else if let Submessage2(s) = sub {
          s.b2
        }

        type = match sub {
          Submessage1(__match_value) => __match_value.a2,
          Submessage2(__match_value) => __match_value.b2,
        }

        */

        let ir = IR::from_ast(&protocol).unwrap();

        let rf = Ref::str("vars3", "type");

        let cs = ir.constraint_map.get(&rf).unwrap();

        assert_eq!(
            cs,
            &vec![IRExpression::Match(
                Ref::str("vars3", "sub"),
                vec![
                    (
                        "Submessage1".to_string(),
                        IRExpression::Parameter(Ref::str("__match_value", "a2"))
                    ),
                    (
                        "Submessage2".to_string(),
                        IRExpression::Parameter(Ref::str("__match_value", "b2"))
                    ),
                ]
            ),]
        );

        assert!(ir.expr_for_field("vars3", "len").is_ok());

        println!(
            "expr for field {}",
            ir.expr_for_field("vars3", "type").unwrap()
        );
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
pub struct Ref {
    pub message: String,
    pub field: String,
}

impl Ref {
    fn new(message: String, field: String) -> Ref {
        Ref { message, field }
    }

    fn str(message: &str, field: &str) -> Ref {
        Ref {
            message: message.to_string(),
            field: field.to_string(),
        }
    }

    fn to(message: &Message, field: &Field) -> Ref {
        Ref::new(message.name.clone(), field.name.clone())
    }
}

impl fmt::Display for Ref {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.message, self.field)
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
pub enum IRExpression {
    Value(Value),
    Variable(Ref),
    Parameter(Ref),
    Binary(BinOp, Box<IRExpression>, Box<IRExpression>),
    Unary(UnaryOp, Box<IRExpression>),
    Match(Ref, Vec<(String, IRExpression)>),
}

impl IRExpression {
    fn from_ast(message: &str, expr: &ast::Expression) -> IRExpression {
        match expr {
            ast::Expression::Value(v) => IRExpression::Value(v.clone()),
            ast::Expression::Variable(field) => IRExpression::Variable(Ref {
                message: message.to_string(),
                field: field.to_string(),
            }),
            // TODO: is this correct?
            ast::Expression::Parameter(field) => IRExpression::Variable(Ref {
                message: message.to_string(),
                field: field.to_string(),
            }),
            ast::Expression::Binary(op, lh, rh) => IRExpression::Binary(
                *op,
                Box::new(IRExpression::from_ast(message, &*lh)),
                Box::new(IRExpression::from_ast(message, &*rh)),
            ),
            ast::Expression::Unary(op, arg) => {
                IRExpression::Unary(*op, Box::new(IRExpression::from_ast(message, &*arg)))
            }
        }
    }

    fn simplify_step(&self) -> Result<IRExpression, String> {
        use self::IRExpression::*;
        use crate::ast::Value::*;

        Ok(match self {
            Binary(op, lh, rh) => match (op, &**lh, &**rh) {
                (BinOp::Plus, Value(Number(l)), Value(Number(r))) => Value(Number(l + r)),
                (BinOp::Minus, Value(Number(l)), Value(Number(r))) => Value(Number(l - r)),
                (BinOp::Multiply, Value(Number(l)), Value(Number(r))) => Value(Number(l * r)),
                (BinOp::Divide, Value(Number(l)), Value(Number(r))) => Value(Number(l / r)),
                (op, Value(Number(_)), Value(r)) => {
                    return Err(format!("{} cannot be applied to {:?}", op, r))
                }
                (op, Value(l), _) => return Err(format!("{} cannot be applied to {:?}", op, l)),
                (op, l @ Binary(..), r @ Binary(..)) => {
                    Binary(*op, Box::new(l.simplify()?), Box::new(r.simplify()?))
                }
                (op, l @ Binary(..), _) => Binary(*op, Box::new(l.simplify()?), rh.clone()),
                (op, _, r @ Binary(..)) => Binary(*op, lh.clone(), Box::new(r.simplify()?)),
                _ => Binary(*op, lh.clone(), rh.clone()),
            },
            v => v.clone(),
        })
    }

    pub fn simplify(&self) -> Result<IRExpression, String> {
        let mut expr = self.clone();
        loop {
            let next = expr.simplify_step()?;
            if next == expr {
                break;
            }
            expr = next;
        }
        Ok(expr)
    }

    pub fn replace(&self, rf: &Ref, rep: &IRExpression) -> IRExpression {
        match self {
            IRExpression::Variable(var) if var == rf => rep.clone(),
            IRExpression::Variable(var) => IRExpression::Variable(var.clone()),
            IRExpression::Binary(op, lh, rh) => IRExpression::Binary(
                *op,
                Box::new(lh.replace(rf, rep)),
                Box::new(rh.replace(rf, rep)),
            ),
            IRExpression::Unary(op, arg) => {
                IRExpression::Unary(*op, Box::new(arg.replace(rf, rep)))
            }
            IRExpression::Match(_, _) => unimplemented!(),
            ex => ex.clone(),
        }
    }
}

impl fmt::Display for IRExpression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        fn print(f: &mut Formatter, expr: &IRExpression) -> fmt::Result {
            match expr {
                e @ IRExpression::Binary(..) => write!(f, "({})", e),
                e => write!(f, "{}", e),
            }
        }

        match self {
            IRExpression::Value(v) => v.fmt(f),
            IRExpression::Variable(v) => write!(f, "{}", v),
            IRExpression::Parameter(v) => write!(f, "{}", v),
            IRExpression::Binary(op, l, r) => {
                print(f, &**l)?;
                write!(f, " {} ", op)?;
                print(f, &**r)
            }
            IRExpression::Unary(op, arg) => write!(f, "{}({})", op, arg),
            IRExpression::Match(v, arms) => {
                writeln!(f, "match {} {{", v)?;
                for arm in arms {
                    writeln!(f, "  {} => {}", arm.0, arm.1)?;
                }
                write!(f, "}}")
            }
        }
    }
}

pub struct Equation {
    lh: IRExpression,
    rh: IRExpression,
}

impl Equation {
    pub fn solve_for(&self, var: &Ref) -> Result<IRExpression, String> {
        fn contains_var(var: &Ref, expr: &IRExpression) -> bool {
            match expr {
                IRExpression::Value(_) => false,
                IRExpression::Variable(v) => v == var,
                IRExpression::Parameter(v) => v == var,
                IRExpression::Binary(_, lh, rh) => contains_var(var, lh) || contains_var(var, rh),
                IRExpression::Unary(_, arg) => contains_var(var, arg),
                IRExpression::Match(_, arms) => *&arms.iter().any(|(_, e)| contains_var(var, e)),
            }
        }

        // make sure the var we're solving for is on the right side

        let (mut lh, mut rh) = match (contains_var(var, &self.lh), contains_var(var, &self.rh)) {
            (true, true) => return Err(format!("Var {} appears on both sides of equation", var)),
            (false, false) => return Err(format!("Var {} do not appear in the equation", var)),
            (true, false) => (self.rh.clone(), self.lh.clone()),
            (false, true) => (self.lh.clone(), self.rh.clone()),
        };

        // now we reduce the expression until only the var is on the right side
        loop {
            let next = match &rh {
                IRExpression::Variable(v) | IRExpression::Parameter(v) => {
                    if v == var {
                        return Ok(lh);
                    } else {
                        panic!(
                            "Somehow ended up with incorrect variable on right in {:?} = {:?}",
                            lh, rh
                        );
                    }
                }
                IRExpression::Value(_) => {
                    panic!(
                        "Somehow ended up with value on right in {:?} = {:?}",
                        lh, rh
                    );
                }
                IRExpression::Binary(op, l, r) => {
                    match (contains_var(var, &l), contains_var(var, &r)) {
                        (true, true) => return Err(format!("found two instances of var {}", var)),
                        (false, false) => panic!("found no instances of var"),
                        (true, false) => {
                            lh = IRExpression::Binary(op.inverse(), Box::new(lh), r.clone());
                            *l.clone()
                        }
                        (false, true) => {
                            lh = IRExpression::Binary(op.inverse(), Box::new(lh), l.clone());
                            *r.clone()
                        }
                    }
                }
                IRExpression::Unary(..) => unimplemented!(),
                IRExpression::Match(..) => unimplemented!(),
            };

            if next == rh {
                panic!("stopped making progress");
            }

            rh = next;
        }
    }
}

type Constraint = (Ref, IRExpression);

fn find_vars(output: &mut Vec<Ref>, expr: &IRExpression) {
    match expr {
        IRExpression::Variable(v) => output.push(v.clone()),
        IRExpression::Binary(_, lh, rh) => {
            find_vars(output, lh);
            find_vars(output, rh);
        }
        _ => {}
    };
}

fn find_constraints(messages: &[Message]) -> Result<Vec<Constraint>, String> {
    let mut cs = vec![];
    for message in messages {
        for field in &message.fields {
            // Handle values
            if let Some(value) = &field.value {
                let this = Ref::to(message, field);

                let expr = IRExpression::from_ast(&message.name, value);

                match expr {
                    IRExpression::Value(_) => cs.push((this, expr)),
                    IRExpression::Variable(var) => {
                        cs.push((var.clone(), IRExpression::Variable(this)))
                    }
                    expr => {
                        let mut vars = vec![];
                        find_vars(&mut vars, &expr);

                        if vars.is_empty() {
                            cs.push((this, expr.simplify()?));
                        } else {
                            let eq = Equation {
                                lh: IRExpression::Variable(this),
                                rh: expr,
                            };

                            for v in vars {
                                cs.push((v.clone(), eq.solve_for(&v)?.simplify()?));
                            }
                        }
                    }
                }
            }

            // Handle arrays
            if let DataType::Array { length, .. } = &field.data_type {
                let this = Ref::to(message, field);

                let expr = IRExpression::from_ast(&message.name, length);
                let mut vars = vec![];
                find_vars(&mut vars, &expr);

                let eq = Equation {
                    lh: IRExpression::Unary(UnaryOp::Len, Box::new(IRExpression::Variable(this))),
                    rh: expr,
                };

                for v in vars {
                    cs.push((v.clone(), eq.solve_for(&v)?.simplify()?));
                }
            }

            // Handle apply_to
            if let Some(target) = &field.apply_to {
                cs.push((
                    Ref::new(message.name.clone(), target.to_string()),
                    IRExpression::Unary(
                        UnaryOp::Serialize,
                        Box::new(IRExpression::Variable(Ref::to(message, field))),
                    ),
                ));

                // handle arguments
                match &field.data_type {
                    DataType::Choose(vs) => {
                        let mut var_map = HashMap::new();

                        for v in vs.iter() {
                            match &v.data_type {
                                DataType::Message { name, args } => {
                                    let m = messages
                                        .iter()
                                        .find(|m| name == &m.name)
                                        .ok_or(format!("could not find message {}", name))?;

                                    for (arg, p) in args.iter().zip(&m.args) {
                                        let mut vars = vec![];

                                        // for each argument to the constructor, we need to find all
                                        // relevant constraints for the variables in those expressions
                                        let lh = IRExpression::from_ast(&message.name, &arg);
                                        find_vars(&mut vars, &lh);

                                        let eq = Equation {
                                            lh,
                                            rh: IRExpression::Parameter(Ref::new(
                                                "__match_value".to_string(),
                                                p.name.clone(),
                                            )),
                                        };

                                        for var in vars {
                                            let r = eq.solve_for(&var)?;
                                            var_map
                                                .entry(var.clone())
                                                .or_insert(vec![])
                                                .push((v.name.clone(), r));
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                        for (k, v) in var_map {
                            if v.len() != vs.len() {
                                return Err(format!("Var {} must appear as a parameter in every variant of the choose", k));
                            }

                            cs.push((k, IRExpression::Match(Ref::to(message, field), v)));
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    Ok(cs)
}

fn ref_field(message: &str, field: &Field) -> Ref {
    Ref {
        message: message.to_string(),
        field: field.name.to_string(),
    }
}

fn expr_for_field(
    field: &Ref,
    cs: &HashMap<Ref, Vec<IRExpression>>,
    complete: &HashSet<Ref>,
) -> Result<IRExpression, String> {
    if complete.contains(&field) {
        return Ok(IRExpression::Variable(field.clone()));
    }

    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();
    if let Some(es) = cs.get(&field) {
        for e in es {
            queue.push_back((0, e.clone()));
        }
    }

    while !queue.is_empty() {
        let (depth, c) = queue.pop_front().unwrap();
        println!("{}", c);
        if visited.contains(&c) || depth > 100 {
            // we're in a loop
            continue;
        }

        visited.insert(c.clone());

        // if all variables in this expression are complete, we're done
        let mut vars = vec![];
        find_vars(&mut vars, &c);
        vars.retain(|r| !complete.contains(r));
        if vars.is_empty() {
            return Ok(c);
        }

        // otherwise, we need to try to expand the expression, so pick a variable and replace it
        // with all possible replacements. if there are none, we're done
        for v in vars {
            for replacement in cs.get(&v).unwrap_or(&vec![]) {
                queue.push_back((depth + 1, c.replace(&v, &replacement)));
            }
        }
    }

    Err(format!(
        "Couldn't find expression to generate field {}.{}",
        field.message, field.field
    ))
}

pub struct IR {
    constraint_map: HashMap<Ref, Vec<IRExpression>>,
    complete: HashSet<Ref>,
}

impl IR {
    pub fn from_ast(protocol: &Protocol) -> Result<IR, String> {
        let constraints = find_constraints(&protocol.messages)?;

        let mut constraint_map = HashMap::new();
        for (r, c) in &constraints {
            constraint_map
                .entry(r.clone())
                .or_insert_with(Vec::new)
                .push(c.clone());
        }

        let complete: HashSet<Ref> = protocol
            .messages
            .iter()
            .flat_map(|m| {
                let name = m.name.clone();
                m.fields
                    .iter()
                    .filter(|f| !f.variable)
                    .map(move |f| ref_field(&name, f))
            })
            .collect();

        Ok(IR {
            constraint_map,
            complete,
        })
    }

    pub fn expr_for_field(&self, message: &str, field: &str) -> Result<IRExpression, String> {
        expr_for_field(
            &Ref {
                message: message.to_string(),
                field: field.to_string(),
            },
            &self.constraint_map,
            &self.complete,
        )
    }
}
