// use petgraph::Graph;

use std::fmt;
use std::fmt::Formatter;

#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;

    #[test]
    fn solve_for() {
        let lh = expression("(@var - 5) * 10;".as_bytes()).unwrap().1;
        let rh = Expression::Value(Value::Number(20));

        let solved = Equation { lh, rh }.solve_for("@var").unwrap();
        assert_eq!("(20 / 10) + 5", solved.to_string());
        assert_eq!("7", solved.simplify().unwrap().to_string());
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
pub enum Op {
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Op::Plus => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Multiply => write!(f, "*"),
            Op::Divide => write!(f, "/"),
        }
    }
}

impl Op {
    fn inverse(&self) -> Op {
        match self {
            Op::Plus => Op::Minus,
            Op::Minus => Op::Plus,
            Op::Multiply => Op::Divide,
            Op::Divide => Op::Multiply,
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum Value {
    String(String),
    ByteArray(Vec<u8>),
    Number(u64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Number(x) => write!(f, "{}", x),
            Value::ByteArray(a) => {
                write!(f, "[")?;

                if !a.is_empty() {
                    for b in &a[..a.len() - 1] {
                        write!(f, "{}, ", b)?;
                    }
                    write!(f, "{}", a[a.len() - 1])?;
                }

                write!(f, "]")
            }
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum Expression {
    Value(Value),
    Variable(String),
    Binop(Op, Box<Expression>, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        fn print(f: &mut Formatter, expr: &Expression) -> fmt::Result {
            match expr {
                e @ Expression::Binop(..) => write!(f, "({})", e),
                e => write!(f, "{}", e),
            }
        }

        match self {
            Expression::Value(v) => v.fmt(f),
            Expression::Variable(v) => write!(f, "{}", v),
            Expression::Binop(op, l, r) => {
                print(f, &**l)?;
                write!(f, " {} ", op)?;
                print(f, &**r)
            }
        }
    }
}

impl Expression {
    fn simplify_step(&self) -> Result<Expression, String> {
        use Expression::*;
        use Value::*;

        Ok(match self {
            Binop(op, lh, rh) => match (op, &**lh, &**rh) {
                (Op::Plus, Value(Number(l)), Value(Number(r))) => Value(Number(l + r)),
                (Op::Minus, Value(Number(l)), Value(Number(r))) => Value(Number(l - r)),
                (Op::Multiply, Value(Number(l)), Value(Number(r))) => Value(Number(l * r)),
                (Op::Divide, Value(Number(l)), Value(Number(r))) => Value(Number(l / r)),
                (op, Value(Number(_)), Value(r)) => {
                    return Err(format!("{} cannot be applied to {:?}", op, r))
                }
                (op, Value(l), _) => return Err(format!("{} cannot be applied to {:?}", op, l)),
                (op, l @ Binop(..), r @ Binop(..)) => {
                    Binop(*op, Box::new(l.simplify()?), Box::new(r.simplify()?))
                }
                (op, l @ Binop(..), _) => Binop(*op, Box::new(l.simplify()?), rh.clone()),
                (op, _, r @ Binop(..)) => Binop(*op, lh.clone(), Box::new(r.simplify()?)),
                _ => Binop(*op, lh.clone(), rh.clone()),
            },
            v => v.clone(),
        })
    }

    pub fn simplify(&self) -> Result<Expression, String> {
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
}

pub struct Equation {
    lh: Expression,
    rh: Expression,
}

impl Equation {
    pub fn solve_for(&self, var: &str) -> Result<Expression, String> {
        fn contains_var(var: &str, expr: &Expression) -> bool {
            match expr {
                Expression::Value(_) => false,
                Expression::Variable(v) => v == var,
                Expression::Binop(_, lh, rh) => contains_var(var, lh) || contains_var(var, rh),
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
                Expression::Variable(v) => {
                    if v == var {
                        return Ok(lh);
                    } else {
                        panic!(
                            "Somehow ended up with incorrect variable on right in {:?} = {:?}",
                            lh, rh
                        );
                    }
                }
                Expression::Value(_) => {
                    panic!(
                        "Somehow ended up with value on right in {:?} = {:?}",
                        lh, rh
                    );
                }
                Expression::Binop(op, l, r) => {
                    match (contains_var(var, &l), contains_var(var, &r)) {
                        (true, true) => return Err(format!("found two instances of var {}", var)),
                        (false, false) => panic!("found no instances of var"),
                        (true, false) => {
                            lh = Expression::Binop(op.inverse(), Box::new(lh), r.clone());
                            *l.clone()
                        }
                        (false, true) => {
                            lh = Expression::Binop(op.inverse(), Box::new(lh), l.clone());
                            *r.clone()
                        }
                    }
                }
            };

            if next == rh {
                panic!("stopped making progress");
            }

            rh = next;
        }

        unreachable!();

        //        match self {
        //            v @ Expression::Value(_) => v.clone(),
        //            v @ Expression::Variable(_) => v.clone(),
        //            Expression::Binop(op, lh, rh) => {
        //
        //
        //            }
        //        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum DataType {
    Value(String),
    Array {
        data_type: Box<DataType>,
        length: Expression,
    },
    Message {
        name: String,
        args: Vec<Expression>,
    },
    ManyCombinator {
        data_type: Box<DataType>,
    },
    RestCombinator,
    Choose(Vec<ChooseVariant>),
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Field {
    pub public: bool,
    pub variable: bool,
    pub name: String,
    pub apply_to: Option<String>,
    pub data_type: DataType,
    pub value: Option<Expression>,
    pub constraints: Option<Vec<Expression>>,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct ChooseVariant {
    pub name: String,
    pub data_type: DataType,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct Arg {
    pub public: bool,
    pub name: String,
    pub data_type: DataType,
    pub value: Option<Value>,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Message {
    pub name: String,
    pub args: Vec<Arg>,
    pub fields: Vec<Field>,
}

enum Edge {
    ApplyTo,
    Expression(Expression),
}

//fn graph(message: &Message) {
//    let mut graph = Graph::<&str, Edge>::new();
//
//    for f in message.fields {
//        graph.add_node(&f.name);
//    }
//
//     for f in message.fields {
//         if f.apply_to
//     }
//}
