// use petgraph::Graph;

use std::collections::HashMap;
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

    #[test]
    fn test_graph() {
        let message: Message = message(
            "wave = {
  chunk_id: [u8; 4] | [b\"RIFF\"];
  @chunk_size: u32;
  public format: [u8; 4] | [b\"WAVE\"];
  @data_size: u32 = @chunk_size - 8;
  @data: [u8; @data_size];
  public chunks: apply @data many!(subchunk());
}
"
            .as_bytes(),
        )
        .unwrap()
        .1;

        println!("{:#?}", message.graph());
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
pub enum BinOp {
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            BinOp::Plus => write!(f, "+"),
            BinOp::Minus => write!(f, "-"),
            BinOp::Multiply => write!(f, "*"),
            BinOp::Divide => write!(f, "/"),
        }
    }
}

impl BinOp {
    pub fn inverse(&self) -> BinOp {
        match self {
            BinOp::Plus => BinOp::Minus,
            BinOp::Minus => BinOp::Plus,
            BinOp::Multiply => BinOp::Divide,
            BinOp::Divide => BinOp::Multiply,
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
pub enum UnaryOp {
    Len,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            UnaryOp::Len => write!(f, "len"),
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
    Binary(BinOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        fn print(f: &mut Formatter, expr: &Expression) -> fmt::Result {
            match expr {
                e @ Expression::Binary(..) => write!(f, "({})", e),
                e => write!(f, "{}", e),
            }
        }

        match self {
            Expression::Value(v) => v.fmt(f),
            Expression::Variable(v) => write!(f, "{}", v),
            Expression::Binary(op, l, r) => {
                print(f, &**l)?;
                write!(f, " {} ", op)?;
                print(f, &**r)
            }
            Expression::Unary(op, arg) => write!(f, "{}({})", op, arg),
        }
    }
}

impl Expression {
    fn simplify_step(&self) -> Result<Expression, String> {
        use Expression::*;
        use Value::*;

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
                Expression::Binary(_, lh, rh) => contains_var(var, lh) || contains_var(var, rh),
                Expression::Unary(_, arg) => contains_var(var, arg),
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
                Expression::Binary(op, l, r) => {
                    match (contains_var(var, &l), contains_var(var, &r)) {
                        (true, true) => return Err(format!("found two instances of var {}", var)),
                        (false, false) => panic!("found no instances of var"),
                        (true, false) => {
                            lh = Expression::Binary(op.inverse(), Box::new(lh), r.clone());
                            *l.clone()
                        }
                        (false, true) => {
                            lh = Expression::Binary(op.inverse(), Box::new(lh), l.clone());
                            *r.clone()
                        }
                    }
                }
                Expression::Unary(UnaryOp::Len, ..) => unimplemented!(),
            };

            if next == rh {
                panic!("stopped making progress");
            }

            rh = next;
        }
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

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Edge {
    ApplyTo,
    Expression(Expression),
}

type FieldGraph = HashMap<String, Vec<(String, Edge)>>;

impl Message {
    pub fn graph(&self) -> FieldGraph {
        let mut graph: FieldGraph = HashMap::new();

        for f in &self.fields {
            graph.insert(f.name.clone(), vec![]);
        }

        for f in &self.fields {
            fn handle_expr(graph: &mut FieldGraph, field: &Field, expr: &Expression, is_len: bool) {
                fn find_vars(output: &mut Vec<String>, expr: &Expression) {
                    match expr {
                        Expression::Variable(v) => output.push(v.clone()),
                        Expression::Binary(_, lh, rh) => {
                            find_vars(output, lh);
                            find_vars(output, rh);
                        }
                        _ => {}
                    };
                }

                let mut vars = vec![];
                find_vars(&mut vars, &expr);
                if vars.len() > 1 {
                    panic!("expressions with more than one variable are not yet supported");
                }

                if vars.len() == 1 {
                    let mut var = Expression::Variable(format!("@{}", field.name));

                    if is_len {
                        var = Expression::Unary(UnaryOp::Len, Box::new(var));
                    }

                    let eq = Equation {
                        lh: expr.clone(),
                        rh: var,
                    };

                    let expr = eq.solve_for(&vars[0]).unwrap().simplify().unwrap();

                    let edges = graph.get_mut(&vars[0][1..]).unwrap();
                    edges.push((field.name.clone(), Edge::Expression(expr)));
                }
            }

            if let Some(v) = &f.apply_to {
                graph
                    .get_mut(&v[1..])
                    .unwrap()
                    .push((f.name.clone(), Edge::ApplyTo));
            }

            if let Some(expr) = &f.value {
                handle_expr(&mut graph, f, expr, false);
            }

            if let DataType::Array { length, .. } = &f.data_type {
                handle_expr(&mut graph, f, length, true);
            }
        }
        graph
    }
}
