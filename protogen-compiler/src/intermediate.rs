// use petgraph::Graph;

use crate::parser;

use parser::*;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;

    #[test]
    fn solve_for() {
        let pexpr = parser::expression("(@var - 5) * 10;".as_bytes()).unwrap().1;
        let lh = Expression::from_parser("message", &pexpr);
        let rh = Expression::Value(Value::Number(20));

        let solved = Equation { lh, rh }
            .solve_for(&Ref::new("message".to_string(), "var".to_string()))
            .unwrap();
        assert_eq!("(20 / 10) + 5", solved.to_string());
        assert_eq!("7", solved.simplify().unwrap().to_string());
    }

    #[test]
    fn test_graph() {
        let mut messages = vec![];

        messages.push(
            parser::message(
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
            .1,
        );

        messages.push(
            parser::message(
                "subchunk = {
  @id: [u8; 4];
  @size: u32;
  @data: [u8; @size];
  public subchunk: apply @data choose {
    FormatSubchunk = fmt_subchunk(@id) |
    DataSubchunk = data_subchunk(@id)
  };
}
"
                .as_bytes(),
            )
            .unwrap()
            .1,
        );

        messages.push(
            parser::message(
                "fmt_subchunk ($id: [u8; 4] = b\"fmt \") = {
  public audio_format: u16;
  public num_channels: u16;
  public sample_rate: u32;
  public byte_rate: u32;
  public block_align: u16;
  public bits_per_sample: u16;
}
"
                .as_bytes(),
            )
            .unwrap()
            .1,
        );

        messages.push(
            parser::message(
                "data_subchunk ($id: [u8; 4] = b\"data\") = {
  public data: rest!();
}
"
                .as_bytes(),
            )
            .unwrap()
            .1,
        );

        for c in find_constraints(&messages).unwrap() {
            println!("{:15} | {}", c.0.to_string(), c.1);
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct Ref {
    message: String,
    field: String,
}

impl Ref {
    fn new(message: String, field: String) -> Ref {
        Ref { message, field }
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

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum Expression {
    Value(Value),
    Variable(Ref),
    Binary(BinOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Match(Ref, Vec<(String, String)>),
}

impl Expression {
    fn from_parser(message: &str, expr: &parser::Expression) -> Expression {
        match expr {
            parser::Expression::Value(v) => Expression::Value(v.clone()),
            parser::Expression::Variable(field) => Expression::Variable(Ref {
                message: message.to_string(),
                field: field[1..].to_string(),
            }),
            parser::Expression::Binary(op, lh, rh) => Expression::Binary(
                *op,
                Box::new(Expression::from_parser(message, &*lh)),
                Box::new(Expression::from_parser(message, &*rh)),
            ),
            parser::Expression::Unary(op, arg) => {
                Expression::Unary(*op, Box::new(Expression::from_parser(message, &*arg)))
            }
        }
    }

    fn simplify_step(&self) -> Result<Expression, String> {
        use self::Expression::*;
        use parser::Value::*;

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
            Expression::Match(v, arms) => {
                write!(f, "match {} {{\n", v)?;
                for arm in arms {
                    write!(f, "  {} => {}\n", arm.0, arm.1)?;
                }
                write!(f, "}}")
            }
        }
    }
}

pub struct Equation {
    lh: Expression,
    rh: Expression,
}

impl Equation {
    pub fn solve_for(&self, var: &Ref) -> Result<Expression, String> {
        fn contains_var(var: &Ref, expr: &Expression) -> bool {
            match expr {
                Expression::Value(_) => false,
                Expression::Variable(v) => v == var,
                Expression::Binary(_, lh, rh) => contains_var(var, lh) || contains_var(var, rh),
                Expression::Unary(_, arg) => contains_var(var, arg),
                Expression::Match(_, _) => false,
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
                Expression::Unary(..) => unimplemented!(),
                Expression::Match(..) => unimplemented!(),
            };

            if next == rh {
                panic!("stopped making progress");
            }

            rh = next;
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Edge {
    ApplyTo,
    Expression(Expression),
    Len(Expression),
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Node<'a> {
    message: &'a str,
    field: &'a str,
}

impl<'a> Node<'a> {
    fn new(message: &'a str, field: &'a str) -> Node<'a> {
        Node { message, field }
    }
}

type Constraint = (Ref, Expression);

fn find_vars(output: &mut Vec<Ref>, expr: &Expression) {
    match expr {
        Expression::Variable(v) => output.push(v.clone()),
        Expression::Binary(_, lh, rh) => {
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

                let expr = Expression::from_parser(&message.name, value);

                match expr {
                    Expression::Value(_) => cs.push((this, expr)),
                    Expression::Variable(var) => cs.push((var.clone(), Expression::Variable(this))),
                    expr => {
                        let mut vars = vec![];
                        find_vars(&mut vars, &expr);

                        if vars.is_empty() {
                            cs.push((this, expr.simplify()?));
                        } else if vars.len() > 1 {
                            return Err(format!(
                                "Found multiple variables in {}; this is not yet supported",
                                this
                            ));
                        } else {
                            let eq = Equation {
                                lh: Expression::Variable(this),
                                rh: expr,
                            };
                            cs.push((vars[0].clone(), eq.solve_for(&vars[0])?.simplify()?));
                        }
                    }
                }
            }

            // Handle arrays
            if let DataType::Array { length, .. } = &field.data_type {
                let this = Ref::to(message, field);

                let expr = Expression::from_parser(&message.name, length);
                let mut vars = vec![];
                find_vars(&mut vars, &expr);

                if vars.len() > 1 {
                    return Err(format!(
                        "Found multiple variables in {}; this is not yet supported",
                        this
                    ));
                } else if !vars.is_empty() {
                    let eq = Equation {
                        lh: Expression::Unary(UnaryOp::Len, Box::new(Expression::Variable(this))),
                        rh: expr,
                    };
                    cs.push((vars[0].clone(), eq.solve_for(&vars[0])?.simplify()?));
                }
            }

            // Handle apply_to
            if let Some(target) = &field.apply_to {
                cs.push((
                    Ref::new(message.name.clone(), target[1..].to_string()),
                    Expression::Unary(
                        UnaryOp::Serialize,
                        Box::new(Expression::Variable(Ref::to(message, field))),
                    ),
                ));

                // handle arguments
                match &field.data_type {
                    DataType::Choose(vs) => {
                        let mut arms = vec![];
                        let mut var = None;
                        for v in vs.iter() {
                            match &v.data_type {
                                DataType::Message { name, args } => {
                                    // TODO this is so hacky
                                    let mut vars = vec![];

                                    if args.len() != 1 {
                                        return Err(format!(
                                            "Only unary messages supported right now"
                                        ));
                                    }

                                    for arg in args {
                                        find_vars(
                                            &mut vars,
                                            &Expression::from_parser(&message.name, &arg),
                                        );
                                    }
                                    if vars.len() != 1 {
                                        return Err(format!(
                                            "Expected 1 variable in {:?}",
                                            v.data_type
                                        ));
                                    }

                                    let m = messages
                                        .iter()
                                        .find(|m| name == &m.name)
                                        .ok_or(format!("could not find message {}", name))?;

                                    println!("Var {:?} ", vars[0]);
                                    if var.is_some() {
                                        if var.unwrap() != vars[0] {
                                            return Err(format!(
                                                "All vars in choose currently must be the same"
                                            ));
                                        }
                                    }

                                    var = Some(vars[0].clone());

                                    arms.push((
                                        v.name.clone(),
                                        m.args.get(0).unwrap().name.clone(),
                                    ));
                                }
                                _ => {}
                            }
                        }

                        if let Some(var) = var {
                            cs.push((
                                var.clone(),
                                Expression::Match(Ref::to(message, field), arms),
                            ));
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    Ok(cs)
}

// Map of (node, edges *from* the node)

#[derive(Debug, Eq, PartialEq)]
struct FieldGraph<'a> {
    map: HashMap<Node<'a>, Vec<(Node<'a>, Edge)>>,
}

//impl<'a> FieldGraph<'a> {
//    pub fn outgoing_edges(&'a self, node: &'a Node) -> &'a [(Node<'a>, Edge)] {
//        self.map.get(node).map(|x| x.as_ref()).unwrap_or(&[][..])
//    }
//
//    pub fn add_edge(&'a mut self, source: Node<'a>, target: Node<'a>, edge: Edge) {
//        self.map
//            .entry(source)
//            .or_insert_with(|| vec![])
//            .push((target, edge));
//    }
//
//    pub fn construct(messages: &'a [Message]) -> FieldGraph<'a> {
//        let mut graph: FieldGraph = FieldGraph {
//            map: HashMap::new(),
//        };
//
//        for message in messages {
//            for f in &message.fields {
//                graph.map.insert(Node::new(&message.name, &f.name), vec![]);
//            }
//
//            for f in &message.fields {
//                // types of edges:
//
//                // Handle apply to edges
//                if let Some(v) = &f.apply_to {
//                    graph.add_edge(
//                        Node::new(&message.name, &v[1..]),
//                        Node::new(&message.name, &f.name),
//                        Edge::ApplyTo,
//                    );
//                }
//
//                fn find_vars(output: &mut Vec<String>, expr: &Expression) {
//                    match expr {
//                        Expression::Variable(v) => output.push(v.clone()),
//                        Expression::Binary(_, lh, rh) => {
//                            find_vars(output, lh);
//                            find_vars(output, rh);
//                        }
//                        _ => {}
//                    };
//                }
//
//                // handle assignment edges
//                if let Some(expr) = &f.value {
//                    let mut vars = vec![];
//                    find_vars(&mut vars, &expr);
//                    if vars.len() > 1 {
//                        panic!("expressions with more than one variable are not yet supported");
//                    }
//
//                    if vars.len() == 1 {
//                        let mut var = Expression::Variable(format!("@{}", field.name));
//
//                        //                        if is_len {
//                        //                            var = Expression::Unary(UnaryOp::Len, Box::new(var));
//                        //                        }
//
//                        let eq = Equation {
//                            lh: expr.clone(),
//                            rh: var,
//                        };
//
//                        let expr = eq.solve_for(&vars[0]).unwrap().simplify().unwrap();
//
//                        if vars[0].starts_with("$") {
//                            // this is a variable
//                        }
//
//                        graph.add_edge(
//                            Node::new(&message.name, &v[1..]),
//                            Node::new(&message.name, &f.name),
//                            Edge::ApplyTo,
//                        );
//
//                        let edges = graph.get_mut(&vars[0][1..]).unwrap();
//
//                        edges.push((field.name.clone(), Edge::Expression(expr)));
//                    }
//                }
//
//                //                fn handle_expr(graph: &mut FieldGraph, field: &Field, expr: &Expression, is_len: bool) {
//                //                    fn find_vars(output: &mut Vec<String>, expr: &Expression) {
//                //                        match expr {
//                //                            Expression::Variable(v) => output.push(v.clone()),
//                //                            Expression::Binary(_, lh, rh) => {
//                //                                find_vars(output, lh);
//                //                                find_vars(output, rh);
//                //                            }
//                //                            _ => {}
//                //                        };
//                //                    }
//                //
//                //                    let mut vars = vec![];
//                //                    find_vars(&mut vars, &expr);
//                //                    if vars.len() > 1 {
//                //                        panic!("expressions with more than one variable are not yet supported");
//                //                    }
//                //
//                //                    if vars.len() == 1 {
//                //                        let mut var = Expression::Variable(format!("@{}", field.name));
//                //
//                //                        if is_len {
//                //                            var = Expression::Unary(UnaryOp::Len, Box::new(var));
//                //                        }
//                //
//                //                        let eq = Equation {
//                //                            lh: expr.clone(),
//                //                            rh: var,
//                //                        };
//                //
//                //                        let expr = eq.solve_for(&vars[0]).unwrap().simplify().unwrap();
//                //
//                //                        let edges = graph.get_mut(&vars[0][1..]).unwrap();
//                //                        edges.push((field.name.clone(), Edge::Expression(expr)));
//                //                    }
//                //                }
//                //
//                //
//                //
//                //                if let DataType::Array { length, .. } = &f.data_type {
//                //                    handle_expr(&mut graph, f, length, true);
//                //                }
//            }
//        }
//        graph
//    }
//}
