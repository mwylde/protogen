use std::fmt;
use std::fmt::Formatter;

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
    Serialize,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            UnaryOp::Len => write!(f, "len"),
            UnaryOp::Serialize => write!(f, "serialize"),
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
