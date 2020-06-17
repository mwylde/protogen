use std::fmt;
use std::intrinsics::write_bytes;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct StructField {
    pub name: String,
    pub data_type: String,
}

impl fmt::Display for StructField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.data_type)
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]\n"
        )?;
        write!(f, "pub struct {} {{\n", self.name)?;

        for field in &self.fields {
            write!(f, "    {},\n", field)?;
        }

        write!(f, "}}")
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub data_type: String,
}

impl fmt::Display for EnumVariant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.name, self.data_type)
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

impl fmt::Display for Enum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#[allow(non_camel_case_types)]\n")?;
        write!(
            f,
            "#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]\n"
        )?;
        write!(f, "pub enum {} {{\n", self.name)?;

        for field in &self.variants {
            write!(f, "    {},\n", field)?;
        }

        write!(f, "}}")
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum RustValue {
    Number(i64),
    Byte(u8),
    String(String),
    Variable(String),
}

impl fmt::Display for RustValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RustValue::Number(d) => write!(f, "0x{:X}", d),
            RustValue::Byte(b) => write!(f, "{}u8", b),
            RustValue::String(s) => write!(f, "\"{}\"", s),
            RustValue::Variable(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum RustExpression {
    Value(RustValue),
    Array(Vec<RustExpression>),
    ArrayRef(Box<RustExpression>),
    Struct {
        name: String,
        fields: Vec<(String, RustExpression)>,
    },
    FunctionCall {
        name: String,
        parameters: Vec<RustExpression>,
    },
    MethodCall {
        target: Box<RustExpression>,
        name: String,
        parameters: Vec<RustExpression>,
    },
    If {
        condition: Box<RustExpression>,
        true_block: Box<RustExpression>,
        false_block: Option<Box<RustExpression>>,
    },
    Block {
        expressions: Vec<RustExpression>,
        terminated: bool,
    },
    Let {
        name: String,
        typ: Option<String>,
        value: Box<RustExpression>,
    },
    Cast {
        expression: Box<RustExpression>,
        typ: String,
    },
    BinOp {
        op: String,
        lh: Box<RustExpression>,
        rh: Box<RustExpression>,
    },
    Return(Box<RustExpression>),
}

impl fmt::Display for RustExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RustExpression::Value(value) => write!(f, "{}", value),
            RustExpression::Array(v) => {
                write!(f, "[")?;
                for e in v {
                    write!(f, "{}, ", e)?;
                }
                write!(f, "]")
            }
            RustExpression::ArrayRef(a) => write!(f, "&[{}][..]", a),
            RustExpression::FunctionCall { name, parameters } => {
                write!(f, "{}(", name)?;
                for p in parameters {
                    write!(f, "{}, ", p)?;
                }
                write!(f, ")")
            }
            RustExpression::MethodCall {
                target,
                name,
                parameters,
            } => {
                write!(f, "({}).{}(", target, name)?;
                for p in parameters {
                    write!(f, "{}, ", p)?;
                }
                write!(f, ")")
            }
            RustExpression::If {
                condition,
                true_block,
                false_block,
            } => {
                write!(f, "if {} {{\n{}\n}}", condition, true_block)?;
                if let Some(fb) = false_block {
                    write!(f, " else {{\n{}\n}}", fb)?;
                }

                Ok(())
            }
            RustExpression::Block {
                expressions,
                terminated,
            } => {
                let (last, es) = expressions.split_last().unwrap();
                for e in es {
                    write!(f, "    {};\n", e)?;
                }
                write!(f, "    {}", last)?;
                if *terminated {
                    write!(f, ";")?;
                }
                Ok(())
            }
            RustExpression::Let { name, typ, value } => {
                write!(f, "let {}", name)?;
                if let Some(t) = typ {
                    write!(f, ": {}", t)?;
                }
                write!(f, " = {}", value)
            }
            RustExpression::Cast { expression, typ } => write!(f, "{} as {}", expression, typ),
            RustExpression::BinOp { op, lh, rh } => write!(f, "({}) {} ({})", lh, op, rh),
            RustExpression::Return(e) => write!(f, "return {}", e),
            RustExpression::Struct { name, fields } => {
                write!(f, "{} {{\n", name)?;
                for (n, v) in fields {
                    write!(f, "    {}: {},\n", n, v)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Function {
    pub name: String,
    pub public: bool,
    pub generics: Vec<String>,
    pub args: Vec<String>,
    pub return_type: Option<String>,
    pub body: Vec<RustExpression>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}fn {}{}({})",
            if self.public { "pub " } else { "" },
            self.name,
            if !self.generics.is_empty() {
                format!("<{}>", self.generics.join(", "))
            } else {
                String::new()
            },
            self.args.join(", ")
        )?;

        if let Some(t) = &self.return_type {
            write!(f, " -> {}", t)?;
        }

        write!(f, " {{\n")?;

        for l in &self.body {
            write!(f, "    {}\n", l)?;
        }

        write!(f, "}}")
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Impl {
    pub struct_name: String,
    pub trait_name: Option<String>,
    pub functions: Vec<Function>,
}

impl fmt::Display for Impl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "impl ")?;
        if let Some(t) = &self.trait_name {
            write!(f, "{} for ", t)?;
        }

        write!(f, "{} {{\n", self.struct_name)?;

        for function in &self.functions {
            let s = format!("{}", function);
            for l in s.split("\n") {
                write!(f, "    {}\n", l)?;
            }
            write!(f, "\n")?;
        }

        write!(f, "}}")
    }
}
