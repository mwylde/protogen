use std::fmt;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct StructField {
    pub name: String,
    pub data_type: String,
}

impl fmt::Display for StructField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.data_type)
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
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

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub data_type: String,
}

impl fmt::Display for EnumVariant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.name, self.data_type)
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
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

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
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

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum Destructurer {
    Struct(String, Vec<String>),
    TupleStruct(String, Vec<String>),
}

impl fmt::Display for Destructurer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Destructurer::Struct(name, fields) => {
                write!(f, "{} {{\n", name)?;
                for n in fields {
                    write!(f, "    {},\n", n)?;
                }
                write!(f, "}}")
            }
            Destructurer::TupleStruct(name, fields) => {
                write!(f, "{} (", name)?;
                for n in fields {
                    write!(f, "{}, ", n)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum RustExpression {
    Value(RustValue),
    Array(Vec<RustExpression>),
    ArrayRef(Box<RustExpression>),
    Range(Box<RustExpression>, Box<RustExpression>),
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
    Field {
        target: Box<RustExpression>,
        name: String,
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
        is_mut: bool,
        name: String,
        typ: Option<String>,
        value: Box<RustExpression>,
    },
    TupleLet {
        is_mut: bool,
        names: Vec<String>,
        types: Option<Vec<String>>,
        value: Box<RustExpression>,
    },
    Assign(Box<RustExpression>, Box<RustExpression>),
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
    Loop(Box<RustExpression>),
    ForIn {
        i: Box<RustExpression>,
        iter: Box<RustExpression>,
        body: Box<RustExpression>,
    },
    Tuple(Vec<RustExpression>),
    Ref(Box<RustExpression>),
    Postfix(&'static str, Box<RustExpression>),
    Match(Box<RustExpression>, Vec<(Destructurer, RustExpression)>),
    Break,
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
            RustExpression::ArrayRef(a) => write!(f, "&{}[..]", a),
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
            RustExpression::Field { target, name } => write!(f, "({}).{}", target, name),
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
                write!(f, "{{\n")?;
                if let Some((last, es)) = expressions.split_last() {
                    for e in es {
                        write!(f, "    {};\n", e)?;
                    }
                    write!(f, "    {}", last)?;
                    if *terminated {
                        write!(f, ";")?;
                    }
                }
                write!(f, "}}\n")?;
                Ok(())
            }
            RustExpression::Let {
                is_mut,
                name,
                typ,
                value,
            } => {
                write!(f, "let ")?;
                if *is_mut {
                    write!(f, "mut ")?;
                }
                write!(f, "{}", name)?;
                if let Some(t) = typ {
                    write!(f, ": {}", t)?;
                }
                write!(f, " = {}", value)
            }
            RustExpression::TupleLet {
                is_mut,
                names,
                types,
                value,
            } => {
                write!(f, "let ")?;
                if *is_mut {
                    write!(f, "mut ")?;
                }
                write!(f, "(")?;
                for name in names {
                    write!(f, "{},", name)?;
                }
                write!(f, ")")?;

                if let Some(ts) = types {
                    write!(f, ": (")?;
                    for t in ts {
                        write!(f, "{},", t)?;
                    }
                    write!(f, ") ")?;
                }
                write!(f, " = {}", value)
            }
            RustExpression::Assign(to, from) => write!(f, "{} = {}", to, from),
            RustExpression::Cast { expression, typ } => write!(f, "({}) as {}", expression, typ),
            RustExpression::BinOp { op, lh, rh } => write!(f, "({}) {} ({})", lh, op, rh),
            RustExpression::Return(e) => write!(f, "return {}", e),
            RustExpression::Struct { name, fields } => {
                write!(f, "{} {{\n", name)?;
                for (n, v) in fields {
                    write!(f, "    {}: {},\n", n, v)?;
                }
                write!(f, "}}")
            }
            RustExpression::Tuple(items) => {
                write!(f, "(")?;
                for item in items {
                    write!(f, "{}, ", item)?;
                }
                write!(f, ")")
            }
            RustExpression::Loop(body) => write!(f, "loop {{\n{}\n}}", body),
            RustExpression::Ref(e) => write!(f, "&{}", e),
            RustExpression::Postfix(op, expr) => write!(f, "{}{}", expr, op),
            RustExpression::Range(from, to) => write!(f, "({})..({})", from, to),
            RustExpression::ForIn { i, iter, body } => {
                write!(f, "for {} in {} {{\n{}\n}}", i, iter, body)
            }
            RustExpression::Match(target, variants) => {
                write!(f, "match {} {{\n", target)?;
                for (d, b) in variants {
                    write!(f, "    {} => {},\n", d, b)?;
                }
                write!(f, "}}")
            }
            RustExpression::Break => write!(f, "break"),
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct Function {
    pub name: String,
    pub public: bool,
    pub generics: Vec<String>,
    pub args: Vec<String>,
    pub return_type: Option<String>,
    pub body: RustExpression,
    pub annotations: Vec<String>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for annotation in &self.annotations {
            write!(f, "#[{}]\n", annotation)?;
        }

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

        match &self.body {
            RustExpression::Block { .. } => write!(f, "\n{}\n", self.body),
            _ => write!(f, " {{\n{}\n}}", self.body),
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
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
