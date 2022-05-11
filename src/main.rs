use std::{
    collections::HashMap,
    fmt::{self, format},
    hash::Hash,
    io,
    num::ParseFloatError,
    string,
};

#[derive(Clone)]
enum RispExp {
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp, RispError>),
}

#[derive(Debug)]
enum RispError {
    Reason(String),
}

#[derive(Clone)]
struct RispEnv {
    data: HashMap<String, RispExp>,
}

fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse<'a>(tokens: &'a [String]) -> Result<(RispExp, &'a [String]), RispError> {
    let (token, rest) = tokens
        .split_first()
        .ok_or(RispError::Reason("could not get token".to_string()))?;

    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(RispError::Reason("unexpected `)`".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq<'a>(tokens: &'a [String]) -> Result<(RispExp, &'a [String]), RispError> {
    let mut res: Vec<RispExp> = vec![];
    let mut xs = tokens;
    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(RispError::Reason("could not find closing `)`".to_string()))?;

        if next_token == ")" {
            return Ok((RispExp::List(res), rest));
        }

        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

fn parse_atom(token: &str) -> RispExp {
    let potential_float: Result<f64, ParseFloatError> = token.parse();
    match potential_float {
        Ok(v) => RispExp::Number(v),
        Err(_) => RispExp::Symbol(token.to_string().clone()),
    }
}

fn parse_single_float(exp: &RispExp) -> Result<f64, RispError> {
    match exp {
        RispExp::Number(num) => Ok(*num),
        _ => Err(RispError::Reason("expected a number".to_string())),
    }
}

fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>, RispError> {
    args.iter().map(|x| parse_single_float(x)).collect()
}

fn default_env() -> RispEnv {
    let mut data: HashMap<String, RispExp> = HashMap::new();
    data.insert(
        "+".to_string(),
        RispExp::Func(|args: &[RispExp]| -> Result<RispExp, RispError> {
            let sum = parse_list_of_floats(args)?
                .iter()
                .fold(0.0, |sum, a| sum + a);
            Ok(RispExp::Number(sum))
        }),
    );

    data.insert(
        "-".to_string(),
        RispExp::Func(|args: &[RispExp]| -> Result<RispExp, RispError> {
            let floats = parse_list_of_floats(args)?;
            let first = *floats.first().ok_or(RispError::Reason(
                "Expected at least one number".to_string(),
            ))?;
            let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);
            Ok(RispExp::Number(first - sum_of_rest))
        }),
    );

    RispEnv { data: data }
}

fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp, RispError> {
    match exp {
        RispExp::Symbol(k) => env
            .data
            .get(k)
            .ok_or(RispError::Reason(format!("unexpected symbol k=`{}`", k)))
            .map(|x| x.clone()),
        RispExp::Number(_a) => Ok(exp.clone()),
        RispExp::List(list) => {
            let first_form = list
                .first()
                .ok_or(RispError::Reason("expected a non-empty list".to_string()))?;
            let arg_forms = &list[1..];
            let first_eval = eval(first_form, env)?;
            match first_eval {
                RispExp::Func(f) => {
                    let args_eval = arg_forms
                        .iter()
                        .map(|x| eval(x, env))
                        .collect::<Result<Vec<RispExp>, RispError>>();
                    f(&args_eval?)
                }
                _ => Err(RispError::Reason(
                    "first form must be a function".to_string(),
                )),
            }
        }
        RispExp::Func(_) => Err(RispError::Reason("unexpected form".to_string())),
    }
}

impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            RispExp::Symbol(s) => s.clone(),
            RispExp::Number(n) => n.to_string(),
            RispExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            RispExp::Func(_) => "Function {}".to_string(),
        };
        write!(f, "{}", &str)
    }
}

fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExp, RispError> {
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;
    Ok(evaled_exp)
}

fn slurp_expr() -> String {
    let mut expr = String::new();
    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");
    expr
}

fn main() {
    let env = &mut default_env();
    loop {
        println!("risp >");
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!("// 🔥 => {}", res),
            Err(e) => match e {
                RispError::Reason(msg) => println!("// 🐱 => {}", msg),
            },
        }
    }
}
