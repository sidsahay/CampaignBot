use pest::Parser;
use pest::prec_climber::*;
use pest_derive::Parser;
use pest::iterators::{Pairs, Pair};
use lazy_static::lazy_static;
use std::str::FromStr;

extern crate QuantumRandom;

#[derive(Parser)]
#[grammar = "expression.pest"]
pub struct ExpressionParser;

#[derive(Default, Clone)]
pub struct LoggedResult {
    value: i64,
    log: String,
}

impl LoggedResult {
    fn add(&mut self, other: &LoggedResult) {
        self.value = self.value + other.value;
        self.log.push_str(&other.log);
    }

    fn sub(&mut self, other: &LoggedResult) {
        self.value = self.value - other.value;
        self.log.push_str(&other.log);
    }

    fn mul(&mut self, other: &LoggedResult) {
        self.value = self.value * other.value;
        self.log.push_str(&other.log);
    }

    fn div(&mut self, other: &LoggedResult) {
        if other.value == 0 {
            self.value = 0;
            self.log.push_str("\nInvalid operation: division by zero")
        } else {
            self.value = self.value / other.value;
            self.log.push_str(&other.log);
        }
    }

    fn roll(&mut self, other: &LoggedResult) {
        let rolled = roll_dice(self.value, other.value);
        self.value = rolled.value;
        self.log.push_str(&other.log);
        self.log.push('\n');
        self.log.push_str(&rolled.log);
    }
}

fn str_to_logged_result(s: &str) -> Result<LoggedResult, &str> {
    match FromStr::from_str(s) {
        Ok(i) => Ok(LoggedResult { value: i, log: "".to_string() }),
        Err(_e) => Err("Number parsing error.")
    }
}

fn roll_dice(num: i64, value: i64) -> LoggedResult {
    if num < 0 || value < 1 {
        let mut s = String::from("Invalid operation in ");
        s.push_str(&num.to_string());
        s.push('d');
        s.push_str(&value.to_string());
        s.push_str(": x < 0 || y < 1 in xdy");

        return LoggedResult { value: 0, log: s }
    } else if num > 1024 {
        return LoggedResult { value: 0, log: "Too many required random numbers for the QRNG.".to_string() }
    }

    let randoms = QuantumRandom::random::next_u64s(num as u32).expect("QRNG failed.");

    let mut rolls = String::new();
    rolls.push_str(&num.to_string());
    rolls.push_str("d");
    rolls.push_str(&value.to_string());
    rolls.push_str(" rolled");

    let mut sum: i64 = 0;

    for n in randoms {
        let n_shifted = 1 + ((n / 2) as i64) % value;
        sum = sum + n_shifted;
        rolls.push_str(" ");
        rolls.push_str(&n_shifted.to_string());
    }

    LoggedResult { value: sum, log: rolls }
}

pub fn stringify(l: LoggedResult) -> String {
    let mut s = l.value.to_string();
    s.push_str(&l.log);
    s
}

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left),
            Operator::new(roll, Left)
        ])
    };
}

fn eval(expression: Pairs<Rule>) -> LoggedResult {
    PREC_CLIMBER.climb(
        expression,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::num => str_to_logged_result(pair.as_str()).unwrap(),
            Rule::expr => eval(pair.into_inner()),
            _ => unreachable!(),
        },
        |lhs: LoggedResult, op: Pair<Rule>, rhs: LoggedResult| {
            let mut lhs = lhs;
            match op.as_rule() {
                Rule::add => {lhs.add(&rhs); lhs},
                Rule::subtract => {lhs.sub(&rhs); lhs},
                Rule::multiply => {lhs.mul(&rhs); lhs},
                Rule::divide => {lhs.div(&rhs); lhs},
                Rule::roll => {lhs.roll(&rhs); lhs},
                _ => unreachable!(),
            }
        },
    )
}

pub fn evaluate_expression(s: &str) -> String {
    match ExpressionParser::parse(Rule::calculation, s) {
        Ok(exprs) => stringify(eval(exprs)),
        Err(e) => format!("{}", e)
    }
}
