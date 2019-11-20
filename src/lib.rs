pub mod parsers {
    use std::str::FromStr;
    use nom::{
        IResult,
        branch::alt,
        combinator::map_res,
        character::complete::char,
        bytes::complete::tag,
        character::complete::{digit1 as digit, space0 as space},
        multi::fold_many0,
        sequence::{delimited, pair},
    };

    #[derive(Default, Clone)]
    pub struct LoggedResult {
        value: i64,
        log: String,
    }

    fn str_to_logged_result(s: &str) -> Result<LoggedResult, &str> {
        match FromStr::from_str(s) {
            Ok(i) => Ok(LoggedResult { value: i, log: "".to_string() }),
            Err(_e) => Err("Number parsing error.")
        }
    }

    fn parens(i: &str) -> IResult<&str, LoggedResult> {
        delimited(
            space,
            delimited(
                tag("("),
                expr,
                tag(")"),
            ),
            space,
        )(i)
    }

    fn factor(i: &str) -> IResult<&str, LoggedResult> {
        alt((
            map_res(delimited(space, digit, space), str_to_logged_result),
            parens
        )
        )(i)
    }

    use rand::distributions::{Distribution, Uniform};

    fn roll_dice(num: i64, value: i64) -> LoggedResult {
        let mut rng = rand::thread_rng();
        let die = Uniform::new_inclusive(1, value);

        let mut rolls = String::new();
        rolls.push_str(&num.to_string());
        rolls.push_str("d");
        rolls.push_str(&value.to_string());
        rolls.push_str(" rolled");

        let mut sum: i64 = 0;

        for _ in 0..num {
            let n = die.sample(&mut rng);
            sum = sum + n;
            rolls.push_str(" ");
            rolls.push_str(&n.to_string());
        }

        LoggedResult { value: sum, log: rolls }
    }

    fn roll_term(i: &str) -> IResult<&str, LoggedResult> {
        let (i, init) = factor(i)?;

        fold_many0(
            pair(char('d'), factor),
            init,
            |acc, (_, val): (char, LoggedResult)| {
                let mut dice_result = roll_dice(acc.value, val.value);
                dice_result.log.push_str("\n");
                dice_result.log.push_str(&acc.log);
                dice_result
            },
        )(i)
    }

    fn term(i: &str) -> IResult<&str, LoggedResult> {
        let (i, init) = roll_term(i)?;

        fold_many0(
            pair(alt((char('*'), char('/'))), roll_term),
            init,
            |acc, (op, val): (char, LoggedResult)| {
                let new_val = if op == '*' { acc.value * val.value } else { acc.value / val.value };
                let mut acc = acc;
                acc.value = new_val;
                acc.log.push_str(&val.log);
                acc
            },
        )(i)
    }

    fn expr(i: &str) -> IResult<&str, LoggedResult> {
        let (i, init) = term(i)?;

        fold_many0(
            pair(alt((char('+'), char('-'))), term),
            init,
            |acc, (op, val): (char, LoggedResult)| {
                let new_val = if op == '+' { acc.value + val.value } else { acc.value - val.value };
                let mut acc = acc;
                acc.value = new_val;
                acc.log.push_str(&val.log);
                acc
            },
        )(i)
    }

    pub fn evaluate(s: &str) -> Result<LoggedResult, &str> {
        match expr(s) {
            Ok((_, res)) => Ok(res),
            Err(_) => Err("Parser error."),
        }
    }

    pub fn stringify(l: LoggedResult) -> String {
        let mut s = l.value.to_string();
        s.push_str("\n");
        s.push_str(&l.log);
        s
    }
}