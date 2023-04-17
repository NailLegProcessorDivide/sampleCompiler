use itertools::Itertools;
use regex::Regex;
use std::collections::BTreeMap;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum OP {
    Plus,
    Minus,
    Times,
    Div,
    Lshift,
    BitOr,
    BitAnd,
    Lt,
    Gt,
    Eq,
    And,
    Or,
}

#[derive(PartialEq, Clone)]
pub enum Token {
    Num(i64),
    Ident(String), /* identifiers represent various names, including function
                   and variable names */
    Op(OP),
    Uop(UOP),
    /* Punctuation */
    Lparen,
    Rparen,
    Lcurly,
    Rcurly,
    Lbrac,
    Rbrac,
    Colon,
    Comma,
    /* Identifier-like keywords */
    While,
    Do,
    If,
    Then,
    Else,
    Assign,
    True,
    False,
    Input,
    Output,
    Array,
    Int,
    Bool,
    Let,
    Function,
    Return,
}

pub struct TokLoc {
    pub tok: Token,
    pub loc: usize,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum UOP {
    Not,
}

pub const fn show_op(op: &OP) -> &'static str {
    match op {
        OP::Plus => r"+",
        OP::Minus => r"-",
        OP::Times => r"*",
        OP::Div => r"/",
        OP::Lshift => r"<<",
        OP::BitOr => r"|",
        OP::BitAnd => r"&",
        OP::Lt => r"<",
        OP::Gt => r">",
        OP::Eq => r"=",
        OP::And => r"&&",
        OP::Or => r"||",
    }
}

pub const fn show_uop(uop: &UOP) -> &'static str {
    match uop {
        UOP::Not => "!",
    }
}

pub fn show_token_static(token: &Token) -> &'static str {
    match token {
        Token::Op(op) => show_op(op),
        Token::Uop(uop) => show_uop(uop),
        Token::Lparen => r"(",
        Token::Rparen => r")",
        Token::Lcurly => r"{",
        Token::Rcurly => r"}",
        Token::Lbrac => r"[",
        Token::Rbrac => r"]",
        Token::Colon => r":",
        Token::Comma => r",",
        Token::While => r"while",
        Token::Do => r"do",
        Token::If => r"if",
        Token::Then => r"then",
        Token::Else => r"else",
        Token::Assign => r":=",
        Token::True => r"true",
        Token::False => r"false",
        Token::Input => r"input",
        Token::Output => r"output",
        Token::Array => r"array",
        Token::Int => r"int",
        Token::Bool => r"bool",
        Token::Let => r"let",
        Token::Function => r"function",
        Token::Return => r"return",
        _ => todo!(),
    }
}

pub fn show_token(token: &Token) -> String {
    match token {
        Token::Num(val) => val.to_string(),
        Token::Ident(identifier) => identifier.clone(),
        _ => show_token_static(token).to_string(),
    }
}

pub fn print_token_list(token_list: &[TokLoc]) {
    for tokloc in token_list.iter() {
        println!("{}: {}", tokloc.loc, show_token(&tokloc.tok));
    }
}

fn make_keyword_map() -> BTreeMap<&'static str, Token> {
    let mut res = BTreeMap::new();
    res.insert(
        show_token_static(&Token::Uop(UOP::Not)),
        Token::Uop(UOP::Not),
    );

    res.insert(show_token_static(&Token::Op(OP::Plus)), Token::Op(OP::Plus));
    res.insert(
        show_token_static(&Token::Op(OP::Minus)),
        Token::Op(OP::Minus),
    );
    res.insert(
        show_token_static(&Token::Op(OP::Times)),
        Token::Op(OP::Times),
    );
    res.insert(show_token_static(&Token::Op(OP::Div)), Token::Op(OP::Div));
    res.insert(show_token_static(&Token::Op(OP::Lt)), Token::Op(OP::Lt));
    res.insert(show_token_static(&Token::Op(OP::Gt)), Token::Op(OP::Gt));
    res.insert(show_token_static(&Token::Op(OP::Eq)), Token::Op(OP::Eq));
    res.insert(show_token_static(&Token::Op(OP::And)), Token::Op(OP::And));
    res.insert(show_token_static(&Token::Op(OP::Or)), Token::Op(OP::Or));
    res.insert(
        show_token_static(&Token::Op(OP::Lshift)),
        Token::Op(OP::Lshift),
    );
    res.insert(
        show_token_static(&Token::Op(OP::BitOr)),
        Token::Op(OP::BitOr),
    );
    res.insert(
        show_token_static(&Token::Op(OP::BitAnd)),
        Token::Op(OP::BitAnd),
    );

    res.insert(show_token_static(&Token::Do), Token::Do);
    res.insert(show_token_static(&Token::While), Token::While);
    res.insert(show_token_static(&Token::If), Token::If);
    res.insert(show_token_static(&Token::Then), Token::Then);
    res.insert(show_token_static(&Token::Else), Token::Else);
    res.insert(show_token_static(&Token::Array), Token::Array);
    res.insert(show_token_static(&Token::Assign), Token::Assign);
    res.insert(show_token_static(&Token::True), Token::True);
    res.insert(show_token_static(&Token::Input), Token::Input);
    res.insert(show_token_static(&Token::Output), Token::Output);
    res.insert(show_token_static(&Token::False), Token::False);
    res.insert(show_token_static(&Token::Lparen), Token::Lparen);
    res.insert(show_token_static(&Token::Rparen), Token::Rparen);
    res.insert(show_token_static(&Token::Lcurly), Token::Lcurly);
    res.insert(show_token_static(&Token::Rcurly), Token::Rcurly);
    res.insert(show_token_static(&Token::Lbrac), Token::Lbrac);
    res.insert(show_token_static(&Token::Rbrac), Token::Rbrac);
    res.insert(show_token_static(&Token::Int), Token::Int);
    res.insert(show_token_static(&Token::Bool), Token::Bool);
    res.insert(show_token_static(&Token::Colon), Token::Colon);
    res.insert(show_token_static(&Token::Let), Token::Let);
    res.insert(show_token_static(&Token::Return), Token::Return);
    res.insert(show_token_static(&Token::Function), Token::Function);
    res.insert(show_token_static(&Token::Comma), Token::Comma);
    res
}

pub fn lex(s: &str, mut pos: usize, mut line_n: usize) -> Result<Vec<TokLoc>, String> {
    let mut chr: usize = 0;
    //no const deref for string so cant do as const :(
    let keywords: BTreeMap<&'static str, Token> = make_keyword_map();

    let mut keyword_re_string: String = keywords
        .keys()
        .cloned()
        .rev()
        .map(|s| regex::escape(s))
        .intersperse(r"|".to_string())
        .collect();
    keyword_re_string = r"\A(".to_string() + &keyword_re_string + r")";

    let keyword_re: regex::Regex = Regex::new(&keyword_re_string).unwrap();
    let number_re: regex::Regex = Regex::new(r"\A([0-9]+)").unwrap();
    let ident_re: regex::Regex = Regex::new(r"\A([a-zA-Z_][a-zA-Z0-9_]*)").unwrap();
    let space_re: regex::Regex = Regex::new(r"\A([ \t]+|//.*)").unwrap();
    let newline_re: regex::Regex = Regex::new(r"\A(\n|\r\n)").unwrap();
    let mut res: Vec<TokLoc> = Vec::new();
    while pos < s.len() {
        if let Some(white_space) = space_re.find(&s[pos..]) {
            pos += white_space.end();
            chr += white_space.end();
        } else if let Some(newline) = newline_re.find(&s[pos..]) {
            line_n += 1;
            pos += newline.end();
            chr = 0;
        } else if let Some(identifyer) = ident_re.find(&s[pos..]) {
            let id = identifyer.as_str();
            if let Some(token) = keywords.get(id) {
                //println!("OMG IT MATCHED {}", identifyer.as_str());
                res.push(TokLoc {
                    tok: token.clone(),
                    loc: line_n,
                });
            } else {
                //println!("DIDNT MATCHED {}", identifyer.as_str());
                res.push(TokLoc {
                    tok: Token::Ident(id.to_string()),
                    loc: line_n,
                });
            }
            pos += identifyer.end();
            chr += identifyer.end();
        } else if let Some(tok) = keyword_re.find(&s[pos..]) {
            let id = tok.as_str();
            res.push(TokLoc {
                tok: keywords.get(id).unwrap().clone(),
                loc: line_n,
            });
            pos += tok.end();
            chr += tok.end();
        } else if let Some(num) = number_re.find(&s[pos..]) {
            //TODO: handle integers that pass regex and fail parse
            res.push(TokLoc {
                tok: Token::Num(num.as_str().parse::<i64>().unwrap()),
                loc: line_n,
            });
            pos += num.end();
            chr += num.end();
        } else {
            return Err(format!(
                "lex error pos = {}; line = {}:{}\nparser doesnt get here",
                pos,
                line_n + 1,
                chr
            ));
        }
    }
    return Ok(res);
}
