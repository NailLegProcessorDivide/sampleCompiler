use regex::Regex;
use std::collections::HashMap;
use itertools::Itertools;

#[derive(Clone, PartialEq)]
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
    Or
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

#[derive(Clone, PartialEq)]
pub enum UOP {
    Not
}

const fn show_op(op: &OP) -> &'static str {
    match op {
        OP::Plus => "+",
        OP::Minus => "-",
        OP::Times => "*",
        OP::Div => "/",
        OP::Lshift => "<<",
        OP::BitOr => "|",
        OP::BitAnd => "&",
        OP::Lt => "<",
        OP::Gt => ">",
        OP::Eq => "=",
        OP::And => "&&",
        OP::Or => "||",
    }
}

const fn show_uop(uop : &UOP) -> &'static str {
    match uop {
        UOP::Not => "uop",
    }
}

fn show_token_static(token: &Token) -> &'static str {
    match token {
        Token::Op(op) => show_op(op),
        Token::Uop(uop) => show_uop(uop),
        Token::Lparen => "(",
        Token::Rparen => ")",
        Token::Lcurly => "{",
        Token::Rcurly => "}",
        Token::Lbrac => "[",
        Token::Rbrac => "]",
        Token::Colon => ":",
        Token::Comma => ",",
        Token::While => "while",
        Token::Do => "do",
        Token::If => "if",
        Token::Then => "then",
        Token::Else => "else",
        Token::Assign => ":=",
        Token::True => "true",
        Token::False => "false",
        Token::Input => "input",
        Token::Output => "output",
        Token::Array => "array",
        Token::Int => "int",
        Token::Bool => "bool",
        Token::Let => "let",
        Token::Function => "function",
        Token::Return => "return",
        _ => todo!(),
    }
}

fn show_token(token: &Token) -> String {
    match token {
        Token::Num(val) => val.to_string(),
        Token::Ident(identifier) => identifier.clone(),
        _ => show_token_static(token).to_string(),
    }
}

pub fn print_token_list(token_list : &Vec<TokLoc>) {
    for tokloc in token_list.iter() {
        println!("{}: {}", tokloc.loc, show_token(&tokloc.tok));
    }
}

fn make_keyword_map() -> HashMap<&'static str, Token>{
    let mut res = HashMap::new();
    res.insert(show_token_static(&Token::Uop(UOP::Not)), Token::Uop(UOP::Not));

    res.insert(show_token_static(&Token::Op(OP::Plus)), Token::Op(OP::Plus));
    res.insert(show_token_static(&Token::Op(OP::Minus)), Token::Op(OP::Minus));
    res.insert(show_token_static(&Token::Op(OP::Times)), Token::Op(OP::Times));
    res.insert(show_token_static(&Token::Op(OP::Div)), Token::Op(OP::Div));
    res.insert(show_token_static(&Token::Op(OP::Lt)), Token::Op(OP::Lt));
    res.insert(show_token_static(&Token::Op(OP::Gt)), Token::Op(OP::Gt));
    res.insert(show_token_static(&Token::Op(OP::Eq)), Token::Op(OP::Eq));
    res.insert(show_token_static(&Token::Op(OP::And)), Token::Op(OP::And));
    res.insert(show_token_static(&Token::Op(OP::Or)), Token::Op(OP::Or));
    res.insert(show_token_static(&Token::Op(OP::Lshift)), Token::Op(OP::Lshift));
    res.insert(show_token_static(&Token::Op(OP::BitOr)), Token::Op(OP::BitOr));
    res.insert(show_token_static(&Token::Op(OP::BitAnd)), Token::Op(OP::BitAnd));

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

pub fn lex(s: &str, mut pos: usize, mut line_n: usize) -> Vec<TokLoc> {
    //no const deref for string so cant do as const :(
    let keywords : HashMap<&'static str, Token> = make_keyword_map();

    let mut keyword_re_string : String = keywords.keys().cloned().map(|s| regex::escape(s)).intersperse(r"|".to_string()).collect();
    keyword_re_string = r"\A(".to_string() + &keyword_re_string + r")";

    let keyword_re : regex::Regex = Regex::new(&keyword_re_string).unwrap();
    let number_re : regex::Regex = Regex::new(r"\A([0-9]+)").unwrap();
    let ident_re : regex::Regex = Regex::new(r"\A([a-zA-Z_][a-zA-Z0-9_]*)").unwrap();
    let space_re : regex::Regex = Regex::new(r"\A([ \t]+|//.*)").unwrap();
    let newline_re : regex::Regex = Regex::new(r"\A(\n|\r\n)").unwrap();
    let mut res : Vec<TokLoc> = Vec::new();
    while pos < s.len() {
        if let Some(white_space) = space_re.find(&s[pos..]) {
            pos += white_space.end();
        }
        else if let Some(newline) = newline_re.find(&s[pos..]) {
            line_n += 1;
            pos += newline.end();
        }
        else if let Some(identifyer) = ident_re.find(&s[pos..]) {
            let id = identifyer.as_str();
            if let Some(token) = keywords.get(id) {
                res.push(TokLoc {tok: token.clone(), loc: line_n});
            }
            else {
                res.push(TokLoc {tok: Token::Ident(id.to_string()), loc: line_n});
            }
            pos += identifyer.end();
        }
        else if let Some(tok) = keyword_re.find(&s[pos..]) {
            let name = tok.as_str();
            res.push(TokLoc {tok: Token::Ident(name.to_string()), loc: line_n});
            pos += tok.end();
        }
        else if let Some(num) = number_re.find(&s[pos..]) {
            //TODO: handle integers that pass regex and fail parse
            res.push(TokLoc {tok: Token::Num(num.as_str().parse::<i64>().unwrap()), loc: line_n});
            pos += num.end();
        }
        else {
            println!("lex error pos = {}; line = {}", pos, line_n);
            panic!("parser doesnt get here");
        }
    }
    return res;

}
