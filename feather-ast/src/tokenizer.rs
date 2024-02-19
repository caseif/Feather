use lazy_static::lazy_static;
use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize};
use std::cmp;
use std::fmt::{Display, Formatter};

#[derive(Deserialize)]
struct TokenDef {
    id: String,
    #[serde(deserialize_with = "deserialize_regex")]
    regex: Regex,
}

#[derive(Deserialize)]
struct TokenDefs {
    token_types: Vec<TokenDef>,
}

#[derive(Serialize)]
pub struct Token {
    #[serde(rename = "type")]
    type_id: String,
    value: Option<String>,
    line: usize,
    col: usize,
    len: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Some(val) => write!(f, "{}({})", self.type_id, val),
            None => write!(f, "{}", self.type_id),
        }
    }
}

#[derive(Serialize)]
pub struct TokenList {
    tokens: Vec<Token>,
}

pub struct InvalidTokenError {
    pub line: usize,
    pub col: usize,
    pub line_content: String,
}

lazy_static! {
    static ref TOKEN_DEFS: Vec<TokenDef> = serde_json::from_str::<TokenDefs>(include_str!["../res/token_types.json"])
            .unwrap().token_types;
}

fn deserialize_regex<'de, D>(deserializer: D) -> Result<Regex, D::Error>
    where D: Deserializer<'de>,
{
    let s: String = Deserialize::deserialize(deserializer)?;
    Regex::new(&s).map_err(serde::de::Error::custom)
}

fn compile_regex_unchecked(re: &str) -> Regex {
    Regex::new(re).expect("Failed to compile regular expression")
}

pub fn tokenize(str: &String) -> Result<TokenList, InvalidTokenError> {
    let token_defs = &TOKEN_DEFS;

    let mut tokens = Vec::<Token>::new();
    let mut cursor = 0;

    let ws_regex = compile_regex_unchecked(r"^\s+");

    while cursor < str.len() {
        while let Some(m) = ws_regex.find(&str[cursor..]) {
            cursor += m.end();
        }

        if cursor == str.len() {
            break;
        }

        let mut found_token = false;

        let last_newline_pos = str[0..cursor].rfind("\n").unwrap_or(0);
        let next_newline_pos = cursor + str[cursor..].find("\n").unwrap_or(str.len());
        let line_num = str[0..cursor].chars().filter(|c| c == &'\n').count() + 1;
        let col_num = cmp::max(1, cursor - last_newline_pos);

        for token_def in token_defs.iter() {
            match token_def.regex.captures(&str[cursor..]) {
                Some(m) => {
                    cursor += m.get(0).expect("Regex match does not contain any groups").end();
                    let value = if m.len() > 1 {
                        Some(m.get(1).expect("Regex match does not contain explicit capture").as_str().to_string())
                    } else {
                        None
                    };

                    tokens.push(Token {
                        type_id: token_def.id.to_owned(),
                        value,
                        line: line_num,
                        col: col_num,
                        len: m.get(0).expect("Regex match does not contain any groups").len(),
                    });

                    found_token = true;
                    break;
                },
                None => {
                    continue;
                }
            }
        }

        if !found_token {
            return Err(InvalidTokenError {
                line: line_num,
                col: col_num,
                line_content: str[(last_newline_pos + 1)..next_newline_pos].to_string(),
            });
        }
    }

    return Ok(TokenList { tokens });
}
