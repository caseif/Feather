use std::cmp;
use regex::Regex;
use std::fmt::{Display, Formatter};

struct TokenDef {
    id: String,
    regex: Regex,
}

pub struct Token {
    type_id: String,
    value: Option<String>,
    line: usize,
    col: usize,
    len: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Some(val) => write!(f, "{}({}) (:{}:{}) ({})", self.type_id, val, self.line, self.col, self.len),
            None => write!(f, "{} (:{}:{}) ({})", self.type_id, self.line, self.col, self.len),
        }
    }
}

pub struct InvalidTokenError {
    pub line: usize,
    pub col: usize,
    pub line_content: String,
}

fn compile_regex_unchecked(re: &str) -> Regex {
    Regex::new(re).expect("Failed to compile regular expression")
}

fn get_token_defs() -> Vec<TokenDef> {
    vec![
        TokenDef { id: "string_literal".to_string(), regex: compile_regex_unchecked(r#"^"((?:[^"\\\n]|\\.)*)""#) },
        TokenDef { id: "decimal_literal".to_string(), regex: compile_regex_unchecked(r"^(\d+\.\d+)") },
        TokenDef { id: "integer_literal".to_string(), regex: compile_regex_unchecked(r"^(\d+)") },
        TokenDef { id: "hex_literal".to_string(), regex: compile_regex_unchecked(r"^(0x[0-9A-Fa-f]+)") },
        TokenDef { id: "import".to_string(), regex: compile_regex_unchecked(r"^import") },
        TokenDef { id: "function_def".to_string(), regex: compile_regex_unchecked(r"^fn") },
        TokenDef { id: "var_decl".to_string(), regex: compile_regex_unchecked(r"^let") },
        TokenDef { id: "const_qualifier".to_string(), regex: compile_regex_unchecked(r"^const\b") },
        TokenDef { id: "else_if".to_string(), regex: compile_regex_unchecked(r"^else if") },
        TokenDef { id: "if".to_string(), regex: compile_regex_unchecked(r"^if") },
        TokenDef { id: "else".to_string(), regex: compile_regex_unchecked(r"^else") },
        TokenDef { id: "for".to_string(), regex: compile_regex_unchecked(r"^for") },
        TokenDef { id: "while".to_string(), regex: compile_regex_unchecked(r"^while") },
        TokenDef { id: "nil".to_string(), regex: compile_regex_unchecked(r"^nil") },
        TokenDef { id: "range".to_string(), regex: compile_regex_unchecked(r"^\.\.") },
        TokenDef { id: "range_inclusive".to_string(), regex: compile_regex_unchecked(r"^\.\.=") },
        TokenDef { id: "open_paren".to_string(), regex: compile_regex_unchecked(r"^\(") },
        TokenDef { id: "close_paren".to_string(), regex: compile_regex_unchecked(r"^\)") },
        TokenDef { id: "open_brace".to_string(), regex: compile_regex_unchecked(r"^\{") },
        TokenDef { id: "close_brace".to_string(), regex: compile_regex_unchecked(r"^}") },
        TokenDef { id: "begin_decorator".to_string(), regex: compile_regex_unchecked(r"^\[\[") },
        TokenDef { id: "end_decorator".to_string(), regex: compile_regex_unchecked(r"^]]") },
        TokenDef { id: "open_bracket".to_string(), regex: compile_regex_unchecked(r"^\[") },
        TokenDef { id: "close_bracket".to_string(), regex: compile_regex_unchecked(r"^]") },
        TokenDef { id: "static_accessor".to_string(), regex: compile_regex_unchecked(r"^::") },
        TokenDef { id: "instance_accessor".to_string(), regex: compile_regex_unchecked(r"^\.") },
        TokenDef { id: "comma".to_string(), regex: compile_regex_unchecked(r"^,") },
        TokenDef { id: "right_arrow".to_string(), regex: compile_regex_unchecked(r"^->") },
        TokenDef { id: "equals".to_string(), regex: compile_regex_unchecked(r"^==") },
        TokenDef { id: "not_equals".to_string(), regex: compile_regex_unchecked(r"^!=") },
        TokenDef { id: "less_than".to_string(), regex: compile_regex_unchecked(r"^<") },
        TokenDef { id: "less_equal".to_string(), regex: compile_regex_unchecked(r"^<=") },
        TokenDef { id: "greater_than".to_string(), regex: compile_regex_unchecked(r"^>") },
        TokenDef { id: "greater_equal".to_string(), regex: compile_regex_unchecked(r"^>=") },
        TokenDef { id: "and".to_string(), regex: compile_regex_unchecked(r"^&&") },
        TokenDef { id: "or".to_string(), regex: compile_regex_unchecked(r"^\|\|") },
        TokenDef { id: "not".to_string(), regex: compile_regex_unchecked(r"^!") },
        TokenDef { id: "assign".to_string(), regex: compile_regex_unchecked(r"^=") },
        TokenDef { id: "plus_assign".to_string(), regex: compile_regex_unchecked(r"^\+=") },
        TokenDef { id: "minus_assign".to_string(), regex: compile_regex_unchecked(r"^-=") },
        TokenDef { id: "multiply_assign".to_string(), regex: compile_regex_unchecked(r"^\*=") },
        TokenDef { id: "divide_assign".to_string(), regex: compile_regex_unchecked(r"^/=") },
        TokenDef { id: "plus".to_string(), regex: compile_regex_unchecked(r"^\+") },
        TokenDef { id: "minus".to_string(), regex: compile_regex_unchecked(r"^-") },
        TokenDef { id: "multiply".to_string(), regex: compile_regex_unchecked(r"^\*") },
        TokenDef { id: "divide".to_string(), regex: compile_regex_unchecked(r"^/") },
        TokenDef { id: "identifier".to_string(), regex: compile_regex_unchecked(r"^([A-Za-z0-9_]+)") },
    ]
}

pub fn tokenize(str: &String) -> Result<Vec<Token>, InvalidTokenError> {
    let token_defs = get_token_defs();

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

    return Ok(tokens);
}
