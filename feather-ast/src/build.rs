use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::path::Path;
use std::process::exit;
use serde::{Deserialize, Serialize};
use featherparse::{LRAction, Production, SerializableActionEntry, SerializableGotoEntry};

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let action_path = Path::new(&out_dir).join("action_table.json");
    let goto_path = Path::new(&out_dir).join("goto_table.json");

    let (action_table, goto_table) = featherparse::build_parsing_tables();

    let mut serialized_actions: HashMap<String, Vec<SerializableActionEntry>> = HashMap::new();
    let mut serialized_gotos: HashMap<String, Vec<SerializableGotoEntry>> = HashMap::new();

    for (key, action) in action_table {
        let (shift_state, reduce_prod) = match action {
            LRAction::Shift(state) => (Some(state), None),
            LRAction::Reduce(prod) => (None, Some(prod.clone())),
            LRAction::Accept() => (None, None),
        };
        serialized_actions.entry(key.token).or_insert(Vec::new()).push(SerializableActionEntry {
            state: key.state,
            shift_state,
            reduce_prod,
        });
    }

    for (key, goto_state) in goto_table {
        serialized_gotos.entry(key.symbol).or_insert(Vec::new()).push(SerializableGotoEntry {
            cur_state: key.state,
            goto_state: goto_state });
    }

    let action_file = File::create(action_path).unwrap();
    if let Err(e) = serde_json::to_writer_pretty(action_file, &serialized_actions) {
        eprintln!("Failed to write to output file: {e}");
        exit(exitcode::IOERR);
    }

    let goto_file = File::create(goto_path).unwrap();
    if let Err(e) = serde_json::to_writer_pretty(goto_file, &serialized_gotos) {
        eprintln!("Failed to write to output file: {e}");
        exit(exitcode::IOERR);
    }
}
