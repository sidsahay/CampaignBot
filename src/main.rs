use std::sync::Mutex;
use std::fs;
use serenity::{
    model::{channel::Message, gateway::Ready},
    prelude::*,
};
use std::collections::HashMap;

struct Handler {
    mtx_hashmap: Mutex<HashMap<String, String>>,
}

impl Handler {
    fn new(mtx_hashmap: Mutex<HashMap<String, String>>) -> Handler {
        Handler { mtx_hashmap }
    }
}

impl EventHandler for Handler {
    fn message(&self, ctx: Context, msg: Message) {
        let ch = &msg.content[0..1];

        if ch == "$" {
            /*if let Err(why) = msg.react(&ctx, "👀") {
                println!("Error reacting: {:?}", why);
            }
*/
            let ans = campaignbot::evaluate_expression(&msg.content[1..]);

            if let Err(why) = msg.reply(&ctx, &ans) {
                println!("Error sending message: {:?}", why);
            }
        } else if ch == "%" {
            /*if let Err(why) = msg.react(&ctx, "👀") {
                println!("Error reacting: {:?}", why);
            }
*/
            let mut hashmap = self.mtx_hashmap.lock().unwrap();

            let s = &msg.content[1..];
            match s.find('=') {
                None => {
                    let reply = match (*hashmap).get(s) {
                        None => "Error: key not found.",
                        Some(v) => v,
                    };
                    if let Err(why) = msg.reply(&ctx, reply) {
                        println!("Error sending message: {:?}", why);
                    }
                }
                Some(pos) => {
                    let key = &s[..pos];
                    let val = &s[pos + 1..];
                    (*hashmap).insert(key.to_string(), val.to_string());
                    if let Err(why) = msg.reply(&ctx, "key-val stored.") {
                        println!("Error sending message: {:?}", why);
                    }
                }
            }
        }
    }

    fn ready(&self, _: Context, ready: Ready) {
        println!("{} is connected!", ready.user.name);
    }
}


fn main() {
    let mut hashmap = HashMap::new();
    hashmap.insert("test_key".to_string(), "test_value".to_string());

    let mtx_hashmap = Mutex::new(hashmap);

    let token = fs::read_to_string("key.key").expect("Couldn't read key file.");
    let mut client = Client::new(&token, Handler::new(mtx_hashmap)).expect("Error creating client");

    if let Err(why) = client.start() {
        println!("Client error {}", why);
    }
}