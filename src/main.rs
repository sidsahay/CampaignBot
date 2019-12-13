use std::sync::Mutex;
use std::fs;
use serenity::{
    model::{channel::Message, gateway::Ready},
    prelude::*,
};
use redis::Commands;

struct Handler {
    mtx_client: Mutex<redis::Client>,
}

impl Handler {
    fn new(mtx_client: Mutex<redis::Client>) -> Handler {
        Handler { mtx_client }
    }
}

impl EventHandler for Handler {
    fn message(&self, ctx: Context, msg: Message) {
        let ch = &msg.content[0..1];

        if ch == "$" {
            let ans = campaignbot::evaluate_expression(&msg.content[1..]);

            if let Err(why) = msg.reply(&ctx, &ans) {
                println!("Error sending message: {:?}", why);
            }
        } else if ch == "%" {
            let client = self.mtx_client.lock().unwrap();
            let mut conn = client.get_connection().unwrap();

            let s = &msg.content[1..];
            match s.find('=') {
                None => {
                    let reply = {
                        let val : Option<String> = conn.get(s).unwrap();
                        match val{
                        None => "Error: key not found.".to_owned(),
                        Some(v) => v,
                    }};
                    if let Err(why) = msg.reply(&ctx, reply) {
                        println!("Error sending message: {:?}", why);
                    }
                }
                Some(pos) => {
                    let key = &s[..pos];
                    let val = &s[pos + 1..];
                    let _: () = conn.set(key, val).unwrap();
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
    let client = redis::Client::open("redis://127.0.0.1/").unwrap();
    let mtx_client = Mutex::new(client);

    let token = fs::read_to_string("key.key").expect("Couldn't read key file.");
    let mut client = Client::new(&token, Handler::new(mtx_client)).expect("Error creating client");

    if let Err(why) = client.start() {
        println!("Client error {}", why);
    }
}