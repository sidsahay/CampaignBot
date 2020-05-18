use std::sync::Mutex;
use std::fs;
use serenity::{
    utils::MessageBuilder,
    model::{channel::Message, gateway::Ready},
    prelude::*,
};
use redis::Commands;
use ddg::Query;
use ddg::RelatedTopic::*;

const APP_NAME: &'static str = "CampaignBot";

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
            let cmd = &msg.content[1..2];
            if cmd == "s" {
                let s = &msg.content[3..];
                let query = Query::new(s, APP_NAME).no_html();
                let response = query.execute().unwrap();
                if response.abstract_text != "" {
                    let m = MessageBuilder::new()
                                .push_line_safe(&response.abstract_url)
                                .push_line_safe(&response.abstract_text)
                                .push_line_safe(&response.image)
                                .build();
                    if let Err(why) = msg.reply(&ctx, &m) {
                        println!("Error sending message: {:?}", why);
                    }
                }
                else {
                    for related in response.related_topics {
                        match related {
                            TopicResult(topic_result) => {
                                let m = MessageBuilder::new()
                                            .push_line_safe(&topic_result.first_url)
                                            .push_line_safe(&topic_result.text)
                                            .push_line_safe(&topic_result.icon.url)
                                            .build();

                                if let Err(why) = msg.reply(&ctx, &m) {
                                    println!("Error sending message: {:?}", why);
                                }
                                break;
                            },
                            Topic(_) => (),
                        }
                    }
                }
            }
            else if cmd == "k" {
                let s = &msg.content[3..];

                let client = self.mtx_client.lock().unwrap();
                let mut conn = client.get_connection().unwrap();

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
            else {
                let ans = campaignbot::evaluate_expression(&msg.content[1..]);

                if let Err(why) = msg.reply(&ctx, &ans) {
                    println!("Error sending message: {:?}", why);
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
