use dice::DiceParseError;
use serenity::async_trait;
use serenity::model::channel::Message;
use serenity::prelude::*;
use std::env;

mod dice;

struct Handler;

#[async_trait]
impl EventHandler for Handler {
    async fn message(&self, ctx: Context, msg: Message) {
        if msg.content.starts_with('!') {
            let is_kori = msg.author.id.as_u64() == &159989678559330304;
            match msg.content[1..].parse::<dice::DiceRoll>() {
                Ok(roll) => {
                    let result = roll.roll();
                    let message = format!("{}", result);
                    let message = if message.len() < 1000 {
                        message
                    } else {
                        format!("{} = **{}**", result.roll(), result.total)
                    };
                    if let Err(why) = msg.reply_ping(&ctx.http, message).await {
                        println!("Error sending message: {:?}", why);
                    }
                }
                Err(err) => {
                    match &err {
                        DiceParseError::IllegalExplodeArgument(s, _) => {
                            let err_msg = if is_kori {
                                let react = msg.react(&ctx.http, '😠');
                                if let Err(why) = react.await {
                                    println!("Error reacting to message: {:?}", why);
                                }
                                "Bad 0N1, infinite explosions are forbidden!".to_string()
                            } else {
                                format!(
                                    "Bad dice roll `{}`: explode value must be greater than one",
                                    s
                                )
                            };
                            if let Err(why) = msg.reply_ping(&ctx.http, err_msg).await {
                                println!("Error sending message: {:?}", why);
                            }
                        }
                        DiceParseError::IllegalDropArgument(s, drop) => {
                            let err_msg = if is_kori {
                                let react = msg.react(&ctx.http, '🖕');
                                if let Err(why) = react.await {
                                    println!("Error reacting to message: {:?}", why);
                                }
                                "Greedy robot! You can't take more than you have!".to_string()
                            } else {
                                match drop {
                                    dice::DropRule::KeepHighest(_)
                                    | dice::DropRule::KeepLowest(_) => {
                                        format!("Bad dice roll `{}`: keep count must be less than number of dice", s)
                                    }
                                    dice::DropRule::DropHighest(_)
                                    | dice::DropRule::DropLowest(_) => {
                                        format!("Bad dice roll `{}`: drop count must be less than number of dice", s)
                                    }
                                }
                            };
                            if let Err(why) = msg.reply_ping(&ctx.http, err_msg).await {
                                println!("Error sending message: {:?}", why);
                            }
                        }
                        DiceParseError::MalformedModifiers(s)
                        | DiceParseError::InvalidArgument(s, _) => {
                            let err_msg = if is_kori {
                                let react = msg.react(&ctx.http, '🤭');
                                if let Err(why) = react.await {
                                    println!("Error reacting to message: {:?}", why);
                                }
                                "You're outputting garbage 0N1, I think you need to defragment your memory!".to_string()
                            } else {
                                format!("Bad dice roll `{}`: invalid modifier (should be `r`,`e`,`c`,`k`,`kl`,`kh`,`dl`,`dh`)", s)
                            };
                            if let Err(why) = msg.reply_ping(&ctx.http, err_msg).await {
                                println!("Error sending message: {:?}", why);
                            }
                        }
                        DiceParseError::TooManyDice(s) => {
                            let err_msg = if is_kori {
                                let react = msg.react(&ctx.http, '🤣');
                                if let Err(why) = react.await {
                                    println!("Error reacting to message: {:?}", why);
                                }
                                "Nice try 0N1, you can't DOS me!".to_string()
                            } else {
                                format!("Bad dice roll `{}`: too many dice (max 1024)", s)
                            };
                            if let Err(why) = msg.reply_ping(&ctx.http, err_msg).await {
                                println!("Error sending message: {:?}", why);
                            }
                        }
                        DiceParseError::TooManyFaces(s) => {
                            let err_msg = if is_kori {
                                let react = msg.react(&ctx.http, '🤨');
                                if let Err(why) = react.await {
                                    println!("Error reacting to message: {:?}", why);
                                }
                                "Did you overflow a register 0N1?".to_string()
                            } else {
                                format!("Bad dice roll `{}`: too many faces (max 1048576)", s)
                            };
                            if let Err(why) = msg.reply_ping(&ctx.http, err_msg).await {
                                println!("Error sending message: {:?}", why);
                            }
                        }
                        DiceParseError::InvalidLabel(s) => {
                            let err_msg = if is_kori {
                                let react = msg.react(&ctx.http, '🤭');
                                if let Err(why) = react.await {
                                    println!("Error reacting to message: {:?}", why);
                                }
                                "You're outputting garbage 0N1, I think you need to defragment your memory!".to_string()
                            } else {
                                format!("Bad dice roll `{}`: invalid label (should be single pair of `[]` at end of roll containing label)", s)
                            };
                            if let Err(why) = msg.reply_ping(&ctx.http, err_msg).await {
                                println!("Error sending message: {:?}", why);
                            }
                        }
                        _ => {}
                    }
                    println!("Error parsing dice roll: {:?}", err);
                }
            }
        }
    }
}

#[tokio::main]
async fn main() {
    // Configure the client with your Discord bot token in the environment.
    let token = env::var("DISCORD_TOKEN").expect("Expected a token in the environment");
    // Set gateway intents, which decides what events the bot will be notified about
    let intents = GatewayIntents::GUILD_MESSAGES | GatewayIntents::MESSAGE_CONTENT;

    // Create a new instance of the Client, logging in as a bot. This will
    // automatically prepend your bot token with "Bot ", which is a requirement
    // by Discord for bot users.
    let mut client = Client::builder(&token, intents)
        .event_handler(Handler)
        .await
        .expect("Err creating client");

    // Finally, start a single shard, and start listening to events.
    //
    // Shards will automatically attempt to reconnect, and will perform
    // exponential backoff until it reconnects.
    if let Err(why) = client.start().await {
        println!("Client error: {:?}", why);
    }
}
