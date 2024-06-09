use edeas_blessing::{DiceParseError, DiceRoll, DropRule};
use serenity::async_trait;
use serenity::model::channel::Message;
use serenity::prelude::*;
use std::env;

struct Handler;

#[async_trait]
impl EventHandler for Handler {
    async fn message(&self, ctx: Context, msg: Message) {
        if msg.content.starts_with('!') {
            let is_kori = msg.author.id.as_u64() == &159989678559330304;
            let response = match msg.content[1..].parse::<DiceRoll>() {
                Ok(roll) => {
                    let result = roll.roll();
                    let message = format!("{}", result);
                    let message = if message.len() < 1000 {
                        message
                    } else {
                        format!("{} = **{}**", result.roll(), result.total)
                    };
                    Some(message)
                }
                Err(err) => match &err {
                    DiceParseError::IllegalExplodeArgument(s, _) => Some(if is_kori {
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
                    }),
                    DiceParseError::IllegalDropArgument(s, drop) => Some(if is_kori {
                        let react = msg.react(&ctx.http, '🖕');
                        if let Err(why) = react.await {
                            println!("Error reacting to message: {:?}", why);
                        }
                        "Greedy robot! You can't take more than you have!".to_string()
                    } else {
                        match drop {
                            DropRule::KeepHighest(_) | DropRule::KeepLowest(_) => {
                                format!("Bad dice roll `{}`: keep count must be less than number of dice", s)
                            }
                            DropRule::DropHighest(_) | DropRule::DropLowest(_) => {
                                format!("Bad dice roll `{}`: drop count must be less than number of dice", s)
                            }
                        }
                    }),
                    DiceParseError::MalformedModifiers(s)
                    | DiceParseError::InvalidArgument(s, _) => Some(if is_kori {
                        let react = msg.react(&ctx.http, '🤭');
                        if let Err(why) = react.await {
                            println!("Error reacting to message: {:?}", why);
                        }
                        "You're outputting garbage 0N1, I think you need to defragment your memory!"
                            .to_string()
                    } else {
                        format!("Bad dice roll `{}`: invalid modifier (should be `r`,`e`,`c`,`k`,`kl`,`kh`,`dl`,`dh`)", s)
                    }),
                    DiceParseError::TooManyDice(s) => Some(if is_kori {
                        let react = msg.react(&ctx.http, '🤣');
                        if let Err(why) = react.await {
                            println!("Error reacting to message: {:?}", why);
                        }
                        "Nice try 0N1, you can't DOS me!".to_string()
                    } else {
                        format!("Bad dice roll `{}`: too many dice (max 1024)", s)
                    }),
                    DiceParseError::TooManyFaces(s) => Some(if is_kori {
                        let react = msg.react(&ctx.http, '🤨');
                        if let Err(why) = react.await {
                            println!("Error reacting to message: {:?}", why);
                        }
                        "Did you overflow a register 0N1?".to_string()
                    } else {
                        format!("Bad dice roll `{}`: too many faces (max 1048576)", s)
                    }),
                    DiceParseError::NotEnoughFaces(s) => Some(if is_kori {
                        let react = msg.react(&ctx.http, '🤨');
                        if let Err(why) = react.await {
                            println!("Error reacting to message: {:?}", why);
                        }
                        "Did you underflow a register 0N1?".to_string()
                    } else {
                        format!("Bad dice roll `{}`: dice need at least one face", s)
                    }),
                    DiceParseError::InvalidLabel(s) => Some(if is_kori {
                        let react = msg.react(&ctx.http, '🤭');
                        if let Err(why) = react.await {
                            println!("Error reacting to message: {:?}", why);
                        }
                        "You're outputting garbage 0N1, I think you need to defragment your memory!"
                            .to_string()
                    } else {
                        format!("Bad dice roll `{}`: invalid label (should be single pair of `[]` at end of roll containing label)", s)
                    }),
                    DiceParseError::NoDFound(s) if s.parse::<f32>().is_ok() => Some(if is_kori {
                        let react = msg.react(&ctx.http, '🤔');
                        if let Err(why) = react.await {
                            println!("Error reacting to message: {:?}", why);
                        }
                        "No matter how many points you float, you can't trick me into a rounding error 0N1!"
                            .to_string()
                    } else {
                        format!("Bad input `{}`: numbers must be integers", s)
                    }),
                    _ => None,
                },
            };
            if let Some(message) = response {
                if let Err(why) = msg.reply_ping(&ctx.http, message).await {
                    println!("Error sending message: {:?}", why);
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
