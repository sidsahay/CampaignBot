# CampaignBot
Discord bot for playing TTRPG campaigns

This bot evaluates expressions with a custom `d` operator acting as a dice roll. So for example, `(1+2)d(3*3)` is essentially `3d9`. Arbitrary expressions using the usual mathematical operators, brackets, and the D&D style `d` operator are supported (uses a recursive descent parser with operator precedence, via Pest). To roll, type `${expression}` into Discord, for example `$2d4+2 - 1`.
It also has a Redis backend for storing string key-value pairs. To store a key-val pair, type `%key=val`, to retrieve type `%key`.

## Running the bot
The code expects your Discord bot token in a file called "key.key" in the same directory as the `Cargo.toml` file. It also expects a Redis server running at localhost with no password. Just compile and run via Cargo.
