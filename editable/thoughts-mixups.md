# Janet's Thoughts on Mixups

To mix or not to mix, that is the question! Whether 'tis nobler in the mindgames to risk suffering the yomis and parries of outrageous fortune, or to take arms with a rushing river of troubles, etc etc, you probably get the point I'm doing that famous soliloquy from Hamlet but themed around mixups.

There are lots of decisions to make in Fantasy Strike, and one of the most important ones that will often decide entire rounds is whether or not to use a mixup. You heard that right, the decision at play is not which side of the mixup to go for, it's whether to perform the mixup at all!

Any situation where you force your opponent to make a decision on what they expect you to do, where that decision dictates the outcome of that interaction, is a mixup.

<a name="sum-parts">[**Greater than the sum of its parts:**](#sum-parts)</a>

Mixups are a tool to create an opening in your opponent's defenses. Let's use the most basic mixup, strike/throw, as an example: if you always attack, they always block - if you always throw, they always yomi counter! Each option, taken on its own, is just kinda rubbish.

| Attack | Defense |
| -----: | :------ |
| Strike | Block   |
| Throw  | Yomi    |

However, if you *can* do either, and they don't know which you'll go for, they have to predict your behaviour, creating openings when they guess wrong. This is the core foundation of a mixup.

Now, when they predict a throw and yomi counter, you might throw and get yomi countered, but you also might attack and deal heavy damage as they fail to block. When they predict a strike and block, you might attack and at most deal some chip and lose some amount of initiative, but you also might throw, dealing damage and gaining oki! Where each option individually wasn't great, they are each strongest when the other is weak, allowing them to combine very effectively.

| Attack | Defense                        |
| -----: | :----------------------------- |
| Strike | Block or take 1-3 damage       |
| Throw  | Yomi or take 1-2 damage and kd |

This is what makes Rook so strong up close - his basic mixup is between strike, throw, and command throw, adding a third option into the mix. Each option to defend against these - block, yomi counter, and jump, respectively - loses to the other two options! Also, they all give rook oki upon success, making them loop!

To use a marvellous turn of phrase coined by ErickDRedd, Rook plays rock paper scissors against you with two hands.

| Attack        | Defense                                                     |
| ------------: | :---------------------------------------------------------- |
| Strike        | Block or take 1-2 damage and kd (depending on which strike) |
| Throw         | Yomi or take 2 damage and kd                                |
| Command Throw | Jump or take 2 damage and kd                                |

<a name="ev">[**Expected Value:**](#ev)</a>

Each mixup has a certain “expected value”, often shortened to “EV”, which is a fancy term borrowed from probability to describe how good it is on average. An EV of 1 just means that, on average, you'll get 1 damage out of it, or at least 1 more damage than you'll take - this struggles to account for discrete factors like knockdowns that lack a clear amount of value to translate into damage, but it summarises how good a mixup is very effectively despite that!

Usually they won't be specific numbers, as that takes more complicated analysis than is needed to decide whether it's a good option, so no one bothers.

| Expected Value | Translation                                                 | Example             |
| -------------: | :---------------------------------------------------------- | :------------------ |
| Negative       | This is a bad mixup, you take more damage than you deal! D: | Setsuki's Ninjaport |
| Zero           | This mixup is okay, but won't benefit you.                  | Dunno honestly      |
| Positive       | This mixup is great for you, go for it!                     | Strike/Throw        |

Note that this can only apply to the entire mixup, it makes no sense to apply it to an individual option!

Additionally, mixups have a best case scenario and a worst case scenario. The worst case scenario in strike/throw is that you get counterhit and take heavy damage, unless it's on oki where it becomes a frame 1 invulnerable reversal or a yomi counter due to the limited set of options. The best case scenario in strike/throw is that you yourself *deal* the counterhit with a strike, which usually allows you to combo for extra damage! For instance, with DeGrey this best case scenario would normally be 3 damage with nA nAA.

The final, and perhaps most underrated, descriptive value mixups have is variance. This is generally very hard to precisely value, just like the EV, but it hugely effects the usage of a mixup. The lower the variance, the more reliable it is, but generally the less exciting the outcome.

| Variance | Translation                                                              | Example      |
| -------: | :------------------------------------------------------------------------| :----------- |
| High     | If you win the interaction, awesome! If you lose, oh no! Decides rounds. | Quince gB    |
| Low      | If you win the interaction, nice. If you don't, who cares? Very common.  | Strike/Throw |
| Zero     | This isn't a thing.                                                      | Stop it.     |

<a name="weights">[**Weights:**](#weights)</a>

Each option in a mixup has a particular “weight”, which is higher the better it works when it works, lower the worse it works when it doesn't, and then shifted about by your general preferences. This is… vague, but as a general rule, it's most easily figured out by how you expect your opponent to defend. This starts simple, then gets more complicated, but you only need to worry about the simple bit.

Let's return to the most basic example, the strike/throw mixup, and assume you're playing as Grave and that knockdowns magically don't exist (they're not a raw number, so they're hard).

| Option | Damage |
| -----: | :----- |
| Strike | 2      |
| Throw  | 1      |

Looking at this, how do you think your opponent will defend against it? The answer is surprisingly simple: they defend against strikes about twice as much as throws (as long as you don't strike or throw more than they expect). This is great for you, go with it and punish them if they don't defend in this pattern - if they're blocking too often, throw more for bonus damage, and vice versa!

| Option | Chance          |
| -----: | :-------------- |
| Strike | 66.6% (2 in 3)  |
| Throw  | 33.3% (1 in 3)  |

Supplementary material you can, and probably should, skip: [weights, the complicated bit!](thoughts-weights)

<a name="decision">[**To mixup or not to mixup:**](#decision)</a>

Okay, now that you know about what you get out of mixups, you can learn *why* to mixup. A mixup is a way to break through your opponent's defenses, as I mentioned earlier, but they're more than that: they're a decision point which introduces far more possible outcomes in the match! You'll know what that means soon, if I do my job properly.

Without mixups, doing things outside of neutral becomes deterministic. That means that if you leave neutral in a particular way, the same thing will happen every time until you reenter neutral, as long as both players play as well as they can. Mixups are, therefore, a tool to stop this: they allow you to potentially change that predetermined outcome!

If the mixup has 2 different outcomes, then you've branched it off into 2 different outcomes instead of just the one. If that original outcome was bad for you, and one of these new outcomes is good, that's a net benefit, and if the original outcome was good and either of these new outcomes isn't, it's a risk.

As you can see, your decision and your opponent's decision in response is what will define the round from that point onwards. This is called a decision point!

If you're confident you can win the round without introducing decision points, don't - if you aren't, do. This translates to who's advantaged in the round - the winning player avoids mixups at all costs, the losing player uses them. The winning player slowly chips away at their opponent's health, the losing player uses explosive options with high potential damage. The winning player does not risk mixups, and the losing player can't afford not to.

In place of mixups on oki, the winning player uses safejumps. In place of unsafe mixups to open up the opponent's guard, the winning player aims for chip and forcing errors with minimal risk. Because if they can achieve this without returning to neutral, then they have already won.

<a name="note">[**An interesting note:**](#note)</a>

A very noteworthy example of a mixup is DeGrey's callouts against Valerie's cyan magenta yellow/rainbow mixup! It's not great for DeGrey, but it's a fascinating progression of tactics.

1. To start, Valerie can always use yellow, or always use rainbow.
2. DeGrey recognises this, and punishes accordingly - he beats yellow with truth punch for 2 damage, and rainbow with nA for 1 damage.
3. Valerie adapts by mixing between yellow and rainbow, as yellow counterhits nA for 3 damage, and rainbow counterhits truth punch for 3 damage.

As you can see, before DeGrey threatens those options, Valerie is free to do whatever she wants. After, he must never use those options because it's hugely negative EV for him. However, whenever Valerie lets down her guard and fails to perform the mixup properly, DeGrey can call her out with a powerful option that negates her pressure entirely, so he must always be on the lookout for something he should never be able to do.

Dynamics like this are everywhere when you look close enough, and I recommend you do!

<a name="reading">[**Further reading:**](#reading)</a>
* [“Critical Points”](https://www.sirlin.net/ptw-book/8-critical-points), a chapter from Sirlin's ‘Play to Win‘
* The Dustloop wiki's [article on mixups](https://www.dustloop.com/wiki/index.php?title=Mixup)
* DHD's [guide on opening people up](https://docs.google.com/document/d/1R1iAKVb-5rjPbX5CXkp75L3L7-P3Ef3_fgA1Xk6rHV0)

**[Other thoughts on stuff!](/thoughts)**
