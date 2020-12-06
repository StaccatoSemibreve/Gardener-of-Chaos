# Weights, the Complicated Bit

Beware, for here be maths. [Click here](thoughts-mixups) to return. This is my second write-up of The Maths, because there is a neat piece of information that makes the maths side of things a lot simpler than it otherwise would be. If you're curious to see what I wrote up before I was aware of this, [here it is](thoughts-weights), but beware even more, for there be overcomplicated incorrect maths!

Here is the key piece of information: if your opponent plays suboptimally, or in other words they favour some option more than they should and thus increase the usefulness of its responses, the optimal response (ignoring that a human opponent will adapt to your behaviour) is to always, 100% of the time, use your newly highest EV options (with the caveat that a human opponent who isn't a mathematical model of unchanging probabilities would adapt to this, so realistically you should just use those more often). In my earlier work I came across inklings of this, but always assumed I had messed up in my model somewhere!

In game theory, this suboptimal behaviour is better called "exploitable behaviour", a name which indicates what it really means to play suboptimally - to play exploitably is to play in a way where they get higher EV out of some option than others, and thus should never choose those others. Optimal behaviour is to play unexploitably, such that your opponent can do nothing to gain an upper hand - other options may have higher EV, but if they are exploitable (which they are), then they are suboptimal under this definition.

As such, unexploitable behaviour is when no option has higher EV than any other, meaning they all have equal EV. This is called "game theory optimal", or GTO, and enables a very useful technique to discover what this unexploitable behaviour is, using information such as the following!



| Option | Damage on Block | Damage on Yomi | Usage Chance | Defending Damage Taken EV |
| -----: | :---------------- | :---------------- | :----------- | :----------- |
| Strike | ⅓                 | 2                | x            | ⅓x + 1(1-x)  |
| Throw  | 1                 | -1                | 1-x          | 2x - 1(1-x)  |

We know that the defender must take the same amount of damage, on average, either way.

```
⅓x + 1(1-x) = 2x - 1(1-x)
1 - ⅔x = 3x - 1
3 - 2x = 9x - 3
11x = 6
```

Thus, through solving that equality, we show that the attacker should attack six out of eleven times, and throw five out of eleven times, with an EV of ending up seven elevenths of a point of damage ahead! I will include notable examples in a table at the end, though for brevity I won't include the working out. Note that if two defensive options are equal EV and the rest are worse, this is still unexploitable, the equilibrium just does not include the lesser options - if they ever use those options, they are playing exploitably.

But what should the defender do? Strictly speaking, if the attacker plays unexploitably, it doesn't matter what the defender does, but this comes with an inherent, very interesting risk - if the defender plays exploitably, the attacker would see higher EV by exploiting this, which can only be done by *also* playing exploitably, creating openings for the defender. This exploitable behaviour from both players is very risky, and thus should be an active decision rather than the default, so let's also calculate the unexploitable defensive options.

| Option | Block | Yomi | Usage Chance | Attacking Damage Dealt EV |
| -----: | :---------------- | :---------------- | :----------- | :----------- |
| Strike  | ⅓                | 1                 | x            | ⅓x + 2(1-x)  |
| Throw   | -1                | 2                 | 1-x          | 1x - 1(1-x)  |

```
⅓x + 2(1-x) = 1x - 1(1-x)
x + 6(1-x) = 3x - 3(1-x)
6-5x = 6x-3
11x=9
```

So block nine times out of eleven if you don't want to risk luring your opponent into exploitable behaviour! And block a different number of times if you *do* want to do that. And let's quickly verify that we get the same resulting EV as with the other way round: yes we do! Yay!

<a name="calc">[**Calculation**](#calc)</a>

The keen-eyed among you with the convenient background knowledge may have recognised that what we were doing here was a bunch of simultaneous equations! And there's the entire field of linear algebra available full of tools for solving those conveniently, so let's have a look at how we'd do that.

Firstly, we want to represent this as a matrix, which I unfortunately have no good way to write in this page (there's a reason wikipedia uses images and not actual webpage elements for maths!), so I'll write it out as a table and trust you to know it's meant to be a matrix.

| Option | Strike | Throw |
| -: | :- | :- |
| Block | ⅓ | 1 |
| Yomi | 2 | -1 |

Now it's worth noting that we can't be sure this matrix will be square, the attacker may have more or fewer options than the defender. As such, there are 3 cases for the simultaneous equations here:

* There are infinitely many solutions, with some formula describing them.
* There is a solution.
* There are no solutions.

These typically occur when the attacker has more options, equal options, and fewer options, respectively - note that options that are equivalent to another combination of options don't count here, as they add zero information to the system.

However, we don't care about this, we care whether it's calculable! The first two cases absolutely are, and it turns out that the vast majority of the time, systems where the defender has more options than the attacker are still calculable. And very conveniently, whenever it is calculable, the pseudoinverse (yes this is a real thing, I know it sounds silly) does the job for us!

It's probably better for me to show you, through the language of linear algebra. In the following, M is the matrix of values, where each row represents a defensive option (ie, an option by the player whose weights we are not calculating), N is that matrix's pseudoinverse, w is the vertical vector of weights for the attacker (well, for the player whose options are represented by the columns of M), E is the EV, O is a vertical vector of 1s that is the correct length for the calculation, and Ot is similar but horizontal. First, we shall calculate w, then we shall calculate E in terms of w.

```
M×w = E×O
N×M×w = N×E×O [multiply both sides by the pseudoinverse on the left]
w = E×N×O [cancel on the left, rearrange on the right because ev is a scalar]

Ot × w = E×Ot×N×O
1 = E×Ot×N×O [multiplying a vector by another vector of 1s is equivalent to summing its elements, which we know to equal 1 because it's a complete set of probabilities]
E = 1/(Ot×N×O) [rearrange to get ev, note that Ot×N×O is a scalar]
```

Note that this is only possible to evaluate because we know the total sum of the weights, and this assumes that no weights are negative. This can be false when calculated if one option should never be used, so be careful to verify your results and remove any such options from the calculation should they appear!

Anyway, yay, now we have formulae that, while awkward to do by hand (they involve calculating the inverse of a matrix), require minimal analysis and thus you can just throw them at a computer! Which is exactly what I've done for any results from here on out.

Interesting notes:

* Rearranging the matrix rows and columns doesn't change the results, but rearranging the columns does reorder the weights.
* The transpose of the matrix (flipping it across its diagonal) gives the weights for the defender, but gives the same EV (and, later, variance and standard deviation).
     * Note that the pseudoinverse of the transpose is the transpose of the pseudoinverse, so we don't have to calculate that twice!
* Multiplying the matrix by a constant gives the same weights, but does scale the EV and standard deviation by that factor.
* A stronger option will generally have a reduced weight.
* If one side has more options than the other, they receive an infinite set of options, with new options calculated from old ones.
    * This is done by adding any vector multiple of \[the identity matrix minus \[the pseudoinverse times the original matrix]] - if the pseudoinverse is a true inverse or a simple left inverse, this is just zero, which is very convenient!

Now, the variance: this is, admittedly, cheesed somewhat, but just subtract the EV from every element of the matrix, then square every value, then multiply every value by how common it is (the corresponding element of the vertical vector of defensive weights times the horizontal vector of offensive weights), then sum the elements. That's the variance, AKA how far each element deviates from the average! Next, take the square root of that result to scale it to the actual damage value, giving a much more usable number called the "standard deviation" for some reason. The higher the standard deviation, the less consistent the results.

<a name="octave">[**Octave**](#octave)</a>

I defined these functions in a maths scripting program called Octave, here they are! To use them, put each of them in EV.m, weights.m, and Var.m respectively, then load them into Octave and have fun!

```
function ev = EV( m )
  ev = 1/(ones(1,columns(m)) * pinv(m) * ones(rows(m),1))
endfunction

function w = weights( m, v = zeros(columns(m),1))
  w = EV(m) * pinv(m) * ones(rows(m),1) + (eye(columns(m)) - pinv(m)*m)*v
endfunction

function var = Var( m, v = zeros(columns(m),1) )
  ev = EV(m)
  var = sum(sum(((m .- ev)) .* ((m .- ev)) .* (weights(m') * weights(m)')))
endfunction
```

<a name="example">[**A Notable Example**](#example)</a>

Let's take a particular interaction (that I love to use as an example because it's fascinating) between Valerie and DeGrey, and see what it really means to play exploitably. After DeGrey blocks Valerie's magenta, Valerie has a certain set of options, bB/nB/fB/gC, and DeGrey *technically* has a truly ridiculous number of options (including both directions of blocking), b/f/nA/gB/gC/gS. Analytically, we can see that everything but blocking and parry are disastrous when they fail, so we should also consider the mixup where DeGrey only has b/f/gS, and technically he also doesn't always have parry, which will also be listed in the final table of mixups.

First, the big one! We find that the weight for Valerie's nB options in the big one is… negative? Huh, I guess it's technically strictly worse than some combination of other options here, so we'll exclude that and treat its weight as zero. When DeGrey is doing all these predictive punish options, never use sameside yellow, I guess.

Okay, apart from that weirdness it seems relatively normal, each move has a reasonable weight, but DeGrey should almost never block if he wants to play unexploitably. He should mostly nA, then gB, then gC (surprisingly, considering that only makes backstep awkward and otherwise takes 3 damage, but Valerie is backstepping nearly two thirds of the time!), then gS, and then finally at a combined 12% of the time should he block in either direction. It has an overall EV of 1.0 in Valerie's favour, and that's even worse at 1.1 without super available, so it's pretty great for Valerie!

Next, the simplified one. Immediately, we see that the EV is now 0.3 in Valerie's favour - less than half as good as a basic throw mixup! That's a huge change, and this is still with no possible exploitation from Valerie - without parry, she should now never backstep, sameside yellow one time out of two, and crossup yellow one times out of two. With parry, it's similar for those three, but backstep happens 40% of the time, hence the lower EV. How do we explain this? Technically this is a subset of the bigger set of options, so it should be a possible result surely? Well, honestly I dunno, we're doing weird things with linear algebra and I'm somewhat out of my depth, so let's just say that restricting the option set will change the results, and remind you that optimal play and unexploitable play are two different things. Besides, even calculating anything for cases like this took some wizardry.

What I choose to take from it is that DeGrey has to actively choose not to exploit Valerie's play with those additional moves (which he could start doing at any point) in order to keep the EV low. While it would benefit DeGrey in the short term to act in such a way, as soon as Valerie catches on (and she will, it involves entirely new moves) the EV rockets upward to the point where this interaction will lose entire rounds for DeGrey.

Anyway, what makes this so interesting is that this means Valerie is always goading DeGrey into exploiting her play using those moves, at which point the nature of the game changes to heavily favour her. Meanwhile, DeGrey can, at any point, use one of those moves as a one-off to punish a predictable choice between yellow and rainbow. So every neutral yellow in that matchup needs to sometimes be the nearly equivalent rainbow instead, to avoid this! Her choice of moves is altered by the mere existence of an option DeGrey would rather avoid.

So choose your options wisely!

<a name="summary">[**Summary of Mixups**](#summary)</a>

Finally, here's the table of different mixups, not accounting for the different okis they give and such. To resolve it with okis, treat each given oki as a variable, and calculate it separately for when they are at key low health values (eg, Degrey oki vs a 1 hp opponent), calculating the limits when it comes to looping okis. I will not be doing that, because effort and because oh no limits of weird functions. Strictly speaking, these also vary per opponent, especially when it comes to new defensive options like reversals. This is only a summary, a fully comprehensive list would be huge!!! Multiple mixups for each of the 144 (well, 169, dragon Midori exists) characters.

I do invite others to take on such a project, though!

Oh, and remember, this is only *unexploitable* play, it is totally possible for exploitable play to have higher EV (as demonstrated by the Val BB DeGrey mixup).

| [**Mixup**](#summary) | Attacking Options | Attacking Weights | Defending Options | Defending Weights | EV | Standard Deviation |
| -: | :- | :- | :- | :- | :- | :- |
| Throw | Strike/Throw | 55%:45% | Block/Yomi | 82%:18% | 0.6 | 0.7 |
| Throw (2HP) | Strike/Throw | 33%:67% | Block/Yomi | 67%:33% | ∞ | ∞ |
| Throw (2HP, no oki off throw) | Strike/Throw | 0%:100% | Block/Yomi | 0%:100% | 1 | 1.1 |
| Throw (Rook) | Sweep/Throw/Cmd | 74%:16%:10% | Block/Yomi/Jump | 34%:41%:25% | 0.8 | 0.9 |
| Throw (Rook, super) | Sweep/Throw/Cmd/gS | 75%:17%:0%:8% | Block/Yomi/Jump | 25%:39%:36% | 0.8 | 1 |
| Throw (Midori) | Strike/Throw | 71%:29% | Block/Yomi | 71%:29% | 0.8 | 1.2 |
| Throw (Dragon Midori) | Strike/Throw/Cmd | 53%:29%:18% | Block/Yomi/Jump | 53%:29%:18% | 1.1 | 1.2 |
| Grave Go Spinny | jA/jB | 1:4 | b/f | 4:1 | 0.9 | 0.3 |
| Val BB DeGrey | bB/nB/fB/gC | 47%:0%:19%:34% | b/f/nA/gB/gC | 4%:6%:42%:33%:15% | 1.1 | 1.6 |
| Val BB DeGrey (Parry) | bB/nB/fB/gC | 64%:0%:10%:26% | b/f/nA/gB/gC/gS | 5%:7%:31%:25%:22%:10% | 1.0 | 1.5 |
| Val BB DeGrey (Simplified) | bB/nB/fB | 0%:50%:50% | b/f | 50%:50% | 0.7 | 0.3 |
| Val BB DeGrey (Simplified, Parry) | bB/nB/fB | 40%:30%:30% | b/f/gS | 43%:43%:14% | 0.3 | 0.8 |

Missing: Onimaru stance (too opponent-dependent), mixups vs reversal characters, Lum's everything (too overwhelming and too nested), Quince's everything (see Onimaru, see Lum), Grave's other options out of jA (nested), Setsuki's everything (would be pointless without accounting for kd, as she has no reversal), many others, I'm sorry but it's a pain to come up with and type in a meaningful matrix for each of them when every part of the scoring is arbitrary (seriously, I could choose to only count damage dealt and not damage taken, and I haven't touched less easily quantified things like spacing, and technically the mixup should be redone whenever some scores are really infinite because they win rounds)

Note that, for the 2HP throw, I calculated that the EV for specifically the successful throw was half of whatever huge number I put in to represent an autowin, so I put in half of the huge number I had to represent an autowin in the strike mixup - if throw did not give oki, thus was only 1 damage, the system gave weiiird numbers, because infinity is weird and computers struggle with big finite numbers too.

The general rule here is "the stronger a move is, the less often you should do it but the more they should guard against it".

Thanks to BillyTheBanana for pointing out and explaining the concept of GTO play to me, prompting this entire writeup in the first place!

[Click here to return to mixups, probably confused, but hopefully with stuff to think about.](thoughts-mixups)

<a name="reading">[**Further reading:**](#reading)</a>

* [A poke at fighting games with game theory](https://www.lalaheadpats.com/2019/09/22/poke-at-fighting-games-with-game-theory.html)
* [GTO and Exploitative Play](httpd://alexspuffstuff.blogspot.com/2017/01/gto-and-exploitive-play.html)
* [Online GTO calculator](https://eqcalc.jounin.jp/eq_calc/1_0/index.html?q=SSFVDUBlocksWW9taSBDb3VudGVyDVAttackUThrowkLjMzMwDCA)
