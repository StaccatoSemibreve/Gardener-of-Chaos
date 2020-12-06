# Weights, the Complicated Bit

Beware, for here be maths. [Click here](thoughts-mixups) to return to the useful mixups stuff. Please click there if you don't want to overanalyse a minor aspect of mixups to get numbers very close to the rule of thumb.

Okay, so they're not gonna just defend against strikes two thirds of the time no matter what, because then you would never need to throw! You could just strike every time, and a third of them would land, and it would be amazing. They don't want things to go amazingly for you.

So they're watching what you do, estimating how often you pick each option, doing *something* to that in their head, and picking their defensive options based on that. If we were to describe it mathematically, where x is the proportion of the time they estimate that you strike, they are blocking F(x) amount of the time.

Known properties of F:

* It is an *increasing function* - in other words, F(a) ≤ F(b) while a < b.
* F(0) = 0, and F1) = 1

If you've studied probability, this probably looks very familiar, as it's just a cumulative distribution function normalised to apply to 0≤x≤1! But we won't worry about that here, that's for your own personal thinkies. If either of those assumptions isn't true, you can skip this entire section and just always throw or always strike.

So, this gives us the following table of known information:

| Option | Damage on Success | Damage on Failure | Usage Chance | Success Chance |
| -----: | :---------------- | :---------------- | :----------- | :------------- |
| Strike | 2                 | ⅓                | x            | 1-F(x)         |
| Throw  | 1                 | -1                | 1-x          | F(x)           |

Which lets us do some magic to work out the expected values of each option, given the context of the nature of the mixup and the interplay between players! And then the total expected value of the mixup, which is very useful information.

| Option | Expected Value       | Weighted Value  |
| -----: | :------------------- | :-------------  |
| Strike | 2·(1-F(x)) + ⅓·F(x) | x·EV(Strike)    |
| Throw  | 1·F(x) + -1·(1-F(x)) | (1-x)·EV(Throw) |

Where the total expected value is calculated as the sum of the weighted values. This is not… the most convenient form for it to appear in, I realise. At this level of abstraction, it's hard to reason around, after all! But we can assume values of F and reason around those!

For instance, assuming F(x) is the [normal distribution's](https://en.wikipedia.org/wiki/Normal_distribution) cumulative distribution function, we can mess around with graphs (in my case, using [Desmos](https://www.desmos.com/calculator)) and figure out that yeah, for sensible variables, the 2 thirds ballpark figure was about right. But the normal distribution is a pain, because it doesn't have its domain restricted so it doesn't have F(0)=0 and F(1)=1 like we wanted, so let's try the infinitely weirder but extremely suitable [beta distribution](https://en.wikipedia.org/wiki/Beta_distribution) instead - this looks about right for what we'd expect at α=0.5, β=0.5, so wiggle things around there in your graph and see, once again, that the ballpark figure is about right!

Now, both players here want to hit something called a [Nash equilibrium](https://en.wikipedia.org/wiki/Nash_equilibrium), where their expected value won't be improved by them making a different decision - the attacker's decision being x, the defender's being F(x) - but that's hard, we'd need to assume that the defender always uses the beta distribution's CDF, and then their choice is its α and β variables, so we've got to do calculus on some annoying stuff. I don't want to do that, so I haven't done that.

Description of the task at hand that I don't want to do and I doubt anyone else will want to do, and which has probably been done in some mathematical paper or something I can't be bothered to find:

1. Take the partial derivatives with respect to α and β of the integral of the expected value across x=\[0,1\].
2. Find where those partial derivatives are both equal to 0 - this is either a maximum or a minimum. As the expected value being lower is good for the defender, we want a minimum. This gives us the optimal F(x).
3. Find where the expected value is highest for that F(x). This gives us the optimal x, which I shall call X. Calculate F(X) to find how often the defender should defend.
4. This is why I used a rule of thumb earlier!!!

Instead, let's just look at [the wiggly graph](https://www.desmos.com/calculator/qbhcalcaxh), find the maximum on the graph for how often you should be attacking, and look at what F(x) is there - for α=0.5, β=0.5, we get x at about 0.75 and F(x)=0.67, which seems about right. Which is a relief, quite honestly, because this got away from me somewhat.

Something you may have noticed is that this is a 2 option mixup. What if we get, say, a 3 option mixup? We run. It all applies similarly, but we don't get the wiggly graphs because it's got too many dimensions, so we flee for our lives.

[Click here to return to mixups, having gained little from this excursion.](thoughts-mixups)
