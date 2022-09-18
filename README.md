# Stan Miscellaneous

Welcome to my repository! Glad to have you here. The repo's plan is ambitious.
In a nutshell: I want to learn Stan. But how to start? I broke it down into
three essential steps. First, let's recode Richard McElreath's *Statistical
Rethinking*[^1] in Stan -- avoiding all of R's convenience functions. (I already
graduated with the convenient approach last summer). The Stan way of things,
however, is way more in-depth. Who said that grasping all the naughty details
would be easy? Going statistically barefoot does require two things: time and
perseverance. 

For the leftovers, the approach stays the same. To go forward in my
psychometric modeling, I'll recode Rov Levy's and Robert J. Mislevys's
monograph *Bayesian Psychometric Modeling*[^2]. They earned critiques because
they (still) used Gibbs sampling (specifically: WinBugs). That's a dinosaur.
Stan[^3], instead, is a state-of-the-art battle station utilizing Hamiltonian Monte
Carlo[^4]. Its diagnostic information is irreplaceable. Modeling the hell out
of your statistical problems, Stan keeps you on track. Lastly, I put my hands
on Gelman et al.'s 2013 masterpiece *Bayesian Data Analysis*[^5]. I hope
they'll release a new version of the book before I need to buy it.

## #TowardsAPrincipledBayesianWorkflow

---

## Collection of awesome websites

- https://mc-stan.org/
- https://distribution-explorer.github.io/
- https://chi-feng.github.io/mcmc-demo/
- https://betanalpha.github.io/
- https://statmodeling.stat.columbia.edu/
- https://avehtari.github.io/modelselection/
- https://elevanth.org/blog/
– https://vasishth.github.io/bayescogsci/book/

Note: Long run times? Read:
https://mc-stan.org/docs/stan-users-guide/parallelization.html and think about
switching to pure cmdstan. See: https://github.com/rmcelreath/cmdstan_map_rect_tutorial

---

[^1]: https://github.com/rmcelreath/rethinking

[^2]: http://bayespsychometrics.com/

[^3]: https://mc-stan.org/

[^4]: https://mc-stan.org/docs/2_29/reference-manual/hamiltonian-monte-carlo.html

[^5]: http://www.stat.columbia.edu/~gelman/book/

