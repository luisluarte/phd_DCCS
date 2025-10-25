# mice use the prior to optimize their performance

- premise: knowledge of the current prior would help the mice to perform well
- premise: the prior is the only source of information on zero contrast trials
- premise: mice could form only a subjective estimate of the true block prior from the trial history
- premise: at best, they could compute the estimate of the true block prior given full knowledge of the task structure and the sequence of previous stimulus sides since the start of the session

## model 1
- bayes-optimal prior
    - is a hidden markov model (HMM)
        - contains: observed data, hidden state label, and block length
        - maintains a belief over hidden state label
