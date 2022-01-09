# Coursework Report for Computation Logic for Artificial Intelligence

### Author [Isabella Degen](https://github.com/isabelladegen) 

This is the report for the 2021 Assignment Computational Logic for Artificial Intelligence COMSM0022.

This report goes into details about how I extended Prolexa. It's accompanied by
the [Colab Demo Notebook](https://colab.research.google.com/github/isabelladegen/ComputationalLogic/blob/prolexa-plus/Demo_Notebook.ipynb)
that demonstrates these changes and allows playing with this extended version of Prolexa.

All code can be found on [Github](https://github.com/isabelladegen/ComputationalLogic)

### Contents

1. [Motivation](#motivation)
2. [Method](#method)
3. [Implementation](#implementation)
   1. [Default rules](#defaultrules)
   2. [Negated rules](#negatedrules)
   3. [Adding new default rules and negated rules](#addingnewdefaultrulesandnegatedrules)
4. [Limitations](#limitations)


# <a name="motivation">Motivation #

My goal for this coursework was to get some experience with logical programming by extending Prolexa to be able to do
default reasoning as well as deal with negations.

In summary the following new interactions are now possible:

- Adding rules that have exception (Default Rules):```'some birds fly'``` &rarr; 'I will remember that some birds fly'
- Adding negated rules: ```'Penguins dont fly'``` &rarr; 'I will remember that Penguins dont fly'
- Report about default rules: ```'Tell me all'``` &rarr; 'Some birds fly' instead of 'Every bird flies'
- Report about negated rules: ```'Tell me all'``` &rarr; 'Penguins dont fly'
- Report default rules and negations in questions about proper nouns: ```'Tell me about Peep'``` &rarr; 'peep is a bird.
  peep flies' and
  ```'Tell me about Opus'``` &rarr; 'opus is a bird. opus is a penguin. opus doesnt fly'
- Queries about proper nouns: ```'Does Peep fly'``` &rarr; 'Peep flies' and ```'Does Opus fly'``` &rarr;
  'Sorry, I don't think this is the case'
- Explain default rules: ```'Explain why peep flies'``` &rarr; 'peep is a bird; some birds fly; therefore peep flies'
  and
  ```'Explain why opus doesnt fly'``` %&rarr; 'opus is a penguin; penguins dont fly; therefore opus doesnt fly'

# <a name="method">Method #

I started off my work by forking
the [assignment github repository](https://github.com/simply-logical/ComputationalLogic). This way I could make my
changes, while continuing to pull from the original repository if needed or indeed push back to it.

To learn what Prolexa can do and to avoid breaking existing functionality, I started off by making a list of commands
that are working both on my computer and via Google Colab. I decided to keep track of these by writing few high level '
test cases' in a new Colab Notebook. This notebook eventually evolved into the
[Demo Notebook](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/Demo_Notebook.ipynb) for the
coursework. This Notebook gives a good walk through about what can now be done with Prolexa.

Once I had a high level idea about how I can interact with Prolexa, I wanted to learn more about how each of these
commands were handled in Prolog. I used the SWI Prolog GUI and its debugger to step through each of the main cases in
the predicate ```handle_utterance(SessionId,Utterance,Answer)``` defined in
[prolexa.pl](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/prolexa/prolog/prolexa.pl):

- A. Utterance is a sentence
- B. Utterance is a question that can be answered
- C. Utterance is a command that succeeds
- D. None of the above

That way I learned how the input string is translated into a Prolog Goal and how the outcome of such a Goal was
translated back into a 'string' answer. Both ways, this translation happens mainly in
[prolexa_grammar.pl](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/prolexa/prolog/prolexa_grammar.pl)
, while the proving of the Goal happens in
[prolexa_engine.pl](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/prolexa/prolog/prolexa_engine.pl)
.

Now I was ready to start changing these three files. I took a top down approach starting by defining what the user would
ask Prolexa and what they would expect as response to their question. I could use my Colab notebook to specify new test
for the new functionality in the same way: ```test('new query', 'expected answer')```. These tests were my 'Acceptance
tests' for the new behaviour. For the code changes to Prolog I continued to use the debugger to verify that the query
was handled as expected. All my changes were done in small cycles of:
add a failing test &rarr; change the code to fix it &rarr; run the tests to verify it was working &rarr; check-in and
start with the next new failing test. I started off by adding default rules and negated rules directly to Prolog and
ensuring that the cases *B. Utterance is a question that can be answered* and *C. Utterance is a command that succeeds*
worked. Then I made sure that default rules and negated rules could also be added via *A. Utterance is a sentence*.

I noticed early on a discrepancy between directly querying Prolexa via the commandline and querying it via the Colab
notebook which was using Prolexa plus. Prolexa plus is using Python to extend the vocabulary on the fly. While this is
useful for queries of type 'A. Utterance is a sentence', Prolexa plus is also extending the grammar for queries of type
'B. Utterance is a question that can be answered' and 'C. Utterance is a command that succeeds'. This results in
unexpected words being added to the vocabulary which then result in strange answers. For example the query *'Explain why
tweety tweets'* adds the following *pred* functors to the grammar:

```
  pred(tweet, 1,[v/tweet,n/tweet]).
  pred(doe, 1,[v/doe]).
  pred(bird, 1,[n/bird]).
  pred(is, 1,[v/is]).
  pred(tweety, 1,[a/tweety,n/tweety]).
  pred(explain, 1,[v/explain]).
```

Note that amongst other unexpected additions *tweet* is added as noun as well as a verb which then results in the
unexpected answers to the query *'Explain why tweety tweets'* &rarr; *tweety is a bird; every bird is **a tweet**;
therefore tweety is **a tweet***
instead of what I expected *tweety is a bird; every bird **tweets**; therefore tweety **tweets***. For this reason I
decided to directly query Prolexa. The Colab Notebook I used to debug Prolexa Plus has more examples of different
grammar additions and can be found here:
[Testing_Prolexa_Plus_Notebook.ipynb](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/Testing_Prolexa_Plus_Notebook.ipynb)

# <a name="implementation">Implementation #

In this chapter I'm going to walk through the changes I've made in detail.

To have some examples to work with I directly extended
the [prolexa_grammar.pl](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/prolexa/prolog/prolexa_grammar.pl)
with the following proper nouns:

```
proper_noun(s,tweety) --> [tweety].
proper_noun(s,opus) --> [opus].
proper_noun(s,peep) --> [peep].
```

And the vocabulary with the following words:

```
pred(bird, 1,[n/bird]).
pred(penguin, 1,[n/penguin]).
pred(fly, 1,[v/fly]).
```

## Default rules <a name="defaultrules">

First I looked at how to handle default rules directly added to the rule base

### Acceptance Tests:

I defined the acceptance tests for the simplest case of handling default rules are as following:

- B. Utterance is a question that can be answered

```
test('Does peep fly','peep flies')
```

- C. Utterance is a command that succeeds

```
test('Explain why peep flies', 'peep is a bird; some birds fly; therefore peep flies')
test('Tell me about peep', 'peep is a bird. peep flies')
```

### Required changes:

Utterances are handled in 
[prolexa.pl](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/prolexa/prolog/prolexa.pl).

The question *'Does peep fly'* is translated into a Prolog query using the 
[prolexa_grammar.pl](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/prolexa/prolog/prolexa_grammar.pl). 
The query is then proved by the *prove_question/3* meta-interpreter in 
[prolexa_engine.pl](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/prolexa/prolog/prolexa_engine.pl).
These are the relevant lines of code:

```
% prolex.pl -  handle_utterance
phrase(question(Query),UtteranceList),
prove_question(Query,SessionId,Answer) -> true

% prolexa_grammar.pl:
question(Q) --> qword,question1(Q).
question1(Q) --> [does],proper_noun(_,X),verb_phrase(_,X=>Q).
```

Commands are also translated into Prolog goals using the grammar before the Goal itself is called. 
*'Explain why peep flies'* is translated into the goal `explain_question(Q,_,Answer)` and
*'Tell me about peep'* is translated into the goal `all_answers(PN,Answer),Answer))`:

```
% prolex.pl -  handle_utterance
phrase(command(g(Goal,Answer)),UtteranceList),
call(Goal) -> true

% prolexa_grammar.pl:
command(g(explain_question(Q,_,Answer),Answer)) --> [explain,why],sentence1([(Q:-true)]).
command(g(all_answers(PN,Answer),Answer)) --> tellmeabout,proper_noun(s,PN).
```

### New Rules
I needed a new syntax for rules to be able to distinguish between rules with no exception and default rules 
and added the following rules

```
% prolexa.pl:
stored_rule(1,[(default(fly(X):-bird(X)))]).
stored_rule(1,[(bird(peep):-true)]).
```

The first rule is the new syntax for default rules, the second rule is using the existing syntax for rules without
exceptions.

### Handling Default Rules in language
To be able to understand and explain default rules using 'some' instead of 'every' I added a new determiner
to the grammar which allows the existing `sentence1` expression to understand default rules and translate
queries into a default rules as well as default rules into language:

```
% prolexa_grammar.pl:
command(g(explain_question(Q,_,Answer),Answer)) --> [explain,why],sentence1([(Q:-true)]).

sentence1(C) --> determiner(N,M1,M2,C),noun(N,M1),verb_phrase(N,M2).

determiner(p,X=>B,X=>H,[(default(H:-B))]) --> [some].
```

This now meant I could explain why a bird flies using *some* instead of
*every*:
```
prolexa> "Explain why peep flies".
*** utterance(Explain why peep flies)
*** goal(explain_question(fly(peep),_49814,_49802))
*** answer(peep is a bird; some birds fly; therefore peep flies)
peep is a bird; some birds fly; therefore peep flies
```

### New meta-interpreter for Default Rules
The new default rule syntax required adding a new meta interpreter that can handle default rules. 
The new **explain_rb/4** meta-interpreter is adapted from
chapter [8.1 in Simply Logical](https://too.simply-logical.space/src/text/3_part_iii/8.1.html)
to fit in with the existing Prolexa code. This new meta-interpreter is now called form *explain_question/3*:
`explain_rb(Query,Rulebase,[],Proof)` instead of `prove_rb(Query,Rulebase,[],Proof)`. It 
is also called from *prove_question/3* instead of `prove_rb(Query,Rulebase)` for questions. For this it needed 
a 2 argument version `explain_rb(Query,Rulebase)` that simply calls the arity 4 version with an empty list
and an anonymous variable for collecting the proves. Finally, *explain_rb/2* is also called from *prove_question/2* 
which is called from *all_answers/2* to deal with 'Tell me about...' commands.

The new meta-interpreter *explain_rb/4* works as following:

1. It first attempts to proof the goal using the existing `prove_rb()` meta-interpreter
2. If that fails, it looks for a default rule `default(A:-B)` in the rule base using the existing `find_clause()`
   functor
3. If it finds such a default rule it attempts to explain the body B the same way B using the existing `p(A,Rule)` to
   later generate the message from the proof
4. If that succeeds it then makes sure that A is not a contradiction

```
% prolexa_engine.pl

% meta-interpreter for rules and defaults from chapter 8.1
explain_rb(true,_Rulebase,P, P):-!.
explain_rb((A,B),Rulebase,P0,P):-!,
  explain_rb(A,Rulebase,P0,P1),
  explain_rb(B,Rulebase,P1,P).
explain_rb(A,Rulebase,P0,P):-
  prove_rb(A,Rulebase,P0,P). % explain by rules only
explain_rb(A,Rulebase,P0,P):-
  find_clause(default(A:-B),Rule,Rulebase),
  explain_rb(B,Rulebase,[p(A,Rule)|P0],P),
  not contradiction(A,Rulebase,P).  % A consistent with P
	
% top-level version that ignores proof
explain_rb(Q,RB):-
	explain_rb(Q,RB,[],_P).
```

Ensure Tell me about proper noun also uses default rules as long as they are not a contradiction. To do so `all_answers`
which is used for queries like "Tell me about <proper_noun>" also uses explain instead of just proof.

```
prolexa> "Tell me all about peep".
*** utterance(Tell me all about peep)
*** goal(all_answers(peep,_48568))
*** answer(peep is a bird. peep flies)
peep is a bird. peep flies
```


## Negated rules <a name="negatedrules">

To properly test default rules I needed to implement negation in rules.

### Acceptance Tests

I wanted to be able to ask negated questions as well has having Prolog use negations in explanations and answers.
I defined the following acceptance tests for negated rules:

- B. Utterance is a question that can be answered 
```
test('Does opus fly', 'Sorry, I don\'t think this is the case')
```
- C. Utterance is a command that succeeds 
```
test('Explain why opus doesnt fly', 'opus is a penguin; penguins dont fly; therefore opus doesnt fly')
test('Explain why opus flies', 'Sorry, I don\'t think this is the case')
test('Tell me about opus', 'opus is a bird. opus is a penguin. opus doesnt fly')
```

### Required Changes:

Given that I was again dealing with questions and commands the changes were required in the same areas as for 
default rules.

### New Rules:

For these new behaviours I directly added the following new rules about Opus as well as a not operator:

```
% prolexa.pl:
:-op(900,fy,not).

not flies(X):-penguin(X)
bird(X):-penguin(X)
penguin(opus):-true
```

### Handling negation in language

I needed to extend the grammar to deal with not in rules by extending the verb_phrases and adding a new determiner:

```
% prolexa_grammar.pl
sentence1([(not(L):-true)]) --> proper_noun(N,X),verb_phrase(N,not(X=>L)).

verb_phrase(s,not(M)) --> [isnt],property(s,M). 
verb_phrase(p,not(M)) --> [arent],property(p,M).
verb_phrase(p,not(M)) --> [dont], iverb(p,M).
verb_phrase(s,not(M)) --> [doesnt], iverb(p,M). %verb_phrases are in plural form for negations

determiner(p,X=>B,not(X=>H),[(not(H):-B)]) --> [].
```

For the command *'Tell me all'* where I wanted the
rule `not fly(X):-penguin(X)` to be translated to *'Penguins dont fly'*. This is handled by
`verb_phrase(p,not(M)) --> [dont], iverb(p,M).` and worked
as `sentence1(C) --> determiner(N,M1,M2,C),noun(N,M1),verb_phrase(N,M2).`
has M2 in the form of `not(X=>fly(X))` which then matches the verb_phrase for 'dont' sentences.

However, question about a proper noun such as *'Tell me all about Opus'* match
`sentence1([(L:-true)]) --> proper_noun(N,X),verb_phrase(N,X=>L).` with L being `not(fly(opus))`
but M becomes `opus=>not(fly(opus))` which does no longer match `not(M)` clauses for verb_phrase. To handle this I added 
`sentence1([(not(L):-true)]) --> proper_noun(N,X),verb_phrase(N,not(X=>L)).` which correctly deals
with the 'not' without propagating it to the verb_phrase.


### Handling negated rules in the existing meta-interpreters

Because I wanted Prolexa to include not rules in queries like *'Tell me about opus'* by listing the negated rules as well,
I needed to make sure that the negated known rules were found as well. Previously, Prolexa ignored the
the `stored_rule(1,[(not fly(X):-penguin(X))]).` due to `all_answers(Opus, Answers)` collecting all the known
predicates `pred(P, 1, _)` and then attempting to prove each of them. However, Prolexa was not attempting to 
proof `not` clauses. I decided to make this work explaining both the 'clause' and 'not clause' 
in *prove_question/2* using an else-if, which perhaps is a bit of a procedural approach but required very few 
code changes:

```
% prolexa_engine.pl
prove_question(Query,Answer):-
	findall(R,prolexa:stored_rule(_SessionId,R),Rulebase),
	( explain_rb(Query,Rulebase) ->
		transform(Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; explain_rb(not Query,Rulebase) ->
		transform(not Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = ""
	).
```

The introduction of negated rules also required me to deal with contradictions. `fly(opus)` only fails if
I properly check for contradictions so that only rules are found that don't contradict the knowledge base which 
`fly(oppus)` does. *contradiction/3* gets called both in *explain_rb/4* and *prove_rb/4* which would result in a 
circular call. To avoid this I introduced a simple *prove_s/4* version that does not check for contradictions.

```
% prolexa_engine.pl:

% check contradiction against rules, use simple proof to avoid circular contradition calls
contradiction(not A,Rulebase,P):-!,
	prove_s(A,Rulebase,P,_P1).
contradiction(A,Rulebase,P):-
	prove_s(not A,Rulebase,P,_P1).

% proof simple same as proof_rb but does not check for contradictions in proofs
prove_s(true,_Rulebase,P,P):-!.
prove_s((A,B),Rulebase,P0,P):-!,
	find_clause((A:-C),Rule,Rulebase),
	conj_append(C,B,D),
  prove_s(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_s(A,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_s(B,Rulebase,[p(A,Rule)|P0],P).
```

## Adding new default rules and negated rules <a name="addingnewdefaultrulesandnegatedrules">
I wanted to make sure default rules could also be added directly via queries as long as they
used words and proper nouns already defined in the Prolexa grammar. Statements like *'All birds tweet'*
should add standard rule whereas statements like *'Some birds tweet'* should add a default rule.

### Acceptance Tests

The Acceptance tests for this were the following. Most of these worked out of the box with the above changes.

- A. Utterance is a sentence
```
test('Some birds sing', 'I will remember that Some birds sing') #adds a new default: stored_rule(1,[(default(sing(X):-bird(X)))])
test('Swans dont sing', 'I will remember that Swans dont sing') #adds a new rule: stored_rule(1,[(not sing(X):-swan(X))])
test('All swans are birds', 'I will remember that All swans are birds') #adds a new rule: stored_rule(1,[(bird(X):-swan(X))])
test('Tweety is a swan', 'I will remember that Tweety is a swan') #adds a new rule: stored_rule(1,[(swan(tweety):-true)])

#Ensure that adding the rules twice does not add the rules twice - (Idempotence)
test('Some birds sing', 'I already knew that Some birds sing') 
test('Swans dont sing', 'I already knew that Swans dont sing') 
test('All swans are birds', 'I already knew that All swans are birds')
test('Tweety is a swan', 'I already knew that Tweety is a swan')
```
- B. Utterance is a question that can be answered

```
test('Does tweety sing', 'Sorry, I don\'t think this is the case')
test('Does peep fly','peep flies')
test('Is tweety a bird', 'tweety is a bird')
test('Is opus a bird', 'opus is a bird')
test('Is peep a bird', 'peep is a bird')
```

- C. Utterance is a command that succeeds

```
test('Tell me about tweety', 'tweety is a bird. tweety is a swan. tweety flies. tweety doesnt sing')
test('Tell me about peep', 'peep is a bird. peep flies. peep sings')
test('Explain why peep sings', 'peep is a bird; some birds sing; therefore peep sings')
test('Explain why tweety flies', 'tweety is a swan; every swan is a bird; some birds fly; therefore tweety flies')
test('Explain why tweety doesnt sing', 'tweety is a swan; swans dont sing; therefore tweety doesnt sing') 
```

### Required Changes:

New rules are added when the utterance is interpreted as a sentence. This first checks if a rule is already
known which will avoid adding in more than once:

```
% prolexa.pl
% A. Utterance is a sentence
	( phrase(sentence(Rule),UtteranceList),
	  write_debug(rule(Rule)),
	  ( known_rule(Rule,SessionId) -> % A1. It follows from known rules
			atomic_list_concat(['I already knew that',Utterance],' ',Answer)
	  ; otherwise -> % A2. It doesn't follow, so add to stored rules
			assertz(prolexa:stored_rule(SessionId,Rule)),
			atomic_list_concat(['I will remember that',Utterance],' ',Answer)
	  )
```

Therefore, I needed to extend the*known_rule/2* meta-interpreter to also check if a default rules already existed. 
I again used an `else if` to do so which is probably more of a procedural way of programming:

```
%prolexa-engine.pl
known_rule([Rule],SessionId):-
	findall(R,prolexa:stored_rule(SessionId,R),Rulebase),
	( try((numbervars(Rule,0,_),
	     Rule=(H:-B), %try normal rules
	     add_body_to_rulebase(B,Rulebase,RB2),
	     explain_rb(H,RB2)
	   )) -> true
		; try((numbervars(Rule,0,_),
		     Rule=default(H:-B), %try default rules
		     add_body_to_rulebase(B,Rulebase,RB2),
		     explain_rb(H,RB2)
		  ))
	 ).
```

# <a name="limitations">Limitations #

Limitations of my implementation that I'm aware of are:

- Negated queries need to be done without using an  **'**. This means using doesnt and dont instead of doesn't and don't due 
to not escaping ' with '\\'
- New words cannot be added to the vocabulary
- Can only use proper nouns to query *'Does tweety fly'* not *'Do birds fly'*
- If there is a negated rule in the rulebase such as `not fly(X):-penguin(X)` and `penguin(opus):-true` and we ask
  Prolexa *'Does opus fly'*. She answers *'I don't think that's the case'* due to not checking to proof the negative rules
  which she should do. So it's by coincidence that this query works not by design. Ideally, Prolexa should
  answer definitive for such queries with *'No opus doesn't fly'* and only answer with *'Sorry I don't think this is the case'*
when she cannot proof the query.
