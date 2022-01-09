# Report for 2021 Assignment Computational Logic for Artificial Intelligence COMSM0022

### Author

[Isabella Degen](https://github.com/isabelladegen)

[//]: # (How to use this report)

- The colab notebook walks you through what has been changed and what you can do on a high level
- The report goes into more details about the changes....

### Contents

1. [Motivation](#motivation)
2. [Method](#method)
3. [Implementation](#implementation)

Appendix?

5. [Testing](#testing)

Perhaps write about

1. [Coding Practices](#codingpractices) and write about testing as a sub set

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
with the proper nouns:

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

## Handling default rules

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

### Deciding what needs to be changed:

The utterances are handled
in [prolexa.pl](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/prolexa/prolog/prolexa.pl):

The question 'Does peep fly' is translated into a Prolog Query using the grammar. This Query then needs to be proved
using the *prove_question/3* meta-interpreter from the engine:

```
phrase(question(Query),UtteranceList),
...
prove_question(Query,SessionId,Answer) -> true
```

The query if formed by:

```
question(Q) --> qword,question1(Q).
...
question1(Q) --> [does],proper_noun(_,X),verb_phrase(_,X=>Q).
```

Commands are translated into Prolog goals that then are proved or disproved by calling them:

```
phrase(command(g(Goal,Answer)),UtteranceList),
...
call(Goal) -> true
```

The command 'Explain why peep flies' is translated into the Prolog goal `explain_question(Q,_,Answer)`
in the grammar by

```
command(g(explain_question(Q,_,Answer),Answer)) --> [explain,why],sentence1([(Q:-true)]).
```

The command 'Tell me about peep' is translated into the Prolog goal `all_answers(PN,Answer),Answer))`
in the grammar by

```
command(g(all_answers(PN,Answer),Answer)) --> tellmeabout,proper_noun(s,PN).
```

From this to pass the acceptance tests I needed to add some new rules, make changes to the grammar and the existing meta-interpreters.

### New Rules
Changes to [prolexa.pl](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/prolexa/prolog/prolexa.pl).

I needed a new syntax for rules to be able to distinguish between rules with no exception and default rules. I extended
prolexa.pl with the following two rules:

```
stored_rule(1,[(default(fly(X):-bird(X)))]).
stored_rule(1,[(bird(peep):-true)]).
```

The first rule is a new syntax for default rules, the second rule is using the existing syntax for rules without
exceptions.

### Handling Default Rules in Language
Changes to [prolexa_grammar.pl](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/prolexa/prolog/prolexa_grammar.pl).

To be able to understand and explain default rules using 'some' instead of 'every' I added the following determiner:

```
determiner(p,X=>B,X=>H,[(default(H:-B))]) --> [some].
```

This allows default rules to be understood as a sentence and translated into a query Q:

```
sentence1(C) --> determiner(N,M1,M2,C),noun(N,M1),verb_phrase(N,M2).

...

command(g(explain_question(Q,_,Answer),Answer)) --> [explain,why],sentence1([(Q:-true)]).
```


### New meta-interpreter for Default Rules
Changes to [prolexa_engine.pl](https://github.com/isabelladegen/ComputationalLogic/blob/prolexa-plus/prolexa/prolog/prolexa_engine.pl).

The new default rule syntax required adding a new meta interpreter that could handle default rules. 
The new **explain_rb/4** meta-interpreter is adapted from
chapter [8.1 in Simply Logical](https://too.simply-logical.space/src/text/3_part_iii/8.1.html)
to fit in with the existing Prolexa code. This new meta-interpreter now is called form **explain_question/3**:
`explain_rb(Query,Rulebase,[],Proof)` instead of `prove_rb(Query,Rulebase,[],Proof)`. It 
is also called from **prove_question/3** for which it needs a 2 argument version: `explain_rb(Query,Rulebase)`
instead of `prove_rb(Query,Rulebase)`.

In summary the new explain-rb() meta interpreter works as following:

1. It first attempts to proof the goal using the existing `prove_rb()` meta-interpreter
2. If that fails, it looks for a default rule `default(A:-B)` in the rule base using the existing `find_clause()`
   functor
3. If it finds such a default rule it attempts to explain the body B the same way B using the existing `p(A,Rule)` to
   later generate the message from the proof
4. If that succeeds it then makes sure that A is not a contradiction

```
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
  
% check contradiction against rules, use simple proof to avoid circular contradition calls
contradiction(not A,Rulebase,P):-!,
	prove_s(A,Rulebase,P,_P1).
contradiction(A,Rulebase,P):-
	prove_s(not A,Rulebase,P,_P1).
	
% top-level version that ignores proof
explain_rb(Q,RB):-
	explain_rb(Q,RB,[],_P).
```

Explain prove_s vs prove_rb TODO



### Handling negated rules

### Acceptance Tests

AC B. Utterance is a question that can be answered test('Does opus fly', 'Sorry, I don\'t think this is the case')
C. Utterance is a command that succeeds test('Explain why opus doesnt fly', 'opus is a penguin; penguins dont fly;
therefore opus doesnt fly')
test('Explain why opus flies', 'Sorry, I don\'t think this is the case')
test('Tell me about opus', 'opus is a bird. opus is a penguin. opus doesnt fly')
test('Spill the beans', 'every human is mortal. peter is human. some birds fly. penguins dont fly. every penguin is a
bird. opus is a penguin. peep is a bird')

### Deciding what needs to be cahnged:

### Rules:

- `not flies(X):-penguin(X)`
- `bird(X):-penguin(X)`
- `penguin(opus):-true`

### Grammar

** More on Grammar and Negation**

The solution I chose is to do the phrasing in the grammar. For the query "Tell me all" where I expect the
rule `not fly(X):-penguin(X)` to be translated to
"Penguins don't fly". This is handled by the following`verb_phrase(p,not(M)) --> [dont], iverb(p,M).` and works
as `sentence1(C) --> determiner(N,M1,M2,C),noun(N,M1),verb_phrase(N,M2).`
has M2 in the form of `not(X=>fly(X))` which then matches the verb_phrase for "don't" sentences.

However the query for a proper noun e.g "Tell me all about Opus" matches
`sentence1([(L:-true)]) --> proper_noun(N,X),verb_phrase(N,X=>L).` L being `not(fly(opus))`
but m becomes `opus=>not(fly(opus))` which does no longer match `not(M)` clauses for verb_phrase. For this a new
clause `sentence1([(not(L):-true)]) --> proper_noun(N,X),verb_phrase(N,not(X=>L)).` has been added which correctly deals
with the not without propagating it to the verb_phrase avoiding them having to be duplicated.


## Handling Adding default rules

## Acceptance Tests
AC A

## Handling Adding negated rules 

## Acceptance Tests
AC A

AC A



-----------


### Approach:

Adding new meta interpreters `explain` for default rules and `contradiction`. Reusing the existing meta
interpreter `prove_rb` to first attempt an explanation using a rule. If this fails it will attempt to find a default
instead. See changes for more details.

First checking that all the explanation using existing rules still work. Which they don't so I extended the grammar to
deal with `not` in the head of a rule.

Change the grammar to explain why a bird that's not a penguin flies with using *some* instead of
*every* for default rules.

```
prolexa> "Explain why peep flies".
*** utterance(Explain why peep flies)
*** goal(explain_question(fly(peep),_49814,_49802))
*** answer(peep is a bird; some birds fly; therefore peep flies)
peep is a bird; some birds fly; therefore peep flies
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

Ensure not rules are also included in the "Tell me about <proper_noun>" queries. For "Tell me all about opus"  I'd
expect Prolexa to answer "opus is a bird. opus is a penguin. opus doesn't fly". But instead she ignores
the `stored_rule(1,[(not fly(X):-penguin(X))]).` due to `all_answers(Opus, Answers)` collecting all the known
predicates `pred(P, 1, _)` and then attempting to prove each of them. However it's not attempting to proof  `not`
predicates.

```
% collect everything that can be proved about a particular Proper Noun
all_answers(PN,Answer):-
	findall(Q,(pred(P,1,_),Q=..[P,PN]),Queries), % collect known predicates from grammar
	maplist(prove_question,Queries,Msg),
	delete(Msg,"",Messages),
	( Messages=[] -> atomic_list_concat(['I know nothing about',PN],' ',Answer)
	; otherwise -> atomic_list_concat(Messages,". ",Answer)
	).
```

There's two ways I thought about achieving it:

1. extending the predicates list in the grammar with not fly &rarr; requires duplication of rules into predicates which
   will make maintaining the rules and and keeping them consistent harder
2. trying to proof both the predicates and not predicates &rarr; this is more work but once it's working it will be less
   maintenance and easier to keep rules consistent, which should make teaching Prolexa things easier too

```
prolexa> "Tell me all about opus".
*** utterance(Tell me all about opus)
*** goal(all_answers(opus,_62220))
*** answer(opus is a bird. opus is a penguin)
opus is a bird. opus is a penguin
```

Adding new default and normal rules: Statements like "All birds tweet" should add standard rule whereas statements
like "Some birds tweet" should add a default rule as "some" clearly suggests exceptions to the rule. This is working if
the verb is a pred() in the grammar and a stored rule like `fly(X):-bird(X)` is added but no new pred() can be added.

I've added a new predicate in the grammar for singing. For birds this allows adding a default rules "Some birds sing".
To test this crate the following rules:

1. "Some birds sing" &rarr; adds a stored rule `default(bird(X):-sing(X)`
1. "Swans dont sing" &rarr; adds a stored rule `not sing(X):-swan(X)`
1. "Swans are birds" &rarr; adds a stored rule `bird(X):-swan(X)`
1. "Tweety is a swan." &rarr; adds a stored rule `swan(tweety):-true)`
1. "Explain why peep sings" &rarr; "Peep is a bird. Some birds sing. Peep sings."
1. "Does tweety sing." &rarr; "I dont think that's the case"
1. "Tell me about tweety." &rarr; "Tweety is a swan. Tweety doesn't sing"
1. "Explain why tweety doesn't sing." &rarr; "Tweety is a bird. Tweety is a swan. Swans don't sing. Tweety doesn't sing"

TODO implement adding new pred TODO: "Every birds tweet", "Most birds tweet"

The grammar is used in both ways on the way in to translate human language into Prolog and on the way out to translate
Prolog to human language.

My extensions have also implemented negative statements:

1. "Peter doesnt sing". &rarr; "I will remember that Peter doesnt sing" &rarr; adds a rule `not sing(Peter):-true`

### How to test:

- <instruction on what can be done in colab>

### Changes:

**prolexa_grammar.pl**

- added opus and peep as a proper noun to help testing on the command line
  ```
  proper_noun(s,opus) --> [opus].
  proper_noun(s,peep) --> [peep].
  ```
- extended grammar to deal with not in rules. Behaviour without change: *Tell me all.* &rarr; *I heard you say, tell me
  all , could you rephrase that please?*
  Should be: *Tell me all.* &rarr; *I heard you say, tell me all , could you rephrase that please?* &rarr; *every human
  is mortal. peter is human. penguins dont fly. every bird flies. every penguin is a bird. opus is a penguin. peep is a
  bird*

    1. Added a new `determiner()` to deal with rules with `not` in the head:
  ```
  determiner(p,X=>B,not(X=>H),[(not(H):-B)]) --> [].
  ```
    2. Added new `verb_phrase()` to deal with negations both for singular and plural verbs and adjectives:
  ```
  verb_phrase(s,not(M)) --> [isnt],property(s,M). %TODO TEST
  verb_phrase(p,not(M)) --> [arent],property(p,M). %TODO test
  verb_phrase(p,not(M)) --> [dont], iverb(p,M).
  verb_phrase(s,not(M)) --> [doesnt], iverb(s,M). %TODO TEST
  ```
- add a determiner for default rules that uses 'some' instead of 'every'
  ```
  determiner(p,X=>B,X=>H,[(default(H:-B))]) --> [some].
  ```

**prolexa_engine.pl**

- added new meta interpreter for default rules `explain_rb(Query,Rulebase,[],Proof)`
  ```
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
  ```
    - this meta interpreter calls the existing `prove_rb` to first attempt an explanation via rules:
      ```
      explain_rb(A,Rulebase,P0,P):-
        prove_rb(A,Rulebase,P0,P). % explain by rules only
      ```
    - then it looks for `default(A:-B)` clauses in the rulebase, `explains` B and uses the same `p(A,Rule)` to later
      generate the message from the proof
    - after that it checks that A is consistent with P
    - Both `explain` and `contradiction` are similar to the ones in section 8.1 of the book but changed to use the
      Rulebase and matching Prolexa's variable naming

  The new meta interpreter gets called in `explain_question()` like this: `explain_rb(Query,Rulebase,[],Proof)` instead
  of calling `prove_rb(Query,Rulebase,[],Proof)` like was done before.

- added a top level version that can be used to `prove_questions` including default rules
  ```
  explain_rb(Q,RB):-
  	explain_rb(Q,RB,[],_P).
  ```
  This gets called in both `prove_question(Query,Answer)` versions like this:  `explain_rb(Query,Rulebase)` instead
  of `prove_rb(Query,Rulebase)`

- ensure that default rules are not added twice by using `explain_rb` in known rules instead of directly `prove_rb` and
  check for normal rules as well as default rules (probably not the most elegant solution to use an if else)

```
%%% test if a rule can be deduced from stored rules %%%
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

**prolexa.pl**

- added new rules for default reasoning

<!-- TODO Put the changes here -->

### Limitations:

- can only query about proper nouns "Does tweety fly" not "Do birds fly"
- cannot add new words to the vocabulary
- Doesn't deal with ' in negation properly. Expects doesnt and dont instead of doesn't and don't should be easy to
  escape with '\\'
- If there is a negated rule in the rulebase such as `not fly(X):-penguin(X)` and `penguin(opus):-true` when we ask
  Prolexa "Does opus fly". She answers "I don't think that's the case" due to not checking to proof the negative rules
  which she should do. So it's by coincidence that this query works not by design. However in my opinion Prolexa should
  answer definitive for such rules "No opus doesn't fly" and only answer "Sorry I don't think this is the case" when she
  cannot proof the query.

# <a name="testing">Testing #

To learn what Prolexa can do and to avoid breaking existing functionality with my changes, I started off by making a
list of commands that are working:

**Tell me about...**

- `"tell me about unknownnoun".` - none existing noun &rarr;

  ```
  *** utterance(tell me about unknownnoun)
  *** answer(I heard you say,  tell me about unknownnoun , could you rephrase that please?)
  I heard you say,  tell me about unknownnoun , could you rephrase that please?
  ```

- `"tell me about tweety".` - existing noun with no knowledge &rarr;
  ```
  *** utterance(tell me about tweety)
  *** goal(all_answers(tweety,_8290))
  *** answer(I know nothing about tweety)
  I know nothing about tweety
  ```

- `"tell me about peter".` - existing noun with knowledge &rarr;
  ```
  *** utterance(tell me about peter)
  *** goal(all_answers(peter,_2548))
  *** answer(peter is human. peter is mortal)
  peter is human. peter is mortal
  ```

**Is ...**

- `"Is unknownnoun mortal".` - existing noun without rule rule &rarr;
  ```
  *** utterance(Is unknownnoun mortal)
  *** answer(I heard you say,  Is unknownnoun mortal , could you rephrase that please?)
  I heard you say,  Is unknownnoun mortal , could you rephrase that please?
  ```

- `"Is tweety a bird".` - existing noun without rule rule &rarr;
  ```
  *** utterance(Is tweety a bird)
  *** query(bird(tweety))
  *** answer(Sorry, I don't think this is the case)
  Sorry, I don't think this is the case
  ```

- `"Is peter mortal".` - existing noun and rule &rarr;
  ```
  *** utterance(Is peter mortal)
  *** query(mortal(peter))
  *** answer(peter is mortal)
  peter is mortal
  ```

**Explain...**

- `"Explain why unknownnoun is mortal".` - none existing noun &rarr;
  ```
  *** utterance(Explain why unkownnoun is mortal)
  *** answer(I heard you say,  Explain why unkownnoun is mortal , could you rephrase that please?)
  I heard you say,  Explain why unkownnoun is mortal , could you rephrase that please?
  ```

- `"Explain why peter is a mortal".` - existing noun that's explainable &rarr;
  ```
  *** utterance(Explain why peter is a mortal)
  *** goal(explain_question(mortal(peter),_5252,_5240))
  *** answer(peter is human; every human is mortal; therefore peter is mortal)
  peter is human; every human is mortal; therefore peter is mortal
  ```

- `"Explain why tweety is a bird".` - existing noun that's not explainable &rarr;
  ```
  *** utterance(Explain why tweety is a bird)
  *** goal(explain_question(bird(tweety),_4470,_4458))
  *** answer(Sorry, I don't think this is the case)
  Sorry, I don't think this is the case
  ```

**Add a new rule**

- `"Unknownnoun is mortal".` - none existing noun &rarr; **this does not work on the cli comand line but it does work
  with ProlexaPlus**
  ```
  *** utterance(Unknownnoun is mortal)
  *** answer(I heard you say,  Unknownnoun is mortal , could you rephrase that please?)
  I heard you say,  Unknownnoun is mortal , could you rephrase that please?
  ```

- `"Peter is mortal".` - already existing rule &rarr;
  ```
  *** utterance(Peter is mortal)
  *** rule([(mortal(peter):-true)])
  *** answer(I already knew that Peter is mortal)
  I already knew that Peter is mortal
  ```

- `"Tweety is a bird".` - new rule &rarr;
  ```
  *** utterance(Tweety is a bird)
  *** rule([(bird(tweety):-true)])
  *** answer(I will remember that Tweety is a bird)
  I will remember that Tweety is a bird
  ```

**Things about the grammar**

- It can do simple singular and plural even in self added rules:
  ```
  prolexa> "Tweety flies".
  *** utterance(Tweety flies)
  *** rule([(fly(tweety):-true)])
  *** answer(I will remember that Tweety flies)
  I will remember that Tweety flies

  prolexa> "Does Tweety fly".
  *** utterance(Does Tweety fly)
  *** query(fly(tweety))
  *** answer(tweety flies)
  tweety flies
  ```

  also with grammatical errors in the query:
  ```
  prolexa> "Does tweety flies".
  *** utterance(Does tweety flies)
  *** query(fly(tweety))
  *** answer(tweety flies)
  tweety flies
  ```

I then decided that it would take too long to manually run through these every time I'm making a change and decided to
build a testing notebook that I can execute on colab where I can assert that existing and new functionality works.

To ensure the tests can be run as many times as wanted they do reset the grammar in the cleanup function!

There also seems to be some interesting differences between Google colab and running on my command line:

**Google colab**

  ```
  print(query('tweety flies'))
  print(query('tell me all'))
  ```

  ```
  I already knew that tweety flies
  every human is mortal. peter is human. tweety is a fly
  ```

*Note the wrong rule: tweety is a fly. On reseting the grammar it says:*

  ```
  every human is mortal. peter is human. tweety flies.
  ```

That's very strange, might explore if I got time

**prolexa_cli on my mac (SAME CODE!)**

  ```
  prolexa> "Tweety flies".
  *** utterance(Tweety flies)
  *** rule([(fly(tweety):-true)])
  *** answer(I will remember that Tweety flies)
  I will remember that Tweety flies
  *** utterance(Tell me all)
  *** goal(all_rules(_77782))
  *** answer(every human is mortal. peter is human. tweety flies)
  every human is mortal. peter is human. tweety flies
  ```

Also interesting to know that I can only add rules about proper nouns. I cannot add rules about e.g birds
--------

# some other fixes

Due to how the grammar deals with verbs I've decided to use base forms for verbs

TOOD:
allow a user via colb to add verb Rules currently they would have to do it via:
`peep fly` instead of `peep flies`

1. If tall is defined as a verb this is how prolexa answers:

  ```
  prolexa> "Tell me all about peter".
  *** utterance(Tell me all about peter)
  *** goal(all_answers(peter,_48058))
  *** answer(peter is human. peter is mortal. peter is a tall)
  peter is human. peter is mortal. peter is a tall
  prolexa> "Does peter tall".
  *** utterance(Does peter tall)
  *** query(tall(peter))
  *** answer(peter is a tall)
  peter is a tall
  ```

2. If tall is defined as an adjective this is how prolexa answers:

  ```
  "Tell me all about peter".
  *** utterance(Tell me all about peter)
  *** goal(all_answers(peter,_48066))
  *** answer(peter is human. peter is mortal. peter is tall)
  peter is human. peter is mortal. peter is tall
  ```

Does... questions work with verbs Is... questions work with adjectives However should be the same:
'Is peter tall' -> Peter is tall
'Does opus fly' -> Opus flies not Opus is a fly"

Figured out that "Does opus fly" will be translated into fly(opus). If I formulate the rule as fly(opus) it works (TODO
the not doesn't work so needs to be fixed too):

Which I actually think it's better to keep the predicate in the base form of the verb so flies(opus) better as fly(opus)
IMO

```
prolexa> "Tell me all about opus".
*** utterance(Tell me all about opus)
*** goal(all_answers(opus,_48058))
*** answer(opus is a bird. opus is a penguin. opus flies)
opus is a bird. opus is a penguin. opus flies
prolexa> "tell me all about peep".
*** utterance(tell me all about peep)
*** goal(all_answers(peep,_51174))
*** answer(peep is a bird. peep flies)
peep is a bird. peep flies
prolexa>
```

and it also works form

```
prolexa> "Does opus fly".
*** utterance(Does opus fly)
*** query(fly(opus))
*** answer(opus flies)
opus flies
```

Not:
it finds `goal(all_answers(opus, Answers))`

- this finds all the predicates and proves each
  query [human(opus), mortal(opus), penguin(opus), sparrow(opus), fly(opus)] &rarr; fly(opus) should fail but it doesn't
  check for contradictions

**Solution**

Add a contradiction check for rules so that only rules are added that are not contradicted

```
prove_rb(A,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_rb(B,Rulebase,[p(A,Rule)|P0],P),
	not contradiction(A,Rulebase,P). % A consistent with P
```

For the contradiction check use a simple prove instead of prove_rb to avoid circular contradiction checks

```
prove_s(A,Rulebase,P0,P):-
  find_clause((A:-B),Rule,Rulebase),
	prove_s(B,Rulebase,[p(A,Rule)|P0],P).
```

With the new contradiction check the output is correct and Prolexa can now handle rules with contradictions by leaving
them out:

```
prolexa> "tell me all about opus".
*** utterance(tell me all about opus)
*** goal(all_answers(opus,_58646))
*** answer(opus is a bird. opus is a penguin)
opus is a bird. opus is a penguin
prolexa> "tell me all about peep".
*** utterance(tell me all about peep)
*** goal(all_answers(peep,_60810))
*** answer(peep is a bird. peep flies)
peep is a bird. peep flies
```

And also:

```
prolexa> "Does opus fly".
*** utterance(Does opus fly)
*** query(fly(opus))
*** answer(Sorry, I don't think this is the case)
Sorry, I don't think this is the case
prolexa> "Does peep fly".
*** utterance(Does peep fly)
*** query(fly(peep))
*** answer(peep flies)
peep flies
```

Test that everything still works apart from the new default rules

Now for the explain the result is:

```
prolexa> "Explain why opus flies".
*** utterance(Explain why opus flies)
*** goal(explain_question(fly(opus),_64504,_64492))
*** answer(Sorry, I don't think this is the case)
Sorry, I don't think this is the case
prolexa> "Explain why peep flies".
*** utterance(Explain why peep flies)
*** goal(explain_question(fly(peep),_65306,_65294))
*** answer(peep is a bird; every bird flies; therefore peep flies)
peep is a bird; every bird flies; therefore peep flies
```

But for a normal bird like peep it should be:
`peep is a bird; most birds fly except penguins; peep is not a penguin, therefore peep flies`

And for an abnormal bird like opus it should be:
`opus is a bird; most birds fly execept pengings; opus is a penguin`

-------

## Details on 'Tell me all'. &rarr; all_rules()##

What I want instead is: [Penguins, dont, fly]

1. determiner not to add every and map to Plural
2. verb_phrase not to stumble over that there's no predicate &rarr; simple solution would be to add a predicate for not(
   fly(X)) other solution would be to use the not to add a dont and then remove the not and test the verb_phrase on just
   fly(X)

- Calls Goal `all_rules(Answer)`
    - findsall stored_rules &rarr; Rules

  Then for every rule:
    - maplist Goals rule2message, List1=Rules, List2 (where the message will be going)
        - maplist(Rules, List2, rule2message)
            - calls Goal rule2message with first item of the Rules and List 2 `rule2message(Rule, Message)`
                - calls `phrase(sentence1(Rule), Sentence)` &rarr; C in sentence one is the Rule (See grammar)
                    - calls `determiner` &rarr; N=s (cause it was the first), M1=body of rule, M2=head of rule, Message
                      now has 'every' added
                    - calls `noun(N, M)` &rarr; N is the numerous (Singular/Plural), M is the term e.g human
                        - adds the noun to message [every M1] (M1 being the body) and calls `pred2gr(_P, 1, n/Noun, M1)`
                            - pred2gr &rarr; P = _P, 1 ignore its session id, C/W is e.g n/Noun, X=>Lit with M1 (human):
                                1. pred(P, 1, L) &rarr; all new variables &rarr; that goes through each `pred(..)`
                                   clause in the Grammar: pred(P,1,L) &rarr; pred(fly,1,[v/fly])  &rarr P is the
                                   predicate
                                2. member(C/W,L) &rarr; member(n/Noun, [v/fly]) passes if C/W is element of L
                                3. Lit=..[P,X] &rarr; P=body=W, C=n (from C/W) -> not too sure
                    - calls `verb_phrase(N, M2)` &rarr; M2 = head, N=numerous from determiner
                      e.g `verb_phrase(s, mortal)`
                        - uses is if N is s, are if N is p, calls `property(s,M)` with M being head
                            - adjective() calls `pred2gr(_P, 1, a/Adj, M2)`
                              **sentence1(C) matches to [noun, adjective] or [noun, noun] or [noun, iverb]**
                              Singular or plural get's defined in the determiner and is used from then on
                    - calls atomics_to_string(Sentence, " ", Message) &rarr;
                      atomics_to_string([every, human, is, mortal], " ", Message) $rarr; Message="every human is mortal"

What goes wrong for rule `not(fly(X)):-penguin(X)`:

- `phrase(sentence1([not(fly(X)):-penguin(X)]), Sentence)`
    - `determiner(N,M1,M2,C)` &rarr; `determiner(N,M1,M2,[not(fly(X)):-penguin(X)])`
        - N=s, M1=B=penguin(X), M2=H=not(fly(X)), Sentence=[every]
        - **starts the sentence wrong for negation**
    - `noun(N,M1)` &rarr; `noun(s,X=>penguin(X))`
        - Noun = penguin
        - **wrongly mapped to signular**
    - `verb_phrase(N, M2)` &rarr; `verb_phrase(s, X=not(fly(X)))`
        - adds [is]
        - **cannot find a predicate not(fly(X))**

--------

## Tell me all about peep. &rarr; all_answers(peep, Answers)

`all_answers()` uses `prove_question()` which uses `prove_rb()` to proof all the answers the first one finds.

## Explain why peep flies. &rarr;

- `all_answers()` uses `explain_question()` which uses the new `explain_rb()` meta interpreter
    - this finds proof: `[ p(bird(peep),[(bird(peep):-true)]), default(fly(peep),[default((fly(A):-bird(A)))])
      ]`
- `pstep2message` reusing the same p() for proving

Explain is a method that already exists in SWI so I also called it explain_rb instead of explain to avoid error

## Collecting Answers for nouns and proper noun_s2p

It would be nice if `all_answers()` would not take the things to prove from the pred list in the grammar

```
% collect everything that can be proved about a particular Proper Noun
all_answers(PN,Answer):-
	findall(Q,(pred(P,1,_),Q=..[P,PN]),Queries), % collect known predicates from grammar
	maplist(prove_question,Queries,Msg),
	delete(Msg,"",Messages),
	( Messages=[] -> atomic_list_concat(['I know nothing about',PN],' ',Answer)
	; otherwise -> atomic_list_concat(Messages,". ",Answer)
	).
```

But instead from the rules `all_rules` but with X assigned. This would also allow queries like
"Are Humans mortal".

```
all_rules(Answer):-
	findall(R,prolexa:stored_rule(_ID,R),Rules),
	maplist(rule2message,Rules,Messages),
	( Messages=[] -> Answer = "I know nothing"
	; otherwise -> atomic_list_concat(Messages,". ",Answer)
	).
```

First solution:

```
% collect everything that can be proved about a particular Proper Noun
all_answers(PN,Answer):-
	findall(Q,(pred(P,1,_),Q=..[P,PN]),Queries), % collect known predicates from grammar
	maplist(prove_not_question,Queries,NotMsg),
	maplist(prove_question,Queries,Msg),
	delete(NotMsg,"",NotMsg1),
	delete(Msg,"",Msg1),
	append(Msg1, NotMsg1, Messages),
	( Messages=[] -> atomic_list_concat(['I know nothing about',PN],' ',Answer)
	; otherwise -> atomic_list_concat(Messages,". ",Answer)
	).

% two-argument version that can be used in maplist/3 (see all_answers/2)
prove_not_question(Query,Answer):-
	findall(R,prolexa:stored_rule(_SessionId,R),Rulebase),
	( explain_rb(not Query,Rulebase) ->
		transform(not Query,Clauses),
		phrase(sentence(Clauses),AnswerAtomList),
		atomics_to_string(AnswerAtomList," ",Answer)
	; Answer = ""
	).
```

It's not very neat as loads of duplication but it works. Second solution is to do the explanation for the negated
predicate in an else_if clause directly in the `prove_question`
which means `all_answers(PN,Answer)` does not need to get changed. I believe this is a less messy solution.

```
% two-argument version that can be used in maplist/3 (see all_answers/2)
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




