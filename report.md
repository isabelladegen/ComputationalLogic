# Report for 2021 Assignment Computational Logic for Artificial Intelligence COMSM0022

### Author
[Isabella Degen](https://github.com/isabelladegen)

### Contents
 1. [Default Rules](#defaultrules)
 1. [Testing](#testing)

 Perhaps write about
 1. [Coding Practices](#codingpractices) and write about testing as a sub set



# <a name="defaultrules">Default Rules #

Example:

`Most birds fly except penguins. Tweety is a bird. Therefore, assuming Tweety is not a penguin, Tweety flies.
`
### Approach:
Extend Prolexa to do default reasoning by introducing to types of rules: `default()` and `rules()`
The grammar already maps 'Explain why ...' questions to `explain_question`: `command(g(explain_question(Q,_,Answer),Answer)) --> [explain,why],sentence1([(Q:-true)]).`

For default reasoning to work I need to be able to differentiate between rules and defaults. Statements
without exceptions are written as a rule:
- `rule(not flies(X):-penguin(X))`
- `rule(bird(X):-penguin(X))`
- `rule(penguin(opus):-true)`
- `rule(bird(tweety):-true)`


Statements with exceptions are written as defaults:
-`default(flies(X):-bird(X))`

Adding new meta interpreters `explain` for default rules and `contradiction`.  Reusing the existing meta interpreter `prove_rb` to first attempt an explanation using a rule. If this fails it will attempt to find a default instead. See changes for more details.

First checking that all the explanation using existing rules still work. Then I extended the 'Rulebase' with defaults.

----
First:
"Explain why opus flies".
"default((flies(opus):-bird(opus))), rule((bird(opus):-true))" &rarr; `Most birds fly. Tweety is a bird` &rarr;
Default gets extended with most. The rules are handeled as now.

Second:
Add exception text. 'Most birds fly except xyz'


 "Explain why peter is human".
 gets translated to
 explain_question(human(peter), _91778, Answer)


### How to test:
- <instruction on what can be done in colab>

### Changes:

**prolexa_grammar.pl**
- added opus and peep as a proper noun to help testing on the command line
  ```
  proper_noun(s,opus) --> [opus].
  proper_noun(s,peep) --> [peep].
  ```
- extended grammar to deal with not in rules.
  Behaviour without change: *Tell me all.* &rarr; *I heard you say,  tell me all , could you rephrase that please?*
  Should be: *Tell me all.* &rarr; *I heard you say,  tell me all , could you rephrase that please?* &rarr; *every human is mortal. peter is human. penguins dont fly. every bird flies. every penguin is a bird. opus is a penguin. peep is a bird*

  1. Added a new `determiner()` to deal with rules with `not` in the head:
  ```
  determiner(p,X=>B,not(X=>H),[(not(H):-B)]) --> [].
  ```
  2. Added new `verb_phrase()` to deal with negations both for singular and plural verbs and adjectives:
  ```
  verb_phrase(s,not(M)) --> [isnt],property(s,M).
  verb_phrase(p,not(M)) --> [arent],property(p,M).
  verb_phrase(p,not(M)) --> [dont], iverb(p,M).
  verb_phrase(s,not(M)) --> [doesnt], iverb(s,M).
  ```

**prolexa_engine.pl**
- added new meta interpreter for default rules `explain(Query,Rulebase,[],Proof)`
  - this meta interpreter calls the existing `prove_rb` to attempt an explanation via rules first:
    ```
    explain(A,Rulebase,P0,P):-
      prove_rb(A,Rulebase,P0,P). % explain by rules only
    ```
  - it also checks for contradictions
  - Both `explain` and `contradiction` are similar to the ones in section 8.1 of the book but changed to use the Rulebase and matching Prolexa's variable naming
- In `explain_question` call the new `explain(Query,Rulebase,[],Proof)` meta interpreter instead of `prove_rb(Query,Rulebase,[],Proof)`

**prolexa.pl**
- added new rules for default reasoning
<!-- TODO Put the changes here -->

### Limitations:


# <a name="testing">Testing #

To learn what Prolexa can do and to avoid breaking existing functionality with my changes, I started off by making a list of commands that are working:

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
- `"Unknownnoun is mortal".` - none existing noun &rarr; **this does not work on the cli comand line but it does work with ProlexaPlus**
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

I then decided that it would take too long to manually run through these every time I'm making a change and decided to build a testing notebook that I can execute on colab where I can assert that existing and new functionality works.

To  ensure the tests can be run as many times as wanted they do reset the grammar in the cleanup function!

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
allow a user via colb to add verb Rules
currently they would have to do it via:
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
2.  If tall is defined as an adjective this is how prolexa answers:
  ```
  "Tell me all about peter".
  *** utterance(Tell me all about peter)
  *** goal(all_answers(peter,_48066))
  *** answer(peter is human. peter is mortal. peter is tall)
  peter is human. peter is mortal. peter is tall
  ```

Does... questions work with verbs
Is... questions work with adjectives
However should be the same:
'Is peter tall' -> Peter is tall
'Does opus fly' -> Opus flies not Opus is a fly"

Figured out that "Does opus fly" will be translated into fly(opus). If I formulate the rule
as fly(opus) it works (TODO the not doesn't  work so needs to be fixed too):

Which I actually think it's better to keep the predicate in the base form of the verb so
flies(opus) better as fly(opus) IMO

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
- this finds all the predicates and proves each query [human(opus), mortal(opus), penguin(opus), sparrow(opus), fly(opus)] &rarr; fly(opus) should fail but it doesn't check for contradictions

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

With the new  contradiction check the output is correct and Prolexa can now handle rules with contradictions by leaving them out:

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
## Details on 'Tell me all'.'##

What I want instead is: [Penguins, dont, fly]
1. determiner not to add every and map to Plural
2. verb_phrase not to stumble over that there's no predicate &rarr; simple solution would be to add a predicate for not(fly(X)) other solution would be to use the not to add a dont and then remove the not and test the verb_phrase on just fly(X)

- Calls Goal `all_rules(Answer)`
  - findsall stored_rules &rarr; Rules

  Then for every rule:
  - maplist Goals rule2message, List1=Rules, List2 (where the message will be going)
    - maplist(Rules, List2, rule2message)
      - calls Goal rule2message with first item of the Rules and List 2 `rule2message(Rule, Message)`
        - calls `phrase(sentence1(Rule), Sentence)` &rarr; C in sentence one is the Rule (See grammar)
          - calls `determiner` &rarr; N=s (cause it was the first), M1=body of rule, M2=head of rule, Message  now has 'every' added
          - calls `noun(N, M)` &rarr; N is the numerous (Singular/Plural), M is the term e.g human
            - adds the noun to message [every M1] (M1 being the body) and calls `pred2gr(_P, 1, n/Noun, M1)`
              - pred2gr &rarr; P = _P, 1 ignore its session id, C/W is e.g n/Noun, X=>Lit with M1 (human):
                1. pred(P, 1, L) &rarr; all new variables &rarr; that goes through each `pred(..)` clause in the Grammar: pred(P,1,L) &rarr; pred(fly,1,[v/fly])  &rarr P is the predicate
                2. member(C/W,L) &rarr; member(n/Noun, [v/fly]) passes if C/W is element of L
                3. Lit=..[P,X] &rarr; P=body=W, C=n (from C/W) -> not too sure
          - calls `verb_phrase(N, M2)` &rarr; M2 = head, N=numerous from determiner e.g `verb_phrase(s, mortal)`
            - uses is if N is s, are if N is p, calls `property(s,M)` with M being head
              - adjective() calls `pred2gr(_P, 1, a/Adj, M2)`
          **sentence1(C) matches to [noun, adjective] or [noun, noun] or [noun, iverb]**
          Singular or plural get's defined in the determiner and is used from then on
          - calls atomics_to_string(Sentence, " ", Message) &rarr; atomics_to_string([every, human, is, mortal], " ", Message) $rarr; Message="every human is mortal"

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

---------
Write about:
- What did you change
  - Example
- What can I do
- How did you go  about solving it


Eval:
- Can I see what they did?
- was it a reasonable way?
- Does it immediately break?
