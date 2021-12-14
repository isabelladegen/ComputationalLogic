# Report for 2021 Assignment Computational Logic for Artificial Intelligence COMSM0022

### Author
[Isabella Degen](https://github.com/isabelladegen)

### Contents
 1. [Default Rules](#defaultrules)
 1. [Testing](#testing)



# <a name="defaultrules">Default Rules #

Example:

`Most birds fly except penguins. Tweety is a bird. Therefore, assuming Tweety is not a penguin, Tweety flies.
`
### Approach:
Extend prolexa to do default reasoning by introducing to types of rules: `default()` and `rules()`

### How to test:
- bla

### Changes:

### Limitations:


# <a name="testing">Testing #

To avoid breaking existing functionality with my changes I started of by  making a list of commands that need to keep working:

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
- `"Unknownnoun is mortal".` - none existing noun &rarr;
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
