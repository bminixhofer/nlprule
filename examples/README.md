Example uses of NLPRule.

## Postprocessing for Natural Language Generation

NLPRule can be used as postprocessing for e. g. GPT2. `correct_nlg.py` generates a fixed number of tokens with the first sentence of a wikipedia article as prompt and checks how many suggestions NLPRule finds in the generated text. Sample output:

```
[...]

Before: ...t out, as a condition of its being operated. Each lock keeper should ensure that all locks are operated and tha...
After: ...t out, as a condition of its being operated. Each lockkeeper should ensure that all locks are operated and tha...
Message: This noun is normally spelled as one word.
Type: grammar
---

Before: ...The Washington Post reported in October of 1963 that the Washington Post reported that "Mr. Saul ...
After: ...The Washington Post reported in October 1963 that the Washington Post reported that "Mr. Saul Gru...
Message: When specifying a month and year, 'of' is unnecessary: October 1963.
Type: misspelling
---

Before: ...n saying that he had written a book on Napoleon's life so he was born in 1906. The book, The Secret History...
After: ...n saying that he had written a book on Napoleon's life, so he was born in 1906. The book, The Secret Histor...
Message: Use a comma before 'so' if it connects two independent clauses (unless they are closely connected and short).
Type: typographical
---

[...]

Before: ...The title track on this record has been included in the album...
After: ...The title track on this record has been included on the album...
Message: The usual collocation for "album" is "on", not "in" when "album" refers to a collection of recorded music. If by "album" you mean a collection of photos, "in an album" is correct. Did you mean on the album?
Type: grammar
---

Before: ...he Z-machine version (in the standardised format) is comprised of 32 (in total) bytes, one per line. ...
After: ...he Z-machine version (in the standardised format) comprises 32 (in total) bytes, one per line. ...
Message: Did you mean comprises or consists of or is composed of?
Type: misspelling
---

Before: ...ith your friends and family when I went out for a weekend and it seemed like no other band coul...
After: ...ith your friends and family when I went out for a weekend, and it seemed like no other band coul...
Message: Use a comma before 'and' if it connects two independent clauses (unless they are closely connected and short).
Type: typographical
---

Before: ...esake, its unique appearance allows for increased damage and it does not require the use of an AOE spell. The ...
After: ...esake, its unique appearance allows for increased damage, and it does not require the use of an AOE spell. The...
Message: Use a comma before 'and' if it connects two independent clauses (unless they are closely connected and short).
Type: typographical
---

Before: ...er came in September at the London Olympics . The runners up were the French and the Swedish. The women's 3000...
After: ...er came in September at the London Olympics . The runners-up were the French and the Swedish. The women's 3000...
Message: The noun runners-up (= didn't finish first place) is spelled with a hyphen.
Type: grammar
---


Generated 192300 tokens.
misspelling:    35 suggestions  (0.18 per 1000 tokens)
style:          53 suggestions  (0.28 per 1000 tokens)
typographical:  112 suggestions (0.58 per 1000 tokens)
grammar:        29 suggestions  (0.15 per 1000 tokens)
none:           3 suggestions   (0.02 per 1000 tokens)
inconsistency:  2 suggestions   (0.01 per 1000 tokens)
```

Which shows that NLPRule finds a significant amount of suggestions for current NLG models! Note that not all of these are errors, some are just suggestions for improvement.