from collections import Counter
from datasets import load_dataset
from transformers import AutoModelForCausalLM, AutoTokenizer, pipeline
from nlprule import Rules, Tokenizer, SplitOn
from argparse import ArgumentParser


# gets a window from start - offset to end + offset or until a newline character is reached
def window(text, start, end, offset=50):
    new_start, new_end = start, end

    while new_start > 0 and start - new_start < offset and text[new_start - 1] != "\n":
        new_start -= 1

    while (
        new_end < len(text) - 1 and new_end - end < offset and text[new_end + 1] != "\n"
    ):
        new_end += 1

    return text[new_start:new_end]


def correct(text, rules):
    suggestions = list(
        filter(
            # there are frequently suggestions for fixing e. g. quotation marks
            # i am excluding those here
            lambda x: any(c.isalnum() for c in text[x.start : x.end]),
            rules.suggest(text),
        )
    )

    counter = Counter()

    for x in suggestions:
        corrected = rules.apply_suggestions(text, [x])
        print(
            "Before:", "..." + window(text, x.start, x.end) + "...",
        )
        print(
            "After:", "..." + window(corrected, x.start, x.end) + "...",
        )
        print("Message:", x.message)
        print("Type:", rules.rule(x.source).category_type)
        print("---")
        print()

        counter[rules.rule(x.source).category_type] += 1

    return rules.apply_suggestions(text, suggestions), counter


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--rule_lang", choices={"de", "en"}, default="en")
    parser.add_argument("--wikipedia_corpus", type=str, default="20200501.en")
    parser.add_argument("--model_name", type=str, default="gpt2")
    parser.add_argument("--tokens_per_sample", default=100)
    parser.add_argument("--n_samples", default=2000)

    args = parser.parse_args()

    dataset = load_dataset("wikipedia", args.wikipedia_corpus)
    rules = Rules.load(
        args.rule_lang, Tokenizer.load(args.rule_lang), SplitOn([".", "!", "?"])
    )

    tokenizer = AutoTokenizer.from_pretrained(args.model_name)

    pipe = pipeline(
        "text-generation",
        model=AutoModelForCausalLM.from_pretrained(args.model_name),
        device=0,
        tokenizer=tokenizer,
    )

    suggestion_counter = Counter()
    n_tokens = 0

    for text in dataset["train"][: args.n_samples]["text"]:
        try:
            first_sentence = text[: text.index(".") + 1]
        except ValueError:
            continue

        generated = pipe(
            first_sentence,
            min_length=args.tokens_per_sample,
            max_length=args.tokens_per_sample,
            pad_token_id=tokenizer.eos_token_id,
        )[0]["generated_text"][len(first_sentence) :].strip()

        n_tokens += args.tokens_per_sample

        _, counter = correct(generated, rules)
        suggestion_counter.update(counter)

    print(f"Generated {n_tokens} tokens.")

    for key, value in suggestion_counter.items():
        key = ((key or "none") + ":").ljust(15)
        print(
            f"{key} {value} suggestions\t({value / n_tokens * 1000:.2f} per 1000 tokens)"
        )
