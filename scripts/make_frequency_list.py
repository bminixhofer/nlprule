# uses wordfreq==2.3.2
import argparse

import wordfreq

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("languages", nargs="+")
    parser.add_argument("--out-pattern", type=str)
    parser.add_argument("--top_n", type=int, default=1_000)

    args = parser.parse_args()

    for language in args.languages:
        with open(args.out_pattern.format(language), "w") as f:
            # wordfreq lists lowercase all words
            for word in wordfreq.top_n_list(language, args.top_n):
                f.write(word + "\n")
                f.write(word.title() + "\n")
