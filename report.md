# Task 4

*Yong Zheng Yew & Tay Wei Han Joel*

## Idea
For Task 4, we had the idea, inspired by this video (https://www.youtube.com/watch?v=ueGC3xVcDlc) to create a very simple joke extension to SIMP where the programmer can type in ChatGPT prompts in quotes, and this gets filled in during compile time. Technically, if a spec were to be written, as long as it is declared in the spec that the programmer is responsible for making sure the prompts return values that are valid in SIMP, the language is technically functional.

We've taken the liberty of calling this SIMP-AI.

## Implementation

Originally, it was going to be done in Haskell, but the package that was to be used (https://github.com/braejan/haskell-openai) produced some technical difficulties. Thus, Python was used with the official OpenAI API.

- `compiler.sh` is the script the user interacts with, which calls `main.py` as well as `simp`.
- `main.py` reads the SIMP-AI file, calls the ChatGPT API and returns what should be a valid SIMP file (assuming prompts were responsible)
- `simp` is a binary copy of the compiler completed and built in Task 3

### API Key
A side effect of using ChatGPT is that an API key is needed to run it; Zheng Yew has one and will contact you to enquire if/when you would need this.

## Syntax Description / Running Instructions

For these, please see `simp-ai/README.md`.
