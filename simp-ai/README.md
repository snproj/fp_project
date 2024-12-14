# SIMP-AI

*AI-enhanced SIMP for the modern era.*

A novel language based on SIMP. Syntax is exactly the same, except that we have included a new construct, the prompt. Prompts are resolved at compile time, and are directly rewritten as the result of the prompt.

## Typechecking
Responsibility is on the user to ensure that results of prompts typecheck. Other typechecking rules are based (well, copied) from SIMP.


## Syntax
Prompts are any string enclosed in **double quotes**.

## Example SIMP-AI Code
The following is a fibonnacci example. It's included as `fib.simpai` in the repo:
```simp-ai
x = input;
f = 0;
s = 1;
c = 0;
t = 0;
while c < x {
    t = "Return as your response the letter between E and G. Return it as lowercase. Do not return anything else.";
    f = s;
    s = t + f;
    c = c + 1;
}
return s;
```

## Example Compiler Usage

Running SIMP-AI requires an OpenAI API Key; feel free to contact the developers for one if needed.

```bash
export OPENAI_API_KEY="..."

./compiler.sh fib.simpai fib.wasm fib

wasmtime --invoke fib fib.wasm 30
```

This should return 1346269.

An intermediary SIMP file named `o.simp` will be generated, which is useful for debugging. Feel free to remove it.