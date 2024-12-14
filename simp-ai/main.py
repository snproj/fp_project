import os
from openai import OpenAI
import sys

assert len(sys.argv) == 2

filename = sys.argv[1]
outname = "o.simp"

with open(filename, "r") as simp_file:
    raw = simp_file.read()

client = OpenAI(
    api_key=os.environ.get("OPENAI_API_KEY"),
)

def separate_into_segments(raw):
    ls = []
    buf = []
    inquote = False
    for c in raw:
        if c == '"':
            ls.append((''.join(buf), inquote))
            buf = []
            inquote = not inquote
        else:
            buf.append(c)
    ls.append(("".join(buf), inquote))
    return ls


def cover_prompts(ls):
    new = []
    for line, inquote in ls:
        if inquote:
            new.append(ai_assist(line))
        else:
            new.append(line)
    return ''.join(new)

def ai_assist(line):
    chat_completion = client.chat.completions.create(
        messages=[
            {
                "role": "user",
                "content": line,
            }
        ],
        model="gpt-4o-mini",
    )
    res = chat_completion.choices[0].message.content
    assert len(res) > 0
    return res

x = separate_into_segments(raw)

y = cover_prompts(x)

with open(outname, "w") as ofile:
    ofile.write(y)
