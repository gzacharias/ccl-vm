#!/usr/bin/env python3
"""Compare .bc files, allowing gensyms to be renamed.

Native bc bundles are byte-identical across machines (compare-bundles.sh checks that), but a bundle
rebuilt in the VM gets different gensym names, since the gensym counter depends on everything
compiled before; this compares such a bundle with a native one (ccl-vm#13).

usage: bc-compare.py A B          A and B are .bc files, or bundle directories (all *.bc files under them)

Whitespace is ignored.  An uninterned symbol is dumped as ($FS-SYMBOL ($FS-STRING "name") NIL); its
name may differ between A and B as long as the renaming is consistent: one-to-one, the same over the
whole file.  Everything else must match token for token.

For each pair of files prints one of
  identical            same tokens
  gensyms-renamed      same up to renaming gensyms (with the number of renamed gensyms)
  DIFFERENT            and the first mismatch, with line numbers and some context
Exit status 0 if no pair is DIFFERENT, 1 otherwise.
"""
import os
import re
import sys

TOKEN = re.compile(r'"(?:[^"\\]|\\.)*"|[()]|[^\s()"]+', re.S)


def tokenize(path):
    """List of (token, line) for the file at PATH."""
    text = open(path, encoding='utf-8', errors='surrogateescape').read()
    tokens = []
    line = 1
    pos = 0
    for match in TOKEN.finditer(text):
        line += text.count('\n', pos, match.start())
        pos = match.start()
        tokens.append((match.group(), line))
    return tokens


def gensym_name_positions(tokens):
    """Indices of the name strings of uninterned symbols: ( $FS-SYMBOL ( $FS-STRING "name" ) NIL )."""
    positions = set()
    for index in range(4, len(tokens) - 3):
        if (tokens[index][0].startswith('"')
                and tokens[index - 1][0] == '$FS-STRING'
                and tokens[index - 2][0] == '('
                and tokens[index - 3][0] == '$FS-SYMBOL'
                and tokens[index + 1][0] == ')'
                and tokens[index + 2][0] == 'NIL'
                and tokens[index + 3][0] == ')'):
            positions.add(index)
    return positions


def context(tokens, index, before=12, after=8):
    return ' '.join(token for token, _ in tokens[max(0, index - before):index + after])


def compare_files(path_a, path_b):
    """Return (verdict, detail)."""
    tokens_a = tokenize(path_a)
    tokens_b = tokenize(path_b)
    gensyms_a = gensym_name_positions(tokens_a)
    gensyms_b = gensym_name_positions(tokens_b)
    a_to_b = {}
    b_to_a = {}

    def mismatch(index, why):
        line_a = tokens_a[index][1] if index < len(tokens_a) else 'EOF'
        line_b = tokens_b[index][1] if index < len(tokens_b) else 'EOF'
        return ('DIFFERENT',
                f'{why} at token {index} (line {line_a} vs {line_b})\n'
                f'    A: {context(tokens_a, index)}\n'
                f'    B: {context(tokens_b, index)}')

    for index in range(min(len(tokens_a), len(tokens_b))):
        token_a = tokens_a[index][0]
        token_b = tokens_b[index][0]
        a_is_gensym = index in gensyms_a
        b_is_gensym = index in gensyms_b
        if a_is_gensym != b_is_gensym:
            return mismatch(index, 'gensym in one file only')
        if a_is_gensym:
            if a_to_b.setdefault(token_a, token_b) != token_b:
                return mismatch(index, f'gensym {token_a} in A already matched to {a_to_b[token_a]} in B')
            if b_to_a.setdefault(token_b, token_a) != token_a:
                return mismatch(index, f'gensym {token_b} in B already matched to {b_to_a[token_b]} in A')
        elif token_a != token_b:
            return mismatch(index, 'tokens differ')
    if len(tokens_a) != len(tokens_b):
        return mismatch(min(len(tokens_a), len(tokens_b)), 'one file is longer')
    renamed = sum(1 for name_a, name_b in a_to_b.items() if name_a != name_b)
    if renamed:
        return 'gensyms-renamed', f'{renamed} of {len(a_to_b)} gensyms renamed'
    return 'identical', ''


def bc_files(directory):
    found = set()
    for root, _, files in os.walk(directory):
        for name in files:
            if name.endswith('.bc'):
                found.add(os.path.relpath(os.path.join(root, name), directory))
    return found


def main(argv):
    if len(argv) != 3:
        sys.exit(__doc__)
    a, b = argv[1], argv[2]
    if os.path.isdir(a) and os.path.isdir(b):
        files_a, files_b = bc_files(a), bc_files(b)
        pairs = [(os.path.join(a, f), os.path.join(b, f), f) for f in sorted(files_a & files_b)]
        only = [f'only in {a}: {f}' for f in sorted(files_a - files_b)] + \
               [f'only in {b}: {f}' for f in sorted(files_b - files_a)]
    else:
        pairs = [(a, b, f'{a} {b}')]
        only = []
    counts = {}
    for path_a, path_b, label in pairs:
        verdict, detail = compare_files(path_a, path_b)
        counts[verdict] = counts.get(verdict, 0) + 1
        if verdict != 'identical':
            print(f'{verdict:16} {label}' + (f'  ({detail})' if verdict == 'gensyms-renamed' else ''))
            if verdict == 'DIFFERENT':
                print(detail)
    for line in only:
        print(line)
    if len(pairs) > 1:
        print('summary: ' + ', '.join(f'{n} {v}' for v, n in sorted(counts.items())))
    return 1 if counts.get('DIFFERENT') or only else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
