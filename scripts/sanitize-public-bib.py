"""Remove private Zotero fields from a BibTeX export without reformatting entries.

Usage:
    python scripts/sanitize-public-bib.py docs/references.bib

The rewrite is atomic. Git remains the backup for tracked bibliography files.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path


DROP_FIELDS = {"abstract", "annotation", "file", "note"}
FIELD_START = re.compile(r"^\s*([A-Za-z][A-Za-z0-9_-]*)\s*=\s*(.*)$")


def brace_delta(text: str) -> int:
    """Count unescaped braces in a BibTeX value fragment."""
    delta = 0
    escaped = False
    for char in text:
        if escaped:
            escaped = False
            continue
        if char == "\\":
            escaped = True
        elif char == "{":
            delta += 1
        elif char == "}":
            delta -= 1
    return delta


def sanitize(text: str) -> tuple[str, dict[str, int]]:
    output: list[str] = []
    removed = {field: 0 for field in sorted(DROP_FIELDS)}
    dropping = False
    brace_depth = 0
    quoted = False

    for line in text.splitlines(keepends=True):
        if dropping:
            if quoted:
                unescaped_quotes = len(re.findall(r'(?<!\\)"', line))
                if unescaped_quotes % 2 == 1:
                    quoted = False
                    dropping = False
            else:
                brace_depth += brace_delta(line)
                if brace_depth <= 0:
                    dropping = False
            continue

        match = FIELD_START.match(line)
        if match is None or match.group(1).lower() not in DROP_FIELDS:
            output.append(line)
            continue

        field = match.group(1).lower()
        value = match.group(2).lstrip()
        removed[field] += 1

        if value.startswith("{"):
            brace_depth = brace_delta(value)
            dropping = brace_depth > 0
        elif value.startswith('"'):
            quoted = len(re.findall(r'(?<!\\)"', value)) % 2 == 1
            dropping = quoted
        else:
            dropping = not value.rstrip().endswith(",")

    if dropping:
        raise ValueError("Unterminated BibTeX field while sanitizing")

    return "".join(output), removed


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("Usage: sanitize-public-bib.py PATH")

    path = Path(sys.argv[1]).resolve()
    if not path.is_file():
        raise SystemExit(f"Not a file: {path}")

    original = path.read_text(encoding="utf-8-sig")
    cleaned, removed = sanitize(original)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(cleaned, encoding="utf-8", newline="\n")
    temporary.replace(path)

    summary = ", ".join(f"{key}={value}" for key, value in removed.items())
    print(f"Sanitized {path}: {summary}")


if __name__ == "__main__":
    main()
