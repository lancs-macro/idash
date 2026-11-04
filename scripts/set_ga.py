"""Point every page's Google tag at one GA4 measurement ID.

Run from the repo root: `python scripts/set_ga.py` rewrites the tracked HTML files,
`python scripts/set_ga.py --check` only lists files that differ (exit 1), for CI or a hook.
The same file lives in the uk, int and main repos; change GA_ID in all three.
"""
import re
import subprocess
import sys

GA_ID = "G-ZHNETED6TG"  # GA4 stream "Housing Observatory - GA4", property 382231458

SRC = re.compile(r"(gtag/js\?id=)[^\"']+")
CONFIG = re.compile(r"(gtag\('config',\s*')[^']+(')")
# leftover template stub that loads gtag/js?id=TRACKING_ID
STUB = re.compile(r"[ \t]*<!--[^>]*gtag[^>]*-->\r?\n[ \t]*<script[^>]*id=TRACKING_ID[^>]*></script>\r?\n(?:[ \t]*\r?\n)*", re.I)


def rewrite(text: str) -> str:
    text = STUB.sub("", text)
    text = SRC.sub(rf"\g<1>{GA_ID}", text)
    return CONFIG.sub(rf"\g<1>{GA_ID}\g<2>", text)


def main() -> int:
    check = "--check" in sys.argv
    files = subprocess.run(["git", "ls-files", "*.html"], capture_output=True, text=True, check=True).stdout.split()
    changed = []
    for f in files:
        with open(f, encoding="utf-8", newline="") as fh:
            old = fh.read()
        if "googletagmanager.com/gtag/js" not in old:
            continue
        new = rewrite(old)
        if new != old:
            changed.append(f)
            if not check:
                with open(f, "w", encoding="utf-8", newline="") as fh:
                    fh.write(new)
    print(("would change" if check else "changed"), len(changed), "files")
    for f in changed:
        print(" ", f)
    return 1 if check and changed else 0


if __name__ == "__main__":
    sys.exit(main())
