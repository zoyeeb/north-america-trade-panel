"""
naics_concordance_fetch.py

[PROTOCOL Step 6.2] Check the official NAICS concordance tables for splits,
merges or redefinitions.

Downloads the three official Census NAICS revision concordances spanning the
2005-2025 benchmark window and flattens them into one tidy edge list:

    $DATA/naics_concordance/naics_concordance_edges.csv
        vintage, old_code, old_title, new_code, new_title, code_changed

R/04_related_party/rp_apply_threshold.R reads that CSV as the authority for
which NAICS6 codes belong to the same lineage, falling back to its
year-adjacency heuristic only for the Census trade-specific codes the official
tables cannot cover.

naics_concordance_fetch.R is the primary implementation; this is the
equivalent with no package dependencies (the two newer concordances are .xlsx,
which is a zip of XML and readable with the standard library alone, and the
oldest is legacy .xls).

Set NA_PANEL_DATA_DIR to put the output somewhere other than <repo>/data.

Run from the repo root:
    python R/03_concordance/naics_concordance_fetch.py
"""


import collections
import csv
import os
import re
import sys
import urllib.request
import xml.etree.ElementTree as ET
import zipfile

BASE = "https://www.census.gov/naics/concordances/"
OUT_DIR = os.path.join(
    os.environ.get("NA_PANEL_DATA_DIR", "data"), "naics_concordance")

FILES = [
    ("2007->2012", "2007_to_2012_NAICS.xls"),
    ("2012->2017", "2012_to_2017_NAICS.xlsx"),
    ("2017->2022", "2017_to_2022_NAICS.xlsx"),
]

NS = "{http://schemas.openxmlformats.org/spreadsheetml/2006/main}"

def read_xlsx(path):
    """Read the first worksheet of an .xlsx as a list of row-lists.

    An .xlsx file is a zip archive of XML parts, so this needs nothing beyond
    the standard library. Cells carrying text point into a shared-strings
    table rather than holding the text inline, which is why that table is
    read first.
    """
    z = zipfile.ZipFile(path)
    shared = []
    if "xl/sharedStrings.xml" in z.namelist():
        root = ET.fromstring(z.read("xl/sharedStrings.xml"))
        for si in root.findall(NS + "si"):
            shared.append("".join(t.text or "" for t in si.iter(NS + "t")))
    sheets = sorted(n for n in z.namelist()
                    if re.match(r"xl/worksheets/sheet\d+\.xml$", n))
    root = ET.fromstring(z.read(sheets[0]))
    rows = []
    for row in root.iter(NS + "row"):
        cells = {}
        for c in row.findall(NS + "c"):
            col = re.match(r"([A-Z]+)", c.get("r")).group(1)
            v = c.find(NS + "v")
            if v is None:
                inline = c.find(NS + "is")
                txt = ("".join(t.text or "" for t in inline.iter(NS + "t"))
                       if inline is not None else "")
            elif c.get("t") == "s":
                txt = shared[int(v.text)]
            else:
                txt = v.text or ""
            cells[col] = txt
        if cells:
            cols = sorted(cells, key=lambda s: (len(s), s))
            rows.append([cells.get(c, "") for c in cols])
    return rows

def read_xls(path):
    """Read the legacy binary .xls. pandas handles this format; the two newer
    files do not need it."""
    import pandas as pd
    x = pd.ExcelFile(path)
    d = x.parse(x.sheet_names[0], header=None, dtype=str)
    return [["" if (v is None or str(v) == "nan") else str(v) for v in row]
            for row in d.values.tolist()]

def main():
    if not os.path.isdir(os.path.dirname(OUT_DIR) or "."):
        sys.exit("Run this from the repo root, or set NA_PANEL_DATA_DIR.")
    os.makedirs(OUT_DIR, exist_ok=True)

    edges = []
    for vintage, fname in FILES:
        path = os.path.join(OUT_DIR, fname)
        if not os.path.exists(path):
            url = BASE + fname
            print("downloading", url)
            with urllib.request.urlopen(url, timeout=180) as r, \
                    open(path + ".part", "wb") as f:
                f.write(r.read())
            os.replace(path + ".part", path)
        else:
            print("using cached", path)

        rows = read_xls(path) if fname.endswith(".xls") else read_xlsx(path)

        start = None
        for i, row in enumerate(rows[:10]):
            if row and row[0].strip().endswith("NAICS Code"):
                start = i + 1
                break
        if start is None:
            sys.exit("Could not find the header row in %s - layout changed." % fname)

        n_before = len(edges)
        for row in rows[start:]:
            if len(row) < 3:
                continue
            old, new = row[0].strip(), row[2].strip()
            if not (re.fullmatch(r"\d{6}", old) and re.fullmatch(r"\d{6}", new)):
                continue
            edges.append({
                "vintage": vintage,
                "old_code": old,
                "old_title": row[1].strip().replace("\n", " ") if len(row) > 1 else "",
                "new_code": new,
                "new_title": row[3].strip().replace("\n", " ") if len(row) > 3 else "",
                "code_changed": "1" if old != new else "0",
            })
        got = len(edges) - n_before
        changed = sum(1 for e in edges[n_before:] if e["code_changed"] == "1")
        print("  %s: %d six-digit pairs, %d where the code changed"
              % (vintage, got, changed))

    if not edges:
        sys.exit("No edges parsed - refusing to write an empty concordance.")

    out = os.path.join(OUT_DIR, "naics_concordance_edges.csv")
    with open(out, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(edges[0].keys()))
        w.writeheader()
        w.writerows(edges)

    adj = collections.defaultdict(set)
    for e in edges:
        if e["code_changed"] == "1":
            adj[e["old_code"]].add(e["new_code"])
            adj[e["new_code"]].add(e["old_code"])
    seen, comps = set(), 0
    for n in adj:
        if n in seen:
            continue
        comps += 1
        stack = [n]
        while stack:
            u = stack.pop()
            if u in seen:
                continue
            seen.add(u)
            stack.extend(adj[u] - seen)

    print("\nwrote %s" % out)
    print("  %d rows; %d distinct changed-code edges; %d official lineages "
          "spanning %d codes" % (len(edges), sum(len(v) for v in adj.values()) // 2,
                                 comps, len(seen)))

if __name__ == "__main__":
    main()
