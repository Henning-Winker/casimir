# Assemble the self-contained concept page: template + landing chart + figures + numbers.
# Run from the package root after dev/concept/figures.R.
import base64, json, pathlib, re
root = pathlib.Path(".")
fig = root / "dev/concept/fig"
tpl = (root / "dev/concept/template.html").read_text(encoding="utf-8")
svg = (root / "inst/app/www/casimir-landing.svg").read_text(encoding="utf-8")
svg = re.sub(r'\s(width|height)="\d+"', "", svg, count=2)  # scale with the page
imgs = {p.stem: "data:image/png;base64," + base64.b64encode(p.read_bytes()).decode() for p in fig.glob("*.png")}
nums = json.loads((fig / "numbers.json").read_text())
out = (tpl.replace("{{LANDING_SVG}}", svg)
          .replace("{{NUMS}}", json.dumps(nums))
          .replace("{{IMGS}}", json.dumps(imgs))
          .replace("{{YEAR}}", str(nums["year"])))
dest = root / "pkgdown/assets/concept.html"
dest.parent.mkdir(parents=True, exist_ok=True)
dest.write_text(out, encoding="utf-8")
print(dest, round(len(out) / 1e6, 2), "MB")
