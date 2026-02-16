import fontforge
import psMat
import os

script_dir = os.path.dirname(os.path.abspath(__file__))
base = fontforge.open(os.path.join(script_dir, "SpaceGrotesk-Bold.ttf"))

for lookup in base.gsub_lookups:
    base.removeLookup(lookup)

allowed = set("abcdefghijklmnopqrstuvwxyz0123456789,./;:-':_[ ")
adjustments = {
    ".": (2.5, 0),
    ",": (2, 0.3),
    "'": (2, -0.3),
    ";": (1.5, 0.2),
    "-": (1.5, 0),
}

to_remove = [
    g.glyphname
    for g in base.glyphs()
    if g.unicode == -1 or chr(g.unicode) not in allowed
]
for name in to_remove:
    base.removeGlyph(name)

for g in base.glyphs():
    if g.references:
        g.unlinkRef()

for g in base.glyphs():
    if g.unicode == -1:
        continue
    char = chr(g.unicode)

    if char not in adjustments:
        continue

    scale, y_shift = adjustments[char]

    bbox = g.boundingBox()
    h = bbox[3] - bbox[1]
    if h == 0:
        continue

    cx, cy = (bbox[0] + bbox[2]) / 2, (bbox[1] + bbox[3]) / 2

    g.transform(
        psMat.compose(
            psMat.translate(-cx, -cy),
            psMat.compose(psMat.scale(scale), psMat.translate(cx, cy)),
        )
    )

    if y_shift != 0:
        g.transform(psMat.translate(0, y_shift * h))

    bbox = g.boundingBox()
    gw = bbox[2] - bbox[0]
    padding = gw * 0.2
    g.width = int(gw + padding)
    g.transform(psMat.translate(padding - bbox[0], 0))

base.generate(
    os.path.join(os.path.join(os.path.dirname(script_dir), "assets"), "font.ttf")
)
