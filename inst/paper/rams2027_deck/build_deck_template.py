#!/usr/bin/env python3
"""Build the 15-slide RGF deck on the official RAMS 2027 template.

Preserves the RAMS slide master (red star logo on every slide) and Calibri fonts;
re-skins content to navy + RAMS-red + grey. Run from the deck directory.
"""
from pptx import Presentation
from pptx.util import Inches, Pt, Emu
from pptx.dml.color import RGBColor
from pptx.enum.text import PP_ALIGN, MSO_ANCHOR
from pptx.enum.shapes import MSO_SHAPE

TPL = "RAMS-2027-Presentation-Template.pptx"
OUT = "RGF_RAMS2027_RAMS.pptx"

# ---- palette ----
NAVY = "14243A"; RED = "C32135"; GREY = "8A8F98"; LIGHT = "F1F4F8"
WHITE = "FFFFFF"; INK = "1C2733"; MUTE = "5B6875"; STEEL = "3E5C76"; PALE = "EAF0F6"
BFONT = "Calibri"; HFONT = "Calibri"
C = lambda h: RGBColor.from_string(h)

FIGS = {  # path -> aspect (w/h)
    "baseline": ("figs/fig_baseline.png", 1600 / 1200),
    "rga": ("figs/fig_rga.png", 1600 / 1200),
    "comparison": ("figs/fig_comparison.png", 1700 / 1250),
    "countratio": ("figs/fig_countratio.png", 1500 / 1050),
    "mc": ("figs/fig_mc.png", 1600 / 1050),
    "sensitivity": ("figs/fig_sensitivity.png", 1600 / 1100),
}

prs = Presentation(TPL)
BLANK = prs.slide_layouts[6]   # "Blank" (logo inherited from master)
TITLE = prs.slide_layouts[0]   # "Title Slide"

# ---- delete the 8 sample slides cleanly (drop rel + sldId) ----
sldIdLst = prs.slides._sldIdLst
for sldId in list(sldIdLst):
    prs.part.drop_rel(sldId.rId)
    sldIdLst.remove(sldId)


# ---------- helpers ----------
def _set_para(par, runs, size, color, bold, italic, font, align, space_after, line_spacing):
    par.alignment = align
    if space_after is not None:
        par.space_after = Pt(space_after)
    if line_spacing is not None:
        par.line_spacing = line_spacing
    if isinstance(runs, str):
        runs = [(runs, {})]
    for text, o in runs:
        r = par.add_run()
        r.text = text
        f = r.font
        f.name = o.get("font", font)
        f.size = Pt(o.get("size", size))
        f.bold = o.get("bold", bold)
        f.italic = o.get("italic", italic)
        f.color.rgb = C(o.get("color", color))


def text(slide, x, y, w, h, paras, *, size=14, color=INK, bold=False, italic=False,
         font=BFONT, align=PP_ALIGN.LEFT, anchor=MSO_ANCHOR.TOP, space_after=6,
         line_spacing=None, wrap=True):
    tb = slide.shapes.add_textbox(Inches(x), Inches(y), Inches(w), Inches(h))
    tf = tb.text_frame
    tf.word_wrap = wrap
    tf.vertical_anchor = anchor
    for m in ("margin_left", "margin_right", "margin_top", "margin_bottom"):
        setattr(tf, m, 0)
    if isinstance(paras, str) or (paras and isinstance(paras[0], tuple)):
        paras = [paras]
    for i, para in enumerate(paras):
        p = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
        _set_para(p, para, size, color, bold, italic, font, align, space_after, line_spacing)
    return tb


def bullets(slide, x, y, w, h, items, *, size=13.5, color=INK, space_after=8, font=BFONT):
    """items: list of str or runs-list; each becomes a '•  ' paragraph."""
    tb = slide.shapes.add_textbox(Inches(x), Inches(y), Inches(w), Inches(h))
    tf = tb.text_frame
    tf.word_wrap = True
    for m in ("margin_left", "margin_right", "margin_top", "margin_bottom"):
        setattr(tf, m, 0)
    for i, it in enumerate(items):
        p = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
        run_list = [(it, {})] if isinstance(it, str) else list(it)
        run_list = [("•  ", {"color": RED, "bold": True})] + run_list
        _set_para(p, run_list, size, color, False, False, font, PP_ALIGN.LEFT, space_after, 1.0)
    return tb


def rect(slide, x, y, w, h, fill, *, radius=0.06, line=None, line_w=1.0, shape=MSO_SHAPE.ROUNDED_RECTANGLE):
    sp = slide.shapes.add_shape(shape, Inches(x), Inches(y), Inches(w), Inches(h))
    sp.shadow.inherit = False
    sp.fill.solid()
    sp.fill.fore_color.rgb = C(fill)
    if line:
        sp.line.color.rgb = C(line)
        sp.line.width = Pt(line_w)
    else:
        sp.line.fill.background()
    if shape == MSO_SHAPE.ROUNDED_RECTANGLE:
        try:
            sp.adjustments[0] = radius
        except Exception:
            pass
    sp.text_frame.paragraphs[0].text = ""
    return sp


def num_circle(slide, x, y, n, d=0.5):
    sp = slide.shapes.add_shape(MSO_SHAPE.OVAL, Inches(x), Inches(y), Inches(d), Inches(d))
    sp.shadow.inherit = False
    sp.fill.solid(); sp.fill.fore_color.rgb = C(NAVY)
    sp.line.color.rgb = C(RED); sp.line.width = Pt(1.5)
    tf = sp.text_frame; tf.word_wrap = False
    tf.vertical_anchor = MSO_ANCHOR.MIDDLE
    p = tf.paragraphs[0]; p.alignment = PP_ALIGN.CENTER
    r = p.add_run(); r.text = str(n)
    r.font.name = HFONT; r.font.size = Pt(18); r.font.bold = True; r.font.color.rgb = C(WHITE)
    return sp


def diamond(slide, x, y, d, fill=RED):
    sp = slide.shapes.add_shape(MSO_SHAPE.DIAMOND, Inches(x), Inches(y), Inches(d), Inches(d))
    sp.shadow.inherit = False
    sp.fill.solid(); sp.fill.fore_color.rgb = C(fill); sp.line.fill.background()
    return sp


def kicker_title(slide, eyebrow, title, *, tcolor=NAVY):
    text(slide, 0.6, 0.30, 12.1, 0.32, [(eyebrow, {"color": RED, "bold": True, "size": 12.5, "font": BFONT})])
    diamond(slide, 0.62, 0.78, 0.17)
    text(slide, 0.92, 0.62, 11.8, 0.72, [(title, {})], size=29, bold=True, color=tcolor, font=HFONT)


def fig(slide, key, x, y, box_w, box_h):
    path, aspect = FIGS[key]
    w = box_w; h = w / aspect
    if h > box_h:
        h = box_h; w = h * aspect
    nx = x + (box_w - w) / 2; ny = y + (box_h - h) / 2
    rect(slide, nx - 0.1, ny - 0.1, w + 0.2, h + 0.2, WHITE, radius=0.04, line=GREY, line_w=0.75)
    slide.shapes.add_picture(path, Inches(nx), Inches(ny), Inches(w), Inches(h))


def notes(slide, txt):
    slide.notes_slide.notes_text_frame.text = txt


def blank():
    return prs.slides.add_slide(BLANK)


# =============================================================== 1. TITLE
s = prs.slides.add_slide(TITLE)
ph = {p.placeholder_format.idx: p for p in s.placeholders}
# title
tp = ph[0].text_frame; tp.clear()
r = tp.paragraphs[0].add_run(); r.text = "Growth-Adjusted Life Distribution Forecasting"
r.font.name = HFONT; r.font.size = Pt(34); r.font.bold = True; r.font.color.rgb = C(NAVY)
tp.paragraphs[0].alignment = PP_ALIGN.CENTER
# subtitle
sp = ph[1].text_frame; sp.clear()
p0 = sp.paragraphs[0]; p0.alignment = PP_ALIGN.CENTER
for t, o in [("A simulation-based framework that carries a development growth trend into a fleet-level life estimate.",
              {"italic": True, "size": 15, "color": MUTE})]:
    rr = p0.add_run(); rr.text = t
    rr.font.name = BFONT; rr.font.size = Pt(o["size"]); rr.font.italic = True; rr.font.color.rgb = C(o["color"])
p1 = sp.add_paragraph(); p1.alignment = PP_ALIGN.CENTER; p1.space_before = Pt(10)
for t, o in [("Paul Govan", {"bold": True, "color": NAVY}), ("   ·   GE Aerospace", {"color": INK})]:
    rr = p1.add_run(); rr.text = t
    rr.font.name = BFONT; rr.font.size = Pt(16); rr.font.bold = o.get("bold", False); rr.font.color.rgb = C(o["color"])
# red kicker above title
text(s, 1.11, 0.62, 11.0, 0.35, [("ANNUAL RELIABILITY & MAINTAINABILITY SYMPOSIUM  ·  RAMS 2027",
     {"color": RED, "bold": True, "size": 12.5})], align=PP_ALIGN.CENTER)
# RAMS 2027 wordmark top-right
s.shapes.add_picture("rams2027_wordmark.png", Inches(9.55), Inches(0.28), Inches(3.05))
# folded outline strip
text(s, 1.5, 6.05, 10.3, 0.4,
     [[("The gap", {"bold": True, "color": NAVY}), ("   →   ", {"color": RED, "bold": True}),
       ("The idea", {"bold": True, "color": NAVY}), ("   →   ", {"color": RED, "bold": True}),
       ("The case study", {"bold": True, "color": NAVY}), ("   →   ", {"color": RED, "bold": True}),
       ("What it means", {"bold": True, "color": NAVY})]],
     size=13, align=PP_ALIGN.CENTER)
notes(s, "Open warm, then straight into the anecdote. Do not read the abstract. ~20 min talk, reserve 5 for Q&A.")

# =============================================================== 2. HOOK
s = blank()
kicker_title(s, "BACKGROUND & INTRODUCTION", "When the growth was real — but the forecast didn't show it")
beats = [
    ("The design got better.", "We drove design improvements through development, and the growth analysis confirmed it: failure intensity was trending down. The system was genuinely improving."),
    ("But the life distribution didn't move — yet.", "When we fitted the fleet's life distribution, that improvement wasn't reflected. The forecast still looked like the un-improved design."),
    ("The growth model couldn't give us the piece we needed.", "Crow-AMSAA is a system-level trend. It never spoke to individual-unit risk — when a given unit is likely to fail."),
]
by = 1.72
for i, (h, b) in enumerate(beats):
    rect(s, 0.85, by, 7.35, 1.5, LIGHT)
    num_circle(s, 1.12, by + 0.28, i + 1, 0.52)
    text(s, 1.9, by + 0.16, 6.1, 0.4, [(h, {})], size=16, bold=True, color=NAVY, font=HFONT)
    text(s, 1.9, by + 0.56, 6.15, 0.85, [(b, {})], size=12, color=INK)
    by += 1.63
fig(s, "rga", 8.5, 1.85, 4.35, 3.35)
text(s, 8.5, 5.35, 4.4, 0.4, [("Development: reliability genuinely improving (βg = 0.60)",
     {"italic": True, "size": 11, "color": MUTE})], align=PP_ALIGN.CENTER)
text(s, 1.05, 6.62, 11.4, 0.45, [("A system that was improving, and a forecast that ignored it — with no bridge between them.",
     {"italic": True, "bold": True, "color": STEEL, "size": 14.5, "font": HFONT})])
notes(s, "The anecdote. Keep products generic (GE disclosure norms). Set up BOTH halves of the gap: (a) growth was real but the life distribution didn't reflect it, (b) the growth model gave no per-unit risk. Slides 11 and 14 pay this off.")

# =============================================================== 3. TWO SILOS
s = blank()
kicker_title(s, "BACKGROUND & INTRODUCTION", "Two analyses that never talk")
rect(s, 0.85, 1.85, 5.15, 3.55, LIGHT)
text(s, 1.15, 2.1, 4.6, 0.35, [("RELIABILITY GROWTH (RGA)", {"color": RED, "bold": True, "size": 13})])
text(s, 1.15, 2.5, 4.6, 0.5, [("“Is the system improving?”", {})], size=19, bold=True, color=NAVY, font=HFONT)
bullets(s, 1.2, 3.2, 4.6, 2.0, [
    "System-level failure intensity", "Tracks a trend over test time",
    "Projects a future failure count / MTBF", "Says nothing about a single unit's life"], size=13)
rect(s, 7.3, 1.85, 5.15, 3.55, LIGHT)
text(s, 7.6, 2.1, 4.6, 0.35, [("LIFE DISTRIBUTION (WEIBULL)", {"color": RED, "bold": True, "size": 13})])
text(s, 7.6, 2.5, 4.6, 0.5, [("“How long does a unit last?”", {})], size=19, bold=True, color=NAVY, font=HFONT)
bullets(s, 7.65, 3.2, 4.55, 2.0, [
    "Unit-level time-to-failure", "Gives B-life and per-unit risk",
    "Fitted to a fixed set of failures", "Assumes the distribution isn't changing"], size=13)
# broken bridge marker between cards
text(s, 6.02, 3.3, 1.28, 0.7, [("→ ⁄⁄", {"color": GREY, "bold": True, "size": 15})], align=PP_ALIGN.CENTER)
text(s, 6.02, 3.95, 1.28, 0.5, [("no bridge", {"color": GREY, "bold": True, "size": 10.5})], align=PP_ALIGN.CENTER)
text(s, 1.05, 5.7, 11.3, 0.7, [("The improvement seen in development never reaches the fleet's life distribution — so forecasts can systematically under-call future reliability.", {})], size=14.5, color=INK)
notes(s, "This is the anecdote generalized. The whole method is the bridge between these two boxes.")

# =============================================================== 4. WHY IT MATTERS
s = blank()
kicker_title(s, "BACKGROUND & INTRODUCTION", "Ignore growth, and the data works against you")
why = [
    ("Censoring", "High-reliability units are pulled before they fail — the effective information is far smaller than the sample size."),
    ("Small-sample bias", "With few failures, MLE is unstable; the Weibull shape parameter is especially sensitive."),
    ("Costly to be wrong", "Extrapolating beyond the data under-calls reliability, with real safety and economic stakes."),
]
wx = 0.85
for i, (h, b) in enumerate(why):
    rect(s, wx, 1.95, 3.75, 3.3, LIGHT)
    c = s.shapes.add_shape(MSO_SHAPE.OVAL, Inches(wx + 0.3), Inches(2.25), Inches(0.62), Inches(0.62))
    c.shadow.inherit = False; c.fill.solid(); c.fill.fore_color.rgb = C(NAVY)
    c.line.color.rgb = C(RED); c.line.width = Pt(1.5); c.text_frame.paragraphs[0].text = ""
    diamond(s, wx + 0.51, 2.46, 0.2)
    text(s, wx + 0.3, 3.1, 3.15, 0.5, [(h, {})], size=18, bold=True, color=NAVY, font=HFONT)
    text(s, wx + 0.3, 3.68, 3.2, 1.4, [(b, {})], size=12.5, color=INK)
    wx += 3.94
text(s, 0.85, 5.75, 11.6, 0.6, [("Reliability growth adds a fourth problem the others don't touch: the life distribution itself isn't fixed.",
     {"italic": True, "bold": True, "color": STEEL, "size": 15, "font": HFONT})])
notes(s, "Existing remedies improve estimation of a STATIC distribution. None propagate a growth trend. That's the gap.")

# =============================================================== 5. THE IDEA
s = blank()
kicker_title(s, "MAIN BODY", "The idea, in one picture")
flow = [
    ("Growth trend", "The development Crow-AMSAA rate βg"),
    ("Fewer expected failures", "Growth lowers the count the fleet will see over a fixed window"),
    ("Life distribution shifts right", "Re-fit to the reduced-failure data → longer characteristic life"),
]
fx = 0.9
for i, (h, b) in enumerate(flow):
    rect(s, fx, 2.5, 3.6, 2.5, LIGHT)
    num_circle(s, fx + 0.28, 2.8, i + 1, 0.52)
    text(s, fx + 0.28, 3.48, 3.05, 0.8, [(h, {})], size=17, bold=True, color=NAVY, font=HFONT)
    text(s, fx + 0.28, 4.18, 3.1, 0.75, [(b, {})], size=12, color=INK)
    if i < 2:
        text(s, fx + 3.58, 3.35, 0.5, 0.7, [("→", {"color": RED, "bold": True, "size": 30})],
             align=PP_ALIGN.CENTER, anchor=MSO_ANCHOR.MIDDLE)
    fx += 4.05
text(s, 0.9, 5.55, 11.5, 0.8, [("Not a fixed growth multiplier on a distribution — the growth trend reshapes how many failures the surviving fleet is expected to produce, and the distribution is re-fitted to match.",
     {"italic": True, "color": MUTE, "size": 13.5})])
notes(s, "Contrast with ad hoc multipliers. Mechanism: growth -> fewer forecast failures -> larger fitted eta. Everything after makes this quantitative and bounded.")

# =============================================================== 6. TWO-PHASE SETUP
s = blank()
kicker_title(s, "MAIN BODY", "One program, two phases")
rect(s, 0.85, 2.15, 5.15, 3.05, LIGHT)
text(s, 1.15, 2.4, 4.0, 0.35, [("PHASE 1", {"color": RED, "bold": True, "size": 12.5})])
text(s, 1.15, 2.75, 4.6, 0.5, [("Developmental growth test", {})], size=18, bold=True, color=NAVY, font=HFONT)
bullets(s, 1.2, 3.4, 4.6, 1.6, ["30 prototypes built & tested in sequence",
        "Test-analyze-fix: each build improves", "Crow-AMSAA quantifies the trend βg"], size=13)
ar = s.shapes.add_shape(MSO_SHAPE.RIGHT_ARROW, Inches(6.15), Inches(3.35), Inches(1.05), Inches(0.65))
ar.shadow.inherit = False; ar.fill.solid(); ar.fill.fore_color.rgb = C(RED); ar.line.fill.background()
ar.text_frame.paragraphs[0].text = ""
text(s, 6.0, 4.05, 1.35, 0.35, [("bridge", {"color": STEEL, "bold": True, "size": 11})], align=PP_ALIGN.CENTER)
rect(s, 7.35, 2.15, 5.1, 3.05, LIGHT)
text(s, 7.65, 2.4, 4.0, 0.35, [("PHASE 2", {"color": RED, "bold": True, "size": 12.5})])
text(s, 7.65, 2.75, 4.6, 0.5, [("Fielded fleet (matured design)", {})], size=18, bold=True, color=NAVY, font=HFONT)
bullets(s, 7.7, 3.4, 4.55, 1.6, ["100 units over a 1000-hour campaign",
        "20 failures, 80 still surviving", "Weibull fit = the no-growth baseline"], size=13)
text(s, 0.85, 5.75, 11.6, 0.5, [("The trend estimated in Phase 1 is projected onto the Phase 2 fleet. Both phases are non-repairable units.", {})], size=14, color=INK)
notes(s, "Genuine growth in Phase 1, bridged onto a real fleet. Forecast is conditional on that Phase-1 trend carrying into service.")

# =============================================================== 7. METHOD
s = blank()
kicker_title(s, "MAIN BODY", "Three stages, one pipeline")
stages = [
    ("Growth lowers the target", "The growth rate scales down how many failures the fleet is expected to see over the forecast window."),
    ("Conditional Weibull simulation", "Draw failure times for surviving units, calibrated to hit that reduced count; censor the rest."),
    ("Re-fit + Monte Carlo", "Fit a growth-adjusted Weibull to the combined data; repeat to get variability bands."),
]
sx = 0.85
for i, (h, b) in enumerate(stages):
    rect(s, sx, 2.0, 3.75, 3.45, LIGHT)
    num_circle(s, sx + 0.32, 2.35, i + 1, 0.6)
    text(s, sx + 0.3, 3.15, 3.2, 0.9, [(h, {})], size=17, bold=True, color=NAVY, font=HFONT)
    text(s, sx + 0.3, 4.0, 3.25, 1.4, [(b, {})], size=12.5, color=INK)
    if i < 2:
        text(s, sx + 3.66, 3.35, 0.4, 0.7, [("→", {"color": RED, "bold": True, "size": 26})],
             align=PP_ALIGN.CENTER, anchor=MSO_ANCHOR.MIDDLE)
    sx += 3.94
text(s, 0.85, 5.85, 11.6, 0.5, [("The equations live in the backup. The story only needs these three moves.",
     {"italic": True, "color": MUTE, "size": 14})])
notes(s, "Keep at altitude. Do not walk equations 1-11 unless asked (backup).")

# =============================================================== 8. COUNT-RATIO
s = blank()
kicker_title(s, "MAIN BODY", "The one number: the count-ratio c(βg)")
fig(s, "countratio", 0.6, 1.7, 6.85, 4.9)
rect(s, 7.75, 1.9, 4.9, 4.3, LIGHT)
text(s, 8.05, 2.2, 4.35, 0.95, [[("Stronger growth (smaller βg) ", {"bold": True}),
     ("lowers the expected failure count over the window.", {})]], size=14.5, color=INK)
text(s, 8.05, 3.25, 4.35, 0.9, [[("c(1) = 1", {"bold": True, "color": RED}),
     ("  —  no growth exactly recovers the no-growth benchmark.", {})]], size=14.5, color=INK)
text(s, 8.05, 4.3, 4.35, 0.9, [[("Case study: c = 0.57", {"bold": True, "color": NAVY}),
     ("  —  growth cuts the expected failures nearly in half.", {})]], size=14.5, color=INK)
text(s, 8.05, 5.35, 4.35, 0.75, [("Scale-free: only the dimensionless growth rate enters — no development time unit is imported into the fleet.",
     {"italic": True, "color": MUTE, "size": 11.5})])
notes(s, "The only relation shown. c multiplies the no-growth count. Below 1 => fewer failures => larger fitted life.")

# =============================================================== 9. BASELINE
s = blank()
kicker_title(s, "MAIN BODY", "Step 1 — the fleet's baseline life")
fig(s, "baseline", 0.6, 1.75, 6.7, 4.85)
rect(s, 7.65, 1.95, 5.0, 4.05, LIGHT)
text(s, 7.95, 2.2, 4.5, 0.4, [("No-growth baseline (MLE)", {})], size=17, bold=True, color=NAVY, font=HFONT)
text(s, 7.95, 2.8, 4.5, 0.4, [[("β = 3.82", {"bold": True, "color": NAVY}), ("   shape > 1 → wear-out", {"color": MUTE})]], size=15)
text(s, 7.95, 3.3, 4.5, 0.4, [[("η = 1479", {"bold": True, "color": NAVY}), ("   characteristic life", {"color": MUTE})]], size=15)
text(s, 7.95, 3.85, 4.5, 0.4, [[("20 failures", {"bold": True, "color": INK}), ("  ·  ", {"color": GREY}), ("80 surviving", {"bold": True, "color": INK})]], size=15)
text(s, 7.95, 4.55, 4.55, 1.2, [("This fit is the reference the growth adjustment is measured against.", {"italic": True})], size=13.5, color=INK)
notes(s, "Heavy censoring: 80 of 100 units survive. Baseline anchors everything downstream.")

# =============================================================== 10. GROWTH MODEL
s = blank()
kicker_title(s, "MAIN BODY", "Step 2 — development shows real growth")
fig(s, "rga", 0.6, 1.75, 6.7, 4.85)
rect(s, 7.65, 1.95, 5.0, 4.05, LIGHT)
text(s, 7.95, 2.2, 4.5, 0.4, [("Crow-AMSAA growth fit", {})], size=17, bold=True, color=NAVY, font=HFONT)
text(s, 7.95, 2.8, 4.5, 0.4, [[("βg = 0.60", {"bold": True, "color": NAVY}), ("   < 1 → improving", {"color": MUTE})]], size=15)
text(s, 7.95, 3.3, 4.5, 0.4, [[("R² = 0.996", {"bold": True, "color": NAVY}), ("   trend fits tightly", {"color": MUTE})]], size=15)
text(s, 7.95, 3.85, 4.5, 0.4, [[("growth rate g = 0.40", {"bold": True, "color": RED})]], size=15)
text(s, 7.95, 4.55, 4.55, 1.2, [("Decreasing failure intensity as the design matured — genuine, not an artifact.", {"italic": True})], size=13.5, color=INK)
notes(s, "This beta_g is SYSTEM-level. Distinct from the unit-level Weibull beta on the previous slide.")

# =============================================================== 11. HEADLINE
s = blank()
kicker_title(s, "MAIN BODY", "The payoff: growth finally moves the life estimate")
fig(s, "comparison", 0.6, 1.75, 6.6, 4.8)
rect(s, 7.55, 1.85, 5.1, 2.0, LIGHT)
text(s, 7.85, 2.05, 4.5, 0.35, [("Characteristic life η", {"bold": True, "color": MUTE, "size": 13})])
text(s, 7.85, 2.4, 3.4, 0.8, [[("1479 ", {"color": GREY}), ("→ 1739", {"color": NAVY, "bold": True})]], size=33, bold=True, font=HFONT)
text(s, 10.85, 2.62, 1.65, 0.55, [("+17.6%", {"color": RED, "bold": True, "size": 22})], align=PP_ALIGN.RIGHT, font=HFONT)
rect(s, 7.55, 4.05, 5.1, 1.95, LIGHT)
text(s, 7.85, 4.25, 4.5, 0.35, [("B10 life  (conservative indicator)", {"bold": True, "color": MUTE, "size": 13})])
text(s, 7.85, 4.6, 3.4, 0.8, [[("820 ", {"color": GREY}), ("→ 839", {"color": NAVY, "bold": True})]], size=33, bold=True, font=HFONT)
text(s, 10.85, 4.82, 1.65, 0.55, [("+2.3%", {"color": RED, "bold": True, "size": 22})], align=PP_ALIGN.RIGHT, font=HFONT)
text(s, 1.05, 6.6, 11.4, 0.5, [("Same fleet data. The development trend, propagated, lifts the fitted life — here's where the improvement finally shows up in unit-level risk.",
     {"italic": True, "color": STEEL, "size": 13})])
notes(s, "Growth shows primarily as a shift in eta (+18%). B10 is conservative because the shape drops slightly. Report both; lead with eta.")

# =============================================================== 12. MONTE CARLO
s = blank()
kicker_title(s, "MAIN BODY", "How much does it vary?")
fig(s, "mc", 0.6, 1.8, 6.75, 4.6)
rect(s, 7.7, 1.9, 4.95, 4.1, LIGHT)
text(s, 8.0, 2.2, 4.4, 0.45, [[("500", {"bold": True, "color": NAVY}), (" valid iterations", {"color": INK})]], size=15)
bullets(s, 8.05, 2.8, 4.5, 2.0, [
    [("MC median β = 3.01", {"bold": True}), ("   (baseline 3.82)", {"color": MUTE})],
    "95% band 2.88 – 3.14",
    "Growth lowers the shape; spread dominated by the simulation, not growth-parameter error"], size=13.5)
text(s, 8.0, 5.05, 4.55, 0.9, [("Read these as forecast variability bands conditional on the fitted model — not inferential confidence intervals.",
     {"italic": True, "color": MUTE, "size": 12})])
notes(s, "The shift is stable across draws, not a lucky sample. Precise: variability band, not a confidence interval; model-form uncertainty not represented.")

# =============================================================== 13. SENSITIVITY
s = blank()
kicker_title(s, "MAIN BODY", "What actually moves the answer")
fig(s, "sensitivity", 0.6, 1.8, 6.75, 4.6)
rect(s, 7.7, 1.95, 4.95, 3.95, LIGHT)
bullets(s, 8.05, 2.25, 4.5, 2.6, [
    [("Stronger growth → larger η", {"bold": True})],
    "monotonic and orderly",
    "At βg = 1 it returns to the baseline — the shift is entirely the growth trend",
    "Robust to WHEN you assess the fleet"], size=13.5)
text(s, 8.0, 5.0, 4.55, 0.8, [("What matters most is having enough observed failures to pin down the baseline.",
     {"italic": True, "color": STEEL, "size": 12.5})])
notes(s, "The 'none' box on the baseline is the credibility check. Variation elsewhere is inherited from the baseline fit under censoring.")

# =============================================================== 14. SUMMARY & CONCLUSIONS
s = blank()
kicker_title(s, "SUMMARY & CONCLUSIONS", "Summary & conclusions")
rect(s, 0.85, 1.75, 11.6, 4.35, LIGHT)
bullets(s, 1.2, 2.1, 11.0, 3.7, [
    [("Bridges the two silos ", {"bold": True, "color": NAVY}),
     ("— reliability growth and unit-level life estimation, normally analyzed apart.", {})],
    [("Growth lifts the fitted life: ", {"bold": True, "color": NAVY}),
     ("characteristic life η +17.6% (1479 → 1739); B10 +2.3% (820 → 839).", {})],
    [("A mechanical consequence, not a coincidence: ", {"bold": True, "color": NAVY}),
     ("growth lowers the expected failure count (count-ratio c = 0.57), forcing a larger characteristic life.", {})],
    [("Stable under Monte Carlo: ", {"bold": True, "color": NAVY}),
     ("median shape β = 3.01 (baseline 3.82), 95% band 2.88 – 3.14 — spread dominated by the simulation, not growth-parameter error.", {})],
    [("An optimistic bound: ", {"bold": True, "color": RED}),
     ("valid only if development growth continues in service; freeze the design → the no-growth baseline applies.", {})],
    [("Honest by construction: ", {"bold": True, "color": RED}),
     ("we quantify the shift's magnitude and variability — never a claim of better predictive accuracy.", {})],
], size=15, space_after=13)
text(s, 1.05, 6.35, 11.4, 0.5, [("The contribution: a repeatable, bounded way to turn an observed growth trend into a fleet-level life estimate.",
     {"italic": True, "bold": True, "color": STEEL, "size": 14, "font": HFONT})])
notes(s, "Conclusions in bulleted form. Lead with eta (+17.6%); B10 is the conservative companion. The last two bullets are the non-negotiable honesty RAMS reviewers look for — never overclaim accuracy.")

# =============================================================== 15. NEXT STEPS & FUTURE WORK
s = blank()
kicker_title(s, "NEXT STEPS & FUTURE WORK", "Next steps & future work")
rect(s, 0.85, 1.75, 11.6, 4.35, LIGHT)
bullets(s, 1.2, 2.15, 11.0, 3.6, [
    [("Validate predictive accuracy ", {"bold": True, "color": NAVY}),
     ("against a fleet that genuinely keeps improving, so the forecast can be compared to held-out ground truth.", {})],
    [("Propagate more uncertainty ", {"bold": True, "color": NAVY}),
     ("— model-form (the Crow-AMSAA power law) and the input Weibull shape β, held fixed here.", {})],
    [("Test alternative growth models ", {"bold": True, "color": NAVY}),
     ("— e.g. a piecewise NHPP with change-point detection, which may yield different counts.", {})],
    [("Relax the homogeneous-fleet assumption ", {"bold": True, "color": NAVY}),
     ("— distinct subpopulations and repair / replacement scenarios.", {})],
    [("Reproducible & open-source ", {"bold": True, "color": NAVY}),
     ("— ReliaGrowR (CRAN) + WeibullR; currently under rOpenSci software review.", {})],
], size=15, space_after=15)
text(s, 1.05, 6.35, 11.4, 0.5, [[("ReliaGrowR on CRAN:  ", {"color": INK, "size": 14}),
     ("cran.r-project.org/package=ReliaGrowR", {"color": RED, "bold": True, "size": 14})]])
notes(s, "Next Steps / Future Work as its own slide, bulleted. Drawn from the paper's Limitations section (validation gap, unpropagated uncertainties, alternative models, fleet homogeneity) plus the open-source tooling.")

# =============================================================== 16. REFERENCES
s = blank()
kicker_title(s, "REFERENCES", "References")
refs = [
    "L. H. Crow, “Reliability analysis for complex, repairable systems,” U.S. Army AMSAA, Tech. Rep. 138, 1975.",
    "P. Govan, “ReliaGrowR: Modeling and plotting functions for reliability growth analysis,” in Proc. 2026 RAMS, doi:10.1109/RAMS50514.2026.11424445.",
    "P. Govan, ReliaGrowR: Reliability Growth Analysis, R package, 2024, doi:10.32614/CRAN.package.ReliaGrowR.",
    "D. Silkworth and J. Symynck, WeibullR: Weibull Analysis for Reliability Engineering, R package v1.2.4, 2025.",
    "H. Guo, A. Mettas, G. Sarakakis, and P. Niu, “Piecewise NHPP models with MLE for repairable systems,” in Proc. 2010 RAMS, doi:10.1109/RAMS.2010.5448029.",
    "C. W. Zhang, T. Zhang, D. Xu, and M. Xie, “Analyzing highly censored reliability data without exact failure times,” Qual. Eng., 2013, doi:10.1080/08982112.2013.783598.",
    "E. Makalic and D. F. Schmidt, “Minimum message length inference of the Weibull distribution,” in AI 2023, LNCS 14471, pp. 291–303, doi:10.1007/978-981-99-8388-9_24.",
    "T. M. Grile and R. A. Bettinger, “Reliability analysis of deep space satellites launched 1991–2020,” Qual. Reliab. Eng. Int., 2024, doi:10.1002/qre.3600.",
    "V. M. R. Muggeo, “segmented: an R package to fit regression models with broken-line relationships,” R News, vol. 8(1), pp. 20–25, 2008.",
]
bullets(s, 0.95, 1.75, 11.6, 5.2, refs, size=12.5, space_after=7)
notes(s, "Standard RAMS closer. Equation/algorithm backup slides can follow (hidden) for Q&A.")

prs.save(OUT)
print("SAVED", OUT, "slides:", len(prs.slides._sldIdLst))
