// RAMS 2027 — RGF growth-adjusted life forecasting deck (16 slides).
// Run from the deck directory so figs/ paths resolve.
const pptxgen = require("pptxgenjs");
const p = new pptxgen();
p.defineLayout({ name: "W", width: 13.333, height: 7.5 });
p.layout = "W";

// ---- palette ----
const NAVY = "14243A", STEEL = "3E5C76", AMBER = "E0A458",
      GREY = "8A8F98", LIGHT = "F1F4F8", WHITE = "FFFFFF",
      INK = "1C2733", PALE = "EAF0F6", MUTE = "5B6875";
const HFONT = "Cambria", BFONT = "Calibri";
const EMU = { W: 13.333, H: 7.5 };

const shadow = () => ({ type: "outer", color: "9AA4B0", blur: 6, offset: 3, angle: 90, opacity: 0.45 });

// amber diamond motif + title, for light slides
function titleLight(s, t, sub) {
  s.addShape(p.ShapeType.diamond, { x: 0.55, y: 0.52, w: 0.19, h: 0.19, fill: { color: AMBER } });
  s.addText(t, { x: 0.85, y: 0.34, w: 11.9, h: 0.62, fontFace: HFONT, fontSize: 30, bold: true, color: NAVY, align: "left", margin: 0 });
  if (sub) s.addText(sub, { x: 0.86, y: 0.98, w: 11.9, h: 0.4, fontFace: BFONT, fontSize: 14, italic: true, color: MUTE, align: "left", margin: 0 });
}
function titleDark(s, t, sub) {
  s.addShape(p.ShapeType.diamond, { x: 0.55, y: 0.52, w: 0.19, h: 0.19, fill: { color: AMBER } });
  s.addText(t, { x: 0.85, y: 0.34, w: 11.9, h: 0.62, fontFace: HFONT, fontSize: 30, bold: true, color: WHITE, align: "left", margin: 0 });
  if (sub) s.addText(sub, { x: 0.86, y: 0.98, w: 11.9, h: 0.4, fontFace: BFONT, fontSize: 14, italic: true, color: AMBER, align: "left", margin: 0 });
}
function card(s, x, y, w, h, fill) {
  s.addShape(p.ShapeType.roundRect, { x, y, w, h, rectRadius: 0.09, fill: { color: fill || LIGHT }, line: { type: "none" }, shadow: shadow() });
}
function numCircle(s, x, y, n, d) {
  d = d || 0.5;
  s.addShape(p.ShapeType.ellipse, { x, y, w: d, h: d, fill: { color: NAVY }, line: { color: AMBER, width: 1.5 } });
  s.addText(String(n), { x, y, w: d, h: d, align: "center", valign: "middle", fontFace: HFONT, fontSize: 18, bold: true, color: WHITE, margin: 0 });
}
// image preserving aspect within a box (top-left anchored, centered horizontally in box)
function fig(s, path, aspect, x, y, boxW, boxH) {
  let w = boxW, h = w / aspect;
  if (h > boxH) { h = boxH; w = h * aspect; }
  const nx = x + (boxW - w) / 2, ny = y + (boxH - h) / 2;
  card(s, nx - 0.12, ny - 0.12, w + 0.24, h + 0.24, WHITE);
  s.addImage({ path, x: nx, y: ny, w, h });
}

// ============================================================ 1. TITLE
let s = p.addSlide();
s.background = { color: NAVY };
s.addShape(p.ShapeType.rect, { x: 0, y: 0, w: EMU.W, h: EMU.H, fill: { color: NAVY } });
// motif: faint concentric growth curve marks (diamonds)
[["10.9", "1.1"], ["11.7", "1.7"], ["12.2", "2.6"]].forEach(([x, y], i) =>
  s.addShape(p.ShapeType.diamond, { x: parseFloat(x), y: parseFloat(y), w: 0.16 + i * 0.03, h: 0.16 + i * 0.03, fill: { color: AMBER, transparency: 40 } }));
s.addText("RAMS 2027  ·  RELIABILITY GROWTH", { x: 0.9, y: 1.5, w: 10, h: 0.4, fontFace: BFONT, fontSize: 15, bold: true, color: AMBER, charSpacing: 2, margin: 0 });
s.addText("Growth-Adjusted Life\nDistribution Forecasting", { x: 0.86, y: 2.15, w: 11.4, h: 2.0, fontFace: HFONT, fontSize: 46, bold: true, color: WHITE, lineSpacingMultiple: 1.0, margin: 0 });
s.addText("A simulation-based framework that carries a development growth trend into a fleet-level life estimate.", { x: 0.9, y: 4.25, w: 10.6, h: 0.6, fontFace: BFONT, fontSize: 17, italic: true, color: PALE, margin: 0 });
s.addText([
  { text: "Paul Govan", options: { bold: true, color: WHITE } },
  { text: "   ·   GE Aerospace", options: { color: PALE } },
], { x: 0.9, y: 5.5, w: 10, h: 0.4, fontFace: BFONT, fontSize: 16, margin: 0 });
s.addText("Key words: reliability growth analysis · life forecasting · Monte Carlo simulation · Weibull analysis", { x: 0.9, y: 6.55, w: 11.5, h: 0.4, fontFace: BFONT, fontSize: 12, color: GREY, margin: 0 });
s.addNotes("Title. Open warm, then straight into the anecdote on the next slide — do not read the abstract.");

// ============================================================ 2. HOOK (anecdote)
s = p.addSlide();
s.background = { color: WHITE };
titleLight(s, "When the growth was real — but the forecast didn't show it");
const beats = [
  ["The design got better.", "We drove design improvements through development, and the growth analysis confirmed it: failure intensity was trending down. The system was genuinely improving."],
  ["But the life distribution didn't move — yet.", "When we fitted the fleet's life distribution, that improvement wasn't reflected. The forecast still looked like the un-improved design."],
  ["And the growth model couldn't tell us the piece we needed.", "Crow-AMSAA is a system-level trend. It never spoke to individual-unit risk — when a given unit is likely to fail."],
];
let by = 1.5;
beats.forEach(([h, b], i) => {
  card(s, 0.85, by, 7.35, 1.55, LIGHT);
  numCircle(s, 1.15, by + 0.28, i + 1, 0.55);
  s.addText(h, { x: 1.95, y: by + 0.18, w: 6.05, h: 0.4, fontFace: HFONT, fontSize: 16.5, bold: true, color: NAVY, margin: 0 });
  s.addText(b, { x: 1.95, y: by + 0.6, w: 6.1, h: 0.85, fontFace: BFONT, fontSize: 12.5, color: INK, margin: 0 });
  by += 1.72;
});
// right: real growth figure grounding beat 1
fig(s, "figs/fig_rga.png", 1600 / 1200, 8.55, 1.75, 4.3, 3.55);
s.addText("Development: reliability genuinely improving  (βg = 0.60)", { x: 8.5, y: 5.5, w: 4.4, h: 0.4, fontFace: BFONT, fontSize: 11.5, italic: true, color: MUTE, align: "center", margin: 0 });
s.addText("A system that was improving, and a forecast that ignored it — with no bridge between them.", { x: 0.85, y: 6.75, w: 11.6, h: 0.45, fontFace: HFONT, fontSize: 15, bold: true, italic: true, color: STEEL, margin: 0 });
s.addNotes("The anecdote. Keep products generic (GE disclosure norms). Set up BOTH halves of the gap: (a) growth was real but the life distribution didn't reflect it, (b) the growth model gave no per-unit risk. Slides 11 and 15 pay this off.");

// ============================================================ 3. TWO SILOS
s = p.addSlide();
s.background = { color: WHITE };
titleLight(s, "Two analyses that never talk");
// RGA card
card(s, 0.85, 1.7, 5.15, 3.7, LIGHT);
s.addText("RELIABILITY GROWTH (RGA)", { x: 1.15, y: 2.0, w: 4.6, h: 0.4, fontFace: BFONT, fontSize: 13, bold: true, color: AMBER, charSpacing: 1, margin: 0 });
s.addText("“Is the system improving?”", { x: 1.15, y: 2.45, w: 4.6, h: 0.5, fontFace: HFONT, fontSize: 20, bold: true, color: NAVY, margin: 0 });
s.addText([
  { text: "System-level failure intensity", options: { bullet: true, breakLine: true } },
  { text: "Tracks a trend over test time", options: { bullet: true, breakLine: true } },
  { text: "Projects a future failure count / MTBF", options: { bullet: true, breakLine: true } },
  { text: "Says nothing about a single unit's life", options: { bullet: true } },
], { x: 1.2, y: 3.15, w: 4.55, h: 2.0, fontFace: BFONT, fontSize: 13.5, color: INK, paraSpaceAfter: 8, margin: 0 });
// Weibull card
card(s, 7.3, 1.7, 5.15, 3.7, LIGHT);
s.addText("LIFE DISTRIBUTION (WEIBULL)", { x: 7.6, y: 2.0, w: 4.6, h: 0.4, fontFace: BFONT, fontSize: 13, bold: true, color: AMBER, charSpacing: 1, margin: 0 });
s.addText("“How long does a unit last?”", { x: 7.6, y: 2.45, w: 4.6, h: 0.5, fontFace: HFONT, fontSize: 20, bold: true, color: NAVY, margin: 0 });
s.addText([
  { text: "Unit-level time-to-failure", options: { bullet: true, breakLine: true } },
  { text: "Gives B-life and per-unit risk", options: { bullet: true, breakLine: true } },
  { text: "Fitted to a fixed set of failures", options: { bullet: true, breakLine: true } },
  { text: "Assumes the distribution isn't changing", options: { bullet: true } },
], { x: 7.65, y: 3.15, w: 4.55, h: 2.0, fontFace: BFONT, fontSize: 13.5, color: INK, paraSpaceAfter: 8, margin: 0 });
// broken bridge
s.addShape(p.ShapeType.line, { x: 6.05, y: 3.5, w: 0.55, h: 0, line: { color: GREY, width: 3, dashType: "dash" } });
s.addShape(p.ShapeType.line, { x: 6.72, y: 3.5, w: 0.55, h: 0, line: { color: GREY, width: 3, dashType: "dash" } });
s.addText("no\nbridge", { x: 6.02, y: 3.62, w: 1.28, h: 0.6, align: "center", fontFace: BFONT, fontSize: 10.5, bold: true, color: GREY, margin: 0 });
s.addText("The improvement seen in development never reaches the fleet's life distribution — so forecasts can systematically under-call future reliability.", { x: 0.85, y: 5.75, w: 11.6, h: 0.8, fontFace: BFONT, fontSize: 14.5, color: INK, margin: 0 });
s.addNotes("This is the anecdote generalized. The whole method is the bridge between these two boxes.");

// ============================================================ 4. WHY IT MATTERS
s = p.addSlide();
s.background = { color: WHITE };
titleLight(s, "Ignore growth, and the data works against you");
const why = [
  ["Censoring", "High-reliability units are pulled before they fail — the effective information is far smaller than the sample size."],
  ["Small-sample bias", "With few failures, MLE is unstable; the Weibull shape parameter is especially sensitive."],
  ["Costly to be wrong", "Extrapolating beyond the data under-calls reliability, with real safety and economic stakes."],
];
let wx = 0.85;
why.forEach(([h, b], i) => {
  card(s, wx, 1.9, 3.75, 3.4, LIGHT);
  s.addShape(p.ShapeType.ellipse, { x: wx + 0.3, y: 2.2, w: 0.7, h: 0.7, fill: { color: NAVY }, line: { color: AMBER, width: 1.5 } });
  s.addShape(p.ShapeType.diamond, { x: wx + 0.53, y: 2.43, w: 0.24, h: 0.24, fill: { color: AMBER }, line: { type: "none" } });
  s.addText(h, { x: wx + 0.3, y: 3.1, w: 3.15, h: 0.5, fontFace: HFONT, fontSize: 19, bold: true, color: NAVY, margin: 0 });
  s.addText(b, { x: wx + 0.3, y: 3.7, w: 3.2, h: 1.4, fontFace: BFONT, fontSize: 13, color: INK, margin: 0 });
  wx += 3.94;
});
s.addText("Reliability growth adds a fourth problem the others don't touch: the life distribution itself isn't fixed.", { x: 0.85, y: 5.7, w: 11.6, h: 0.6, fontFace: HFONT, fontSize: 15.5, bold: true, italic: true, color: STEEL, margin: 0 });
s.addNotes("Existing remedies improve estimation of a STATIC distribution. None propagate a growth trend. That's the gap.");

// ============================================================ 5. THE IDEA
s = p.addSlide();
s.background = { color: NAVY };
titleDark(s, "The idea, in one picture");
const flow = [
  ["Growth trend", "The development Crow-AMSAA rate βg"],
  ["Fewer expected failures", "Growth lowers the count the fleet will see over a fixed window"],
  ["Life distribution shifts right", "Re-fit to the reduced-failure data → longer characteristic life"],
];
let fx = 0.95;
flow.forEach(([h, b], i) => {
  card(s, fx, 2.55, 3.55, 2.6, "1E3350");
  numCircle(s, fx + 0.28, 2.85, i + 1, 0.55);
  s.addText(h, { x: fx + 0.28, y: 3.55, w: 3.0, h: 0.8, fontFace: HFONT, fontSize: 18, bold: true, color: WHITE, margin: 0 });
  s.addText(b, { x: fx + 0.28, y: 4.25, w: 3.05, h: 0.8, fontFace: BFONT, fontSize: 12.5, color: PALE, margin: 0 });
  if (i < 2) s.addText("→", { x: fx + 3.5, y: 3.35, w: 0.6, h: 0.8, align: "center", valign: "middle", fontSize: 30, bold: true, color: AMBER, margin: 0 });
  fx += 4.05;
});
s.addText("Not a fixed growth multiplier on a distribution — the growth trend reshapes how many failures the surviving fleet is expected to produce, and the distribution is re-fitted to match.", { x: 0.95, y: 5.7, w: 11.4, h: 0.8, fontFace: BFONT, fontSize: 14, italic: true, color: PALE, margin: 0 });
s.addNotes("Contrast with ad hoc multipliers. The mechanism: growth -> fewer forecast failures -> larger fitted eta. Everything after this is making that quantitative and bounded.");

// ============================================================ 6. TWO-PHASE SETUP
s = p.addSlide();
s.background = { color: WHITE };
titleLight(s, "One program, two phases");
// Phase 1
card(s, 0.85, 2.1, 5.15, 3.1, LIGHT);
s.addText("PHASE 1", { x: 1.15, y: 2.35, w: 4, h: 0.35, fontFace: BFONT, fontSize: 12.5, bold: true, color: AMBER, charSpacing: 1.5, margin: 0 });
s.addText("Developmental growth test", { x: 1.15, y: 2.7, w: 4.6, h: 0.5, fontFace: HFONT, fontSize: 19, bold: true, color: NAVY, margin: 0 });
s.addText([
  { text: "30 prototypes built & tested in sequence", options: { bullet: true, breakLine: true } },
  { text: "Test-analyze-fix: each build improves", options: { bullet: true, breakLine: true } },
  { text: "Crow-AMSAA quantifies the trend βg", options: { bullet: true } },
], { x: 1.2, y: 3.35, w: 4.6, h: 1.6, fontFace: BFONT, fontSize: 13.5, color: INK, paraSpaceAfter: 8, margin: 0 });
// bridge arrow
s.addShape(p.ShapeType.rightArrow, { x: 6.15, y: 3.3, w: 1.05, h: 0.7, fill: { color: AMBER }, line: { type: "none" } });
s.addText("bridge", { x: 6.0, y: 4.05, w: 1.35, h: 0.35, align: "center", fontFace: BFONT, fontSize: 11, bold: true, color: STEEL, margin: 0 });
// Phase 2
card(s, 7.35, 2.1, 5.1, 3.1, LIGHT);
s.addText("PHASE 2", { x: 7.65, y: 2.35, w: 4, h: 0.35, fontFace: BFONT, fontSize: 12.5, bold: true, color: AMBER, charSpacing: 1.5, margin: 0 });
s.addText("Fielded fleet (matured design)", { x: 7.65, y: 2.7, w: 4.6, h: 0.5, fontFace: HFONT, fontSize: 19, bold: true, color: NAVY, margin: 0 });
s.addText([
  { text: "100 units over a 1000-hour campaign", options: { bullet: true, breakLine: true } },
  { text: "20 failures, 80 still surviving", options: { bullet: true, breakLine: true } },
  { text: "Weibull fit = the no-growth baseline", options: { bullet: true } },
], { x: 7.7, y: 3.35, w: 4.55, h: 1.6, fontFace: BFONT, fontSize: 13.5, color: INK, paraSpaceAfter: 8, margin: 0 });
s.addText("The trend estimated in Phase 1 is projected onto the Phase 2 fleet. Both phases are non-repairable units.", { x: 0.85, y: 5.7, w: 11.6, h: 0.5, fontFace: BFONT, fontSize: 14, color: INK, margin: 0 });
s.addNotes("Two-phase = genuine growth in Phase 1, then a bridge onto a real fleet. The forecast is conditional on that Phase-1 trend carrying into service.");

// ============================================================ 7. METHOD AT ALTITUDE
s = p.addSlide();
s.background = { color: WHITE };
titleLight(s, "Three stages, one pipeline");
const stages = [
  ["Growth lowers the target", "The growth rate scales down how many failures the fleet is expected to see over the forecast window."],
  ["Conditional Weibull simulation", "Draw failure times for surviving units, calibrated to hit that reduced count; censor the rest."],
  ["Re-fit + Monte Carlo", "Fit a growth-adjusted Weibull to the combined data; repeat to get variability bands."],
];
let sx = 0.85;
stages.forEach(([h, b], i) => {
  card(s, sx, 2.0, 3.75, 3.5, LIGHT);
  numCircle(s, sx + 0.32, 2.35, i + 1, 0.62);
  s.addText(h, { x: sx + 0.3, y: 3.15, w: 3.2, h: 0.9, fontFace: HFONT, fontSize: 17.5, bold: true, color: NAVY, margin: 0 });
  s.addText(b, { x: sx + 0.3, y: 4.0, w: 3.25, h: 1.4, fontFace: BFONT, fontSize: 13, color: INK, margin: 0 });
  if (i < 2) s.addText("→", { x: sx + 3.7, y: 3.3, w: 0.35, h: 0.8, align: "center", valign: "middle", fontSize: 26, bold: true, color: AMBER, margin: 0 });
  sx += 3.94;
});
s.addText("The equations live in the backup. The story only needs these three moves.", { x: 0.85, y: 5.9, w: 11.6, h: 0.5, fontFace: BFONT, fontSize: 14, italic: true, color: MUTE, margin: 0 });
s.addNotes("Keep this at altitude. Do not walk through equations 1-11 unless asked in Q&A (backup).");

// ============================================================ 8. COUNT-RATIO
s = p.addSlide();
s.background = { color: WHITE };
titleLight(s, "The one number: the count-ratio c(βg)");
fig(s, "figs/fig_countratio.png", 1500 / 1050, 0.7, 1.55, 6.7, 4.9);
card(s, 7.75, 1.75, 4.9, 4.35, LIGHT);
s.addText([
  { text: "Stronger growth (smaller βg) ", options: { bold: true } },
  { text: "lowers the expected failure count over the window.", options: {} },
], { x: 8.05, y: 2.1, w: 4.35, h: 0.9, fontFace: BFONT, fontSize: 14.5, color: INK, margin: 0 });
s.addText([
  { text: "c(1) = 1", options: { bold: true, color: AMBER } },
  { text: "  —  no growth exactly recovers the no-growth benchmark.", options: {} },
], { x: 8.05, y: 3.15, w: 4.35, h: 0.9, fontFace: BFONT, fontSize: 14.5, color: INK, margin: 0 });
s.addText([
  { text: "Case study: c = 0.57", options: { bold: true, color: NAVY } },
  { text: "  —  growth cuts the expected failures nearly in half.", options: {} },
], { x: 8.05, y: 4.2, w: 4.35, h: 0.9, fontFace: BFONT, fontSize: 14.5, color: INK, margin: 0 });
s.addText("Scale-free: only the dimensionless growth rate enters — no development time unit is imported into the fleet.", { x: 8.05, y: 5.25, w: 4.35, h: 0.75, fontFace: BFONT, fontSize: 11.5, italic: true, color: MUTE, margin: 0 });
s.addNotes("This is the only relation shown. c multiplies the no-growth count. Below 1 => fewer failures => larger fitted life.");

// ============================================================ 9. FLEET BASELINE
s = p.addSlide();
s.background = { color: WHITE };
titleLight(s, "Step 1 — the fleet's baseline life");
fig(s, "figs/fig_baseline.png", 1600 / 1200, 0.7, 1.55, 6.6, 4.95);
card(s, 7.65, 1.9, 5.0, 4.1, LIGHT);
s.addText("No-growth baseline (MLE)", { x: 7.95, y: 2.15, w: 4.5, h: 0.4, fontFace: HFONT, fontSize: 17, bold: true, color: NAVY, margin: 0 });
s.addText([
  { text: "β = 3.82", options: { bold: true, color: NAVY } },
  { text: "   shape > 1 → wear-out", options: { color: MUTE } },
], { x: 7.95, y: 2.75, w: 4.5, h: 0.4, fontFace: BFONT, fontSize: 15, margin: 0 });
s.addText([
  { text: "η = 1479", options: { bold: true, color: NAVY } },
  { text: "   characteristic life", options: { color: MUTE } },
], { x: 7.95, y: 3.25, w: 4.5, h: 0.4, fontFace: BFONT, fontSize: 15, margin: 0 });
s.addText([
  { text: "20 failures", options: { bold: true } },
  { text: "  ·  ", options: { color: GREY } },
  { text: "80 surviving", options: { bold: true } },
], { x: 7.95, y: 3.8, w: 4.5, h: 0.4, fontFace: BFONT, fontSize: 15, color: INK, margin: 0 });
s.addText("This fit is the reference the growth adjustment is measured against.", { x: 7.95, y: 4.5, w: 4.55, h: 1.2, fontFace: BFONT, fontSize: 13.5, italic: true, color: INK, margin: 0 });
s.addNotes("Heavy censoring: 80 of 100 units survive. The baseline anchors everything downstream.");

// ============================================================ 10. GROWTH MODEL
s = p.addSlide();
s.background = { color: WHITE };
titleLight(s, "Step 2 — development shows real growth");
fig(s, "figs/fig_rga.png", 1600 / 1200, 0.7, 1.55, 6.6, 4.95);
card(s, 7.65, 1.9, 5.0, 4.1, LIGHT);
s.addText("Crow-AMSAA growth fit", { x: 7.95, y: 2.15, w: 4.5, h: 0.4, fontFace: HFONT, fontSize: 17, bold: true, color: NAVY, margin: 0 });
s.addText([
  { text: "βg = 0.60", options: { bold: true, color: NAVY } },
  { text: "   < 1 → improving", options: { color: MUTE } },
], { x: 7.95, y: 2.75, w: 4.5, h: 0.4, fontFace: BFONT, fontSize: 15, margin: 0 });
s.addText([
  { text: "R² = 0.996", options: { bold: true, color: NAVY } },
  { text: "   trend fits tightly", options: { color: MUTE } },
], { x: 7.95, y: 3.25, w: 4.5, h: 0.4, fontFace: BFONT, fontSize: 15, margin: 0 });
s.addText([
  { text: "growth rate g = 0.40", options: { bold: true, color: AMBER } },
], { x: 7.95, y: 3.8, w: 4.5, h: 0.4, fontFace: BFONT, fontSize: 15, margin: 0 });
s.addText("Decreasing failure intensity as the design matured — genuine, not an artifact.", { x: 7.95, y: 4.5, w: 4.55, h: 1.2, fontFace: BFONT, fontSize: 13.5, italic: true, color: INK, margin: 0 });
s.addNotes("This beta_g is a SYSTEM-level trend. Distinct from the unit-level Weibull beta on the previous slide.");

// ============================================================ 11. HEADLINE
s = p.addSlide();
s.background = { color: WHITE };
titleLight(s, "The payoff: growth finally moves the life estimate");
fig(s, "figs/fig_comparison.png", 1700 / 1250, 0.7, 1.7, 6.5, 4.9);
// stat callouts
card(s, 7.55, 1.75, 5.1, 2.05, LIGHT);
s.addText("Characteristic life η", { x: 7.85, y: 1.95, w: 4.5, h: 0.35, fontFace: BFONT, fontSize: 13, bold: true, color: MUTE, margin: 0 });
s.addText([
  { text: "1479 ", options: { color: GREY } },
  { text: "→ 1739", options: { color: NAVY, bold: true } },
], { x: 7.85, y: 2.3, w: 3.3, h: 0.8, fontFace: HFONT, fontSize: 34, bold: true, margin: 0 });
s.addText("+17.6%", { x: 10.9, y: 2.55, w: 1.6, h: 0.55, fontFace: HFONT, fontSize: 22, bold: true, color: AMBER, align: "right", margin: 0 });
card(s, 7.55, 4.0, 5.1, 2.0, LIGHT);
s.addText("B10 life  (conservative indicator)", { x: 7.85, y: 4.2, w: 4.5, h: 0.35, fontFace: BFONT, fontSize: 13, bold: true, color: MUTE, margin: 0 });
s.addText([
  { text: "820 ", options: { color: GREY } },
  { text: "→ 839", options: { color: NAVY, bold: true } },
], { x: 7.85, y: 4.55, w: 3.3, h: 0.8, fontFace: HFONT, fontSize: 34, bold: true, margin: 0 });
s.addText("+2.3%", { x: 10.9, y: 4.8, w: 1.6, h: 0.55, fontFace: HFONT, fontSize: 22, bold: true, color: AMBER, align: "right", margin: 0 });
s.addText("Same fleet data. The development trend, propagated, lifts the fitted life — here's where the improvement finally shows up in unit-level risk.", { x: 0.85, y: 6.65, w: 11.7, h: 0.5, fontFace: BFONT, fontSize: 13, italic: true, color: STEEL, margin: 0 });
s.addNotes("Growth manifests primarily as a shift in eta (+18%). B10 is deliberately conservative because the shape drops slightly. Report both honestly; lead with eta.");

// ============================================================ 12. MONTE CARLO
s = p.addSlide();
s.background = { color: WHITE };
titleLight(s, "How much does it vary?");
fig(s, "figs/fig_mc.png", 1600 / 1050, 0.7, 1.7, 6.6, 4.75);
card(s, 7.7, 1.85, 4.95, 4.15, LIGHT);
s.addText([
  { text: "500", options: { bold: true, color: NAVY } },
  { text: " valid iterations", options: { color: INK } },
], { x: 8.0, y: 2.15, w: 4.4, h: 0.45, fontFace: BFONT, fontSize: 15, margin: 0 });
s.addText([
  { text: "MC median B10 825", options: { bullet: true, breakLine: true, bold: true } },
  { text: "95% band 802 – 846", options: { bullet: true, breakLine: true } },
  { text: "Spread dominated by the simulation, not growth-parameter error", options: { bullet: true } },
], { x: 8.05, y: 2.75, w: 4.5, h: 2.0, fontFace: BFONT, fontSize: 13.5, color: INK, paraSpaceAfter: 9, margin: 0 });
s.addText("Read these as forecast variability bands conditional on the fitted model — not inferential confidence intervals.", { x: 8.0, y: 5.05, w: 4.55, h: 0.9, fontFace: BFONT, fontSize: 12, italic: true, color: MUTE, margin: 0 });
s.addNotes("The shift is stable across draws, not a lucky sample. Be precise: variability band, not a confidence interval; model-form uncertainty not represented.");

// ============================================================ 13. SENSITIVITY
s = p.addSlide();
s.background = { color: WHITE };
titleLight(s, "What actually moves the answer");
fig(s, "figs/fig_sensitivity.png", 1600 / 1100, 0.7, 1.75, 6.6, 4.7);
card(s, 7.7, 1.9, 4.95, 4.0, LIGHT);
s.addText([
  { text: "Stronger growth → larger η", options: { bullet: true, breakLine: true, bold: true } },
  { text: "monotonic and orderly", options: { bullet: true, breakLine: true } },
  { text: "At βg = 1 it returns to the baseline — the shift is entirely the growth trend", options: { bullet: true, breakLine: true } },
  { text: "Robust to WHEN you assess the fleet", options: { bullet: true } },
], { x: 8.05, y: 2.2, w: 4.5, h: 2.6, fontFace: BFONT, fontSize: 13.5, color: INK, paraSpaceAfter: 9, margin: 0 });
s.addText("What matters most is having enough observed failures to pin down the baseline.", { x: 8.0, y: 5.0, w: 4.55, h: 0.8, fontFace: BFONT, fontSize: 12.5, italic: true, color: STEEL, margin: 0 });
s.addNotes("The 'none' box sitting on the baseline is the credibility check. The visible variation elsewhere is inherited from the baseline fit under censoring.");

// ============================================================ 14. HONEST LABEL
s = p.addSlide();
s.background = { color: NAVY };
titleDark(s, "Read the honest label");
card(s, 0.85, 1.75, 5.6, 3.05, "1E3350");
s.addText("This is the optimistic bound", { x: 1.15, y: 2.05, w: 5.0, h: 0.5, fontFace: HFONT, fontSize: 20, bold: true, color: AMBER, margin: 0 });
s.addText([
  { text: "Valid only if the developmental growth continues in service.", options: { breakLine: true } },
  { text: "Freeze the design at fielding → the no-growth baseline applies.", options: {} },
], { x: 1.15, y: 2.7, w: 5.05, h: 1.9, fontFace: BFONT, fontSize: 15, color: PALE, paraSpaceAfter: 10, margin: 0 });
// done / future
card(s, 6.75, 1.75, 5.7, 3.05, "1E3350");
s.addText([
  { text: "✓  ", options: { color: AMBER, bold: true } },
  { text: "Done: ", options: { bold: true, color: WHITE } },
  { text: "quantify the magnitude & variability of the shift", options: { color: PALE } },
], { x: 7.05, y: 2.1, w: 5.1, h: 1.0, fontFace: BFONT, fontSize: 15, margin: 0 });
s.addText([
  { text: "○  ", options: { color: GREY, bold: true } },
  { text: "Future: ", options: { bold: true, color: WHITE } },
  { text: "validate predictive accuracy against a fleet that truly keeps improving", options: { color: PALE } },
], { x: 7.05, y: 3.25, w: 5.1, h: 1.3, fontFace: BFONT, fontSize: 15, margin: 0 });
s.addText("We never claim the forecast is more accurate — only that we quantify the shift and its variability, transparently.", { x: 0.85, y: 5.35, w: 11.6, h: 0.8, fontFace: HFONT, fontSize: 16, bold: true, italic: true, color: WHITE, margin: 0 });
s.addNotes("Non-negotiable slide. RAMS reviewers respect this candor. Do not overclaim accuracy.");

// ============================================================ 15. TAKEAWAY
s = p.addSlide();
s.background = { color: NAVY };
titleDark(s, "The decision it enables");
s.addText("“Does the observed developmental trend justify revising fleet reliability upward — and by how much, with what bounds?”", { x: 0.9, y: 1.75, w: 11.5, h: 1.4, fontFace: HFONT, fontSize: 24, bold: true, color: WHITE, italic: true, lineSpacingMultiple: 1.05, margin: 0 });
const take = [
  ["Repeatable", "One workflow from growth trend to bounded life estimate"],
  ["Reproducible", "Fully scripted — seeds, data, and fits all open"],
  ["Open-source", "ReliaGrowR (CRAN) + WeibullR; under rOpenSci review"],
];
let tx = 0.9;
take.forEach(([h, b], i) => {
  card(s, tx, 3.5, 3.75, 1.95, "1E3350");
  s.addShape(p.ShapeType.diamond, { x: tx + 0.3, y: 3.8, w: 0.22, h: 0.22, fill: { color: AMBER } });
  s.addText(h, { x: tx + 0.62, y: 3.68, w: 3.0, h: 0.45, fontFace: HFONT, fontSize: 18, bold: true, color: WHITE, margin: 0 });
  s.addText(b, { x: tx + 0.3, y: 4.25, w: 3.25, h: 1.05, fontFace: BFONT, fontSize: 13, color: PALE, margin: 0 });
  tx += 3.96;
});
s.addText([
  { text: "ReliaGrowR on CRAN:  ", options: { color: PALE } },
  { text: "cran.r-project.org/package=ReliaGrowR", options: { color: AMBER, bold: true } },
], { x: 0.9, y: 6.2, w: 11.5, h: 0.4, fontFace: BFONT, fontSize: 14, margin: 0 });
s.addNotes("Land on the decision, not the math. The package + reproducibility is the durable takeaway.");

// ============================================================ 16. REFERENCES
s = p.addSlide();
s.background = { color: WHITE };
titleLight(s, "References");
const refs = [
  "L. H. Crow, “Reliability analysis for complex, repairable systems,” U.S. Army AMSAA, Tech. Rep. 138, 1975.",
  "P. Govan, “ReliaGrowR: Modeling and plotting functions for reliability growth analysis,” in Proc. 2026 RAMS, doi:10.1109/RAMS50514.2026.11424445.",
  "P. Govan, ReliaGrowR: Reliability Growth Analysis, R package, 2024, doi:10.32614/CRAN.package.ReliaGrowR.",
  "D. Silkworth and J. Symynck, WeibullR: Weibull Analysis for Reliability Engineering, R package v1.2.4, 2025.",
  "H. Guo, A. Mettas, G. Sarakakis, and P. Niu, “Piecewise NHPP models with MLE for repairable systems,” in Proc. 2010 RAMS, doi:10.1109/RAMS.2010.5448029.",
  "C. W. Zhang, T. Zhang, D. Xu, and M. Xie, “Analyzing highly censored reliability data without exact failure times,” Qual. Eng., 2013, doi:10.1080/08982112.2013.783598.",
  "E. Makalic and D. F. Schmidt, “Minimum message length inference of the Weibull distribution,” in AI 2023, LNCS 14471, pp. 291–303, doi:10.1007/978-981-99-8388-9_24.",
  "T. M. Grile and R. A. Bettinger, “Reliability analysis of deep space satellites launched 1991–2020,” Qual. Reliab. Eng. Int., 2024, doi:10.1002/qre.3600.",
  "V. M. R. Muggeo, “segmented: an R package to fit regression models with broken-line relationships,” R News, vol. 8(1), pp. 20–25, 2008.",
];
s.addText(refs.map((r, i) => ({
  text: r,
  options: { bullet: { code: "2022" }, breakLine: true, paraSpaceAfter: 7 },
})), { x: 0.95, y: 1.55, w: 11.5, h: 5.4, fontFace: BFONT, fontSize: 12.5, color: INK, valign: "top", margin: 0 });
s.addNotes("Standard closer. Full bibliography and the equation/algorithm backup slides follow (hidden) for Q&A.");

p.writeFile({ fileName: "RGF_RAMS2027.pptx" }).then(f => console.log("WROTE", f));
