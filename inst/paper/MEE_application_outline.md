# MEE Application Article — Outline (draft prep, not a manuscript)

Status: outline only. Open decisions are flagged below and need confirmation before full drafting.

## 1. Pathway status

- **rOpenSci submission**: [ropensci/software-review#747](https://github.com/ropensci/software-review/issues/747). ReliaGrowR v0.3.2 passed all automated checks (CRAN-published, 95% test coverage, clean R CMD check, all applicable `srr` statistical standards documented under the "Regression and Supervised Learning" category). Submission is ready but awaiting assignment of a handling editor due to an rOpenSci stats-review backlog — no reviewer has commented on scope/framing yet.
- **Article type**: MEE **Application** (via the rOpenSci↔MEE joint-review partnership — package reviewed by rOpenSci, manuscript fast-tracked by MEE). Submit to MEE after or alongside rOpenSci review, using rOpenSci's submission template to select MEE as venue.
- **Word limit**: ~3000 words describing the package (confirmed independently via rOpenSci's author dev guide and MEE's "Applications" feature page).
- **Precedent papers** (general-purpose statistical/simulation R tools published as MEE Applications via rOpenSci review, i.e. the template to follow):
  - Valavi et al. 2019, *blockCV*, MEE 10(2):225–232 — spatial cross-validation, framed around species distribution modelling.
  - Markowska et al. 2025, *rangr*, MEE — mechanistic species range dynamics simulator.
  - Sciaini et al. 2018, *NLMR*/*landscapetools*, MEE — neutral landscape model simulation.
  - Salecker et al. 2019, *nlrx*, MEE — reproducible NetLogo model analysis framework.

  Common pattern across all four: a domain-agnostic computational/statistical method, packaged as an R tool, explicitly tied to one worked ecological example that motivates why ecologists need it.

## 2. Differentiation constraint (must address head-on)

An existing paper already describes the package generally: P. B. Govan, "ReliaGrowR: Modeling and Plotting Functions for Reliability Growth Analysis," *2026 Annual Reliability and Maintainability Symposium (RAMS)*, pp. 1–6, doi:[10.1109/RAMS50514.2026.11424445](https://doi.org/10.1109/RAMS50514.2026.11424445). That paper is a general software/functions description for a reliability-engineering audience.

The MEE Application **cannot repeat that** — it needs to be substantively different: new audience (ecologists/evolutionary biologists), a genuine worked ecological example not present in the RAMS paper, and an explicit statement in the Introduction citing the RAMS paper and clarifying what's new here. This differentiation is the main editorial risk to manage, so the worked example (Section 3) is doing the real work of justifying the paper, not the package description.

(Separately, `inst/paper/RGF_paper.Rmd` is a third, unrelated in-progress manuscript on growth-adjusted life forecasting — also engineering-framed, not a candidate substitute for this piece.)

## 3. Candidate ecological worked examples (ranked, none confirmed yet)

Literature search found **no existing R package or MEE paper applying NHPP power-law growth models (Crow-AMSAA) or piecewise NHPP change-point detection to a genuine ecological dataset** — general recurrent-event tools exist (`reda`, `NHPoisson`, `nhppp`) but none targets trend/growth estimation in an ecological framing. That's good for novelty, but means no ready-made validated dataset from prior literature — one of the candidates below needs to be sourced and vetted.

| # | Use case | Fit to existing functions | Novelty vs. existing ecological tools | Candidate data source |
|---|---|---|---|---|
| 1 | **Wildfire ignition/recurrence trend and regime-shift detection** — has fire frequency in a region increased/decreased, and when did the regime shift? | `nhpp()`'s piecewise change-point detection (via `segmented`) is the most distinctive, least-replicated function in the package — a strong differentiator | High — change-point detection in fire-return intervals is a live topic (climate/fire-management policy) not covered by existing point-process ecology tools | State/national fire occurrence records (e.g. CAL FIRE, National Interagency Fire Center) — public, needs vetting for a clean single-site/region series |
| 2 | **Invasive species reinvasion rate after eradication effort** — does each successive control intervention reduce the reinvasion rate? | Closest **conceptual** match: Crow-AMSAA/`rga()`/`duane()` were designed for systems under successive corrective action, which maps almost 1:1 onto successive eradication/control efforts | Medium-high — reframes a growth-analysis method as a way to quantify whether management interventions are working, distinct from typical invasive-species trend models | EDDMapS (Early Detection & Distribution Mapping System) occurrence records, or a specific published eradication case study |
| 3 | **Wildlife disease outbreak recurrence across sites/populations** — comparing cumulative outbreak burden across monitored populations | `mcf()` (Nelson-Aalen) is designed exactly for comparing recurrent-event burden across multiple "systems" (here: sites/populations); `exposure()` handles uneven observation windows, common in field monitoring | Medium — recurrent-event framing of disease outbreaks exists in epidemiology, but rarely applied to wildlife population monitoring with MCF comparison across sites | USGS National Wildlife Health Center mortality/morbidity event records |

**Recommendation**: lead with #1 (wildfire) as the primary worked example — it showcases the package's most novel function and has strong topical relevance to MEE's readership. Consider folding in #2 as a secondary, shorter illustration if space allows (~3000 words is tight for two full examples, so likely one primary + one brief mention). #3 is a reasonable fallback if a clean wildfire dataset proves hard to source.

**This ranking is a recommendation, not a decision** — needs your sign-off, especially since only you can judge how defensible each dataset will look to ecologist reviewers.

## 4. Proposed manuscript structure (~3000-word budget)

| Section | Target words | Content |
|---|---|---|
| Title | – | Working title, e.g. "ReliaGrowR: reliability growth and recurrent-event models for ecological trend and regime-shift detection" |
| Summary | 150–200 | Problem, method, package, one-line result |
| 1. Introduction | 600–700 | Ecological motivation (recurrent-event trend/regime-shift problems in ecology), gap in existing tools, explicit differentiation from the RAMS 2026 paper, contribution statement |
| 2. Package description | 700–800 | Core functions table (`rga()`, `nhpp()`, `duane()`, `mcf()`, `exposure()`, `rdt()`), underlying methods (Crow-AMSAA/NHPP power law, piecewise NHPP with change-point detection, Nelson-Aalen MCF, Duane regression), S3 plot/print methods |
| 3. Worked example | 800–900 + 1–2 figs | Chosen use case (Section 3 above), data, model fit, interpretation |
| 4. Discussion | 300–400 | Limitations, relationship to existing ecological tools (`unmarked`, `spatstat`, `reda`), future extensions |
| Data & code availability | – | CRAN + GitHub + rOpenSci review link + Zenodo archive DOI (need to create one per MEE's code-publishing policy) |
| References | – | Include RAMS 2026 paper as prior work |

## 5. Open decisions before drafting starts

1. **Which worked example** (Section 3) — confirm #1, #2, #3, or a different one entirely.
2. **Dataset source** — real public dataset vs. a simulated one built on the package's existing `sim_failures.R` pattern (faster, but weaker for reviewer credibility than real ecological data).
3. **Co-authors** — the four precedent papers above are multi-author, often including a domain ecologist; a co-author with ecological credentials could materially strengthen review odds. Solo-authored is fine but worth considering.
4. **Timing** — draft now, or wait for an rOpenSci handling editor/reviewers to weigh in on scope first (their feedback could reshape the ecological framing before locking it into a manuscript).
