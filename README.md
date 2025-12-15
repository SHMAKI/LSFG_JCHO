# LSFG_JCHO

This repository contains the source codes for the analysis and model construction used in the following paper:

**Shigeyuki Magi, Takahiro Maruyama, Seiji Takagi, Atsuhiko T Naito, Yuichi Hori.** *Machine learning assessment of retinal blood flow links metabolic dysfunction and accelerated microvascular aging, Scientific Reports*, in press.

**The analysis code used in the current manuscript will be pushed after final acceptance.**

[Preprint](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5217965)

## Overview

Microvascular aging impairs tissue perfusion and contributes to age-related organ dysfunction; however, direct approaches to assess microvascular aging remain limited. Here, we developed machine learning models to estimate chronological age from retinal blood flow data acquired by laser speckle flowgraphy (LSFG) in 1,008 adults undergoing health checkups. Using 18 predefined parameters and 3,253 automatically extracted time-series features, the best model for all participants achieved a mean absolute percentage error (MAPE) of 10.3% between predicted and chronological age. In addition, sex-specific models for women and men outperformed the model for all participants (MAPE 8.6% in women and 9.3% in men), allowing us to explore sex differences in microvascular aging patterns. Based on bias-corrected residuals, we defined a novel biomarker, the relative microvascular aging index (rmVAI). Higher rmVAI was significantly associated with elevated blood pressure, diabetes, metabolic syndrome, and metabolic dysfunction–associated fatty liver disease (MAFLD). Individuals classified as “model-predicted older” (rmVAI > 10%) had higher fatty liver index values (mean 38.2 vs 26.7) and an approximately twofold higher prevalence of hepatic steatosis (26% vs 12%) than those classified as “model-predicted younger” (rmVAI < −10%). Our LSFG-based machine learning framework provides a scalable and non-invasive tool for quantifying microvascular aging from retinal blood flow and suggests that MAFLD is a potent systemic contributor to accelerated microvascular aging, supporting a putative “liver–microvascular axis” that warrants further investigation for early risk stratification in clinical practice.
