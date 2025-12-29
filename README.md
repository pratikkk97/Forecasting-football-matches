# Football Match Outcome Prediction using GAP Ratings

This repository contains research work focused on improving football match outcome predictions using **Generalised Attacking Performance (GAP) ratings** combined with advanced match statistics such as **expected goals (xG)**, shots on target, smart passes, and penalty-area actions.

The project investigates whether predicting *performance* first (via GAP ratings) leads to more accurate *match outcome probabilities* than relying on goals alone.

---

## 🎯 Research Objectives

- Identify which match statistics (beyond shots and corners) best predict football match outcomes
- Evaluate whether including **expected goals (xG)** improves prediction accuracy
- Compare **observed match statistics** vs **GAP-predicted statistics**
- Assess forecasting performance using probabilistic models

---

## 📊 Data Source

- **Provider:** Wyscout
- **Seasons:** 2015/16 – 2020/21
- **Competitions:**  
  - Premier League  
  - Championship  
  - League One  
  - La Liga  
  - Segunda División  
  - Brazil Serie A & B
- **Total Matches Used:** ~14,000 (after exclusions)

---

## 🧠 Methodology

### GAP Rating System
Each team is assigned:
- Home attacking & defensive ratings
- Away attacking & defensive ratings

Ratings are updated after every match and used to **predict future attacking performance**, not goals directly.

### Models Used
- **Ordinal Logistic Regression**  
  → Predicts probabilities of *Home Win / Draw / Away Win*
- **Linear Regression**  
  → Predicts goals scored by each team
- **Model Selection:** Akaike Information Criterion (AIC)
- **Evaluation Metric:** Ranked Probability Score (RPS)

---

## 🧪 Key Findings

- Goals alone are **poor predictors** due to randomness
- Best-performing statistics:
  - Expected Goals (xG)
  - Shots on Target
  - Smart Passes
  - Touches in Penalty Area
- GAP-predicted statistics outperform simple historical averages
- Combining **xG + attacking actions** significantly improves match outcome predictions

---

## 📈 Results Summary

- GAP predictions consistently outperform baseline benchmarks
- Models using richer attacking data achieved **lower AIC scores** than traditional shot-based models
- Confirms that **performance-based metrics are more reliable than results-based metrics**

---

## 📚 Tools & Techniques

- Statistical modelling (Ordinal Logistic Regression)
- Time-updated rating systems
- Model comparison using AIC
- Football analytics & performance modelling

---

## 👤 Author

**Pratik Shetty**  
MSc Sport Analytics & Technologies  
Loughborough University  

---

## 📜 Reference

Based on:
> Wheatcroft (2020) – *Generalised Attacking Performance (GAP) Ratings*
