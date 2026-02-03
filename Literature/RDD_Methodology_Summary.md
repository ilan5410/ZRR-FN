# Regression Discontinuity Design (RDD) Methodology Summary
## Based on Lee & Lemieux (2010) - Journal of Economic Literature

---

## 1. CORE CONCEPT

**Definition**: RDD estimates treatment effects in settings where treatment is determined by whether an observed "assignment variable" (also called "forcing variable" or "running variable") exceeds a known cutoff point.

**Key Insight**: Individuals with values of the assignment variable just below the cutoff (who did not receive treatment) are good comparisons to those just above the cutoff (who received treatment).

**Formula**: The treatment effect is estimated as:
```
τ = lim(ε↓0) E[Y|X = c + ε] - lim(ε↑0) E[Y|X = c + ε]
```
Where:
- Y = outcome variable
- X = assignment variable (running variable)
- c = cutoff value
- τ = treatment effect

---

## 2. TYPES OF RDD

### 2.1 Sharp RDD
- Treatment is a deterministic function of the assignment variable
- Probability of treatment jumps from 0 to 1 at cutoff
- D = 1 if X ≥ c, D = 0 if X < c

### 2.2 Fuzzy RDD
- Probability of treatment jumps by less than 1 at the cutoff
- Imperfect compliance or other factors affect treatment receipt
- Estimated using instrumental variables approach
- Treatment effect = (Jump in Y) / (Jump in probability of treatment)

---

## 3. VALIDITY CONDITIONS

### 3.1 Critical Assumption: Imprecise Control
**The RDD is valid when individuals have IMPRECISE control over the assignment variable.**

This means:
- Individuals cannot precisely manipulate their position relative to cutoff
- There is a stochastic component to the assignment variable
- The density of X is continuous at the cutoff point

### 3.2 Local Randomization Result
**When individuals have imprecise control, treatment is "as good as randomly assigned" near the cutoff.**

This implies:
- All baseline characteristics should be continuous at cutoff
- Distribution of covariates should be same just above/below cutoff
- RDD can be analyzed like a randomized experiment

### 3.3 Continuity Assumption
**All factors other than treatment must evolve smoothly with respect to X**

- If other variables also jump at c, the estimate will be biased
- The underlying functions E[Y(1)|X] and E[Y(0)|X] must be continuous

---

## 4. TESTING VALIDITY

### 4.1 Density Test (McCrary Test)
**Examine the density of the assignment variable**
- Plot histogram of X with narrow bins
- Check for discontinuous jump at cutoff
- A jump suggests manipulation/sorting
- Use McCrary (2008) formal test

**How to implement**:
```
1. Partition X into equally spaced bins
2. Compute frequencies within bins
3. Run local linear regression on frequencies
4. Test for discontinuity at cutoff
```

### 4.2 Covariate Balance Tests
**Test whether baseline covariates are balanced**
- Run RDD analysis replacing Y with each baseline covariate
- Should find NO discontinuities in predetermined variables
- Can test jointly using SUR or stacked regression with clustering

**Interpretation**:
- Discontinuity in covariates → violation of RDD assumptions
- Similar to checking balance in randomized experiment

---

## 5. GRAPHICAL PRESENTATION

### 5.1 Bin Selection for Graphs
**Create bins and plot local averages**

```
For bandwidth h and K bins:
- Create bins: (bk, bk+1] where bk = c - (K0 - k + 1)h
- Compute average Y in each bin: Ȳk = (1/Nk) Σ Yi × 1{bk < Xi ≤ bk+1}
- Plot Ȳk against bin midpoints
```

**Bandwidth selection**:
- Try multiple bandwidths (0.005, 0.01, 0.02, etc.)
- Use cross-validation procedure
- Balance between precision (wide bins) and bias (narrow bins)

### 5.2 Specification Tests for Bins
1. **Bin test**: Compare fit with K bins vs 2K bins (F-test)
2. **Regression test**: Test if Y-X relationship exists within bins

### 5.3 Visual Guidelines
- Show both raw binned means AND smoothed polynomial
- Use non-overlapping bins
- Avoid excessive smoothing that hides data variation
- Make bins narrow enough to compare near cutoff
- Typical recommendation: bandwidth 0.01-0.03

---

## 6. ESTIMATION METHODS

### 6.1 Parametric Polynomial Regression

**Basic model**:
```
Y = αl + τD + fl(X - c) + ε    (left of cutoff)
Y = αr + fr(X - c) + ε         (right of cutoff)
```

**Pooled regression form**:
```
Y = αl + τD + βl(X - c) + (βr - βl)D(X - c) + ε
```

**Key points**:
- Always allow different slopes on each side (interact D with X)
- Subtract cutoff value from X (center at cutoff)
- Try multiple polynomial orders (linear, quadratic, cubic, quartic)
- Use Akaike Information Criterion (AIC) to select order

**Polynomial specification**:
```
Linear: fl(X-c) = β1(X-c)
Quadratic: fl(X-c) = β1(X-c) + β2(X-c)²
Cubic: fl(X-c) = β1(X-c) + β2(X-c)² + β3(X-c)³
Quartic: fl(X-c) = β1(X-c) + β2(X-c)² + β3(X-c)³ + β4(X-c)⁴
```

### 6.2 Local Linear Regression (Nonparametric)

**Concept**: Use only observations within bandwidth h of cutoff

**Implementation**:
```
Left of cutoff:  Y = αl + βl(X - c) + ε,  where c - h ≤ X < c
Right of cutoff: Y = αr + βr(X - c) + ε,  where c ≤ X ≤ c + h
Treatment effect: τ = αr - αl
```

**Bandwidth selection methods**:

1. **Rule-of-thumb (ROT)**:
```
hROT = 2.702 × σ̃² / [Σ(m̃''(xi))²]^(1/5) × s
```
Where m̃''(·) is second derivative of regression, σ̃ is standard error

2. **Cross-validation**:
```
CVY(h) = (1/N) Σ(Yi - Ŷ(Xi))²
```
- Leave-one-out prediction
- Choose h that minimizes CV function
- Use observations close to cutoff for CV

**Typical optimal bandwidth**: 0.10 - 0.30

### 6.3 Goodness-of-Fit Tests

**Bin dummy test**:
```
Y = αl + τD + βl(X-c) + (βr-βl)D(X-c) + Σϕk×Bk + ε
```
- Add bin dummies to regression
- Test if ϕ2 = ϕ3 = ... = ϕK-1 = 0
- Reject → model is misspecified
- This also tests for jumps at other points (falsification test)

---

## 7. STANDARD ERRORS

### 7.1 Sharp RDD
- Use heteroskedasticity-robust standard errors (White 1980)
- Standard OLS/2SLS formulas apply
- Ensures SEs consistent whether using pooled or separate regressions

### 7.2 Fuzzy RDD  
- Use 2SLS with robust standard errors
- Instrument: T = 1[X ≥ c]
- Same bandwidth/polynomial for both stages

### 7.3 Special Cases
- **Discrete running variable**: Cluster or use weighted regression on cell means
- **Panel data**: Cluster at individual level
- Bandwidth choice affects inference (undersmooth for valid asymptotics)

---

## 8. INCORPORATING COVARIATES

### 8.1 Why Include Covariates?
- Reduce sampling variability (increase precision)
- NOT necessary for identification if RDD is valid
- Provides additional validity check

### 8.2 Two Approaches

**Approach 1: Residualization**
```
Step 1: Y - Wπ (residualize outcome)
Step 2: Run RDD on residuals
```
- Advantage: Can check fit graphically
- Can use any π (OLS of Y on W is natural)

**Approach 2: Direct inclusion**
```
Y = αl + τD + βl(X-c) + (βr-βl)D(X-c) + Wγ + ε
```
- Advantage: More efficient under certain assumptions
- Disadvantage: Harder to diagnose misspecification

### 8.3 Warning
- If estimates change substantially when adding W → potential problem
- Could indicate: misspecified functional form OR discontinuity in W

---

## 9. PRACTICAL IMPLEMENTATION CHECKLIST

### Step 1: Visualize the Data
- [ ] Create histogram of running variable X (check for manipulation)
- [ ] Create RDD plot with binned means (try multiple bandwidths)
- [ ] Overlay polynomial fit on binned means
- [ ] Check if visual discontinuity is evident

### Step 2: Baseline Validity Tests
- [ ] McCrary density test for manipulation
- [ ] Test covariate balance at cutoff
- [ ] Check if treatment/control groups differ only at cutoff

### Step 3: Estimate Treatment Effects
- [ ] Run polynomial regressions (orders 1-4)
- [ ] Run local linear regressions (bandwidths 0.05-0.30)
- [ ] Try both sharp and fuzzy specifications if applicable
- [ ] Always allow different slopes/functions on each side

### Step 4: Specification Tests
- [ ] Goodness-of-fit tests (bin dummies)
- [ ] AIC for polynomial order selection
- [ ] Cross-validation for bandwidth selection
- [ ] Placebo tests at other cutoff points

### Step 5: Robustness Checks
- [ ] Vary bandwidth systematically
- [ ] Vary polynomial order
- [ ] Include/exclude covariates
- [ ] Test sensitivity to outliers
- [ ] Check different samples (donut hole, varying windows)

### Step 6: Report Results
- [ ] Present main graph (binned means + polynomial)
- [ ] Show density plot of running variable
- [ ] Show covariate balance graphs/table
- [ ] Report estimates across specifications (table)
- [ ] Report optimal bandwidth/polynomial selection
- [ ] Present heteroskedasticity-robust standard errors

---

## 10. COMMON PITFALLS

### 10.1 What Invalidates RDD?

❌ **Precise manipulation of running variable**
- Individuals can sort exactly above/below cutoff
- Evidence: discontinuity in density of X

❌ **Other treatments change at same cutoff**
- Multiple programs kick in at same threshold
- Cannot isolate effect of single treatment

❌ **Incorrect functional form**
- Using linear when true relationship is nonlinear
- Evidence: polynomial model rejected by bin tests

❌ **Too narrow bandwidth**
- Very imprecise estimates
- High sampling variability

❌ **Too wide bandwidth**
- Bias from using observations far from cutoff
- Linear approximation fails

### 10.2 Common Mistakes

1. **Not allowing different slopes**: Constraining βr = βl
2. **Not testing covariate balance**: Missing evidence of manipulation
3. **Only reporting one specification**: Need robustness across bandwidths/polynomials
4. **Ignoring density discontinuity**: Sign of manipulation
5. **Oversmoothing graphs**: Hiding important variation
6. **Using quoted significance without robust SEs**: Incorrect inference

---

## 11. INTERPRETATION

### 11.1 What Does RDD Identify?

**Local Average Treatment Effect (LATE)**
- Effect at the cutoff point
- Weighted average across all individuals
- Weights = ex ante probability of being near cutoff

**NOT identified**:
- Effect far from cutoff (unless extrapolate)
- Effect for different treatment definitions
- Long-run effects (for age-based cutoffs)

### 11.2 Generalizability

The RDD estimate applies to:
- Individuals near the margin (near cutoff)
- Weighted by likelihood of being near cutoff
- Those who would comply with treatment rule (fuzzy RDD)

May NOT generalize to:
- Individuals far from cutoff
- Different populations
- Different treatment intensities

---

## 12. SPECIAL CASES

### 12.1 Discrete Running Variable
- Cannot observe very narrow bins
- Must rely more on functional form
- Use goodness-of-fit tests with discrete values
- Cluster standard errors by value of X

### 12.2 Multiple Cutoffs
- Can pool data if treatment effect is same
- Test for heterogeneity across cutoffs
- May increase precision substantially

### 12.3 Panel Data
- Can use lagged Y as covariate
- Cluster standard errors by individual
- Fixed effects generally NOT necessary (can reduce precision)

### 12.4 Geographic Boundaries
- Non-random placement of boundaries is concern
- Careful consideration of sorting across boundaries
- May not satisfy "as good as randomized" criterion

---

## 13. EMPIRICAL EXAMPLE WORKFLOW

### Example: Effect of Winning Election on Vote Share in Next Election

**Setup**:
- Y = vote share in next election (or winning next election)
- X = vote share in current election (margin of victory)
- c = 0.50 (50% cutoff)
- D = 1 if X ≥ 0.50 (won election)

**Step-by-step**:

1. **Data preparation**
   - Center X: X̃ = X - 0.50
   - Restrict to competitive elections (e.g., |X̃| ≤ 0.25)

2. **Density check**
   - Histogram with bandwidth 0.005
   - Visual inspection at cutoff
   - McCrary test (should be insignificant)

3. **Main graph**
   - Bins of width 0.01-0.02
   - Overlay quartic polynomial
   - Clear visual discontinuity expected

4. **Covariate balance**
   - Test: vote share TWO elections ago
   - Should show NO discontinuity
   - Provides validity evidence

5. **Estimation**
   - Polynomials: orders 0-4
   - Bandwidths: 0.05, 0.10, 0.15, 0.25, 0.50
   - Local linear: optimal bandwidth ~0.15-0.28
   - Typical estimate: 7-10 percentage points

6. **Specification tests**
   - Bin test with bins of 0.01
   - Should not reject for reasonable specifications
   - AIC suggests quadratic or cubic

7. **Reporting**
   - Main estimate: ~8 percentage points
   - Robust SEs: ~1-2 percentage points  
   - Stable across reasonable specifications

---

## 14. SOFTWARE IMPLEMENTATION NOTES

### 14.1 R packages
- `rdrobust`: Optimal bandwidth, robust inference
- `rdd`: Basic RDD estimation
- `rddtools`: Multiple cutoffs, placebo tests

### 14.2 Stata commands
- `rdrobust`: Comprehensive RDD toolkit
- `cmogram`: Binned scatterplots
- `DCdensity`: McCrary density test

### 14.3 Python
- `statsmodels`: Basic regression
- `rdrobust-python`: Port of R package
- Custom implementation using pandas/numpy

### 14.4 Key functions needed
1. Bin creation and averaging
2. Polynomial regression with interactions
3. Local linear regression with bandwidth
4. Cross-validation for bandwidth
5. Robust standard errors
6. Density discontinuity test
7. Covariate balance tests

---

## 15. SUMMARY OF KEY FORMULAS

**Treatment effect (sharp RDD)**:
```
τ = lim(ε→0+) E[Y|X=c+ε] - lim(ε→0+) E[Y|X=c-ε]
```

**Treatment effect (fuzzy RDD)**:
```
τF = [lim E[Y|X=c+ε] - lim E[Y|X=c-ε]] / [lim E[D|X=c+ε] - lim E[D|X=c-ε]]
```

**Polynomial regression**:
```
Y = αl + τD + Σβlj(X-c)^j + Σ(βrj-βlj)D(X-c)^j + ε
```

**Local linear**:
```
Y = αl + τD + βl(X-c) + (βr-βl)D(X-c) + ε, |X-c| ≤ h
```

**Cross-validation**:
```
h_opt = argmin_h (1/N)Σ[Yi - Ŷ-i(Xi,h)]²
```

**Optimal bandwidth (ROT)**:
```
h* ∝ N^(-1/5)
```

---

## 16. FINAL RECOMMENDATIONS

### Do's ✓
- Test for manipulation (density test)
- Test covariate balance
- Show graphical evidence
- Try multiple specifications
- Use robust standard errors
- Report sensitivity analysis
- Allow different slopes/functions on each side

### Don'ts ✗
- Don't use if clear manipulation exists
- Don't only report one specification
- Don't ignore density discontinuities
- Don't constrain slopes to be equal
- Don't use very wide bandwidths without checking
- Don't forget to center running variable at cutoff
- Don't claim effects far from cutoff without extrapolation

---

## REFERENCES

Lee, D. S., & Lemieux, T. (2010). Regression discontinuity designs in economics. *Journal of Economic Literature*, 48(2), 281-355.

Additional key references:
- Hahn, Todd, & van der Klaauw (2001) - Formal identification
- Imbens & Lemieux (2008) - Practical guide
- McCrary (2008) - Manipulation testing
- Lee (2008) - Local randomization result

---

## PRACTICAL TIPS FOR IMPLEMENTATION

1. **Always start with graphs** - If you can't see it in the graph, it's probably not there
2. **Transparency is key** - Show the raw data, not just smoothed estimates
3. **Bandwidth matters** - Report results across multiple reasonable bandwidths
4. **Test validity** - Density and covariate tests are not optional
5. **Functional form** - When in doubt, use more flexible specification
6. **Context matters** - Understand the institutional details of your cutoff
7. **Interpretation** - Be clear about local nature of estimates

---

*End of Summary*
