# dsra

**Dynamic Site Response Analysis**

R package for stochastic site response analysis in earthquake engineering. Implements Monte Carlo soil profile generation, empirical small-strain shear modulus models, and VS30-based site classification for seismic hazard studies.

[![R Version](https://img.shields.io/badge/R-%E2%89%A53.5-blue)](https://www.r-project.org/)
[![Version](https://img.shields.io/badge/version-0.3.0-green)](https://github.com/averriK/ar-dsra)
[![License](https://img.shields.io/badge/License-Custom-blue.svg)](LICENSE)

## Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [Core Functions](#core-functions)
- [Datasets](#datasets)
- [Dependencies](#dependencies)
- [License](#license)
- [Citation](#citation)
- [Author](#author)

---

## Installation

```r
remotes::install_github("averriK/ar-dsra")
```

---

## Quick Start

### Site Classification: VS30 ↔ Site Class

```r
library(dsra)

# Convert site class to representative VS30 value
SIDtoVs30("C")
SIDtoVs30(c("B", "C", "D"))

# Convert VS30 value to site class
Vs30toSID(500)
Vs30toSID(c(200, 400, 800))
```

### Build a Site Profile

```r
library(data.table)

site <- geSiteTable(
  Hs = 100,
  USCS = c("GW", "SM", "CL"),
  Water = 0.20,
  POP = 100,
  h = 1.0,
  Vref = 760
)

print(site)
```

### Monte Carlo Site Properties

```r
site_stats <- getSiteProperties(
  Hs = c(90, 100, 110),
  USCS = c("GW", "GP", "SM", "CL"),
  NR = 50,
  levels = c(0.16, 0.50, 0.84),
  POP = 100,
  Water = 0.30,
  Vref = 760
)

print(site_stats)
```

### Compute Site Fundamental Period

```r
vs_profile <- c(200, 300, 400, 500, 600)
layer_thickness <- c(5, 10, 15, 20, 30)
depth_centers <- cumsum(layer_thickness) - layer_thickness/2

Ts <- fitModel.Ts(
  VSm = vs_profile,
  hs = layer_thickness,
  zm = depth_centers
)
```

### Solve Characteristic Equation for Inhomogeneous Cylinder

```r
root <- getCylinderRoots(
  mo = 0.45,
  lo = 0.44,
  no = 1,
  model = "nlm"
)

models <- c("lm", "nlm", "dt", "rf")
comparison <- sapply(models, function(m) {
  getCylinderRoots(mo = 0.45, lo = 0.44, no = 1, model = m)
})
```

---

## Core Functions

### Site Classification

| Function | Description | Input | Output |
|----------|-------------|-------|--------|
| `SIDtoVs30` | Site class → VS30 | Site class ID (A, AB, B, BC, C, CD, D, DE, E) | Representative VS30 (m/s) |
| `Vs30toSID` | VS30 → Site class | VS30 (m/s) | Site class ID |

**Site Class Thresholds:**

| Site Class | VS30 Range (m/s) | Representative VS30 (m/s) | Description |
|------------|------------------|---------------------------|-------------|
| A | ≥ 1500 | 1500 | Hard rock |
| AB | ≥ 1500 | 1500 | (Alternative designation) |
| B | 900 – 1500 | 1200 | Medium hard rock |
| BC | 640 – 900 | 760 | Soft rock |
| C | 440 – 640 | 540 | Very dense soil or hard clay |
| CD | 300 – 440 | 370 | Dense sand or very stiff clay |
| D | 210 – 300 | 255 | Medium dense sand or stiff clay |
| DE | 150 – 210 | 180 | Loose sand or medium stiff clay |
| E | < 150 | 150 | Very loose sand or soft clay |

### Site Profile Generation

| Function | Description | Key Parameters | Output |
|----------|-------------|----------------|--------|
| `geSiteTable` | Build synthetic soil column | `Hs`, `USCS`, `Water`, `POP`, `h`, `Vref` | Site properties table |
| `getSiteProperties` | Monte Carlo statistics | `Hs`, `USCS`, `NR`, `levels`, `POP`, `Water` | Quantile summaries |

**Key Parameters:**
- **`Hs`**: Total site depth (m)
- **`USCS`**: Vector of Unified Soil Classification System codes (e.g., c("GW", "SM", "CL"))
- **`Water`**: Water table depth as fraction of `Hs` (0 = at surface, 1 = at bedrock)
- **`POP`**: Pre-consolidation pressure (kPa), default = 100
- **`h`**: Layer discretization thickness (m), default = 0.50 m
- **`Vref`**: Reference VS for deep foundation / bedrock (m/s), default = 760 m/s
- **`NR`**: Number of Monte Carlo realizations
- **`levels`**: Statistical quantiles (e.g., c(0.16, 0.50, 0.84)) or "mean"
- **`getSiteLayers`**: Logical, if TRUE returns detailed layer properties (default = FALSE)

### Site Period Analysis

| Function | Description | Input | Output |
|----------|-------------|-------|--------|
| `fitModel.Ts` | Site fundamental period | `VSm`, `hs`, `zm` | Ts (s) |

**Parameters:**
- **`VSm`**: Shear wave velocity profile (m/s), vector of layer velocities
- **`hs`**: Layer thickness profile (m), vector of layer thicknesses
- **`zm`**: Depth to layer midpoints (m), vector of midpoint depths

### Characteristic Equation Solver

| Function | Description | Input | Output |
|----------|-------------|-------|--------|
| `getCylinderRoots` | Solve eigenvalue problem | `mo`, `lo`, `no`, `model` | Eigenvalue (root) |

**Parameters:**
- **`mo`**: Inhomogeneity ratio m ∈ [0, 0.95], where m=0 is homogeneous
- **`lo`**: Truncation parameter λ ∈ [0, 0.5]
- **`no`**: Mode number n ∈ {1, 2, ..., 8}
- **`model`**: Solution method:
  - `"lm"`: Linear model (an ~ l + m + l² + m²)
  - `"nlm"`: Nonlinear model with interaction terms (includes l×m and l²×m²)
  - `"dt"`: Decision tree (rpart)
  - `"rf"`: Random forest
- **`extrapolate`**: Allow extrapolation beyond dataset bounds (default = TRUE)
- **`OSF`**: Outlier scale factor for random forest local dataset reduction (default = 0.10)

---

## Datasets

The package includes four datasets:

### ShearModelParameters (20 models)

Empirical shear modulus model parameters from literature:

**Columns:**
- **ModelID**: Model identifier
- **NameID**: Descriptive name  
- **GroupID**: Soil group (Gravels, Sands, Fines)
- **A**: Coefficient (MPa)
- **UN**: Units notation
- **Ce**: Void ratio constant
- **n**: Stress exponent
- **AuthorID**: Citation
- **emin**, **emax**: Applicable void ratio range

**Models by Group:**
- **Sands (7)**: HR63R, HR63A, SS75, IW78, KO80, YR84, LO93
- **Fines (7)**: HB68, MW72K, MW72B, ZU78A, ZU78B, ZU78C, KU82C  
- **Gravels (6)**: PR81, KE81R, KE81G, TA87, GO87, NI85

### CylinderRoots (307,128 solutions)

Pre-computed eigenvalues for cylindrical wave propagation:

**Columns:**
- **m**: Inhomogeneity ratio (0 to 0.95)
- **l**: Truncation parameter (0 to 0.5)
- **n**: Mode number (1 to 8)
- **an**: Eigenvalue (root)

**Usage:**
```r
data(CylinderRoots)
CylinderRoots[m == 0.45 & l == 0.44 & n == 1]
```

### SiteClass

Site classification reference table:

**Columns:**
- **SC**: Site class designation
- **Description**: Soil/rock description  
- **Vs30 (m/s)**: VS30 range
- **Vs30 (ft/s)**: VS30 range in imperial units

---

## Computational Workflow

The implementation in `geSiteTable` follows this sequence:

### Discretization

Divide site depth `Hs` into `NL = ceiling(Hs/h)` layers with thickness `h`:

```r
NL <- ceiling(Hs / h)
hs <- rep(h, NL)
zo <- zi <- zm <- vector(mode = "double", length = NL)
for (k in seq(1, NL)) {
  if (k > 1) zo[k] <- zi[k - 1]
  zi[k] <- zo[k] + h
  zm[k] <- (zi[k] + zo[k]) / 2
}
```

### Soil Type Assignment

Sample USCS codes from provided vector for each layer:

```r
UID <- sample(USCS, size = NL, replace = TRUE)
```

Group assignment:

```r
if (UID[k] %in% ValidGravels) GID[k] <- "Gravels"
if (UID[k] %in% ValidSands)   GID[k] <- "Sands"
if (UID[k] %in% ValidFines)   GID[k] <- "Fines"
```

where:
- `ValidGravels = c("GW", "GP", "GM", "GC")`
- `ValidSands = c("SW", "SP", "SM", "SC")`
- `ValidFines = c("MH", "ML", "CH", "CL", "OH", "OL", "PT")`

### Void Ratio Sampling

For each layer, sample void ratios from ranges in `VoidRatiosUSCS` table:

```r
RANGE <- VoidRatiosUSCS[USCS == UID[k]]
emin[k] <- runif(1, min = RANGE$eminMin, max = RANGE$eminMax)
emax[k] <- runif(1, min = RANGE$emaxMin, max = RANGE$emaxMax)
```

Current void ratio:

$$
e_0 \sim \begin{cases}
\text{Uniform}(e_{\min}, e_{\max}) & \text{if UniformDistribution = TRUE} \\
\text{Triangle}(e_{\min}, e_{\max}, \frac{e_{\min} + e_{\max}}{2}) & \text{otherwise}
\end{cases}
$$

### Relative Density

$$
D_r = \frac{e_{\max} - e_0}{e_{\max} - e_{\min}}
$$

### Unit Weight Sampling

Sample saturated unit weight from `UnitWeightRanges` table:

```r
RANGE <- UnitWeightRanges[USCS == UID[k]]
gsmin[k] <- RANGE$gsatMin  # [kN/m³]
gsmax[k] <- RANGE$gsatMax  # [kN/m³]
gsat[k] <- runif(n = 1, min = gsmin[k], max = gsmax[k])
```

Dry unit weight:

$$
\gamma_d = \max(0, \gamma_{\text{sat}} - 10) \quad [\text{kN/m}^3]
$$

Effective unit weight below water table:

$$
\gamma' = \begin{cases}
\max(0, \gamma_{\text{sat}} - 10) & \text{if } z_m > z_w \\
\gamma_{\text{sat}} & \text{otherwise}
\end{cases}
$$

where $z_w = H_s - H_w$ is water table depth.

### Plasticity Properties

For fine-grained soils (`ML, CL, MH, CH, OL, OH`):

**Liquid limit**:

$$
LL \sim \begin{cases}
\text{Uniform}(50, 100) & \text{if } \text{USCS} \in \{\text{CH, MH, OH}\} \\
\text{Uniform}(8, 50) & \text{if } \text{USCS} \in \{\text{CL, ML, OL}\}
\end{cases}
$$

**Plasticity index bounds**:

$$
\begin{aligned}
\text{U-line:} \quad & IP_U = 0.9 \cdot (LL - 8) \\
\text{A-line:} \quad & IP_A = \begin{cases}
0.73 \cdot (LL - 20) & \text{if } LL \geq 20 \\
0 & \text{otherwise}
\end{cases}
\end{aligned}
$$

**Plasticity index sampling**:

$$
IP \sim \begin{cases}
\text{Uniform}(IP_A, IP_U) & \text{if } \text{USCS} \in \{\text{Clays}\} \\
\text{Uniform}(0, IP_A) & \text{if } \text{USCS} \in \{\text{Silts, Organic}\}
\end{cases}
$$

### Stress State

Octahedral stresses using $K_0 = 0.5$:

$$
\begin{aligned}
\sigma'_0(k) &= \sigma'_i(k-1) \\
\Delta \sigma' &= \frac{1}{3}(1 + 2K_0) \cdot \gamma' \cdot h = \frac{1}{3}(1 + 2 \cdot 0.5) \cdot \gamma' \cdot h = \frac{2}{3} \gamma' \cdot h \\
\sigma'_i(k) &= \sigma'_0(k) + \Delta \sigma' \\
\sigma'_m(k) &= \frac{\sigma'_i(k) + \sigma'_0(k)}{2}
\end{aligned}
$$

### Overconsolidation Ratio

$$
OCR = \max\left(\frac{\sigma'_m + POP}{\sigma'_m}, 1\right)
$$

where $POP$ is pre-consolidation pressure (kPa).

### Small-Strain Shear Modulus

For each layer, evaluate all applicable models from `ShearModelParameters`:

```r
MID <- ShearModelParameters[GroupID == GID[k]]$ModelID
A   <- ShearModelParameters[ModelID %in% MID, A]   # [MPa]
Ce  <- ShearModelParameters[ModelID %in% MID, Ce]
N   <- ShearModelParameters[ModelID %in% MID, n]
```

Void ratio function:

$$
F(e) = \frac{(C_e - e_0)^2}{1 + e_0}
$$

OCR exponent for fine-grained soils:

$$
m_1 = \begin{cases}
0 & \text{if } \text{GroupID} \neq \text{Fines} \\
\text{approx}\left(\{0, 20, 40, 60, 80, 100\}, \{0, 0.18, 0.30, 0.41, 0.48, 0.48\}, IP\right) & \text{if } \text{GroupID} = \text{Fines}
\end{cases}
$$

Shear modulus at reference pressure:

$$
G_{\text{ref}} = A \cdot F(e) \cdot OCR^{m_1} \cdot 100^n \quad [\text{MPa}]
$$

where $p_{\text{ref}} = 100$ kPa.

Shear modulus at current stress:

$$
G_0 = G_{\text{ref}} \cdot \left(\frac{\sigma'_m}{100}\right)^n \quad [\text{MPa}]
$$

Model ensemble average:

$$
G_m = \frac{1}{N_{\text{models}}} \sum_{i=1}^{N_{\text{models}}} G_{0,i} \quad [\text{MPa}]
$$

**Implementation:** Average of all applicable models for the soil group.

### Shear Wave Velocity

$$
V_S = \sqrt{\frac{9.81 \cdot G_m \cdot 1000}{\gamma_{\text{sat}}}} \quad [\text{m/s}]
$$

where:
- $G_m$ is in MPa
- $\gamma_{\text{sat}}$ is in kN/m³
- $9.81$ m/s² is gravitational acceleration
- Factor 1000 converts MPa to kPa

### Power-Law Profile Fitting

Fit power-law to log-transformed shear modulus profile:

$$
G(z) = G_0 \left(\frac{z}{H_s}\right)^{m_0}
$$

Log-linear regression:

$$
\begin{aligned}
X &= \ln\left(\frac{z_m}{H_s}\right) \\
Y &= \ln(G_m) \\
Y &= \beta_0 + \beta_1 \cdot X \\
G_0 &= \exp(\beta_0) \quad [\text{MPa}] \\
m_0 &= \beta_1
\end{aligned}
$$

Surface shear wave velocity:

$$
V_{S0} = \sqrt{\frac{9.81 \cdot G_0 \cdot 1000}{\gamma_{\text{sat}}(N_L)}} \quad [\text{m/s}]
$$

### VS30 Calculation

Travel-time weighted average to 30 m:

$$
V_{S30} = \frac{30}{\sum_{i=1}^{N} \frac{h_i}{V_{Si}}}
$$

where $h_i$ and $V_{Si}$ are thickness and VS of layer $i$.

**Padding for shallow profiles**:

If $\sum h_i < 30$ m, pad with foundation layer:

$$
t_{30} = \sum_{i=1}^{N_L} \frac{h_i}{V_{Si}} + \frac{30 - \sum h_i}{V_{\text{ref}}}
$$

where $V_{\text{ref}} = 760$ m/s (default).

### Site Fundamental Period

Rayleigh quotient for layered shear beam:

$$
T_s = \frac{2\pi}{\omega_s}, \quad \omega_s^2 = \frac{4 \sum_{j=1}^{N_L-1} A_j}{\sum_{j=1}^{N_L-1} B_j}
$$

where:

$$
\begin{aligned}
f_{j+1} &= f_j + \frac{h_j \cdot (H_s - z_{m,j})}{V_{Sj}} \\
A_j &= \frac{\left(V_{Sj} \cdot (f_{j+1} - f_j)\right)^2}{h_j} \\
B_j &= h_j \cdot (f_{j+1} + f_j)^2
\end{aligned}
$$

with $f_1 = 0$ at surface.

### Site Classification

Map $V_{S30}$ to site class via `Vs30toSID`:

$$
\text{SID} = \begin{cases}
\text{A} & \text{if } V_{S30} \geq 1500 \\
\text{B} & \text{if } 900 \leq V_{S30} < 1500 \\
\text{BC} & \text{if } 640 \leq V_{S30} < 900 \\
\text{C} & \text{if } 440 \leq V_{S30} < 640 \\
\text{CD} & \text{if } 300 \leq V_{S30} < 440 \\
\text{D} & \text{if } 210 \leq V_{S30} < 300 \\
\text{DE} & \text{if } 150 \leq V_{S30} < 210 \\
\text{E} & \text{if } V_{S30} < 150
\end{cases}
$$

### Data Formats

#### Site Properties Table (from `geSiteTable`)

| Column | Description | Units |
|--------|-------------|-------|
| `Hs` | Total site depth | m |
| `Hw` | Water table depth | m |
| `NL` | Number of layers | - |
| `VS30` | Average shear wave velocity (0–30m) | m/s |
| `SID` | Site class | - |
| `Go` | Initial shear modulus | MPa |
| `mo` | Inhomogeneity exponent | - |
| `Ts` | Site fundamental period | s |
| `VSo` | Surface shear wave velocity | m/s |
| `Z500`, `Z1000` | Depth to VS = 500, 1000 m/s | m |
| `UID` | Concatenated USCS codes | - |
| `Gravels`, `Sands`, `Fines`, `Clays`, `Silts`, `Organic` | Composition (%) | % |
| `Water` | Water table depth | % of Hs |
| `POP` | Pre-consolidation pressure | kPa |

#### Site Layers Table (from `geSiteTable(..., getSiteLayers = TRUE)`)

| Column | Description | Units |
|--------|-------------|-------|
| `USCS` | Soil type per layer | - |
| `GroupID` | Soil group (Gravels, Sands, Fines) | - |
| `zm` | Depth to layer center | m |
| `hs` | Layer thickness | m |
| `emin`, `eo`, `emax` | Void ratios | - |
| `gs`, `gsat` | Unit weights | kN/m³ |
| `Dr` | Relative density | - |
| `IP`, `LL` | Plasticity indices | - |
| `pm` | Mean effective stress | kPa |
| `OCR` | Overconsolidation ratio | - |
| `Gm` | Shear modulus | MPa |
| `VSm` | Shear wave velocity | m/s |
| `VSa` | Average VS to depth `zm` | m/s |

---

## Theory and Mathematical Background

### Soil Classification (USCS)

The Unified Soil Classification System (ASTM D2487) classifies soils based on grain size distribution and plasticity characteristics.

**Supported USCS Codes:**

| Group | Codes | Description |
|-------|-------|-------------|
| **Gravels** | GW, GP, GM, GC | Well-graded, poorly-graded, silty, clayey |
| **Sands** | SW, SP, SM, SC | Well-graded, poorly-graded, silty, clayey |
| **Silts** | ML, MH | Low plasticity, high plasticity |
| **Clays** | CL, CH | Low plasticity, high plasticity |
| **Organic** | OL, OH, PT | Organic silt/clay, organic clay, peat |

### Site Classification (VS30 and ASCE/SEI 7-22)

The average shear wave velocity in the upper 30 meters, $V_{S30}$, is the primary parameter for seismic site classification:

$$
V_{S30} = \frac{30}{\sum_{i=1}^{N} \frac{h_i}{V_{Si}}}
$$

where $h_i$ and $V_{Si}$ are the thickness and shear wave velocity of layer $i$.

If $\sum h_i < 30$ m, the profile is padded with a foundation layer at $V_{\text{ref}} = 760$ m/s.

**Physical Interpretation:**
- $V_{S30}$ is a travel-time weighted harmonic mean
- Soft layers contribute more to travel time, thus lowering $V_{S30}$
- Used to classify sites into A–E (ASCE/SEI 7-22)

### Void Ratio and Relative Density

**Void ratio** $e$ represents the ratio of void volume to solid volume in soil:

$$
e = \frac{V_v}{V_s}
$$

Sampled from USCS-specific ranges:

$$
\begin{aligned}
e_{\min} &\sim \text{Uniform}(e_{\min,\min}, e_{\min,\max}) \\
e_{\max} &\sim \text{Uniform}(e_{\max,\min}, e_{\max,\max}) \\
e_0 &\sim \text{Uniform}(e_{\min}, e_{\max})
\end{aligned}
$$

**Relative density**:

$$
D_r = \frac{e_{\max} - e_0}{e_{\max} - e_{\min}}
$$

Physical interpretation:
- $D_r = 0$: Loosest possible state ($e_0 = e_{\max}$)
- $D_r = 1$: Densest possible state ($e_0 = e_{\min}$)
- Used to classify coarse-grained soils (Very Loose, Loose, Compact, Dense, Very Dense)

### Unit Weights and Stress State

**Unit weights** sampled from USCS-specific ranges:

$$
\gamma_{\text{sat}} \sim \text{Uniform}(\gamma_{\text{sat,min}}, \gamma_{\text{sat,max}}) \quad [\text{kN/m}^3]
$$

**Effective stress** calculation using $K_0 = 0.5$:

$$
\sigma'_m = \frac{1}{2}\left(\sigma'_0 + \sigma'_i\right) = \frac{1}{2}\left(\sigma'_0 + \sigma'_0 + \frac{2}{3}\gamma'h\right) = \sigma'_0 + \frac{1}{3}\gamma'h
$$

Cumulative stress at layer $k$:

$$
\sigma'_m(k) = \sum_{j=1}^{k} \frac{1}{3}\gamma'_j h_j
$$

### Plasticity Properties (A-Line and U-Line)

For fine-grained soils, the **Casagrande plasticity chart** defines soil classification:

**U-line** (upper bound):

$$
IP_U = 0.9(LL - 8)
$$

**A-line** (separates clays from silts):

$$
IP_A = \begin{cases}
0.73(LL - 20) & \text{if } LL \geq 20 \\
0 & \text{otherwise}
\end{cases}
$$

**Sampling**:

$$
IP \sim \begin{cases}
\text{Uniform}(IP_A, IP_U) & \text{if soil is Clay (CL, CH)} \\
\text{Uniform}(0, IP_A) & \text{if soil is Silt (ML, MH) or Organic}
\end{cases}
$$

Physical interpretation:
- Clays plot above A-line: $IP > 0.73(LL - 20)$
- Silts plot below A-line: $IP < 0.73(LL - 20)$
- All soils plot below U-line: $IP < 0.9(LL - 8)$

### Overconsolidation Ratio

**OCR** represents stress history:

$$
OCR = \max\left(\frac{\sigma'_m + POP}{\sigma'_m}, 1\right) = \max\left(1 + \frac{POP}{\sigma'_m}, 1\right)
$$

where:
- $\sigma'_m$ is current mean effective stress (kPa)
- $POP$ is pre-consolidation pressure (kPa)

Physical interpretation:
- $OCR = 1$: Normally consolidated (never experienced higher stress)
- $OCR > 1$: Overconsolidated (experienced higher stress in past)
- Affects stiffness: overconsolidated soils are stiffer

### Small-Strain Shear Modulus Models

The small-strain shear modulus $G_{\max}$ is modeled using 20 empirical correlations. General form:

$$
G_{\max} = A \cdot F(e) \cdot OCR^{m_1} \cdot \left(\frac{\sigma'_m}{p_{\text{ref}}}\right)^n
$$

where:
- $A$ is model-specific calibration constant (MPa)
- $F(e) = \frac{(C_e - e)^2}{1 + e}$ is void ratio function
- $C_e$ is void ratio constant
- $e$ is current void ratio
- $OCR$ is overconsolidation ratio
- $\sigma'_m$ is mean effective stress (kPa)
- $p_{\text{ref}} = 100$ kPa is reference pressure
- $n$ is stress exponent
- $m_1$ is OCR exponent (plasticity-dependent for fines)

**OCR exponent**:

$$
m_1 = \begin{cases}
0 & \text{if coarse-grained (Gravels, Sands)} \\
f(IP) & \text{if fine-grained (Fines)}
\end{cases}
$$

For fine-grained soils, $m_1$ is interpolated from:

| IP | 0 | 20 | 40 | 60 | 80 | 100 |
|----|---|----|----|----|----|-----|
| $m_1$ | 0 | 0.18 | 0.30 | 0.41 | 0.48 | 0.48 |

**Model inventory** (20 models in ShearModelParameters):

| ModelID | Author | GroupID | A (MPa) | Ce | n | $e_{\min}$ | $e_{\max}$ |
|---------|--------|---------|---------|----|----|------------|------------|
| HR63R | Hardin & Richart (1963) | Sands | 7.00 | 2.174 | 0.50 | 0.3 | 0.8 |
| HR63A | Hardin & Richart (1963) | Sands | 3.27 | 2.973 | 0.50 | 0.6 | 1.3 |
| SS75 | Shibata-Soelarno (1975) | Sands | 42.00 | 0.670 | 0.50 | 0.6 | 0.9 |
| IW78 | Iwasaki et al. (1978) | Sands | 9.00 | 2.174 | 0.38 | 0.6 | 0.9 |
| KO80 | Kokusho (1980) | Sands | 8.40 | 2.173 | 0.50 | 0.6 | 0.8 |
| YR84 | Yu-Richart (1984) | Sands | 7.00 | 2.173 | 0.50 | 0.6 | 0.9 |
| LO93 | Lo Presti et al. (1993) | Sands | 7.10 | 2.270 | 0.43 | 0.6 | 0.9 |
| HB68 | Hardin & Black (1968) | Fines | 3.27 | 2.973 | 0.50 | 0.5 | 1.7 |
| MW72K | Marcuson & Wahls (1972) | Fines | 4.50 | 2.973 | 0.50 | 1.1 | 1.3 |
| MW72B | Marcuson & Wahls (1972) | Fines | 0.45 | 4.400 | 0.50 | 1.6 | 2.5 |
| ZU78A | Zen-Umehara (1978) | Fines | 2.00 | 2.973 | 0.50 | 1.6 | 2.5 |
| ZU78B | Zen-Umehara (1978) | Fines | 3.00 | 2.973 | 0.50 | 1.6 | 2.5 |
| ZU78C | Zen-Umehara (1978) | Fines | 4.00 | 2.973 | 0.50 | 1.6 | 2.5 |
| KU82C | Kokusho (1982) | Fines | 0.14 | 7.320 | 0.60 | 1.7 | 3.8 |
| PR81 | Prange (1981) | Gravels | 7.23 | 2.973 | 0.38 | 0.25 | 0.7 |
| KE81R | Kokusho (1981) | Gravels | 13.00 | 2.173 | 0.55 | 0.25 | 0.7 |
| KE81G | Kokusho (1981) | Gravels | 8.40 | 2.173 | 0.60 | 0.25 | 0.7 |
| TA87 | Tanaka et al. (1987) | Gravels | 3.08 | 2.173 | 0.60 | 0.25 | 0.7 |
| GO87 | Goto et al. (1987) | Gravels | 1.22 | 2.173 | 0.85 | 0.25 | 0.7 |
| NI85 | Nishio et al. (1985) | Gravels | 9.36 | 2.173 | 0.44 | 0.25 | 0.7 |

**Ensemble average**:

For each layer, all applicable models for the soil group are evaluated and averaged:

$$
G_m = \frac{1}{N_{\text{models}}} \sum_{i=1}^{N_{\text{models}}} G_{\max,i}
$$

This averaging reduces model uncertainty by combining multiple empirical relationships.

### Shear Wave Velocity

From small-strain shear modulus:

$$
V_S = \sqrt{\frac{G_{\max}}{\rho}} = \sqrt{\frac{9.81 \cdot G_m \cdot 1000}{\gamma_{\text{sat}}}} \quad [\text{m/s}]
$$

where:
- $G_m$ is shear modulus (MPa)
- $\gamma_{\text{sat}}$ is saturated unit weight (kN/m³)
- $\rho = \gamma_{\text{sat}} / 9.81$ is mass density (Mg/m³)
- Factor 1000 converts MPa to kPa

Physical interpretation:
- Stiffer soil (higher $G$) → higher $V_S$
- Denser soil (higher $\gamma$) → higher $V_S$ (but effect is weaker due to square root)
- Typical range: 150–1500 m/s for soils

### Power-Law Profile Fitting

Fit inhomogeneous shear modulus profile:

$$
G(z) = G_0 \left(\frac{z}{H_s}\right)^{m_0}
$$

Logarithmic transformation:

$$
\ln G = \ln G_0 + m_0 \cdot \ln\left(\frac{z}{H_s}\right)
$$

Linear regression:

$$
\begin{aligned}
Y &= \ln(G_m) \\
X &= \ln(z_m / H_s) \\
Y &= \beta_0 + \beta_1 X \\
\implies G_0 &= e^{\beta_0}, \quad m_0 = \beta_1
\end{aligned}
$$

Physical interpretation:
- $m_0 = 0$: Homogeneous profile
- $m_0 > 0$: Stiffness increases with depth (typical)
- $m_0 < 0$: Stiffness decreases with depth (unusual)
- Typical range: $m_0 \in [0.3, 0.7]$

Similarly for shear wave velocity:

$$
V_S(z) = V_{S0} \left(\frac{z}{H_s}\right)^{m_0/2}
$$

### VS30 Calculation

**Travel-time weighted average**:

$$
V_{S30} = \frac{30}{\sum_{i=1}^{N_{\min(30)}} \frac{h_i}{V_{Si}}}
$$

where sum extends to layers within 0–30 m depth.

**For shallow profiles** ($H_s < 30$ m):

$$
V_{S30} = \frac{30}{\sum_{i=1}^{N_L} \frac{h_{30,i}}{V_{Si}} + \frac{30 - H_s}{V_{\text{ref}}}}
$$

where:
- $h_{30,i} = \min(z_{i,\text{bottom}}, 30) - z_{i,\text{top}}$ is thickness within 0–30 m
- $V_{\text{ref}} = 760$ m/s is assumed foundation velocity

### Site Fundamental Period

**Rayleigh quotient** for layered shear beam:

$$
T_s = \frac{2\pi}{\omega_s}
$$

where the fundamental angular frequency is:

$$
\omega_s^2 = \frac{4 \sum_{j=1}^{N_L-1} A_j}{\sum_{j=1}^{N_L-1} B_j}
$$

**Energy terms**:

$$
\begin{aligned}
A_j &= \frac{\left(V_{Sj} \cdot (f_{j+1} - f_j)\right)^2}{h_j} \quad \text{(kinetic energy)} \\
B_j &= h_j \cdot (f_{j+1} + f_j)^2 \quad \text{(potential energy)}
\end{aligned}
$$

**Displacement function**:

$$
f_{j+1} = f_j + \frac{h_j \cdot (H_s - z_{m,j})}{V_{Sj}}
$$

with $f_1 = 0$ at surface and $H_s$ is total depth.

Physical interpretation:
- Represents fundamental mode of shear beam with fixed base
- Softer soils (lower $V_S$) → longer $T_s$
- Deeper sites (larger $H_s$) → longer $T_s$
- Critical for resonance: if earthquake has energy near $T_s$, amplification is maximized


### Characteristic Equations for Inhomogeneous Media

The `getCylinderRoots` function solves characteristic equations for inhomogeneous cylinders, based on **Dakoulas & Gazetas (1985)**. The solution involves Bessel functions with eigenvalues $\alpha_n$ that satisfy specific boundary conditions.

**Parameters:**
- $m \in [0, 0.95]$: Inhomogeneity ratio ($m=0$ is homogeneous)
- $\lambda \in [0, 0.5]$: Aspect ratio / truncation parameter
- $n \in \{1, 2, \ldots, 8\}$: Mode number

**Pre-computed solutions:** The package provides 307,128 pre-computed eigenvalues in `CylinderRoots` dataset, spanning:
- $m \in \{0.00, 0.02, 0.04, \ldots, 0.96\}$ (49 values)
- $\lambda \in \{0.000, 0.005, 0.010, \ldots, 0.495\}$ (100 values)
- $n \in \{1, 2, \ldots, 8\}$ (8 modes)

**Interpolation methods**:

1. **Linear model** (`model = "lm"`):
   $$
   \alpha_n = \beta_0 + \beta_1 \lambda + \beta_2 m + \beta_3 \lambda^2 + \beta_4 m^2
   $$

2. **Nonlinear model** (`model = "nlm"`):
   $$
   \alpha_n = \beta_0 + \beta_1 \lambda + \beta_2 m + \beta_3 \lambda^2 + \beta_4 m^2 + \beta_5 \lambda m + \beta_6 \lambda^2 m^2
   $$

3. **Decision tree** (`model = "dt"`):
   Recursive partitioning on $(\lambda, m)$ space.

4. **Random forest** (`model = "rf"`):
   Ensemble of decision trees with local dataset reduction.

**Reference:** The Mathematica notebook `data-raw/dg85/buildTables.nb` documents:
```
(* Roots of Cylinder equations. *)
(* From. Dakoulas, Gazetas 1985 *)
```

The characteristic equation involves Bessel functions of fractional order $q = m/(2-m)$:

$$
J_{-1-q}(a \lambda^{1-m/2}) \cdot J_q(a) + J_{-q}(a) \cdot J_{1+q}(a \lambda^{1-m/2}) = 0
$$

where $a = \alpha_n$ is the eigenvalue to be solved.

**Applications:** These eigenvalues are used in analytical solutions for:
- Fundamental periods of truncated inhomogeneous shear beams
- Dynamic response of inhomogeneous soil columns
- Pile foundation dynamics


### Monte Carlo Uncertainty Propagation

The `getSiteProperties` function propagates uncertainty through Monte Carlo sampling:

1. **Sampling**: For each realization $i = 1, \ldots, N_R$:
   - Sample $e_{\min}, e_{\max}$ from USCS-specific ranges (uniform)
   - Sample $e_0$ from $[e_{\min}, e_{\max}]$ (uniform or triangular)
   - Sample $\gamma_{\text{sat}}$ from tabulated ranges (uniform)
   - Sample $LL, IP$ for fine-grained soils
   - Randomly assign USCS codes to layers (discrete uniform)

2. **Deterministic calculation:** For each realization, compute via `geSiteTable`:
   - Layer-by-layer: $G_m(z)$, $V_S(z)$, $\sigma'_m(z)$, $OCR(z)$
   - Aggregate: $G_0$, $m_0$, $V_{S0}$, $V_{S30}$, $T_s$, $Z_{500}$, $Z_{1000}$

3. **Statistical aggregation**:

   **Mean** (if `"mean" %in% levels`):
   $$
   \bar{X} = \frac{1}{N_R} \sum_{i=1}^{N_R} X_i
   $$

   **Quantiles** (for numeric levels):
   $$
   Q_p(X) = \inf\{x : P(X \leq x) \geq p\}
   $$

   Typical usage: `levels = c(0.16, 0.50, 0.84)` provides median and ±1σ bounds for lognormal distributions.

**Why Monte Carlo?**
- Propagates aleatory uncertainty (natural spatial variability)
- Handles complex nonlinear relationships ($V_S \propto \sqrt{G/\rho}$, power-law fitting)
- Provides confidence intervals for design parameters
- No analytical solution due to model complexity


---

## Datasets

The package includes four datasets with soil and site information:

### SiteTable (255,614 profiles)

Pre-computed site characteristics including:
- Site geometry: `Hs` (depth), `NL` (number of layers)
- Site classification: `VS30`, `SID` (site class A–E)
- Shear modulus model: `Go` (initial modulus), `mo` (inhomogeneity exponent)
- Dynamic properties: `Ts` (fundamental period), `VSo` (surface VS)
- Depth horizons: `Z500`, `Z1000` (depth to VS = 500, 1000 m/s)
- Soil composition: % Gravels, Sands, Fines, Clays, Silts, Organic
- Water table: `Hw` (depth), `Water` (% of Hs)
- Consolidation: `POP` (pre-consolidation pressure)

**Usage:**
```r
data(SiteTable)
head(SiteTable)

SiteTable[SID == "C"]
SiteTable[VS30 >= 360 & VS30 <= 760]
```

### ShearModelParameters (20 models)

Shear modulus model parameters from literature sources:
- Model coefficients: `A`, `Ce`, `n`
- Void ratio ranges: `emin`, `emax`
- Soil group: `GroupID` (Gravels, Sands, Fines)
- Reference: `ModelID`, `AuthorID` (bibliographic citation)

**Usage:**
```r
data(ShearModelParameters)
head(ShearModelParameters)

ShearModelParameters[GroupID == "Sands"]
```

### CylinderRoots (307,124 solutions)

Pre-computed eigenvalues for cylindrical wave propagation:
- Parameters: `m` (inhomogeneity), `l` (truncation), `n` (mode)
- Eigenvalue: `an` (root of characteristic equation)

**Usage:**
```r
data(CylinderRoots)
head(CylinderRoots)

CylinderRoots[m == 0.45 & l == 0.44 & n == 1]
```

### SiteClass

ASCE/SEI 7-22 site classification criteria:
- Site class: `SC` (A, B, BC, C, CD, D, DE, E, F)
- Description: `Description`
- VS30 ranges: `Vs30 (m/s)` and `Vs30 (ft/s)`

**Usage:**
```r
data(SiteClass)
print(SiteClass)
```

---

## Dependencies

**Core packages:**
- `data.table`: Fast data manipulation
- `stats`: Statistical functions (lm, predict, quantile)
- `digest`: Hash functions for data integrity

**Domain-specific:**
- `randomForest`: Random forest regression for `getCylinderRoots(..., model = "rf")`
- `rpart`: Decision tree regression for `getCylinderRoots(..., model = "dt")`
- `triangle`: Triangular distribution sampling
- `stringr`: String manipulation for USCS codes
- `utils`: Utility functions

---

## References

### Small-Strain Shear Modulus

Models implemented from 20 literature sources (see `ShearModelParameters` dataset):

**Sands:**

1. Hardin, B.O. & Richart, F.E. (1963). Elastic wave velocities in granular soils. *Journal of the Soil Mechanics and Foundations Division*, ASCE, **89**(SM1), 33-65.

2. Shibata, T. & Soelarno, D.S. (1975). Stress-strain characteristics of sands under cyclic loading. *Proceedings of JSCE*, **239**, 57-65.

3. Iwasaki, T., Tatsuoka, F., & Takagi, Y. (1978). Shear moduli of sands under cyclic torsional shear loading. *Soils and Foundations*, **18**(1), 39-56.

4. Kokusho, T. (1980). Cyclic triaxial test of dynamic soil properties for wide strain range. *Soils and Foundations*, **20**(2), 45-60.

5. Yu, P. & Richart, F.E. (1984). Stress ratio effects on shear modulus of sands. *Journal of Geotechnical Engineering*, ASCE, **110**(3), 331-345.

6. Lo Presti, D.C.F., Jamiolkowski, M., Pallara, O., Cavallaro, A., & Pedroni, S. (1993). Shear modulus and damping of soils. *Géotechnique*, **47**(3), 603-617.

**Fine-grained soils:**

7. Hardin, B.O. & Black, W.L. (1968). Vibration modulus of normally consolidated clay. *Journal of the Soil Mechanics and Foundations Division*, ASCE, **94**(SM2), 353-369.

8. Marcuson, W.F. & Wahls, H.E. (1972). Time effects on dynamic shear modulus of clays. *Journal of the Soil Mechanics and Foundations Division*, ASCE, **98**(SM12), 1359-1373.

9. Zen, K. & Umehara, Y. (1978). Determination of small strain shear modulus from undisturbed samples. *Report of the Port and Harbour Research Institute*, **17**(3), 29-57.

10. Kokusho, T. (1982). Dynamic properties of soft clay for wide strain range. *Soils and Foundations*, **22**(4), 1-18.

**Gravels:**

11. Prange, B. (1981). Resonant column testing of railroad ballast. *Report for the Federal Railroad Administration*, DOT/FRA/ORD-81/27.

12. Kokusho, T. (1981). Nonlinear soil properties determined by a cyclic triaxial test. *Soils and Foundations*, **21**(1), 57-70.

13. Tanaka, Y., Kudo, K., Yoshida, Y., & Ikemi, M. (1987). A study on the mechanical properties of sandy gravel. *Proceedings of the 8th Asian Regional Conference on SMFE*, **1**, 1-4.

14. Goto, S., Shamoto, Y., & Tamaoki, K. (1987). Dynamic properties of gravels sampled by ground freezing. *Proceedings of the 8th Asian Regional Conference on SMFE*, **1**, 141-144.

15. Nishio, S., Kiku, H., & Ohta, S. (1985). Dynamic deformation characteristics of gravelly layer. *Proceedings of the 3rd International Conference on Numerical Methods in Geomechanics*, 265-270.

### Site Classification

16. BSSC (2020). *NEHRP Recommended Seismic Provisions for New Buildings and Other Structures (FEMA P-2082)*. Building Seismic Safety Council. [ASCE/SEI 7-22 site classification criteria]

### Site Fundamental Period

17. Kramer, S.L. (1996). *Geotechnical Earthquake Engineering*. Prentice Hall. [Rayleigh quotient method for site period]

### Characteristic Equations

18. Dakoulas, P. & Gazetas, G. (1985). [CITATION NEEDED - Referenced in data-raw/dg85/buildTables.nb:44-45 but exact title unavailable]

### Soil Mechanics

19. ASTM D2487-17 (2017). *Standard Practice for Classification of Soils for Engineering Purposes (Unified Soil Classification System)*. ASTM International.

20. Casagrande, A. (1948). Classification and identification of soils. *Transactions of the American Society of Civil Engineers*, **113**, 901-991. [Plasticity chart, A-line, U-line]

---

## License

This package is distributed under a custom license. See [LICENSE](LICENSE) file for details.

---

## Citation

When using this package in research, please cite:

```bibtex
@software{dsra2024,
  author = {Verri Kozlowski, Alejandro},
  title = {ar-dsra: Dynamic Site Response Analysis},
  year = {2024},
  version = {0.3.0},
  url = {https://github.com/averriK/ar-dsra}
}
```

---

## Author

**Alejandro Verri Kozlowski**

- Email: averri@fi.uba.ar
- ORCID: [0000-0002-8535-1170](https://orcid.org/0000-0002-8535-1170)
- Affiliation: Facultad de Ingeniería, Universidad de Buenos Aires
