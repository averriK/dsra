---
layout: default
title: Quick start
permalink: /docs/quickstart/
---

# Quick start

A minimal path to a first successful dsra run.

## Install

Requires **R >= 3.5**.

```r
devtools::install_github("averriK/dsra")
```

## Synthetic profile from a USCS list

`getSiteProperties()` runs a Monte-Carlo over USCS-driven random samples of void ratio, unit weight and plasticity, and returns Vs, fundamental period (Ts) and inhomogeneity ratio (mo) at the requested quantile levels.

```r
library(dsra)

site_props <- getSiteProperties(
  Hs = c(100, 50, 131),
  USCS = c("GW", "GP", "GM", "ML", "SM"),
  NR = 25,
  levels = c(0.16, 0.50, "mean", 0.84)
)
```

## Layer-wise site table

`geSiteTable()` builds a layer-wise table from a known mineralogy / Hs / Water / POP setup.

```r
library(data.table)

geSiteTable(
  .newdata = data.table(
    Gravels = c(90, 95, 100),
    Sands   = c(5, 10, 25),
    Hs      = c(90, 100, 110),
    Water   = 0,
    POP     = 100
  )
)
```

## Characteristic roots for modal analysis

`getCylinderRoots(mo, lo, no, model)` returns the eigenvalue used by Gazetas & Dakoulas' inhomogeneous truncated shear-beam model. `model` selects the interpolation backend (`"nlm"`, `"lm"`, `"dt"`, `"rf"`).

```r
getCylinderRoots(mo = 0.45, lo = 0.44)
getCylinderRoots(mo = 0.45, lo = 0.44001, model = "nlm")
getCylinderRoots(mo = 0.99, lo = 0.51)
```

## Site classification utilities

```r
Vs30toSID(760)   # "BC"
SIDtoVs30("C")   # 540
```

## Where to go next

- Function-level reference: `?dsra`, `?getSiteProperties`, `?geSiteTable`, `?getCylinderRoots`, `?fitModel.Ts`, `?Vs30toSID`, `?SIDtoVs30`.
- The full Exported API list is in the project [README]({{ "/" | relative_url }}).
