# dsra

**Dynamic Site Response Analysis**

[![R Version](https://img.shields.io/badge/R-%3E%3D%203.5-blue)](https://www.r-project.org/) [![Version](https://img.shields.io/badge/version-0.3.0-green)](https://github.com/averriK/dsra)

R package for generating synthetic soil profiles and computing fundamental periods of slopes and embankments using inhomogeneous truncated shear beam theory.

## Contents

- [Overview](#overview)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Application](#application)
- [Exported API](#exported-api)
- [Dependencies](#dependencies)
- [References](#references)
- [Documentation (how to read)](#documentation-how-to-read)
- [License](#license)
- [Citation](#citation)
- [Author](#author)

## Overview

dsra generates synthetic soil profiles with random geotechnical properties based on USCS classification and computes fundamental periods (Ts) and shear modulus variation parameters (mo) for seismic analysis of slopes and embankments. Implements Ishihara's shear modulus model and Gazetas & Dakoulas' inhomogeneous truncated shear beam theory.

## Features

- **Synthetic profile generation**: Random sampling of void ratio, unit weight, plasticity based on USCS
- **Ishihara shear modulus model**: G(z,e) from void ratio and octahedral stress
- **Fundamental period calculation**: Ts via Rayleigh method for Vs profiles
- **Inhomogeneity characterization**: mo parameter for power-law shear modulus variation
- **Modal analysis**: Characteristic roots (eigenvalues) for inhomogeneous truncated beams
- **Uncertainty quantification**: Multiple realizations with quantile estimates
- **Site classification**: Conversion between Vs30 and site classes (A, B, BC, C, CD, D, DE, E)

## Installation

```r
devtools::install_github("averriK/dsra")
```

## Usage

```r
library(dsra)

# Generate synthetic profile with quantiles
site_props <- getSiteProperties(
  Hs = 30,                      # Embankment height (m)
  USCS = c("SM", "ML", "CL"),   # Soil types
  h = 1.00,                     # Layer thickness (m)
  NR = 100,                     # Number of realizations
  levels = c(0.16, "mean", 0.84),
  Vref = 760                    # Bedrock Vs (m/s)
)

# Compute characteristic roots for modal analysis
an <- getCylinderRoots(
  mo = 0.5,                     # Inhomogeneity ratio (0 = homogeneous)
  lo = 0.3,                     # Truncation ratio (berm influence)
  no = 1,                       # Mode number (1 = fundamental)
  model = "nlm"                 # Interpolation model
)

# Site classification utilities
Vs30toSID(760)  # "BC"
SIDtoVs30("C")  # 540
```

## Application

dsra is used to estimate fundamental periods (Ts) and inhomogeneity ratios (mo) for seismic slope stability analysis using flexible-block Newmark displacement models (Bray & Travasarou 2007, Bray & Macedo 2017/2019). Applications include tailings storage facilities, waste rock dumps, and site response analysis.

## Exported API

- `geSiteTable(Hs, USCS, ...)` — Build layer-wise site table with Vs profile, fundamental period (Ts), inhomogeneity ratio (mo); optional detailed site layers.
- `getSiteProperties(Hs, USCS, ...)` — Monte Carlo generation of synthetic site properties and quantiles for Ts and mo.
- `getCylinderRoots(mo, lo, ...)` — Compute characteristic roots (eigenvalues) for inhomogeneous truncated shear-beam modal analysis.
- `fitModel.Ts(VSm, hs, zm)` — Fit truncated shear-beam model parameters from Vs profile and geometry to estimate Ts.
- `Vs30toSID(Vs30)` — Convert Vs30 to NEHRP-style site class.
- `SIDtoVs30(SID)` — Map site class back to representative Vs30.

## Dependencies

- R (>= 3.5)
- data.table, stats, utils, stringr, digest, triangle
- randomForest, rpart (for root interpolation)

## References

Gazetas, G., & Dakoulas, P. (1985). Seismic analysis and design of rockfill dams: State-of-the-art. *Soil Dynamics and Earthquake Engineering*, 4(1), 1-14.

Ishihara, K. (1997). *Soil Behaviour in Earthquake Geotechnics*. Oxford University Press.

Bray, J. D., & Travasarou, T. (2007). Simplified procedure for estimating earthquake-induced deviatoric slope displacements. *Journal of Geotechnical and Geoenvironmental Engineering*, 133(4), 381-392.

## Documentation (how to read)

Start at the documentation index:

- [docs/index.md](docs/index.md)

Topic pages:

- [Quick start](docs/quickstart.md) — install + minimal worked examples (`getSiteProperties`, `geSiteTable`, `getCylinderRoots`, `Vs30toSID`/`SIDtoVs30`).

For function-level reference, prefer the in-package R help:

```r
?dsra
?getSiteProperties
?geSiteTable
?getCylinderRoots
```

## License

Custom license - see [LICENSE](LICENSE)

## Citation

```bibtex
@software{dsra2008,
  author = {Verri Kozlowski, Alejandro},
  title = {dsra: Dynamic Site Response Analysis},
  year = {2008},
  version = {0.3.0},
  url = {https://github.com/averriK/dsra}
}
```

---

## Author

**Alejandro Verri Kozlowski**  
**Email:** averri@fi.uba.ar  
**ORCID:** [0000-0002-8535-1170](https://orcid.org/0000-0002-8535-1170)  
**Affiliation:** Universidad de Buenos Aires, Facultad de Ingeniería
