# dsra

**Dynamic Site Response Analysis**

> **Last updated:** November 13, 2025

R package for generating synthetic soil profiles and computing fundamental periods of slopes and embankments using inhomogeneous truncated shear beam theory.

[![R Version](https://img.shields.io/badge/R-%3E%3D%203.5-blue)](https://www.r-project.org/)
[![Version](https://img.shields.io/badge/version-0.3.0-green)](https://github.com/averriK/dsra)

## What is it?

dsra generates synthetic soil profiles with random geotechnical properties based on USCS classification and computes fundamental periods (Ts) and shear modulus variation parameters (mo) for seismic analysis of slopes and embankments. Implements Ishihara's shear modulus model and Gazetas & Dakoulas' inhomogeneous truncated shear beam theory.

## Features

- **Synthetic profile generation**: Random sampling of void ratio, unit weight, plasticity based on USCS
- **Ishihara shear modulus model**: G(z,e) from void ratio and octahedral stress
- **Fundamental period calculation**: Ts via Rayleigh method for Vs profiles
- **Inhomogeneity characterization**: mo parameter for power-law shear modulus variation G ~ z^mo
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

# Generate single synthetic profile
profile <- geSiteTable(
  Hs = 30,                      # Embankment height (m)
  USCS = c("SM", "ML", "CL"),   # Soil types
  h = 0.50,                     # Layer thickness (m)
  Water = 0.2,                  # Water table at 20% of Hs from surface
  POP = 100,                    # Pre-consolidation pressure (kPa)
  Vref = 760                    # Bedrock Vs (m/s)
)

# Generate multiple realizations with quantiles
site_props <- getSiteProperties(
  Hs = 30,
  USCS = c("SM", "ML", "CL"),
  h = 1.00,
  NR = 100,                     # Number of realizations
  levels = c(0.16, "mean", 0.84),
  Vref = 760
)

# Compute characteristic roots for modal analysis
# mo: inhomogeneity ratio (0 = homogeneous)
# lo: truncation ratio (berm influence)
# no: mode number (1 = fundamental)
an <- getCylinderRoots(mo = 0.5, lo = 0.3, no = 1, model = "nlm")

# Calculate fundamental period from eigenvalue
Ts <- (4 * pi * profile$Hs[1]) / (an * (2 - profile$mo[1]) * profile$VSo[1])

# Site classification utilities
Vs30toSID(760)  # "BC"
SIDtoVs30("C")  # 540
```

## Application

dsra is used to estimate fundamental periods (Ts) and inhomogeneity ratios (mo) for:

- **Seismic slope stability**: Ts is input for flexible-block Newmark displacement models (Bray & Travasarou 2007, Bray & Macedo 2017/2019)
- **Tailings storage facilities**: TSF embankment dynamic characterization
- **Waste rock dumps**: WRD seismic response parameters
- **Site response analysis**: Vs30 and basin depth (Z500, Z1000) estimation

Complete workflows available in:
- `~/github/psha/R/runTs.R` - Period calculation for multiple geometries/materials
- `~/github/psha/_chapters/slope.qmd` - Theory and methodology
- `~/github/psha/_appendix/slope_appendix.qmd` - Ishihara parameters and references

## Theory

### Fundamental Period (Gazetas & Dakoulas 1985)

$$T_s^{(j)} = \frac{4\pi H_{max}}{a^{(j)}(2-m_o)v_S^o}$$

where $a^{(j)}$ is the j-th characteristic root (eigenvalue) depending on inhomogeneity ratio $m_o$ and truncation ratio $\lambda_o$.

### Shear Modulus Model (Ishihara 1997)

$$G(z,e_o) = A \frac{(C_e-e_o)^2}{1+e_o} \left(\frac{\sigma'_o(z)}{p_{ref}}\right)^n$$

where A, Ce, n are material constants tabulated for gravels, sands, and fines.

## Dependencies

- R (>= 3.5)
- data.table, stats, utils
- randomForest, rpart (for root interpolation)
- stringr, digest, triangle

## References

Gazetas, G., & Dakoulas, P. (1985). Seismic analysis and design of rockfill dams: State-of-the-art. *Soil Dynamics and Earthquake Engineering*, 4(1), 1-14.

Ishihara, K. (1997). *Soil Behaviour in Earthquake Geotechnics*. Oxford University Press.

## License

Custom license - see [LICENSE](LICENSE)

## Citation

```bibtex
@software{dsra2024,
  author = {Verri Kozlowski, Alejandro},
  title = {dsra: Dynamic Site Response Analysis},
  year = {2024},
  version = {0.3.0},
  url = {https://github.com/averriK/dsra}
}
```

---

**Author:** Alejandro Verri Kozlowski  
**Email:** averri@fi.uba.ar  
**ORCID:** [0000-0002-8535-1170](https://orcid.org/0000-0002-8535-1170)
