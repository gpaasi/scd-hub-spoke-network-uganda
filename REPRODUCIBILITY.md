# Reproducibility

## Scope

This repository supports reproducibility of the manuscript results narrative from processed analysis outputs. It does not reconstruct the full upstream geocoding, facility harmonisation, or spatial modelling pipeline from raw data.

## Steps

1. Install Python dependencies from `requirements.txt`.
2. Run `python scripts/validate_results.py`.
3. Compare the generated validation report with the manuscript results narrative.
4. Use `data/metadata/results_crosswalk.csv` to identify which data files support each manuscript results section.

## Important interpretation limits

- Distances are Euclidean planning distances.
- Assigned routes are modelled potential linkages.
- Hub readiness is based on binary measured service domains.
- Workload is linked-spoke count, not patient volume.
- Strengthening tiers are programme review categories, not final investment decisions.
