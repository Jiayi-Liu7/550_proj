# NBA Data Analysis Project - Group 12

## Project Overview
This project analyzes NBA player statistics to uncover performance trends across positions, age groups, and teams. Key focus areas include scoring efficiency, defensive metrics, age-performance correlations, and team potential evaluation.

## Project Structure
```
nba-analysis/
├── README.md
├── Makefile
├── Data/
│   ├── nba_2025-03-07.csv        # Raw dataset (per 36-minute stats)
├── Config/
│   └── config.yml                # Environment parameters
├── Code/
│   ├── 01_data_clean.R           # Data cleaning & prep (Coder 1)
│   ├── 02_scoring.R              # Scoring analysis (Coder 2)
│   ├── 03_defense.R              # Defense analysis (Coder 3)
│   ├── 04_age.R                  # Age-performance study (Coder 4)
│   └── 05_team_potential.R       # Team potential evaluation (Coder 5)
├── Output/
│   ├── tables/                   # Analysis results
│   └── plots/                    # Visualizations
└── Report/
    └── final_nba_report.Rmd      # Final report source
```

## Run Full Pipeline
```bash
make
```

## Run Inividual Components
```bash
make scripts  # Run all analysis scripts
make report   # Generate final report (requires pre-processed data)
make clean    # Remove all generated files
```