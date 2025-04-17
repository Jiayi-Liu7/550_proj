# Makefile

# R Scripts
SCRIPTS := \
  Code/01_data_clean.R \
  Code/02_scoring.R \
  Code/03_defense.R \
  Code/04_age.R \
  Code/05_team_potential.R

# Report files
RMD  := Report/final_nba_report.Rmd
HTML := Report/final_nba_report.html

.PHONY: all scripts report clean

all: scripts report

scripts:
	@echo "» Running analysis scripts…"
	@for script in $(SCRIPTS); do \
		Rscript $$script; \
	done

report: $(HTML)

$(HTML): $(RMD) scripts
	@echo "» Rendering RMarkdown → HTML…"
	@mkdir -p Report
	@Rscript -e "rmarkdown::render( \
		input = '$(RMD)', \
		output_file = '$(notdir $(HTML))', \
		output_dir = '$(dir $(HTML))' \
	)"

clean:
	@echo "» Cleaning output…"
	@rm -rf Output/*
	@rm -f $(HTML)
	@rm -rf Report/*_cache Report/*_files