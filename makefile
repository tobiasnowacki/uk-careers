# Makefile for UK Careers Project
# Run the entire pipeline to produce all necessary results

R_OPTS=--vanilla
# Directories
TEXFILE = careers
HEADER = header
RDIR = ./code
FIGDIR = ./output/figures
TABDIR = ./output/tables
MOD = ./output/mod_data

# list R files
RFILES := $(wildcard $(RDIR)/*.R)
# pdf figures created by R
PDFFIGS := $(wildcard $(FIGDIR)/*.pdf)
# Indicator files to show R file has run
OUTFILES := $(wildcard *.Rout)

OUT_FILES:= $(RFILES:.R=.Rout)
# Indicator files to show pdfcrop has run
CROP_FILES:= $(PDFFIGS:.pdf=.pdfcrop)

all: $(OUT_FILES) 

# Run specific R files with dependencies

# DATA PREP -------
$(RDIR)/0_functions.Rout: $(RDIR)/0_functions.R
	R CMD BATCH $(R_OPTS) $< $@

# Prepares aggregate election data
$(RDIR)/prepare_data_elections.Rout: $(RDIR)/prepare_data_elections.R $(RDIR)/0_functions.Rout
	R CMD BATCH $(R_OPTS) $< $@

# Computes seat safety, Creates Figure 11
$(RDIR)/compute_chat.Rout: $(RDIR)/compute_chat.R $(RDIR)/prepare_data_elections.Rout
	R CMD BATCH $(R_OPTS) $< $@

# Prepares candidate data
$(RDIR)/prepare_data_candidates.Rout: $(RDIR)/prepare_data_candidates.R $(RDIR)/compute_chat.Rout 
	R CMD BATCH $(R_OPTS) $< $@	

# Merges in a few more variables 
$(RDIR)/prepare_data_government.Rout: $(RDIR)/prepare_data_government.R $(RDIR)/prepare_data_candidates.Rout 
	R CMD BATCH $(R_OPTS) $< $@	

# Prepares within-career data
$(RDIR)/prepare_within_career.Rout: $(RDIR)/prepare_within_career.R $(RDIR)/prepare_data_government.Rout 
	R CMD BATCH $(R_OPTS) $< $@

# Prepares office and whip data
$(RDIR)/prepare_office.Rout: $(RDIR)/prepare_office.R $(RDIR)/prepare_data_government.Rout
	R CMD BATCH $(R_OPTS) $< $@	

# Prepares stepping stone data
$(RDIR)/prepare_stepping_stones.Rout: $(RDIR)/prepare_stepping_stones.R $(RDIR)/prepare_office.Rout
	R CMD BATCH $(R_OPTS) $< $@	

# Prepares E-S whip scores
$(RDIR)/prepare_whip_es.Rout: $(RDIR)/prepare_whip_es.R 
	R CMD BATCH $(R_OPTS) $< $@

# Runs re-districting simulation
$(RDIR)/partisan_simulation.Rout: $(RDIR)/partisan_simulation.R
	R CMD BATCH $(R_OPTS) $< $@	

# CREATE FIGURES ------------

# Creates Figure 1
$(RDIR)/plot_aggregate_stats.Rout: $(RDIR)/plot_aggregate_stats.R $(RDIR)/compute_chat.Rout 
	R CMD BATCH $(R_OPTS) $< $@

# Creates Figure 2
$(RDIR)/analysis_nomvalue.Rout: $(RDIR)/analysis_nomvalue.R $(RDIR)/compute_chat.Rout 
	R CMD BATCH $(R_OPTS) $< $@

# Creates Figure 3
$(RDIR)/plot_safety.Rout: $(RDIR)/plot_safety.R $(RDIR)/compute_chat.Rout 
	R CMD BATCH $(R_OPTS) $< $@

# Creates Figure 4
$(RDIR)/plot_running_again.Rout: $(RDIR)/plot_running_again.R $(RDIR)/prepare_data_government.Rout
	R CMD BATCH $(R_OPTS) $< $@

# Creates Figure 5, Table 4, Table 5, Figure 15, 16, 17
$(RDIR)/analysis_rd.Rout: $(RDIR)/analysis_rd.R $(RDIR)/prepare_data_government.Rout 
	R CMD BATCH $(R_OPTS) $< $@	

# Creates Figure 6
$(RDIR)/analysis_candidates.Rout: $(RDIR)/analysis_candidates.R $(RDIR)/prepare_data_government.Rout
	R CMD BATCH $(R_OPTS) $< $@	

# Creates Figure 7
$(RDIR)/figure7.Rout:$(RDIR)/figure7.R $(RDIR)/prepare_within_career.Rout 
	R CMD BATCH $(R_OPTS) $< $@

# Creates Figure 8
$(RDIR)/plot_stepping_stones.Rout: $(RDIR)/plot_stepping_stones.R $(RDIR)/prepare_stepping_stones.Rout
	R CMD BATCH $(R_OPTS) $< $@	

# Creates Figure 9
$(RDIR)/plot_stepping_decadal.Rout: $(RDIR)/plot_stepping_decadal.R $(RDIR)/prepare_stepping_stones.Rout
	R CMD BATCH $(R_OPTS) $< $@	

# Creates Figure 10
$(RDIR)/cabinet_loyalty.Rout: $(RDIR)/cabinet_loyalty.R $(RDIR)/prepare_whip_es.Rout $(RDIR)/prepare_stepping_stones.Rout $(RDIR)/prepare_data_government.Rout
	R CMD BATCH $(R_OPTS) $< $@	

# Creates (Appendix) Figures 12, 13
$(RDIR)/plot_patronal.Rout: $(RDIR)/plot_patronal.R $(RDIR)/compute_chat.Rout 
	R CMD BATCH $(R_OPTS) $< $@

# Creates (Appendix) Figure 14
$(RDIR)/analysis_double_cand.Rout: $(RDIR)/analysis_double_cand.R $(RDIR)/prepare_data_candidates.Rout
	R CMD BATCH $(R_OPTS) $< $@	

# Creates (Appendix) Figure 18
$(RDIR)/plot_within_career_cabinet.Rout: $(RDIR)/plot_within_career_cabinet.R $(RDIR)/prepare_within_career.Rout $(RDIR)/prepare_stepping_stones.Rout
	R CMD BATCH $(R_OPTS) $< $@

# Generate dataset for analysis check
$(RDIR)/analysis_names_check.Rout: $(RDIR)/analysis_names_check.R $(RDIR)/prepare_data_government.Rout
	R CMD BATCH $(R_OPTS) $< $@

# CREATE TABLES ---------

# Creates Table 1
$(RDIR)/analysis_candidates_did.Rout: $(RDIR)/analysis_candidates_did.R $(RDIR)/prepare_data_government.Rout  
	R CMD BATCH $(R_OPTS) $< $@	

# Create Table 2
$(RDIR)/descriptives.Rout: $(RDIR)/descriptives.R $(RDIR)/prepare_data_government.Rout
	R CMD BATCH $(R_OPTS) $< $@	

# Creates Table 3
$(RDIR)/table_within_career_reg.Rout: $(RDIR)/table_within_career_reg.R $(RDIR)/prepare_within_career.Rout 
	R CMD BATCH $(R_OPTS) $< $@

# Run R files
R: $(OUT_FILES)

# Make .tex file
# ./draft/$(TEXFILE).pdf: ./draft/$(TEXFILE).tex $(OUT_FILES) 
# 		cd draft; \
# 		pdflatex $(TEXFILE) \
# 		bibtex $(TEXFILE) \
# 		pdflatex $(TEXFILE) \
# 		pdflatex $(TEXFILE) 

# Run to remove all .Rout logs
clean_rout:
	rm -fv *.Rout

# Run to remove all output generated by running the scripts
clean_output:
	rm -fv *.Rout
	rm -fv output/figures/*.pdf
	rm -fv output/tables/*.tex
	rm -fv output/mod_data/*.csv


.PHONY: all clean
