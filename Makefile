# Set the number of parallel processes / threads
N_THREADS ?= 4

.PHONY: all plots clean

all: plots

# Risk modeling
data/risk_%.rds: risk.R data/sqf.rds
	@echo "Calculating risk scores ($*)."
	@date
	time Rscript $< --n_workers=$(N_THREADS) --feat_set=$*

data/sens_%.csv: sens.R data/risk_%.rds
	@echo "Performing sensitivity analysis ($*)."
	@date
	time Rscript $< --n_workers $(N_THREADS) --feat_set=$*

data/sens_%_h.csv: sens.R data/risk_%.rds
	@echo "Performing sensitivity analysis ($*, h)."
	@date
	time Rscript $< --n_workers $(N_THREADS) --feat_set=$* --pct_nw=h

data/sens_%_m.csv: sens.R data/risk_%.rds
	@echo "Performing sensitivity analysis ($*, m)."
	@date
	time Rscript $< --n_workers $(N_THREADS) --feat_set=$* --pct_nw=m

data/sens_%_l.csv: sens.R data/risk_%.rds
	@echo "Performing sensitivity analysis ($*, l)."
	@date
	time Rscript $< --n_workers $(N_THREADS) --feat_set=$* --pct_nw=l

# Simulation
data/sim.rds: sim.R data/sqf.rds
	@echo "Simulating estimation of non-parametric estimand."
	@date
	time Rscript $< --n_workers=$(N_THREADS)

# Plot generation
plots: plots.R data/sens_base.csv data/sens_base_h.csv data/sens_base_m.csv \
	data/sens_base_l.csv data/sens_rb.csv data/sens_rb_h.csv data/sens_rb_m.csv \
	data/sens_rb_l.csv data/risk_base.rds data/risk_rb.rds data/risk_conf.rds \
	data/sim.rds
	@echo "Generating plots."
	@date
	@time Rscript plots.R

# Clean up
clean:
	rm -f data/split.rds data/{risk,sens}_* data/sim.rds plots/*.pdf
