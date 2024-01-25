Risk-Adjusted Regression
---

Data and replication code for Jung et al. (2023)
["Mitigating Included- and Omitted-Variable Bias in Estimates of Disparate Impact"](https://arxiv.org/abs/1809.05651).

For reproducibility, these replication materials use the
[`groundhog` package](https://groundhogr.com), which can be installed from CRAN:
```r
install.packages("groundhog")
set.groundhog.folder("/path/to/folder/")
```

To recreate all analyses in the paper, simply run
```bash
make all N_THREADS=$N_THREADS
```
where `$N_THREADS` is the number of threads you wish to use to run the code in
parallel. This code requires around 8 hours of wall-clock time to run with 32
Intel "Cascade Lake" CPUs. (This does not include `groundhog` installation
time the first time these scripts are run.)
