#### rAutoFE
- title : Auto Feature Engineering in R
- author : 2e Consulting DS Lab <jacobgreen1984@2e.co.kr>

#### feature engineering process 
baseline model -> variable importance -> (lagging/descriptiveStatistics) -> numeric tansform -> numeric  binning -> WoE -> categorical encoding(frequency, target) -> interaction features -> error analysis

#### install 
```r
library(devtools)
install_github("jacobgreen1984/rAutoFE")
library(rAutoFE)
```

#### usage
```r
?rAutoFE::frequencyEncoding_fit()
```
