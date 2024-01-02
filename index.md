# Covmuller

Covmuller is an R-package designed for analysis of sequencing metadata deposited on [GISAID](https://www.gisaid.org/). 


![](https://saket-choudhary.me/covmuller/articles/NYC_animated_2023.gif)


## Features

Covmuller currently supports the following features:

* Read and preprocess metadata file downloaded from GISAID: [Vignette](articles/Introduction.html)
* Plot variant prevalence: [Vignette](articles/Introduction.html)
* Fit multinomial models to variant prevalence data:
    - [India](articles/MultinomialModeling_India.html)
    - [USA](articles/MultinomialModeling_USA.html)
    - [Canada](articles/MultinomialModeling_Canada.html)
    - [UK](articles/MultinomialModeling_UK.html)
    - [Australia](articles/MultinomialModeling_Australia.html)
    - [SouthAfrica](articles/MultinomialModeling_SouthAfrica.html)
* Estimate totals prevalence of variants by projecting case prevalence data on multinomial fits:  
    - Countries/States:
        - [India](articles/VariantAnimation-India.html)
        - [New York state](articles/VariantAnimation-NewYork.html)
        - [South Africa](articles/VariantAnimation-SouthAfrica.html)
        - [United Kingdom](articles/VariantAnimation-UK.html)
        - [USA](articles/VariantAnimation-USA.html)
        
    -  Cities:
         - [New York City (USA)](articles/VariantAnimation-NYC.html)
         - [Delhi (India)](articles/VariantAnimation-Delhi.html)
         - [Mumbai (India)](articles/VariantAnimation-Mumbai.html)
         - [Maharashtra (India)](articles/VariantAnimation-Maharashtra.html)
         - [Pune (India)](articles/VariantAnimation-Pune.html)
