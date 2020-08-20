Code
----

This repository contains 3 examples using various R features to analyze
text data. These examples are contained in the following .rmd files:

1.  [Twitter-Scraping-Example.Rmd](https://github.com/aaronmams/rHD-Vignette-Text-Mining/blob/master/Twitter-Scraping-Example.Rmd).
    This is a very simple example of using the
    [rtweet](https://github.com/ropensci/rtweet) package to harvest some
    data from Twitter's Public API.

2.  [Sentiment-Analysis-with-Tweets.Rmd](https://github.com/aaronmams/rHD-Vignette-Text-Mining/blob/master/Sentiment-Analysis-with-Tweets.Rmd).
    This is another relatively simple illustration of how to use the
    [tm](https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf)
    package to parse out text data and do some simple analysis (word
    count).

3.  [Text-Mining-with-Supreme-Court-Opinions.Rmd](https://github.com/aaronmams/rHD-Vignette-Text-Mining/blob/master/Text-Mining-with-Supreme-Court-Opinions.Rmd).
    This is slightly more involved example that has a few interesting
    features:

A. It uses methods from the
[pdftools](https://cran.r-project.org/web/packages/pdftools/pdftools.pdf)
package to read data into R from pdf files. B. It also uses some
elements of functional programming to parse out the text from these pdf
files and make it useable. C. It uses methods from the
[tidytext](https://www.tidytextmining.com/) package to tidy the text
data and do some analysis (again, word counts) with text from Supreme
Court Opinions.

Data
----

The data dependencies for these examples include 1 .csv file and 10 .pdf
files. These files are included in the ["data"" directory in this
project](https://github.com/aaronmams/rHD-Vignette-Text-Mining/tree/master/data).
Here is a brief description of these data files:

1.  `Sentiment.csv` is a .csv file that I obtained from
    [Kaggle](https://www.kaggle.com/crowdflower/first-gop-debate-twitter-sentiment?select=Sentiment.csv).
    These are tweets collected during a GOP Debate in Ohio for the 2016
    Presidential nomination).

2.  There are 10 .pdf files that have a common naming convention in that
    they all start with "slip-2019-19-...." These are Supreme Court
    "slip opinions". Each .pdf file corresponds to a unique case that
    was heard by the Supreme Court during the 2019 session. Per the
    [supremecourt.gov](https://www.supremecourt.gov/opinions/slipopinion/19)
    website:

> Slip opinions are the first version of the Court’s opinions posted on
> this website. A “slip” opinion consists of the majority or principal
> opinion, any concurring or dissenting opinions written by the
> Justices, and a prefatory syllabus prepared by the Reporter’s Office
> that summarizes the decision. The slip opinions collected here are
> those issued during October Term 2019 (October 07, 2019, through
> October 04, 2020).
