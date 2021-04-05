

# Effect of School Policies on COVID-19 Transmission

- Author: Cheyenne Ehman, Yixuan Luo, Zi Yang, Ziyan Zhu
- Advisor: Valerie Ventura
- Collaborator: PHIGHT COVID Team

## Introduction

### What's the motivation behind exploring how school policies affects COVID transmission?
- We observed that percent of young people getting infected has peaked around the start of 2020 Fall Semester.
- We're close to the 2021 Fall, should we reopen or not? 
- What should we do next time if another pandenmic hits?

![Why schools](https://github.com/alexazhu/36726-PIGHT-COVID/blob/cdae234f25759a6ec7d711e9b3a9881be81d02b5/Presentation%20Slides/2.png)


## DATA OVERVIEW

### Data Sources
- COVID19 Cases & Deaths: [John Hopkins](https://coronavirus.jhu.edu/covid-19-daily-video) Open Source Data API
- K12 school policies correspond to COVID: [MCH.com](https://www.mchdata.com/covid19/schoolclosings)
- Mobile Mobility:  [SafeGraph.com](https://www.safegraph.com/) via [CMU DELPHI Group](https://delphi.cmu.edu/covidcast/) 

### Time Range: 01/22/2020 - 02/22/2021

### About Ohio State:
- 86 counties (2 dropped due to missing data)
- 11,755,535 Population 
- 1,615,134 student enrolled in K12 schools (13.7% of population)
- 2,871 schools

![Data Relation](https://github.com/alexazhu/36726-PIGHT-COVID/blob/1f7f1b266ffb68fe20fe465aa7d576b5bb345a20/Presentation%20Slides/1.png)

### Why We Choose Ohio State for Analysis?

![Why ohio](https://github.com/alexazhu/36726-PIGHT-COVID/blob/1f7f1b266ffb68fe20fe465aa7d576b5bb345a20/Presentation%20Slides/3.png)

## Results

### Significant Difference in Averaged Death Proportions among counties


![teaching difference](https://github.com/alexazhu/36726-PIGHT-COVID/blob/6e9f5160b74d04ed5b754fec0ac880269c1fa10a/Presentation%20Slides/4.png)


### Confounding Effects

![confounding effects](https://github.com/alexazhu/36726-PIGHT-COVID/blob/71630ddd0e8f76383aa62989c290da94c13efceb/Presentation%20Slides/5.png)


![y0](https://github.com/alexazhu/36726-PIGHT-COVID/blob/6e9f5160b74d04ed5b754fec0ac880269c1fa10a/Presentation%20Slides/6.png)

### Alternative Explanation for Difference in Deaths

When Fall starts, parents are able to go to work if their children leave home and go to school.

![full time work](https://github.com/alexazhu/36726-PIGHT-COVID/blob/526ff92c0082da9a67cc09b1bf93c4ffdddd66e6/Presentation%20Slides/7.png)


## Recap and Next Steps

- Death proportions averaged within On-Premises, Hybrid and Online Only counties remain significantly different after adjusting for confounder Y0


- We need to identify and adjust for other possible confounders as well as for  variables that could also explain the differences in death tolls between On-Premises, Hybrid and Online Only counties.
   E.g.: Mobility
  - People going to work
  - People going to restaurants



