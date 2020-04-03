<br>
<br>
**Contributors:** <br>
[Rune Christiansen](mailto:krunechristiansen@math.ku.dk): Ph.D. student in statistics, University of Copenhagen.<br>
[Phillip Mogensen](mailto:pbm@math.ku.dk): Ph.D. student in statistics, University of Copenhagen.<br>
[Jonas Peters](mailto:jonas.peters@math.ku.dk): Professor of statistics, University of Copenhagen.<br>
[Niklas Pfister](mailto:np@math.ku.dk): Assistant professor of statistics, University of Copenhagen.<br>
[Nikolaj Thams](mailto:thams@math.ku.dk): : Ph.D. student in statistics, University of Copenhagen.<br>

**About this analysis:** In this analysis, we try to estimate the cumulative number of actual infected individuals above 30, as opposed to reported number of infected, which may be orders of magnitude lower. 

While there are substantial shadow figures for infected, the death figures are more reliable. The number of deaths today gives an indication of the number of infected individuals 2-3 weeks in the past. Fundamentally, we model the number of infected individuals by using data about the South Korean death rate, which is believed to be more accurate than many other, due to the large amounts of tests performed in South Korea. We stress that we only estimate infected above 30, because since very few people below 30 die, it is impossible to estimate the infected from the deaths alone. 

Our approach incorporate varying demographics across countries and varying mortality across age groups. Technical details of our approach can be found in this [white paper](https://github.com/nikolajthams/COVID-19/blob/master/wirvsvirus/paper/concept_online_version.pdf) (which is still work in progress). 

**Assumptions:** To get these estimates, we assume that the true death rates in each age group is the same for all countries. Of course, this is not the case; across countries, people have different genetic compositions, there are different practices regarding treatment, differences in health-care quality and so forth. In other words, we know that this assumption is violated. However, because we do not have any data from a randomized study, it is next to impossible to good estimates of the death rates for every single country. Because South Korea has tested extensively, it is likely that the death rates we see there are the closest to what we could hope to obtain from a randomized study. For deaths rates that are actually higher than those observed in South Korea, the estimated numbers above will be too high and vice versa.

**Hackathon** This methodology was developed as part of the [\#WirVsVirus hackathon](https://wirvsvirushackathon.org) (March 20th-22nd, 2020). See also the [devpost page](https://devpost.com/software/038_daten_infektionszahlenschatzen) related to our solution and this [2 minute pitch](https://www.youtube.com/watch?v=ug6u5wXXD4M&feature=emb_title) describing our solution (both in German).

**Data source:** [Johns Hopkins University](https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6) (infection data) and [UN data](http://data.un.org/Default.aspx) (demographic data). 

