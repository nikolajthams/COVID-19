<br>
<br>
**About this analysis:** In this analysis, we estimate the number of actual infected individuals, as opposed to reported number of infected, which may be orders of magnitude lower. 

While there is substantial shadow figures for infected, the death figures are more reliable. The number of deaths today indicate gives an estimate of the number of infected individuals 2-3 weeks in the past. Fundamentally, we model the number of infected indivduals by using the South Korean death rate, which is believed to be more accurate than many other, due to the large amounts of tests performed in South Korea.

Further our approach incorporate varying demographics across countries and varying mortality across age groups. Technical details of our approach can be found in this [white paper](../wirvsvirus/draft/concept_online_version.pdf) (which is still work in progress). 

**Hackathon** This methodology was developed as part of the [\#WirVsVirus hackathon](https://wirvsvirushackathon.org) (April 20th-22nd, 2020). See also the [devpost page](https://devpost.com/software/038_daten_infektionszahlenschatzen) related to our solution and this [2 minute pitch](https://www.youtube.com/watch?v=ug6u5wXXD4M&feature=emb_title) describing our solution (both in German).

**Source:** [Johns Hopkins University](https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6) (infection data) and [UN data](http://data.un.org/Default.aspx) (demographic data). 