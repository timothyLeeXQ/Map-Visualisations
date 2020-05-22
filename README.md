# Fun with Maps

I wanted to learn to visualise maps with R, and so I did.

## About the Repo
This repo contains 3 activities that I used to learn to visualise
static and interactive maps in R.

The first two activities are
provided as activities and assignments from the QMSS GR5063:
Data Visualisation class that I audited. For these,
* Instructions have been left intact
* Some set-up code and data was also provided as is by the
course instructor. These are mostly at the top of the documents
and instructions mostly make clear what is not my own work.
* All code directly contributing to the map visualisations can
be considered my own work.

## Projects

### Global Immunisations and State Corporate Tax rates in the USA

This activity was a "try-it-out for practice" activity as part of
the data visualisation class. It helped me gain experience with
working with map data of countries and US states in R, notably
dealing with differences in country naming and abbreviation.
Maps are static and plotted mostly with base R tools and `ggplot2`.

### NYC Fires

This activity was to be submitted as an assignment for the class,
but since I was not officially taking it, I only decided to try
it out now. This project helped me gain experience with the
Leaflet package in R for data visualisations, and to access and use
shape file data from the NYC Open Data Protal (notably [this file
on NYC Borough Boundaries](https://data.cityofnewyork.us/City-Government/Borough-Boundaries/tqmj-j8zm).

Plots are made using R mapping packages like the `Leaflet` R package and
ggmap. Supporting libraries like `rgdal` (for shape files) are also used.

### Mata Cams - Singapore Road Cameras

As a Singaporean, I wanted to do a map project on my own country.
This project maps the different types of road cameras in Singapore -
Speeding Cameras, Red Light Cameras, and Road Cameras (enforcing
illegal parking).

The objective of this project is to

* Learn to access and make use of data available on
[data.gov.sg/](data.gov.sg/).
* Gain further practice with the `Leaflet` package in R.
* Develop a publishable map visualisation of personal and public interest.
