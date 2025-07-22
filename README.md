# Lagoon-ES-Model
<!-- ![VLES-salicornia](https://github.com/user-attachments/assets/04a0d529-3a88-49ab-ad03-f3266b77dabf)-->

<img src="https://github.com/user-attachments/assets/04a0d529-3a88-49ab-ad03-f3266b77dabf" alt="VLES-salicornia" width=70% />

![RStudio](https://img.shields.io/badge/RStudio-4285F4?style=for-the-badge&logo=rstudio&logoColor=white)

# VLES - Venice Lagoon's Ecosystem Services
A socio-ecological modelling tool for the ecosystem services (ES) of the [Venice Lagoon](https://en.wikipedia.org/wiki/Venetian_Lagoon).

- [What](#what)
- [Why and Who](#why-and-who)
- [Getting started](#getting-started) 

<img src="https://github.com/user-attachments/assets/5cebd935-1a0b-486f-aa6c-0ae59927b171" alt="Italy-Venice_Lagoon" width=35% />

## What

Here we present VLES, a modelling tool written in R that uses a system of differential equations solved with the ode solver [deSolve](https://cran.r-project.org/web/packages/deSolve/index.html).

The model represents a simplified version of the Socio-ecological system framework that describes the interactions of fauna,
habitats, Actors and governance of the Venice Lagoon from which the Ecosystem Services emerge.

The differential equations of the state variables of the system are defined using a systemic gain-loss approach.
The equations for the ecosystem services on the other are more varied depending on the type of service.

The code requires different additional data to run imported from separate CSV files, that are stored in the folder "data":
* initial conditions
* parameters
* time-series
* forcings
* reference values

The output is printed as an XLSX file. The model can be used to compute different future scenarios,
for example by changing the climate scenario-based forcings.
### License
 The model is licensed under the Academic Free License version 3.0.
 The authors for the attribution are: Brigolin, D., Rampoldi, S., & Rova, S..
 
### Packages used
1. [deSolve](https://cran.r-project.org/web/packages/deSolve/index.html)
2. [ODEsensitivity](https://cran.r-project.org/web/packages/ODEsensitivity/index.html)


## Why and Who
This model is part of a university project developed between the _University Ca' Foscari of Venice_ and the _University IUAV of
Venice_ to assess the ecological sustainability of the ecosystem services of the Venice Lagoon.

The code, in particular, was developed by the contribution of _Daniele Brigolin_<sup>1</sup>, _Stian Rampoldi_<sup>1</sup>, and _Silvia Rova_<sup>2,3</sup>, as part of their research work.

The goal is to propose a possible example of a modelling tool for the dynamic quantification of multiple ecosystem services that can be 
replicated and adapted for other anthropized coastal lagoon systems, and test it for the case study of the Venice Lagoon.

1=_University IUAV of Venice_,
2=_UNIVE - University of Ca' Foscari of Venice_,
3=_CSRCC - International Centre for Climate Change Research and Studies_

## Getting Started
1. Clone the repository
2. Run the code "VLES_model" and you will obtain the outputs of the model:
   * the numerical solution of the ODE solver: yearly values for all variables
3. Run the code "VLES_sensitivity"and you will obtain the outputs of the sensitivity test:
   * the result matrix of the Morris sensitivity test: yearly avereges of `mu*` and `sigma`

## Architecture

The code is divided into two files`VLES_model` and `VLES_sensitivity`, each has two sections.
1. VLES_model:
  * Preparation for the ODEsolver:
  in this section, the data are uploaded from the CSV files contained in the folder "data"
  (`initialConditions.csv`, `forcings.csv`, `parameters.csv`, `references.csv`, `timeseries.csv`)
  * Run the ODEsolver:
  the results are saved as a data frame and printed on the VLESout.xlsx file (they can alternative be plotted by uncommenting the plot section).
 <img src="https://github.com/user-attachments/assets/d53d273e-ffae-492a-8a76-a4132104c63d" alt="OUTPUT-ode-SI" width=30%]>

2. VLES_sensitivity:
  * Preparation for the Morris test:
  in this section, the uncertainty interval is fixed for each parameter 
  * Run the Morris test:
   (⚠️ Be careful! This part takes long to run since it runs millions of times the model.)
   
<img src="https://github.com/user-attachments/assets/79714e88-f1a5-4c97-8616-2d7719d5f9f3" alt="OUTPUT-morris-SG" width=30%]>

Here we present a graphical overview of the model in the form of a stock-and-flow diagram.
<img src="https://github.com/user-attachments/assets/26c65d4e-b58a-490f-b0e4-ac129135043d" alt="model-overview" width=95%]>

### ECOSYSTEM SERVICES

The model focuses on twelve ecosystem services (ES) among those present in the Venice Lagoon, four for each category.

These emerge from the interactions of the system components, that are:
* **Regulating ES**:
  - lifecycle maintenance (LCM) by the habitats’ nursery function
  - water purification of the seawater from nitrogen loads by vegetated habitats
  - climate regulation by ecosystemic processes of carbon sequestration
  - prevention of erosion of the lagoon’s habitats
* **Provisioning ES**:
  - clam harvesting
  - artisanal fishing
  - recreational fishing
  - bird hunting
* **Cultural ES**:
  - traditional rowing (Voga in Italian)
  - cognitive development through environmental education activities
  - tourism in the lagoon
  - recreational navigation
<img src="https://github.com/user-attachments/assets/a5671e26-98b5-4209-b465-38d354f09fa3" alt="ES-overview" width=70%]>

### STATE VARIABLES

The model has a set of 11 state variables of which five are morphologic and six are fauna stocks. 
These variables have a differential equation that describes the dynamic behaviour of the variable in time.

* **The fauna stocks are**:
  - `TA` clams
  - `BI` birds
  - `SB` sea-bream
  - `LA` sea-bass
  - `DE` demersal
  - `MU` mugilidae
  
 <img src="https://github.com/user-attachments/assets/6b3c3221-528d-4cb9-9e54-b03d7f116aa1" alt="FAUNA-overview" width=50%]>

* **The morphologic stocks are**:
  - `SM` salt marshes
  - `SG` sea grasses
  - `BD` benthic diatomes
  - `BB` bare-bottom
  - `NC` navigable canals
  
<img src="https://github.com/user-attachments/assets/6a0d2d1a-8520-486e-9222-8e280572e068" alt="MORPHOLOGY-overview" width=60%]>

* There are other elements of the morphology which are obtained from the state variables:
  - shallow-intertidal `SI=SM+SG+BD+BB`
  - deep-subtidal `DS=409-SI`
  - non-navigable canals `NNC=49-NC`
  - creeks `CR=sm_geom*SM`
    
 <img src="https://github.com/user-attachments/assets/36d8b122-ca5c-4b0f-8953-dfe9efbffc46" alt="CONSTRAINTS-overview" width=50%]>

### FORCINGS

1. There are four demographic forcings:
* residents of the lagoon
* residents in the surrounding mainland
* higher-education students
* Venice tourists

These groups are translated into eight stocks of Actors involved in the ecosystem services:
* clam fishers
* artisanal fishers
* recreational fishers
* bird hunters
* boaters
* voga rowers
* environmental education students
* tourists

2. There are two climatic forcings:
* sea-level rise
* seawater warming.

These can be set in two different climate change scenarios namely:
* medium (`rcp2.6` and `rcp4.5`)
* extreme (`rcp8.5`)

3. There is one extra environmental forcing which is the input of new fauna from outside the system. Birds are always free to access the lagoon, this simulates the open system that lagoon habitats represent for migratory species. The access of fish on the other hand is mediated by the closure of the MOSE system that regulates the inlets. There is no input of new clams, considering that this population concerns predominantly aquaculture and not wild populations.
<img src="https://github.com/user-attachments/assets/9e55e87e-90b2-49eb-ae9e-8aa1135382d5" alt="FORCINGS-overview" width=60%]>


### GOVERNANCE MANAGEMENT

The management is included in the model in different ways:
* restoration rates of the habitats
* regulation of provisioning services
* regulation of the activation of the [MOSE](https://en.wikipedia.org/wiki/MOSE) (movable water barrier system)
* regulation of canals excavation
In the model are included for example the impact of the segrasses meadows done within the project [Seresto](https://www.isprambiente.gov.it/it/progetti/cartella-progetti-in-corso/acque-interne-e-marino-costiere-1/progetti-conclusi/life-seresto).

### TIMEFRAME

The model runs on yearly timesteps and runs three temporal phases:
1. `1980-1999` It starts in 1980 and has a 20-year timeframe used for spin-up to reach the initial stability of the system in 1999.
2. `2000-2019` Validation period use to compare the model dynamics to the historical data, and runs for 20 Years from 2000 to 2019.
3. `2020-2080`Future projections used to explore possible future trends based on the management and the forcings implemented and runs from 2020 to 2080.
   
<img src="https://github.com/user-attachments/assets/17d54371-1986-416d-ad95-898e122ebda2" alt="PHASES-overview" width=60%]>

## Future Development
We invite everyone to contribute to the development of this code and to adapt it to different coastal lagoon systems, for developing ES assessments and promoting coastal lagoons' environmental protection.

