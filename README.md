
# Code for modellistribution of Heracleum Sosnowskyi
> Large scale forecasting the potential distribution of Heracleum Sosnowskyi on the territory of Russia under climate change.

[![License](https://img.shields.io/github/license/EDSEL-skoltech/multi_objective_irrigation)](https://github.com/Disha0903/herscleum_sosnowskyi/blob/main/LICENSE)

[![Rg](https://img.shields.io/badge/ResearchGate-Follow-green)](https://www.researchgate.net/project/Digital-Agro)

__Source code for paper [PDF](link)__


We propose a machine learning approach based on the Random Forest model for forecasting the potential distribution of Heracleum Sosnowskyi. This  research  aims  to  establish  the  possible  habitat suitability of HS in current and future climate conditions across the territory of European part of Russia.

![graphical](plots_ICCS/Graphical_abstract.png)



## Installation

Clone this repository

`git clone https://github.com/Disha0903/herscleum_sosnowskyi.git`

Install R packages

* biomod
* spThin

## Demo data 

### Climatic variables

Climatic variables were collected from the Worldclim database

### Soil data

Soil data were downloaded from the SoilGrids database


## Meta

Diana Koldasbayeva – Diana.Koldasbayeva@skoltech.ru

## License

Distributed under the MIT license. See ``LICENSE`` for more information.


## TO-DO list

- [X] Weather loader from ERA5 and worldclim
- [ ] Remove лл.R file or rename
- [ ] Function to save optimal irrigation dates and volumes to txt file


## Contributing

1. Fork it (<https://github.com/EDSEL-skoltech/multi_objective_irrigation/fork>)
2. Create your feature branch (`git checkout -b feature/fooBar`)
3. Commit your changes (`git commit -am 'Add some fooBar'`)
4. Push to the branch (`git push origin feature/fooBar`)
5. Create a new Pull Request


