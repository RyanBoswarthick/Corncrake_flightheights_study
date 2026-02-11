# 🛰️Crex crex Flight Height Analysis

## 📖 The project

This repository contains a specialized analytical pipeline designed to process and interpret the flight movement patterns of the Corncrake (*Crex* *crex*).

## 🤝 Co-authors
Ireland : J. Carey - NPWS
Scotland : R. Green, J. Wenston, A. Perkins - RSPB Scotland
France : F. Jiguet (CESCO), A. Besnard (CEFE), R. Boswarthick (LPO Anjou)
Estonia :J. Eels, R. Marja
*Czechia (J. Vlcek, L. Peske) and Belgium (G. Spanoghe, K. Janssens) to come*

## 🔬 Objectives

The analysis is structured around X main scientific objectives:

-   j

-   j

-   j

## Methodology 

## 🏔️ Ground elevation data

To ensure the highest accuracy in calculating Height Above Ground Level, the project compares two distinct Digital Elevation Model (DEM) sources. Flight altitude is normalized using:

$Altitude_{true alt} = Altitude_{GPS obs} - Elevation_{ground level}$

The analysis evaluates ground elevation data across two columns:

-   alti_elevatr_10: Extracted using the elevatr package at Zoom Level 10 (~150m resolution).

-   alti_DEM_EU: Extracted from the Copernicus European Digital Elevation Model (25m resolution) - https://ec.europa.eu/eurostat/web/gisco/geodata/digital-elevation-model/eu-dem

## 📂 Project structure

## 📝 Licence