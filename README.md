# Physiological Measurement Analyser

A sample of first year male statistics students at **University of Glasgow** had the grip strength of their dominant hand measured using a grip dynamometer. In addition, a number of physiological measurements were made on their dominant hand and arm; width and length (cm) of the hand, circumference (cm) of forearm and bicep, skinfold thickness (mm) of forearm and bicep. The height (cm) and weight (kg) of the students was also recorded.
The data are stored in the Stata data file male-grip-strength.dta and contains the following variables:

|     Explanatory Variable      |  Unit  |
| ----------------------------- | ------ | 
|      Hwid - hand width        |   cm   |
|      Hlen - hand length       |   cm   |
| Fcirc - forearm circumference |   cm   |
|  Bcirc - bicep circumference  |   cm   |
|    Fskin - forearm skinfold   |   mm   |
|     Bskin - bicep skinfold    |   mm   |
|      Grip - grip strength     |   kg   |
|         Weight - weight       |   kg   |
|         Height - height       |   cm   |


## Results
The weight and height variables are utilised to create 4 categories of Body Mass Index (BMI) as follows:

(i) underweight <18.499

(ii) Normal 18.5-24.999

(iii) overweight 25-29.999

(iv) obese >=30

A parsimonous model is then identified using appropriate statistical methods for predicting grip strength.
