1. Change the plot method so that one does not need to specify the predictor and response. This may not be possible as the second parameter is always the y (i.e. response) variable
2. add a warning or something if the user tries to group without setting the lines parameter to FALSE
3. Add a lattice plot option for grouping in PlotTriangle.
4. Does modeling each lag on its own permit us to assume different variance terms?
5. Sort out the names of the model factors for a model fit
6. Mixed method
7. Tail factor1 - aggregate grouping variable
8. Add weight exponent to regression
9. Tail factor2 - function of model factors
10. 4x4plots
11. Add a development interval slot to the Triangle object
12. Add fixed and stochastic measure slots
13. Zero out stochastic columns in the projection
14. TAIL FACTOR
15. Add fit group to triangle
16. Reconsider use of the name "FitCategory" in the call signature to TriangleModel
17. S4 with list of data frames for Triangle, TriangleModel objects
18. Plotting functions to compare model F stats
19. Change dispatch method so that LatestDiagonal may accept either Triangle or data frame as an input
20. Add labels to the model factor plots
21. Triangle pivot function
22. All of the noise from lubridate functions