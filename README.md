# VoC
Code to reproduce Elmore and Strauss's 2025+ Paper "Is Complexity Virtuous?"

## Notes:
We modified Kelly et al.'s Virtue of Complexity code to keep Y in its original units. Specifically, 
1. /Code/EmpiricalAnalysis/Step1_Predictions contains modified functions:
   
    a. GW_benchmark_functions.m
    b. tryrff_v2_function_for_each_sim.m
   
2. /Code/ProduceExhibits.m is heavily commented due to our post-processing the data in R.
3. Kelly et al.'s original functions are located in /JoFCode/
