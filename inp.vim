syntax clear

syntax match AlfComm /!.*/ 
highlight link AlfComm Comment 

syntax keyword Input Number Technology Power DegradationRate FuelCost FuelLHV Investment Lifetime OnOffCost OeMCost SetPoint Size ElettrEfficiency ThermalEfficiency ChillingEfficiency GridConnection Degradation Efficiency MinUpTime MinDownTime StartPoint UpTime DownTime FirstTimeStep Algorithm writePower writeEnergy writeEfficiency writeElectricRev writeThermalRev writeChillingRev writeDemand writeInput writeCosts writeTrig writeChiller writeBoiler objective Climate Altitude PresCorrection TempCorrection AltCorrection
highlight link Input Include
                   
syntax keyword beginend end begin
highlight link beginend Error

syntax match separator /|/
highlight link separator Statement

syntax match parentesi /(/
syntax match parentesi /)/
highlight link parentesi Number


