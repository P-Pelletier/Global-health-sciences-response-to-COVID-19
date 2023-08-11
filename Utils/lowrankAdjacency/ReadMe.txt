Hi Kevin,

opt_fcts.R provides the decomposition algorithm of Bao, Michailidis, 2018, Core community structure recovery and phase transtion detection...

sim_fcts.R creates networks in simulations (the inverse of the decomposition algorithm) to check whether things work 

simulateOptimize.R - uses sim_fcts.R to simulate networks and then opt_fcts.R to decompose them again.

decompCityAdj.R - prepares the network data for the decomposition, and applies then the decomposition of opt_fcts.R
				  if the input changes you may need to adapt.

Please check again the code - there may be always sth. wrong I didn't see before. 


greetings
Moritz

