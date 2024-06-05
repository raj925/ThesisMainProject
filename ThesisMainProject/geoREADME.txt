Data Dictionary for Geography Dataset:

participantID	- anonymised ID for participant
trialNum - which country (out of 6) this is for the participant.
stage	- which stage of information the participant is at (out of 3)
stageName - Stage 1: Geography, Stage 2: History & Politics, Stage 3: People & Culture
trueCountry - Objective correct answer/country	
requestedTests - how many pieces of information participant clicked on during that stage	
uniqueTests - how many unique pieces of information (i.e. excluding repeat clicks on same info)
pastTests - how many pieces of information from previous stages clicked on (e.g. during History & Politics stage, how many pieces of information were clicked on from Geography stage).	
currentTests - how many pieces of information from the stage that participant is currently on.	
possibleTest - total number of available (unique) pieces of information that participant could have clicked on.	
proportionOfInfo	- currentTests divided by possibleTest.
testNames - which info participant clicked on.	
confidence - confidence reported at this stage (0-100).	
perceivedDifficulty - difficulty reported at the end of all three stages (0-10), so value is the same across all three stages of a single trial.	
highestLikelihood - highest likelihood reported for a single country. 	
likelihoods - list of likelihoods, where each corresponds to 'differentials' field (I.e. first likelihood value correspond to first country in the differentials field)	
numOfDifferentials - how many countries were reported.	
correctCountry - whether the list of countries has a correct country (marked manually).	
likelihoodOfCorrectCountry - for a correct country in the list, what likelihood was assigned to that country (0 if no correct country is present).	
incorrectLikelihood - average likelihood value for	all incorrect countries.
highestLikelihoodCorrect	- is the highest likelihood country correct?
highestLikelihoodCorrectValue - if highest likelihood country is correct, what likelihood was assigned to it?	
differentials - list of reported countries at that stage. 