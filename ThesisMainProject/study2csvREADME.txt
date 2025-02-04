This is the data dictionary for the Study 2 dataset: online vignette study with medical students. 

The goal of the task was to determine a diagnosis, or set of diagnoses, for each presented patient. Information on the patient was split into a series of discrete stages to control what information the participants had access to at any given point of the experiment. Each point of new information is termed as an 'information stage'. Participants were able to seek information freely until they were ready to move on.

The procedure of a single case was as follows. The participant was asked to imagine that they are working in a busy district hospital and they encounter patients in a similar way to how they would in their real medical practice. At the start of each case, the participant was shown a description of a patient, which includes the patient's gender, age and their presenting complaint. An example of this was: "Patient is a 68 year old male presenting with fever and arthralgia". Each case is split into three information stages: Patient History, Physical Examination and Testing (in this order). The Patient History stage included information such as "Allergies", "History of the Presenting Complaint", "Past Medical History" and "Family History". The Physical Examination stage included 'actions' that a doctor may take when examining a patient, such as "auscultate the lungs", "abdominal examination", "take pulse" and "measure temperature". Finally, the Testing stage involved information from any bedside tests or tests they may request from another department. This includes "Chest X-Ray", "Venous Blood Gas", "Urine Dipstick" and "Clotting Test". In total, there were 29 possible information requests across the three stages, with the available set of information being the same for all patient cases.

At any point, they could choose to stop gathering information for that stage. They were then taken to a new screen where they reported a list of all differential diagnoses that they were considering for that patient at that stage. For each differential, participants reported a likelihood rating, ranging from 1 (very unlikely) to 10 (certain), and a "level of concern" (which was how concerned they would be for that patient if this differential really was the patient's underlying condition) on a 4 point scale (labels of "Low", "Medium", "High" and "Emergency"). In subsequent stages, the list from the previous stage was available for participants to update concern/likelihood ratings, and to add/remove differentials from the list. Even at the last information stage, participants could report multiple differentials.

After recording their differentials, participants were then asked to report their confidence that they were "ready to start treating the patient" on a 100 point scale, ranging from not at all confident to fully confident. Participants also indicated using a checkbox whether they are ready to start treating the patient, at which point a text box appeared for them to report what further tests they would perform, any escalations they would make to other medical staff and treatments they would start administering for the patient. This allowed participants to express what actions they would take that were not covered by our set of available information requests. Once all three stages were completed, participants reported how difficult they found it to determine a diagnosis for that case, on a scale from 1 (trivial) to 10 (impossible). At the end of all six patient cases, participants were told the 'true' conditions for all the patients.

Dataset fields:

participantID - anonymised ID for participants. The following IDs are for the experienced clinicians who completed the task (the rest were medical students): "qj4vcw","sz5k4r","kqzd96","s8c6kp","j1bwlt", "jhym2l","gzsfhp"

trialNum - how many cases/trials had the participant seen, including this one? Excluding the practice case. Ranges from 1-6.

stage - Which stage of each case the participant is on. Ranges from 1 - 3.

stageName - Names of the three stages in the stage field. 1 = Patient History, 2 = Physical Examination, 3 = Testing

trueCondition - The correct condition for the current case. Order of cases is randomised for each participant.

differentials - comma separated list of differentials provided by the participant at this stage

requestedTests - During this stage, how many tests/pieces of information did the participant click on? Note this includes repeat tests or tests from previous stages.

uniqueTests - The number of tests from requestedTests, excluding repeat clicks on the same test (within the current stage). Note this is equal to pastTests + currentTests.

pastTests - The number of tests clicked on during this stage that came from a previous stage. (e.g. During Testing stage, the number of tests clicked on from either the Patient History or Physical Examination stages)

currentTests - The number of tests clicked on during this stage that came from this current stage (e.g. During Testing stage, the number of tests clicked on from the Testing stage itself).

possibleTests - The number of available tests for the current stage. This is always the following: Patient History = 6, Physical Examination = 13, Testing = 10.

proportionOfInfo - currentTests / possibleTests

testNames - comma separated list of all tests clicked on.

numOfDifferentials - how many differentials recorded in the participant's list at this stage.

confidence - participants were asked to report their confidence that they were "ready to start treating the patient" on a 100 point scale, ranging from not at all confident to fully confident.

perceivedDifficulty - once all three stages were completed, participants reported how difficult they found it to determine a diagnosis for that case, on a scale from 1 (trivial) to 10 (impossible).

highestSeverity - the highest severity value recorded for any differential in the list. Labels - 0 = "Low", 1 = "Medium", 2 = "High" and 3 = "Emergency"

hasHighSeverity - true if highestSeverity is 3.

highestLikelihood - the highest likelihood value assigned for any differential in the list. Ranges from 1 to 10.

likelihoods - comma separated list for all likelihood values to each differential. Corresponds to the differentials field.

severities - comma separated list for all severity values to each differential. Corresponds to the differentials field.

sevOfHighestLikelihood - severity value assigned to highest likelihood differential.

treatmentPlan - Participants also indicated using a checkbox whether they are ready to start treating the patient, at which point a text box appeared for them to report what further tests they would perform, any escalations they would make to other medical staff and treatments they would start administering for the patient. This allowed participants to express what actions they would take that were not covered by our set of available information requests. Response to this question is recorded here.

infoSeekingTime - how long participants spent (in minutes) seeking information in this stage before moving to the differential reporting screen.

correct - is there a correct diagnosis in the list of the differentials? 1 for yes, 0 for no

brierConfidence - ((confidence/100) - correct)^2. Brier score of confidence

likelihoodOfCorrectDiagnosis - For a case to be considered "correct", the participant should have reported the correct condition for that case within their list of differentials regardless of the number of differentials provided. Likelihoods range from 1-10 when a correct differential is included and has a value of 0 when a correct differential is not included. If multiple differentials that are considered correct are provided, then the likelihood value of the closest differential (as per our marking criteria with help from a medical consultant) to the true condition is used.

sevOfCorrectDiagnosis - Severity value assigned for same 'correct' differential used for likelihoodOfCorrectDiagnosis.

highestLikelihoodCorrect - is the highest likelihood differential correct? 1 for yes, 0 for no

highestLikelihoodCorrectValue - if highestLikelihoodCorrect == 1, the likelihood value assigned to this differential.
