This is the data dictionary for the study in Chapter 5 - VR study

We used VR scenarios implemented by Oxford Medical Simulation (OMS, https://oxfordmedicalsimulation.com/), a company that implements bespoke VR software for medical education and simulation. Participants in this study were medical students based in Oxford who were at the time taking part in VR-based teaching sessions as part of their medical degrees. Students performed the scenarios using Oculus Quest 2 VR headsets. The clinical scenarios were paediatric (i.e cases were presented where a child would be attending the hospital with their legal guardian). Each scenario features a visual 3D implementation of a cubicle in an Emergency Department of a hospital. Participants are shown a range of avatars, including a (child) patient, their guardian and a nurse who can help with certain treatment and investigations/testing. All of the ‘avatars’ in the scenario can be questioned by the participant using a predefined set of requests/actions (e.g. asking the nurse to check blood pressure, asking the patient/child about if they are in pain).

Each participant completed two scenarios over two separate VR sessions. The sessions were held around one month apart. During each session, the participants each performed one scenario in VR and observed another medical student during their scenario. We chose medical scenarios that were considered fairly common to arise for paediatric patients. The scenarios presented in each session are described below (students are split into two groups, shown below as groups A and B, each performing a different pair of scenarios in a fixed order):
Session One:– Group A: patient/child is a 6-year-old-girl presenting with a 1-day history of central abdominal pain and thirst. She was generally unwell for 2 days prior, with a reduced appetite and a sore throat. Collateral history reveals Type 1 Diabetes and erratic blood sugars. (Underlying Condition: Diabetic Ketoacidosis, henceforth referred to as the "DKA" scenario)– Group B: patient/child is a 5-year-old boy presenting with worsening shortness of breath, wheeze, and signs of respiratory distress, on the background of 2 days of likely viral illness. He has a medical history of asthma and has had similar exacerbations in the past. (Underlying Condition: Acute Severe Exacerbation of Asthma, henceforth referred to as the "Asthma" scenario)
Session Two:– Group A: patient/child is a 5-year-old boy presenting with shortness of breath and drowsiness (Underlying Condition: Chest Sepsis/Pneumonia, henceforth referred to as the "Pneumonia" scenario)– Group B: patient/child is a 5-year-old girl with a 1-day history of sore throat and fever. She starts having a generalised tonic-clonic seizure during the scenario. (Underlying Condition: Febrile seizure on background of tonsillitis, henceforth referred to as the "Seizure" scenario)

After 5 minutes in the scenario (by which point it was expected that participants would have gathered key points on the patient’s history and started some early assessment of the patient), participants were asked to pause the scenario (taking off their VR headset) and to fill in a brief questionnaire on paper. Multiple VR participants were performing the scenario simultaneously and were paired with another student who would watch their performance. This other student would aid with administering the questionnaire, with the students subsequently switching roles for the other scenario. The VR participant was asked in the questionnaire to answer the following (this is considered time point 1):

- “Please say all the conditions that you are currently considering or are concerned about for this patient. Include any/all common, rare or contributing conditions you are considering. For each, please rate how likely you think they are on a scale of 1 (low) to 5 (high).”- “On a scale of 1-10, how confident are you that you understand the patient’s condition?”- “How severe do you think the patient’s condition is on a scale of 1 to 10?” (Each point of the scale represented a different clinical action/course, with 1 representing “Discharge in <4 hours, no follow up” and 10 representing “Requires arrest/peri arrest team.”)

After the scenario, the participant answered a second questionnaire (this is considered time point 2). This included the same three questions as above in timepoint 1 (so that participants could record their updated responses), as well as the following additional questions:
- “What is the most likely course of the patient?” (this was a free text response)- “To what extent would you be prepared to leave the patient prior to a senior review. Mark your response below on a scale ranging from Not at All to Completely” (this question was answered using a visual analogue scale)- “Did you complete all the history, examinations and investigations necessary? If not, what else would you do if given more time?”- “What investigation would you give highest priority next?”

Data Fields:

ParticipantID - anonymised participant ID. 

Scenario - which scenario participant is doing.	

t1Diagnoses - comma separated list of differential reported at timepoint 1. 	

t1DiagnosisScore - diagnostic appropriateness score based on differentials at timepoint 1.	

t1Likelihoods - comma separated list of likelihood values for each differentials (corresponds with t1Diagnoses)	

t1Confidence - confidence in understanding of patient's condition at timepoint 1. 	

t1Severity - severity of patient's condition at timepoint 1. 	

t2Diagnoses - comma separated list of differentials reported at timepoint 2. 		

t2Likelihoods - comma separated list of likelihood values for each differential (corresponds with t2Diagnoses)	

t2Confidence - confidence in understanding of patient's condition at timepoint 2. 

t2Severity - severity of patient's condition at timepoint 2. 		

t2SeniorReview - response at timepoint 2 to question "to what extent would you be prepared to leave the patient prior to a senior review?"	

t2InvestigationsComplete - response at timepoint 2 to question “Did you complete all the history, examinations and investigations necessary?". 1 for yes, 0 for no.

OMSScore - performance score calculated in OMS based on completion of objectives and treatment actions and seeking certain information.	

t2ExtraInvestigations - response at timepoint 2 to question "If not (to t2InvestigationsComplete), what else would you do if given more time?”

TIME.SPENT - total time spent in the scenario.	

scenGroup - each participant is either in group DP (did DKA and then Pneumonia scenarios) or AS (did Asthma and then Seizure scenario)	

scenNum	- is this the participant's first or second scenario?

orderNum - each of the four scenarios is done in a fixed order.	

t1numOfDiagnoses - number of differentials in list at timepoint 1	

t2numOfDiagnoses - number of differentials in list at timepoint 2		

t1HighestLikelihood - highest likelihood given to a differential at timepoint 1	

t2HighestLikelihood - highest likelihood given to a differential at timepoint 2		

severityChange - difference in total severity between timepoint 2 and timepoint 2.

confidenceChange - difference in confidence between timepoint 2 and timepoint 2.	

t1numOfDiagnosesChange - difference in number of differentials between timepoint 2 and timepoint 2.		

sbarStart - amount of time passed until participant started their SBAR handover (9999 if they didn't do it at all).	

helpStart - amount of time passed until participant call for help using the phone (9999 if they didn't do it at all).	

totalHelpStart - earliest time between sbarStart and helpStart. Inf if neither were done.

pewsScore - Patient Early Warning Score calculated by participant for the patient.	

totalActions - Total number of actions/information requested by participant (including duplicates)	

uniqueActions - Total number of actions/information requested by participant (excluding duplicates)	

filteredActions	- Total number of actions/information requested by participant (excluding duplicates, only including categorised actions under one of our categories, see actionCategories file)	

actionVector - list of actions requested or not	

actTimesVector - list of times at which each action was performed in seconds

numOfHistoryActions - number of actions requested belonging to the Patient History category.	

numOfExamActions - number of actions requested belonging to the Physical Examinations category.	

numOfTestingActions - number of actions requested belonging to the Testing category.		

numOfTreatmentActions - number of actions requested belonging to the Treatment category.		

numOfActionsAfterHistory - number of actions in Exam or Testing.	

numOfHistoryActionsBeforePause - number of actions in History requested before pause point (<300 seconds)	

numOfHistoryActionsAfterPause - number of actions in History requested after pause point (>300 seconds)		

numOfExamActionsBeforePause - number of actions in Exams requested before pause point (<300 seconds)			

numOfExamActionsAfterPause - number of actions in Exams requested after pause point (>300 seconds)	

percentageHistory - proportion of History actions that took place before pause point, relative to all history actions taken	

numOfTestingActionsBeforePause - number of actions in Testing requested before pause point (<300 seconds)		

numOfTestingActionsAfterPause - number of actions in Testing requested after pause point (>300 seconds)		

percentageTesting - proportion of Testing actions that took place after pause point, relative to all testing actions taken		

numOfTreatmentActionsBeforePause - number of actions in Treatment requested before pause point (<300 seconds)			

numOfTreatmentActionsAfterPause - number of actions in Treatment requested after pause point (>300 seconds)		

historyVectorBeforePause - list of history actions taken before pause point	

numOfActionsBeforePause - number of actions (as per filteredActions) requested before pause point (<300 seconds)

numOfActionsAfterPause - number of actions (as per filteredActions) requested after pause point (>300 seconds)	

infoVal	- information value across all actions, calculated as difference in OMS score without or with each bit of information, then summed.

infoValHistory - information value for History actions.	

infoValExams - information value for Exam actions.		

infoValTesting  - information value for Testing actions.		

infoValHistoryBeforePause - information value for History actions recorded before pause point.		

infoValBeforePause - information value for actions recorded before pause point.	

infoValAfterPause - information value for actions recorded after pause point.		

infoValAfterHistory - information value for actions in Exams and Testing.

