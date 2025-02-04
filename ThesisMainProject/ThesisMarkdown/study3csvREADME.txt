This is the data dictionary for Chapter 4 - In-Person Think Aloud Study

The general procedure was very similar to that of the online experiment, except that participants were given the following standardised instructions at the start of the study:
"Whilst you are doing the task, you will be asked to think aloud. This means that you verbalise what you are thinking about, especially how you interpret the information you receive and what conditions or diagnoses you are considering or are concerned about for each patient case. If you have nothing to say or nothing on your mind, there's no need to say anything but do say whatever is on your mind once it pops up. If you are unsure about anything you see or do not know about what something means, you will not receive any help but verbalise when you are unsure about anything during the task. Please make sure that you speak clearly 'to the room'."

We audio recorded all verbalisations. We aimed to detect which reasoning strategies are used by students on each case. To code for reasoning strategies, we adopt a similar approach to Coderre et al (2003). Codes were initially chosen by two independent coders, and then conflicts were resolved between the two. We defined coding criteria that indicate three different diagnostic reasoning strategies: hypothetico-deductive reasoning, scheme-inductive reasoning and pattern recognition (Coderre et al. 2003). These were defined as follows:
- Hypothetico-Deductive Reasoning (HD) - prior to selecting the most likely diagnosis, the participant analysed any alternative differentials one by one through something akin to a process of elimination.
- Scheme Inductive Reasoning (SI) - participant structures their diagnosis by pathophysiological systems or categories of conditions (e.g., infective vs cardiovascular causes) to determine root causes of patient symptoms rather than focusing on specific conditions.
- Pattern Recognition (PR) - participant considers only a single diagnosis with only perfunctory attention to the alternatives or makes reference to pattern matching when using a prototypical condition to match its symptoms against the current observed symptoms for the patient (e.g., "these symptoms sound like X" or "this fits with a picture of Y").- None - cases are defined as not having a clear reasoning strategy if there are insufficient utterances to make an inference that a participant is using a particular reasoning strategy (as agreed by both coders).

Data Fields - 

ID - Anonymised participant ID. All participants are 5th/6th year medical students (N = 16)

Condition - Which case the participant was doing. Aortic Dissection (AD), Guillain-Barre Syndrome (GBS), Miliary TB (MTB), Temporal Arteritis (TA), Thrombotic Thrombocytopenic Purpura (TTP) and Ulcerative Colitis (UC). Case order was randomised for each participant.

CaseNumber - how many cases had the participant seen so far, including this one?

Correct - did the participant utter/mention a correct differential during this case?

PR.x, HD.x, SI.x, OverallStrat - initial coding activity by a single coder. You can disregard these fields.

InterraterStrat - the strategy coded for this case after interrater coding and resolving conflicts.

DEs - We define a variable to look at the number of instances in which participants evaluate or reevaluate the differentials they are considering: DEs is the number of instances of the below subcodes belonging to this main Differential Evaluation code. Thenumber of such utterances are defined for each individual case. The higher this number, the more participants are ‘updating’ their thinking around what differentials they were considering as likely/unlikely for the patient.

DE-DAs - Differential Added: Number of mentions of a new condition that the participant is considering.

DE-DRs - Differential Removed: Number of mentions of ruling out or eliminating a condition from consideration.

DE-IL - Increased Likelihood: Number of mentions of increased likelihood of a previously mentioned condition, or that information seems to correspond with a condition.

DE-DL - Decreased Likelihood: Number of mentions of decreased likelihood of a previously mentioned condition, or that information seems to go against a particular condition.

DE-CS - Comorbidity or Secondary condition - Number of mentions of a secondary condition, that is not a primary diagnosis but some other condition that the patient is suspected of having.

hybridStrat - is the participant noted to use multiple strategies?

gender - raw responses to gender question 

decisionRationalScore - survey score based on rational subscale of the decision making styles questionnaire (Hamilton, Shih and Mohammed, 2016).

decisionIntuitionScore - survey score based on intuition subscale of the decision making styles questionnaire (Hamilton, Shih and Mohammed, 2016).

relativeRationalism - decisionRationalScore minus decisionIntuitionScoreTAWordCount - Total number of words uttered by participant across the experiment.

condition - full name of case being performed

finalConfidence - final confidence in readiness to treat (after all information seeking)

confidenceChange - difference between confidence at final stage and at the first stage (Patient History)

subjectiveDifficulty - participant rating of difficulty of case, from 1 (trivial) to 10 (impossible)

caseInformationReqs - number of unique information requests made.

proportionOfInfo - caseInformationReqs / 29 (i.e. the total number of available tests/information that participants could request)

confidenceArray - comma separated list of confidence values at each stage of information seeking

reqTestArray - comma separated list of whether each piece of information was sought

tiedStrats/IndividualDominantStrat/matchingIndividualDominantStrat - individual coder fields. You can disregard. The subjective preferred strategies can be found in a separate field in this OSF.

infoValue - value of information seeking for this case.