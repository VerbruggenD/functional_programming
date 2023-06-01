import Debug.Trace
import System.Posix (SystemID(machine))

data Status = Pass | SmallError | LargeError | Fail deriving (Eq, Show)

class QualityCheck a where
  getQuality :: a -> Status

class QualityControl a where
  meetsQualityControl :: [a] -> Bool

data Exam = Exam { score :: Float, studypoints :: Int }

instance QualityCheck Exam where
  getQuality exam
    | score exam >= 10.0 = Pass
    | score exam >= 8.0 = SmallError
    | otherwise = Fail

instance QualityControl Exam where
  meetsQualityControl exams = trace (show weightedScore)
    all (\examen -> getQuality examen /= Fail) exams &&
    numSmallErrors <= 2 && numFails == 0 &&
    ((weightedScore >= 0.58 && numSmallErrors <= 2) || (weightedScore >= 0.54 && numSmallErrors <= 1))
    where
      numSmallErrors = length $ filter (\exam -> getQuality exam == SmallError) exams
      numFails = length $ filter (\exam -> getQuality exam == Fail) exams
      totalWeightedPoints = sum $ map (\exam -> score exam * fromIntegral (studypoints exam)) exams
      totalWeightedCredits = sum $ map (fromIntegral . studypoints) exams
      weightedScore = totalWeightedPoints / (20 * totalWeightedCredits)

examen1 = Exam { score = 15.5, studypoints = 2 }
examen2 = Exam { score = 10.2, studypoints = 1 }
examen3 = Exam { score = 9.5, studypoints = 3 } -- kleine afwijking
examen4 = Exam { score = 12.0, studypoints = 5 }
examen5 = Exam { score = 6.7, studypoints = 2 } -- fail

examens1 = [examen1, examen2, examen5] -- 1 fail
examens2 = [examen1, examen3, examen4] -- kleine afwijking percentage goed
examens3 = [examen2, examen3] -- kleine afwijking percentage slecht
examens4 = [examen1, examen2, examen4] -- alles goed

data ExamQuestion = ExamQuestion { question :: String, answer :: String, isSeriousMistake :: Bool } deriving (Eq, Show)

data ExamAnswer = ExamAnswer { givenAnswer :: String, examQuestion :: ExamQuestion } deriving (Eq, Show)

instance QualityCheck ExamAnswer where
  getQuality examAnswer
    | givenAnswer examAnswer == answer (examQuestion examAnswer) = Pass
    | isSeriousMistake (examQuestion examAnswer) = LargeError
    | otherwise = SmallError

instance QualityControl ExamAnswer where
  meetsQualityControl answers = trace (show totalPoints)
    totalPoints <= 9
    where
      totalPoints = sum $ map calculatePoints answers
      calculatePoints answer
        | getQuality answer == LargeError = 5
        | getQuality answer == SmallError = 1
        | otherwise = 0

-- heavy mistakes
question1 = ExamQuestion { question = "Question 1", answer = "A", isSeriousMistake = True }
answer1 = ExamAnswer { givenAnswer = "C", examQuestion = question1 }
question2 = ExamQuestion { question = "Question 5", answer = "C", isSeriousMistake = True }
answer2 = ExamAnswer { givenAnswer = "B", examQuestion = question2 }

-- small mistake
question3 = ExamQuestion { question = "Question 2", answer = "C", isSeriousMistake = False }
answer3 = ExamAnswer { givenAnswer = "B", examQuestion = question3 }
question4 = ExamQuestion { question = "Question 3", answer = "B", isSeriousMistake = False }
answer4 = ExamAnswer { givenAnswer = "C", examQuestion = question4 }

-- right answers
question5 = ExamQuestion { question = "Question 1", answer = "A", isSeriousMistake = True }
answer5 = ExamAnswer { givenAnswer = "A", examQuestion = question5 }
question6 = ExamQuestion { question = "Question 2", answer = "C", isSeriousMistake = False }
answer6 = ExamAnswer { givenAnswer = "C", examQuestion = question6 }
question7 = ExamQuestion { question = "Question 4", answer = "A", isSeriousMistake = False }
answer7 = ExamAnswer { givenAnswer = "A", examQuestion = question7 }

drivingExam1 = [answer1, answer2, answer5, answer6, answer7] -- two heavy mistakes
drivingExam2 = [answer1, answer5, answer6, answer7] -- one heavy mistakes
drivingExam3 = [answer3, answer4, answer3, answer4, answer3, answer4, answer3, answer4] -- 8 small mistakes
drivingExam4 = [answer3, answer4, answer3, answer4, answer3, answer4, answer3, answer4, answer3, answer4] -- 10 small mistakes
drivingExam5 = [answer5, answer6, answer7, answer6, answer5, answer6, answer7, answer5, answer6, answer5, answer7, answer5, answer6] -- 13 right answers

data ServiceInterval = ServiceInterval { smallError :: Int, largeError :: Int, failure :: Int }

data MachineComponent = MachineComponent { interval :: ServiceInterval, hoursInUse :: Int, lastMaintenance :: Int }

instance QualityCheck MachineComponent where
  getQuality component
    | hours <= smallError (interval component) = Pass
    | hours <= largeError (interval component) = SmallError
    | hours <= failure (interval component) = LargeError
    | otherwise = Fail
    where
      hours = hoursInUse component - lastMaintenance component

instance QualityControl MachineComponent where
  meetsQualityControl components =
    totalErrorPoints <= 2 && all (\component -> getQuality component /= Fail) components
    where
      totalErrorPoints = sum $ map calculateErrorPoints components
      calculateErrorPoints component
        | getQuality component == LargeError = 2
        | getQuality component == SmallError = 1
        | otherwise = 0

failComp = MachineComponent { interval = ServiceInterval { smallError = 2, largeError = 4, failure = 6 }, hoursInUse = 9, lastMaintenance = 2}
smallErrorComp = MachineComponent { interval = ServiceInterval { smallError = 2, largeError = 4, failure = 6 }, hoursInUse = 5, lastMaintenance = 2}
largeErrorComp = MachineComponent { interval = ServiceInterval { smallError = 2, largeError = 4, failure = 6 }, hoursInUse = 7, lastMaintenance = 2}
goodComp = MachineComponent { interval = ServiceInterval { smallError = 2, largeError = 4, failure = 6 }, hoursInUse = 3, lastMaintenance = 2 }

machine1 = [failComp, goodComp] -- should fail
machine2 = [smallErrorComp, goodComp] -- should pass
machine3 = [largeErrorComp, goodComp] -- should pass
machine4 = [smallErrorComp, goodComp, smallErrorComp] -- should pass
machine5 = [largeErrorComp, goodComp, smallErrorComp] -- should fail
