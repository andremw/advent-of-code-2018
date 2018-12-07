module ChronalCalibration exposing
  ( calculateResultingFrequency
  , findFrequencyThatRepeatsTwice
  )

calculateResultingFrequency : List Int -> Int
calculateResultingFrequency frequencies =
  List.sum frequencies

findFrequencyThatRepeatsTwice : List Int -> Int
findFrequencyThatRepeatsTwice frequencies =
  0