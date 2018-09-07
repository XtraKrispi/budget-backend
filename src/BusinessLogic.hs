module BusinessLogic where

import Data.Time.Calendar
import Data.List
import Models

getAllInstances :: StartDate -> EndDate -> [BudgetItemDefinition] -> [BudgetItemInstance]
getAllInstances s e defs = sortOn (\(BudgetItemInstance (def, d)) -> d) $ defs >>= getInstances s e

getInstances :: StartDate -> EndDate -> BudgetItemDefinition -> [BudgetItemInstance]
getInstances s e def = BudgetItemInstance . (,) def <$> getDates s e def

getDates :: StartDate -> EndDate -> BudgetItemDefinition -> [Day]
getDates s e def =
  let days = 
        case frequency def of
          OneTime -> [startDate def]
          Weekly -> (\n -> addDays (7 * n) (startDate def)) <$> [0,1..]
          Monthly -> (\n -> addGregorianMonthsClip n (startDate def)) <$> [0,1..]
          Yearly -> (\n -> addGregorianYearsClip n (startDate def)) <$> [0,1..]
          BiWeekly -> (\n -> addDays (14 * n) (startDate def)) <$> [0,1..]
  in takeWhile (<= e) . dropWhile (< s) $ days