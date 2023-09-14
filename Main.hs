import qualified System.Environment
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, intersperse, intercalate)
import Text.Read (readMaybe)

type Exception = String

data UnitType 
    = Fahrenheit 
    | Celcius
    | Kelvin
    
    | Mile
    | Meter
    | Centimeter
    | Kilometer
    | Inch
    | Foot
    | Yard
    | NauticalMile
    
    | Second
    | Minute
    | Hour
    | Day
    | Week
    | Month
    | Year
    deriving (Eq, Enum, Show)

type Transformation = [Operation]

data Operation = Add Double | Sub Double | Mult Double | Div Double

id_to_id :: Transformation
id_to_id = []

f_to_c :: Transformation
f_to_c = [Sub 32, Div 1.8]

k_to_c :: Transformation
k_to_c = [Sub 273.15]

mi_to_m :: Transformation
mi_to_m = [Mult 1600.9344]

cm_to_m :: Transformation
cm_to_m = [Div 100]

km_to_m :: Transformation
km_to_m = [Mult 1000]

in_to_m :: Transformation
in_to_m = [Div 39.3700787402]

ft_to_m :: Transformation
ft_to_m = [Div 3.28084]

yd_to_m :: Transformation
yd_to_m = [Div 1.093613]

nm_to_m :: Transformation
nm_to_m = [Mult 1852]

min_to_s :: Transformation
min_to_s = [Mult 60]

hr_to_s :: Transformation
hr_to_s = [Mult 3600]

day_to_s :: Transformation
day_to_s = [Mult 86400]

wk_to_s :: Transformation
wk_to_s = [Mult 604800]

mo_to_s :: Transformation
mo_to_s = [Mult 2629800]

yr_to_s :: Transformation
yr_to_s = [Mult 31557600]

unitStrReprs :: [(UnitType, [String])]
unitStrReprs = 
    [ (Fahrenheit, ["fahrenheit", "f"])
    , (Celcius, ["celcius", "c"])
    , (Kelvin, ["kelvin", "k"])
    , (Mile, ["mile", "mi"])
    , (Meter, ["meter", "m"])
    , (Centimeter, ["centimeter", "cm"])
    , (Kilometer, ["kilometer", "km"])
    , (Inch, ["inch", "in"])
    , (Foot, ["foot", "feet", "ft"])
    , (Yard, ["yard", "yd"])
    , (NauticalMile, ["nauticalmile", "nm", "nmi"])
    , (Second, ["second", "s"])
    , (Minute, ["minute", "min"])
    , (Hour, ["hour", "hr"])
    , (Day, ["day"])
    , (Week, ["week", "wk"])
    , (Month, ["month", "mo"])
    , (Year, ["year", "yr"])
    ]

unitConversionMap :: [(UnitType, UnitType, Transformation)]
unitConversionMap = 
    [ (Celcius, Celcius, id_to_id)
    , (Fahrenheit, Celcius, f_to_c)
    , (Kelvin, Celcius, k_to_c)
    , (Meter, Meter, id_to_id)
    , (Mile, Meter, mi_to_m)
    , (Centimeter, Meter, cm_to_m)
    , (Kilometer, Meter, km_to_m)
    , (Inch, Meter, in_to_m)
    , (Foot, Meter, ft_to_m)
    , (Yard, Meter, yd_to_m)
    , (NauticalMile, Meter, nm_to_m)
    , (Second, Second, id_to_id)
    , (Minute, Second, min_to_s)
    , (Hour, Second, hr_to_s)
    , (Day, Second, day_to_s)
    , (Week, Second, wk_to_s)
    , (Month, Second, mo_to_s)
    , (Year, Second, yr_to_s)
    ]

applyTransformation :: Transformation -> Double -> Double
applyTransformation ops d = foldl (flip applyOperation) d ops

applyOperation :: Operation -> Double -> Double
applyOperation (Add a) d = d + a
applyOperation (Sub a) d = d - a
applyOperation (Mult a) d = d * a
applyOperation (Div a) d = d / a

invertTransformation :: Transformation -> Transformation
invertTransformation t = map invertOperation (reverse t)

invertOperation :: Operation -> Operation
invertOperation (Add a) = Sub a
invertOperation (Sub a) = Add a
invertOperation (Mult a) = Div a
invertOperation (Div a) = Mult a

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst pred (x : xs) =
    if pred x
    then Just x
    else findFirst pred xs

findConversionAndApply :: Double -> UnitType -> UnitType -> Either Exception Double
findConversionAndApply d uFrom uTo =
    if uFrom == uTo
    then 
        Right d
    else
        case findFirst (\(u1, _, _) -> u1 == uFrom) unitConversionMap of
            Nothing -> 
                Left $ "Unit " ++ show uFrom ++ " not found"
            Just (u1f, u2f, trf) ->
                case findFirst (\(u1, _, _) -> u1 == uTo) unitConversionMap of
                    Nothing -> 
                        Left $ "Unit " ++ show uTo ++ " not found"
                    Just (u1t, u2t, trt) ->
                        if u2f == u2t
                        then
                            Right $ applyTransformation (invertTransformation trt) (applyTransformation trf d) -- meat and potatoes
                        else
                            Left $ "Unit " ++ show uFrom ++ " cannot be converted to " ++ show uTo

printHelp :: IO ()
printHelp = do
    printUnits
    putStrLn "\nusage: conv <number> <fromUnit> <toUnit>"
    

printVersion :: IO ()
printVersion = putStrLn "1.0"

printUnits :: IO ()
printUnits = putStrLn $ "-- Units --\n" ++ intercalate "\n" [show ut ++ ": " ++ show strings | s@(ut, strings) <- unitStrReprs]

main = do
    args <- System.Environment.getArgs
    if length args == 0 
    then printHelp 
    else
        case head args of
            "-h" -> printHelp
            "--help" -> printHelp
            "-v" -> printVersion
            "--version" -> printVersion
            _ ->
                if length args /= 3
                then printHelp
                else
                    case readMaybe (head args) :: Maybe Double of
                        Nothing -> printHelp
                        Just d ->
                            case findFirst (\(_, strs) -> map toLower (args !! 1) `elem` strs) unitStrReprs of
                                Nothing -> do
                                    putStrLn $ "Unit '" ++ args !! 1 ++ "' not recognized.\n"
                                    printUnits
                                Just (utf, _) ->
                                    case findFirst (\(_, strs) -> map toLower (args !! 2) `elem` strs) unitStrReprs of
                                        Nothing -> do
                                            putStrLn $ "Unit '" ++ args !! 2 ++ "' not recognized.\n"
                                            printUnits
                                        Just (utt, _) ->
                                            case findConversionAndApply d utf utt of
                                                Left e -> putStrLn e
                                                Right dNew -> putStrLn $ show d ++ " " ++ show utf ++ " = " ++ show dNew ++ " " ++ show utt
