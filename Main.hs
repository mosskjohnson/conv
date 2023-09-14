import qualified System.Environment
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, intersperse, intercalate)
import Text.Read (readMaybe)

type Exception = String

type Transformation = [Operation]

data Operation = Add Double | Sub Double | Mul Double | Div Double

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
    | Fathom
    | NauticalMile
    
    | Second
    | Minute
    | Hour
    | Day
    | Week
    | Month
    | Year

    | Kilogram
    | Gram
    | Ounce
    | Pound

    | Liter
    | FluidOunce
    | Cup
    | Pint
    | Quart
    | Gallon
    deriving (Eq, Enum, Show)

unitStrReprs :: [(UnitType, [String])]
unitStrReprs = 
    [ (Fahrenheit,   ["fahrenheit", "f"])
    , (Celcius,      ["celcius", "c"])
    , (Kelvin,       ["kelvin", "k"])
    , (Mile,         ["mile", "mi"])
    , (Meter,        ["meter", "metre", "m"])
    , (Centimeter,   ["centimeter", "cm"])
    , (Kilometer,    ["kilometer", "km"])
    , (Inch,         ["inch", "in"])
    , (Foot,         ["foot", "feet", "ft"])
    , (Yard,         ["yard", "yd"])
    , (Fathom,       ["fathom"])
    , (NauticalMile, ["nauticalmile", "nm", "nmi"])
    , (Second,       ["second", "s"])
    , (Minute,       ["minute", "min"])
    , (Hour,         ["hour", "hr"])
    , (Day,          ["day"])
    , (Week,         ["week", "wk"])
    , (Month,        ["month", "mo"])
    , (Year,         ["year", "yr"])
    , (Kilogram,     ["kilogram", "kilo", "kg"])
    , (Gram,         ["gram", "g"])
    , (Ounce,        ["ounce", "oz"])
    , (Pound,        ["pound", "lb"])
    , (Liter,        ["liter", "litre", "l"])
    , (FluidOunce,   ["fluidounce", "floz"])
    , (Cup,          ["cup"])
    , (Pint,         ["pint"])
    , (Quart,        ["quart"])
    , (Gallon,       ["gallon", "gal", "gl"])
    ]

unitConversionMap :: [(UnitType, UnitType, Transformation)]
unitConversionMap = 
    [ (Celcius,      Celcius,  [])
    , (Fahrenheit,   Celcius,  [Sub 32, Div 1.8])
    , (Kelvin,       Celcius,  [Sub 273.15])
    , (Meter,        Meter,    [])
    , (Mile,         Meter,    [Mul 1600.9344])
    , (Centimeter,   Meter,    [Div 100])
    , (Kilometer,    Meter,    [Mul 1000])
    , (Inch,         Meter,    [Div 39.3700787402])
    , (Foot,         Meter,    [Div 3.28084])
    , (Yard,         Meter,    [Div 1.093613])
    , (Fathom,       Meter,    [Mul 1.8288])
    , (NauticalMile, Meter,    [Mul 1852])
    , (Second,       Second,   [])
    , (Minute,       Second,   [Mul 60])
    , (Hour,         Second,   [Mul 3600])
    , (Day,          Second,   [Mul 86400])
    , (Week,         Second,   [Mul 604800])
    , (Month,        Second,   [Mul 2629800])
    , (Year,         Second,   [Mul 31557600])
    , (Kilogram,     Kilogram, [])
    , (Gram,         Kilogram, [Div 1000])
    , (Ounce,        Kilogram, [Div 35.27396])
    , (Pound,        Kilogram, [Div 2.204623])
    , (Liter,        Liter,    [])
    , (FluidOunce,   Liter,    [Div 33.81413])
    , (Cup,          Liter,    [Div 4.226766])
    , (Pint,         Liter,    [Div 2.113376])
    , (Quart,        Liter,    [Div 1.056688])
    , (Gallon,       Liter,    [Mul 3.7854])
    ]

applyTransformation :: Transformation -> Double -> Double
applyTransformation ops d = foldl (flip applyOperation) d ops

applyOperation :: Operation -> Double -> Double
applyOperation (Add a) d = d + a
applyOperation (Sub a) d = d - a
applyOperation (Mul a) d = d * a
applyOperation (Div a) d = d / a

invertTransformation :: Transformation -> Transformation
invertTransformation t = map invertOperation (reverse t)

invertOperation :: Operation -> Operation
invertOperation (Add a) = Sub a
invertOperation (Sub a) = Add a
invertOperation (Mul a) = Div a
invertOperation (Div a) = Mul a

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
                Left $ "Unit '" ++ show uFrom ++ "' not found"
            Just (u1f, u2f, trf) ->
                case findFirst (\(u1, _, _) -> u1 == uTo) unitConversionMap of
                    Nothing -> 
                        Left $ "Unit '" ++ show uTo ++ "' not found"
                    Just (u1t, u2t, trt) ->
                        if u2f == u2t
                        then
                            Right $ applyTransformation (invertTransformation trt) (applyTransformation trf d) -- meat and potatoes
                        else
                            Left $ "Unit '" ++ show uFrom ++ "' cannot be converted to '" ++ show uTo ++ "'"

printHelp :: IO ()
printHelp = do
    printUnits
    putStrLn "\nusage: conv <number> <fromUnit> <toUnit>"
    

printVersion :: IO ()
printVersion = putStrLn "conv -- Version 1.0\nCreated by Moss Johnson"

printUnits :: IO ()
printUnits = putStrLn $ "-- Units --\n" ++ intercalate "\n" [show ut ++ ": " ++ show strings | (ut, strings) <- unitStrReprs]

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
