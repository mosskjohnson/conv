import qualified System.Environment
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, intersperse, intercalate)
import Text.Read (readMaybe)

type Exception = String

type Transformation = [Operation]

data Operation = Add Double | Sub Double | Mul Double | Div Double

data UnitType 
    = Celcius
    | Fahrenheit 
    | Kelvin
    | Rankine
    
    | Meter
    | Mile
    | Centimeter
    | Kilometer
    | Inch
    | Foot
    | Yard
    | Fathom
    | NauticalMile
    | AstronomicalUnit
    | Micrometer
    | Nanometer
    | Millimeter
    
    | Second
    | Minute
    | Hour
    | Day
    | Week
    | Month
    | Year
    | Decade
    | Century
    | Millennium
    | Millisecond
    | Microsecond
    | Nanosecond
    | Fortnight

    | Kilogram
    | AtomicMassUnit
    | Gram
    | Milligram
    | Ounce
    | Pound
    | Stone
    | MetricTon

    | Liter
    | Milliliter
    | FluidOunce
    | Cup
    | Pint
    | Quart
    | Gallon

    | Radian
    | Degree
    | Gradian
    | Revolution
    | ArcMinute
    | ArcSecond

    | MeterPerSecond
    | MilePerHour
    | FootPerSecond
    | KilometerPerHour
    | Knot
    | Mach

    | Newton
    | Dyne
    | PoundForce
    | Kip
    | KiloNewton
    | GramForce
    | OunceForce
    | KilogramForce
    | TonForce

    | Bit
    | Byte
    | Kilobit
    | Kilobyte
    | Kibibit
    | Kibibyte
    | Megabit
    | Megabyte
    | Mebibit
    | Mebibyte
    | Terabit
    | Terabyte
    | Tebibit
    | Tebibyte

    | Joule
    | Kilojoule
    | Nanojoule
    | Gramcalorie
    | Kilocalorie
    | WattHour
    | KilowattHour
    | Erg
    | BritishThermalUnit
    | ElectronVolt
    | FootPound

    | SqMeter
    | SqMile
    | SqCentimeter
    | SqKilometer
    | SqInch
    | SqFoot
    | SqYard
    | SqMicrometer
    | SqNanometer
    | SqMillimeter
    | Acre
    | Barn
    | Rood
    | SqRod
    | Are
    deriving (Eq, Enum, Show)

unitStrReprs :: [(UnitType, [String])]
unitStrReprs = 
    [ (Celcius,            ["celcius", "c"])
    , (Fahrenheit,         ["fahrenheit", "f"])
    , (Kelvin,             ["kelvin", "k"])
    , (Rankine,            ["rankine", "ra"])
    , (Meter,              ["meter", "metre", "m"])
    , (Mile,               ["mile", "mi"])
    , (Centimeter,         ["centimeter", "cm"])
    , (Kilometer,          ["kilometer", "km"])
    , (Inch,               ["inch", "in"])
    , (Foot,               ["foot", "feet", "ft"])
    , (Yard,               ["yard", "yd"])
    , (Fathom,             ["fathom"])
    , (NauticalMile,       ["nauticalmile", "nmi"])
    , (AstronomicalUnit,   ["astronomicalunit", "au", "ua"])
    , (Micrometer,         ["micrometer", "micrometre", "micron"])
    , (Nanometer,          ["nanometer", "nanometre", "nm"])
    , (Millimeter,         ["millimeter", "mm"])
    , (Second,             ["second", "s", "sec"])
    , (Minute,             ["minute", "min"])
    , (Hour,               ["hour", "hr"])
    , (Day,                ["day", "d"])
    , (Week,               ["week", "wk"])
    , (Month,              ["month", "mo"])
    , (Year,               ["year", "yr"])
    , (Decade,             ["decade"])
    , (Century,            ["century"])
    , (Millennium,         ["millennium"])
    , (Millisecond,        ["millisecond", "ms"])
    , (Microsecond,        ["microsecond"])
    , (Nanosecond,         ["nanosecond", "ns"])
    , (Fortnight,          ["fortnight"])
    , (Kilogram,           ["kilogram", "kilo", "kg"])
    , (AtomicMassUnit,     ["atomicmassunit", "amu"])
    , (Gram,               ["gram", "g"])
    , (Milligram,          ["milligram", "mg"])
    , (Ounce,              ["ounce", "oz"])
    , (Pound,              ["pound", "lb"])
    , (Stone,              ["stone", "st"])
    , (MetricTon,          ["metricton"])
    , (Liter,              ["liter", "litre", "l"])
    , (Milliliter,         ["milliliter", "millilitre", "ml"])
    , (FluidOunce,         ["fluidounce", "floz"])
    , (Cup,                ["cup"])
    , (Pint,               ["pint"])
    , (Quart,              ["quart"])
    , (Gallon,             ["gallon", "gal", "gl"])
    , (Radian,             ["radian", "rad"])
    , (Degree,             ["degree", "deg"])
    , (Gradian,            ["gradian", "grad"])
    , (Revolution,         ["revolution", "rev"])
    , (ArcMinute,          ["arcminute", "amin"])
    , (ArcSecond,          ["arcsecond", "asec"])
    , (MeterPerSecond,     ["meterpersecond", "m/s"])
    , (MilePerHour,        ["mileperhour", "mph", "m/h"])
    , (FootPerSecond,      ["footpersecond", "fps", "ft/s"])
    , (KilometerPerHour,   ["kilometerperhour", "kph", "km/h"])
    , (Knot,               ["knot"])
    , (Mach,               ["mach"])
    , (Newton,             ["newton", "n"])
    , (Dyne,               ["dyne"])
    , (PoundForce,         ["poundforce"])
    , (Kip,                ["kip"])
    , (KiloNewton,         ["kilonewton", "kn"])
    , (GramForce,          ["gramforce"])
    , (OunceForce,         ["ounceforce"])
    , (KilogramForce,      ["kilogramforce", "kgf"])
    , (TonForce,           ["tonforce", "tf"])
    , (Bit,                ["bit"])
    , (Byte,               ["byte"])
    , (Kilobit,            ["kilobit"])
    , (Kilobyte,           ["kilobyte"])
    , (Kibibit,            ["kibibit"])
    , (Kibibyte,           ["kibibyte"])
    , (Megabit,            ["megabit"])
    , (Megabyte,           ["megabyte"])
    , (Mebibit,            ["mebibit"])
    , (Mebibyte,           ["mebibyte"])
    , (Terabit,            ["terabit"])
    , (Terabyte,           ["terabyte"])
    , (Tebibit,            ["tebibit"])
    , (Tebibyte,           ["tebibyte"])
    , (Joule,              ["joule", "j"])
    , (Kilojoule,          ["kilojoule", "kj"])
    , (Nanojoule,          ["nanojoule", "nj"])
    , (Gramcalorie,        ["gramcalorie", "cal"])
    , (Kilocalorie,        ["kilocalorie", "kcal"])
    , (WattHour,           ["watthour", "wh"])
    , (KilowattHour,       ["kilowatthour", "kwh"])
    , (Erg,                ["erg"])
    , (BritishThermalUnit, ["britishthermalunit", "btu"])
    , (ElectronVolt,       ["electronvolt", "ev"])
    , (FootPound,          ["footpound", "ftlb"])
    , (SqMeter,            ["squaremeter", "squaremetre", "sqm"])
    , (SqMile,             ["squaremile", "sqmi"])
    , (SqCentimeter,       ["squarecentimeter", "sqcm"])
    , (SqKilometer,        ["squarekilometer", "sqkm"])
    , (SqInch,             ["squareinch", "sqin"])
    , (SqFoot,             ["squarefoot", "squarefeet", "sqft"])
    , (SqYard,             ["sqareyard", "sqyd"])
    , (SqMicrometer,       ["squaremicrometer", "squaremicrometre", "sqmicron"])
    , (SqNanometer,        ["squarenanometer", "squarenanometre", "sqnm"])
    , (SqMillimeter,       ["squaremillimeter", "sqmm"])
    , (Acre,               ["acre"])
    , (Barn,               ["barn"])
    , (Rood,               ["rood"])
    , (SqRod,              ["squarerod", "sqrod"])
    , (Are,                ["are"])
    ]

unitConversionMap :: [(UnitType, UnitType, Transformation)]
unitConversionMap = 
    [ (Celcius,            Celcius,        [])
    , (Fahrenheit,         Celcius,        [Sub 32, Div 1.8])
    , (Kelvin,             Celcius,        [Sub 273.15])
    , (Rankine,            Celcius,        [Div 1.8, Sub 273.15])
    , (Meter,              Meter,          [])
    , (Mile,               Meter,          [Mul 1609.344])
    , (Centimeter,         Meter,          [Div 100])
    , (Kilometer,          Meter,          [Mul 1000])
    , (Inch,               Meter,          [Div 39.3700787402])
    , (Foot,               Meter,          [Div 3.28084])
    , (Yard,               Meter,          [Div 1.093613])
    , (Fathom,             Meter,          [Mul 1.8288])
    , (NauticalMile,       Meter,          [Mul 1852])
    , (AstronomicalUnit,   Meter,          [Mul 149597870691])
    , (Micrometer,         Meter,          [Div 1000000])
    , (Nanometer,          Meter,          [Div 1000000000])
    , (Millimeter,         Meter,          [Div 1000])
    , (Second,             Second,         [])
    , (Minute,             Second,         [Mul 60])
    , (Hour,               Second,         [Mul 3600])
    , (Day,                Second,         [Mul 86400])
    , (Week,               Second,         [Mul 604800])
    , (Month,              Second,         [Mul 2629800])
    , (Year,               Second,         [Mul 31557600])
    , (Decade,             Second,         [Mul 315576000])
    , (Century,            Second,         [Mul 3155760000])
    , (Millennium,         Second,         [Mul 31557600000])
    , (Millisecond,        Second,         [Div 1000])
    , (Microsecond,        Second,         [Div 1000000])
    , (Nanosecond,         Second,         [Div 1000000000])
    , (Fortnight,          Second,         [Mul 1209600])
    , (Kilogram,           Kilogram,       [])
    , (AtomicMassUnit,     Kilogram,       [Div 602214129011673940000000000])
    , (Gram,               Kilogram,       [Div 1000])
    , (Milligram,          Kilogram,       [Div 1000000])
    , (Ounce,              Kilogram,       [Div 35.27396])
    , (Pound,              Kilogram,       [Div 2.204623])
    , (Stone,              Kilogram,       [Mul 6.35029])
    , (MetricTon,          Kilogram,       [Mul 1000])
    , (Liter,              Liter,          [])
    , (Milliliter,         Liter,          [Div 1000])
    , (FluidOunce,         Liter,          [Div 33.81413])
    , (Cup,                Liter,          [Div 4.226766])
    , (Pint,               Liter,          [Div 2.113376])
    , (Quart,              Liter,          [Div 1.056688])
    , (Gallon,             Liter,          [Mul 3.7854])
    , (Radian,             Radian,         [])
    , (Degree,             Radian,         [Div 57.2957795130823208768])
    , (Gradian,            Radian,         [Div 63.66197723675813430755])
    , (Revolution,         Radian,         [Mul 6.28318530717958647693])
    , (ArcMinute,          Radian,         [Div 3437.74677078493925260789])
    , (ArcSecond,          Radian,         [Div 206264.80624709635515647336])
    , (MeterPerSecond,     MeterPerSecond, [])
    , (MilePerHour,        MeterPerSecond, [Div 2.23693629205440229062])
    , (FootPerSecond,      MeterPerSecond, [Div 3.28084])
    , (KilometerPerHour,   MeterPerSecond, [Div 3.6])
    , (Knot,               MeterPerSecond, [Div 1.943844])
    , (Mach,               MeterPerSecond, [Mul 343])
    , (Newton,             Newton,         [])
    , (Dyne,               Newton,         [Div 100000])
    , (PoundForce,         Newton,         [Mul 4.448222])
    , (Kip,                Newton,         [Mul 4448.222])
    , (KiloNewton,         Newton,         [Mul 1000])
    , (GramForce,          Newton,         [Div 101.9716])
    , (OunceForce,         Newton,         [Div 3.596943])
    , (KilogramForce,      Newton,         [Mul 9.80665])
    , (TonForce,           Newton,         [Mul 9806.65])
    , (Bit,                Bit,            [])
    , (Byte,               Bit,            [Mul 8])
    , (Kilobit,            Bit,            [Mul 1000])
    , (Kilobyte,           Bit,            [Mul 8000])
    , (Kibibit,            Bit,            [Mul 1024])
    , (Kibibyte,           Bit,            [Mul 8192])
    , (Megabit,            Bit,            [Mul 1000000])
    , (Megabyte,           Bit,            [Mul 8000000])
    , (Mebibit,            Bit,            [Mul 1048576])
    , (Mebibyte,           Bit,            [Mul 8388608])
    , (Terabit,            Bit,            [Mul 1000000000])
    , (Terabyte,           Bit,            [Mul 8000000000])
    , (Tebibit,            Bit,            [Mul 1099511627776])
    , (Tebibyte,           Bit,            [Mul 8796093022208])
    , (Joule,              Joule,          [])
    , (Kilojoule,          Joule,          [Mul 1000])
    , (Nanojoule,          Joule,          [Div 1000000000])
    , (Gramcalorie,        Joule,          [Mul 4.184])
    , (Kilocalorie,        Joule,          [Mul 4184])
    , (WattHour,           Joule,          [Mul 3600])
    , (KilowattHour,       Joule,          [Mul 3600000])
    , (Erg,                Joule,          [Div 10000000])
    , (BritishThermalUnit, Joule,          [Mul 1055.056])
    , (ElectronVolt,       Joule,          [Div 6241509343260179000])
    , (FootPound,          Joule,          [Mul 1.355818])
    , (SqMeter,            SqMeter,        [])
    , (SqMile,             SqMeter,        [Mul 2589988.110336])
    , (SqCentimeter,       SqMeter,        [Div 10000])
    , (SqKilometer,        SqMeter,        [Mul 1000000])
    , (SqInch,             SqMeter,        [Div 1550.003])
    , (SqFoot,             SqMeter,        [Div 10.76391])
    , (SqYard,             SqMeter,        [Div 1.19599])
    , (SqMicrometer,       SqMeter,        [Div 1000000000000])
    , (SqNanometer,        SqMeter,        [Div 1000000000000000000])
    , (SqMillimeter,       SqMeter,        [Div 1000000])
    , (Acre,               SqMeter,        [Mul 4046.86])
    , (Barn,               SqMeter,        [Div 10000000000000000000000000000])
    , (Rood,               SqMeter,        [Mul 1011.714])
    , (SqRod,              SqMeter,        [Mul 25.2929])
    , (Are,                SqMeter,        [Mul 100])
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
            "-u" -> printUnits
            "--units" -> printUnits
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
