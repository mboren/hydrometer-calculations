module Brew exposing (hydrometerTempCorrection)

{-| Brewing terminology used in this module:

Specific gravity:
Ratio of the density of a solution to the density of water.
Often abbreviated as "SG" or just "gravity".
The change in specific gravity over time is used to estimate
how much sugar has been converted to ethanol.

Hydrometer:
Device used to measure the specific gravity of a solution

Hydrometer calibration:
Since density changes with temperature, hydrometers are only accurate at the
temperature they were calibrated at. Measurements taken at different
temperatures must be adjusted to get the true specific gravity.

-}


{-| Correction formula source:
<https://homebrew.stackexchange.com/a/4142>
-}
hydrometerTempCorrection : Float -> Float -> Float -> Float
hydrometerTempCorrection measuredGravity measuredTemp hydrometerCalibration =
    let
        mg =
            measuredGravity

        mt =
            measuredTemp

        hc =
            hydrometerCalibration
    in
    mg * ((1.00130346 - 0.000134722124 * mt + 0.00000204052596 * mt * mt - 0.00000000232820948 * mt * mt * mt) / (1.00130346 - 0.000134722124 * hc + 0.00000204052596 * hc * hc - 0.00000000232820948 * hc * hc * hc))
