# Hydrometer temperature correction and ABV calculator
[![Build Status](https://travis-ci.org/mboren/hydrometer-calculations.svg?branch=master)](https://travis-ci.org/mboren/hydrometer-calculations)

This application makes it easy to correct hydrometer readings for temperature
and calculate ABV from specific gravity measurements.

It isn't pretty, but it's the fastest way to do these calculations in a
mobile or desktop web browser that I'm aware of.

The ABV formula used is accurate for both low and high gravity beverages.

Try it [here](https://s3-us-west-1.amazonaws.com/mboren/brew.html).

![screen capture of app in use on a mobile phone](https://s3-us-west-1.amazonaws.com/hi-mom-im-on-the-internet/brewdemo.gif)

## Table of Contents
- [Dependencies](#dependencies)
- [Instructions](#instructions)
- [Use case details](#use-case-details)
- [I don't know what any of this means, help](#i-dont-know-what-any-of-this-means-help)
- [Math](#math)

## Dependencies
I wrote this in [Elm](http://elm-lang.org/).

Elm libraries used:
- Handy table stuff: [evancz/elm-sortable-table](http://package.elm-lang.org/packages/evancz/elm-sortable-table)
- Pretty-printing numbers: [ggb/numeral-elm](http://package.elm-lang.org/packages/ggb/numeral-elm)

From project root directory:
- Install dependencies: `elm-package install`
- Build: `elm-make src/App.elm --output=app.html`

## Instructions
This should be pretty intuitive to anyone who has used a hydrometer before.
Just fill in the blanks and results will be calculated automatically as you
type. When you edit a row, a new row will be added below it automatically,
and you can switch to the new row with the tab key. The new row will copy the
calibration from the previous row.

The typical flow is like this:
1. enter specific gravity and press tab
3. enter temperature and press tab
4. enter calibration (if it has changed) and press tab
5. enter next specific gravity and press tab
6. ...


## Use case details
I typically keep a handwritten table that looks like this in my booze-makin'
notebook:

|   Date     |  SG   | Temperature | Adjusted SG |  ABV   |
|------------|-------|-------------|-------------|--------|
| 2017-05-04 | 1.080 |         75F |      1.082  | 0.00%  |
| 2017-05-20 | 1.037 |         74F |      1.039  | 6.20%  |
| 2017-06-04 | 1.010 |         76F |      1.012  | 9.79%  |
| 2017-06-20 | 0.994 |         74F |      0.996  | 11.87% |

SG and temperature are measured, adjusted SG and ABV are calculated. There
are lots of online calculators and apps that can do these calculations, but
they typically only do one at a time, so I have to do the SG adjustment in
one, and then put that result into another calculator.

This requires *several entire minutes* of my time about once a week or so. I
could be using those minutes to do something productive, like writing
excessively long README files for tiny apps. So I decided to make my own
calculator to speed this process up, based on the following requirements:

- I want to be able to quickly enter a series of rows, each containing measured
  SG and measured temperature.
- Temperature-adjusted SG and ABV should be automatically calculated while
  the user types.
- The common, happy path should require as few keystrokes as possible, in both
  desktop and mobile browsers.
    - Other than 1 click/tap to select the first input box, all further input
      should be possible with the keyboard.
    - If there is a possible trade-off between the efficiency of editing
      existing data and the efficiency of entering new data, editing efficiency
      should be sacrificed.
- Hydrometer calibration should be adjustable, but it will rarely need to be
  modified so it should be filled automatically.
- This is explicitly not for long term storage of data. I prefer pen and paper
  for that. Therefore:
  - No storage necessary.
  - Dates are not necessary.
- Users already have a lot of domain knowledge and know how the input should
  look, so input validation errors should be visible and easy to correct,
  but should not cause interruptions.
- ABV calculation should be accurate for both high and low ABV beverage styles.
- The app should be usable by people other than the author, but this is not a
  high priority since it's mostly a scratch-my-own-itch project.
- Assumption: all temperature measurements will have the same units. Temperature
  handling does not need to be sophisticated.
- Beauty is not a priority.


## I don't know what any of this means, help

Specific gravity (often shortened to "SG" or simply "gravity") is the ratio
of the density of a fluid to the density of a reference (usually water). This
is the most common way to measure the progress of alcoholic fermentation. In
brewing/wine making the reference is always water.

A hydrometer is a tool that measures specific gravity. Since density changes
with temperature, hydrometers are calibrated at a particular temperature, and
readings taken at different temperatures must be adjusted mathematically to
estimate the true SG.

The following facts allow us to estimate the amount of ethanol produced by
fermentation based on the change in specific gravity over time:
- Solutions of sugar in water are denser than water alone
- Solutions of ethanol in water are less dense than water alone
- Every alcoholic fermentation (whether you're making beer, wine, fuel, or
  moonshine) starts with a solution of sugar in water.
- Yeast convert sugar into ethanol and CO2 during fermentation

(There are other chemicals present (eg salts, acids, small amounts of methanol and other alcohols), but the solution is dominated by sugar, ethanol, and water.)

Typically, we will take an initial SG measurement before pitching yeast, and
then take additional measurements periodically as fermentation progresses.
If SG is stable for a long period of time, fermentation has stopped. If it
stops at a low SG, then the beverage is done fermenting and is ready to age
or serve. If it stops at a high SG, then fermentation is "stuck" and we will
have to try to restart it by adjusting temperature, adding nutrients, or
adding water.

The first gravity reading taken is called the original gravity, or OG.

The last gravity reading is called the final gravity, or FG.

## Math

### Temperature correction
mg: measured gravity

mt: measured temperature

hc: hydrometer calibration temperature

Corrected specific gravity = mg * ((1.00130346 - 0.000134722124 * mt + 0.00000204052596 * mt^2  - 0.00000000232820948 * mt^3) / (1.00130346 - 0.000134722124 * hc + 0.00000204052596 * hc^2 - 0.00000000232820948 * hc^3))

[Source](https://homebrew.stackexchange.com/a/4142)

### Alcohol by volume

ABV = (76.08 * (OG - FG) / (1.775 - OG)) * (FG / 0.794)

Note that this equation returns 50 for 50%, 100 for 100%, etc. (instead of
0.5 for 50%, 1.0 for 100%, as you might expect)

[Source](https://www.brewersfriend.com/2011/06/16/alcohol-by-volume-calculator-updated/)

