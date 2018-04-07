module TestCalculations exposing (..)

import Brew
import Expect exposing (Expectation)
import Test exposing (..)


percent : Float -> Float
percent x =
    x / 100.0


abvSuite : Test
abvSuite =
    let
        acceptablePrecision =
            Expect.Absolute 0.02

        perfectPrecision =
            Expect.Absolute 0.0
    in
    describe "ABV calculation tests"
        [ describe "Identical OG and FG should always result in 0% ABV"
            [ test "low identical OG and FG" <|
                \_ ->
                    let
                        original_gravity =
                            1.0

                        final_gravity =
                            1.0

                        expected =
                            percent 0.0
                    in
                    Brew.calculateAbv original_gravity final_gravity
                        |> Expect.within perfectPrecision expected
            , test "high identical OG and FG" <|
                \_ ->
                    let
                        original_gravity =
                            1.08

                        final_gravity =
                            1.08

                        expected =
                            percent 0.0
                    in
                    Brew.calculateAbv original_gravity final_gravity
                        |> Expect.within perfectPrecision expected
            ]
        , describe "Check ABV calculation for a variety of beverage styles"
            [ test "American light lager" <|
                \_ ->
                    let
                        original_gravity =
                            1.04

                        final_gravity =
                            1.005

                        expected =
                            percent 4.6
                    in
                    Brew.calculateAbv original_gravity final_gravity
                        |> Expect.within acceptablePrecision expected
            , test "Belgian Tripel" <|
                \_ ->
                    let
                        original_gravity =
                            1.08

                        final_gravity =
                            1.011

                        expected =
                            percent 9.6
                    in
                    Brew.calculateAbv original_gravity final_gravity
                        |> Expect.within acceptablePrecision expected
            , test "Dry wine" <|
                \_ ->
                    let
                        original_gravity =
                            1.08

                        final_gravity =
                            0.99

                        expected =
                            percent 12.3
                    in
                    Brew.calculateAbv original_gravity final_gravity
                        |> Expect.within acceptablePrecision expected
            , test "Dry mead" <|
                \_ ->
                    let
                        original_gravity =
                            1.11

                        final_gravity =
                            1.01

                        expected =
                            percent 13.3
                    in
                    Brew.calculateAbv original_gravity final_gravity
                        |> Expect.within acceptablePrecision expected
            , test "Sweet mead" <|
                \_ ->
                    let
                        original_gravity =
                            1.135

                        final_gravity =
                            1.05

                        expected =
                            percent 11.5
                    in
                    Brew.calculateAbv original_gravity final_gravity
                        |> Expect.within acceptablePrecision expected
            ]
        ]
