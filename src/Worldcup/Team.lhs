> module Worldcup.Team where

This is the basic data type representing a "Team". It derives from Enum, and simply acts as an Enumeration of all possible teams that can participate in a World Cup:

> data Team  =  ARG  | AUS  | BEL  | BRA  | COL  | CRC  | CRO  | DEN
>            |  EGY  | ENG  | FRA  | GER  | ISL  | IRN  | JPN  | KOR
>            |  MEX  | MAR  | NGA  | PAN  | PER  | POL  | POR  | RUS
>            |  KSA  | SEN  | SRB  | ESP  | SWE  | SUI  | TUN  | URU
>   deriving (Eq, Ord, Enum, Show)
