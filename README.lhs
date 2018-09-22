> import Worldcup.Result
> import Worldcup.Team

> results = [Win, Draw, Loss]

> pointsA, pointsB :: Result -> Int
> pointsA Win   = 3
> pointsA Draw  = 1
> pointsA Loss  = 0
> pointsB Win   = 0
> pointsB Draw  = 1
> pointsB Loss  = 3

> odds :: [(Team,Rational)]
> odds = [  (ARG,8/1),(AUS,300/1),(BEL,12/1),(BRA,5/1),(COL,50/1),
>           (CRC,400/1),(CRO,25/1),(DEN,80/1), (EGY,250/1),(ENG,16/1),
>           (FRA,11/2),(GER,5/1),(ISL,200/1),(IRN,500/1),(JPN,250/1),
>           (KOR,500/1),(MEX,66/1),(MAR,250/1),(NGA,150/1),
>           (PAN,1000/1),(PER,150/1),(POL,40/1),(POR,20/1),(RUS,40/1),
>           (KSA,1000/1),(SEN,150/1),(SRB,150/1),(ESP,15/2),(SWE,80/1),
>           (SUI,66/1),(TUN,400/1),(URU,40/1)]
