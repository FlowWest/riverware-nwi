@echo off
SET "RPath="

FOR %%G IN (
    "C:\Program Files\R\R-4.4.0\bin\x64",
    "C:\Users\%USERNAME%\AppData\Local\Programs\R\R-4.3.2\bin\x64",
    "C:\Users\%USERNAME%\Documents\R\R-4.1.2\bin\x64",
    "C:\Program Files\R\R-4.3.0\bin\x64",
    "C:\Program Files\R\R-4.3.2\bin\x64",
    "C:\Program Files\R\R-4.2.1\bin\x64"
) DO (
    IF EXIST %%G (
        SET "RPath=%%G"
        GOTO Found
    )
)

:Found
IF NOT DEFINED RPath (
    echo R directory not found.
) ELSE (
    echo Found R directory: %RPath%
)

%Rpath%\RScript.exe climate_indicies_download.R
pause