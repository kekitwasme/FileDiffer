@echo off

REM Check if the virtual environment exists
IF EXIST ".\venv\Scripts\activate.bat" (
    CALL ".\venv\Scripts\activate.bat"
) ELSE (
    echo Virtual environment does not exist. Creating virtual environment.
    python -m venv venv
    CALL ".\venv\Scripts\activate.bat"
)

echo Checking dependencies
pip install -r requirements.txt
echo Running Python script
python ".\fuelWatchRender.py"

pause