#!/bin/bash

#Check if the activate script exists
if [ -f "./venv/bin/activate" ]; then
    source "./venv/bin/activate"
else
    echo "Virtual environment does not exist. Creating virtual environment"
    python3 -m venv venv
    source "./venv/bin/activate"
fi

echo "Checking dependancies"
pip install -r requirements.txt -q
echo "Running Python script"
python3 "./app.py"