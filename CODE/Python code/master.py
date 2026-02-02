"""
Master Python Script - ZRR Geographic Data Processing
======================================================
Run this script from the project root directory to execute all Python
preprocessing steps in the correct order.

Usage:
    cd "/path/to/THESIS_REPRO February 2026"
    python CODE/Python\ code/master.py

Scripts executed (in order):
    1. defineBorders.py      -> Creates dataGeoRDD1.xlsx
    2. border_pair.py        -> Creates border_pair.xlsx
    3. distAgglo.py          -> Creates distAgglo.xlsx
    4. defineBordersNotEpicenter1.py -> Creates dataGeoRDD_noEpicenter.xlsx

Optional (slow, run separately if needed):
    5. defineBorders_randomZRR.py -> Creates randomization files
"""

import subprocess
import sys
from pathlib import Path
from config import PROJECT_ROOT, verify_paths

SCRIPT_DIR = Path(__file__).resolve().parent

# Scripts to run in order (excluding randomization which is slow)
SCRIPTS = [
    ("defineBorders.py", "dataGeoRDD1.xlsx"),
    ("border_pair.py", "border_pair.xlsx"),
    ("distAgglo.py", "distAgglo.xlsx"),
    ("defineBordersNotEpicenter1.py", "dataGeoRDD_noEpicenter.xlsx"),
]

def run_script(script_name):
    """Run a Python script and return success status."""
    script_path = SCRIPT_DIR / script_name
    print(f"\n{'='*60}")
    print(f"Running: {script_name}")
    print('='*60)

    result = subprocess.run(
        [sys.executable, str(script_path)],
        cwd=str(PROJECT_ROOT)
    )
    return result.returncode == 0

def main():
    print("ZRR Python Preprocessing Pipeline")
    print("="*60)

    # Verify paths first
    if not verify_paths():
        print("\nERROR: Some required files are missing. Check paths above.")
        sys.exit(1)

    # Run each script
    success_count = 0
    for script_name, output_file in SCRIPTS:
        if run_script(script_name):
            print(f"SUCCESS: {output_file} created")
            success_count += 1
        else:
            print(f"FAILED: {script_name}")

    # Summary
    print(f"\n{'='*60}")
    print(f"COMPLETED: {success_count}/{len(SCRIPTS)} scripts successful")
    print("="*60)

    if success_count < len(SCRIPTS):
        print("\nNote: Some scripts failed. Check error messages above.")
        print("You may need to run scripts individually to debug.")

if __name__ == "__main__":
    main()
