# Run compiler tests
import glob
import os
import subprocess

positive = glob.glob(
    "./compiler/positive/**/*",
    root_dir=os.path.dirname(__file__),
    recursive=True,
)

negative = glob.glob(
    "./compiler/negative/**/*",
    root_dir=os.path.dirname(__file__),
    recursive=True,
)


successful = 0
failed = 0

# TODO: use `check` subcommand when implemented

for file in positive:
    # build positive files
    print("Running test: " + file)

    path = os.path.join(os.path.dirname(__file__), file)

    if os.path.isdir(path):
        continue

    res = subprocess.run(
        ["amp", "run", path], stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )

    if res.returncode != 0:
        print("Test failed: " + file)
        failed += 1
    else:
        successful += 1

for file in negative:
    # build positive files
    print("Running test: " + file)

    path = os.path.join(os.path.dirname(__file__), file)

    if os.path.isdir(path):
        continue

    res = subprocess.run(
        ["amp", "run", path], stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )

    if res.returncode == 0:
        print("Test failed: " + file)
        failed += 1
    else:
        successful += 1

print("Result: " + str(successful) + "/" + str(failed + successful) + " tests passed")
