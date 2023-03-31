# Tests typechecking

import glob
import os
import subprocess


def run():
    tests = glob.glob(
        "./behavior/**/*",
        root_dir=os.path.dirname(__file__),
        recursive=True,
    )

    successful = 0
    failed = 0

    for file in tests:
        # build positive files
        path = os.path.join(os.path.dirname(__file__), file)

        if os.path.isdir(path) or not file.endswith(".amp"):
            continue

        print("Running test: " + file)

        res = subprocess.run(["amp", "run", path])

        if res.returncode != 0:
            print("Test failed: " + file)
            failed += 1
        else:
            successful += 1

    if __name__ == "__main__":
        print(
            "Result: "
            + str(successful)
            + "/"
            + str(failed + successful)
            + " tests passed"
        )

    return successful, failed


if __name__ == "__main__":
    run()
