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

    # TODO: use `check` subcommand when implemented

    for file in tests:
        # build positive files
        path = os.path.join(os.path.dirname(__file__), file)

        if os.path.isdir(path) or not file.endswith(".amp"):
            continue

        res = subprocess.run(["amp", "run", path], stdout=subprocess.PIPE)

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
