import behavior
import compiler

if __name__ == "__main__":
    behavior_successful, behavior_failed = behavior.run()
    compiler_successful, compiler_failed = compiler.run()

    print(
        "Result: "
        + str(behavior_successful + compiler_successful)
        + "/"
        + str(
            behavior_successful
            + behavior_failed
            + compiler_successful
            + compiler_failed
        )
        + " tests passed"
    )
