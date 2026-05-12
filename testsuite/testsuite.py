#! /usr/bin/env python

from argparse import ArgumentParser
import json
import os
from pathlib import Path
import tempfile
import subprocess
import sys

from e3.testsuite import Testsuite, logger
from e3.testsuite.driver.diff import DiffTestDriver, OutputRefiner, Substitute


class InterpreterDriver(DiffTestDriver):
    """
    Driver that runs the LKQL interpreter and analyze its output.
    """

    def run(self) -> None:
        env = dict(os.environ)

        # Ensure color codes are not output during the test execution
        env.pop("TERM", None)

        # Set optional LKQL_PATH
        env["LKQL_PATH"] = os.pathsep.join(
            [self.working_dir(p) for p in self.test_env.get("lkql_path", [])]
        )

        # Get the files to analyze
        analyzed_files = self.test_env.get("analyzed_files", [])
        if not analyzed_files:
            default_source = Path(self.working_dir(), "test.foo")
            if default_source.is_file():
                analyzed_files.append(str(default_source))

        # The run the LKQL script
        lkql_script = self.test_env.get("script", "script.lkql")
        self.shell(
            [
                "lkqlua_jit",
                "--lang-name",
                "foo",
                "--script",
                lkql_script,
                *analyzed_files,
            ],
            env=env,
            catch_error=False,
        )

    @property
    def output_refiners(self) -> list[OutputRefiner]:
        return super().output_refiners + [
            Substitute(self.working_dir(), "<working-dir>")
        ]


class LkqlTestsuite(Testsuite):
    """
    This class is the main entry point of the LKQL testsuite.
    """

    tests_subdir = "tests"
    test_driver_map = {"interpreter": InterpreterDriver}

    def add_options(self, parser: ArgumentParser) -> None:
        parser.add_argument(
            "--rewrite",
            "-r",
            action="store_true",
            help="Rewrite test baselines according to current output.",
        )
        parser.add_argument(
            "--skip-libfoolang-make",
            "-s",
            action="store_true",
            help="Skip the libfoolang building phase",
        )

    def set_up(self) -> None:
        """
        Setup the testsuite.
        """
        # Setup baselines rewrite
        super().set_up()
        self.env.rewrite_baselines = self.env.options.rewrite

        # Build the testing Langkit library
        if not self.env.options.skip_libfoolang_make:
            logger.info("Building libfoolang library...")
            self.lkm("make")

        # Setup the libfoolang environment
        with tempfile.NamedTemporaryFile(
            prefix="lkm-printenv",
            suffix=".json",
            delete_on_close=False,
        ) as tmp:
            tmp.close()
            self.lkm("printenv", "-J", f"--output={tmp.name}")
            with open(tmp.name) as f:
                for k, v in json.load(f).items():
                    os.environ[k] = os.pathsep.join([v, os.environ.get(k, "")])

        # Setup environment to give access to the lkql interpreter
        os.environ["PATH"] = os.pathsep.join(
            [
                str(Path(self.root_dir) / ".." / "target" / "debug"),
                os.environ.get("PATH", ""),
            ]
        )

    def lkm(self, verb, *args):
        """
        Run the `lkm` script in the libfoolang library with the provided
        arguments.
        """
        libfoolang_config = Path(self.root_dir, "libfoolang", "langkit.yaml")
        assert libfoolang_config.is_file()
        subprocess.check_call(
            [
                sys.executable,
                "-m",
                "langkit.scripts.lkm",
                verb,
                "-c",
                str(libfoolang_config.absolute()),
                *args,
            ]
        )


if __name__ == "__main__":
    sys.exit(LkqlTestsuite().testsuite_main())
