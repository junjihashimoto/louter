#!/usr/bin/env python3
"""
Master test runner for all protocol compliance tests.

Runs comprehensive test suites for:
- OpenAI API (using official openai SDK)
- Anthropic API (using official anthropic SDK)
- Gemini API (using official google-generativeai SDK)

Usage:
    python tests/run_all_tests.py                    # Run all tests
    python tests/run_all_tests.py --openai           # Run OpenAI tests only
    python tests/run_all_tests.py --anthropic        # Run Anthropic tests only
    python tests/run_all_tests.py --gemini           # Run Gemini tests only
    python tests/run_all_tests.py --parallel         # Run all tests in parallel
"""

import os
import sys
import argparse
import subprocess
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import List, Tuple, Dict


# Test suite configurations
TEST_SUITES = {
    "openai": {
        "name": "OpenAI API",
        "script": "tests/test_openai_streaming.py",
        "env_vars": {
            "OPENAI_BASE_URL": "http://localhost:9000",
            "OPENAI_API_KEY": "test-key",
            "OPENAI_MODEL": "gpt-4"
        },
        "requirements": ["openai"]
    },
    "anthropic": {
        "name": "Anthropic API",
        "script": "tests/test_anthropic_streaming.py",
        "env_vars": {
            "ANTHROPIC_BASE_URL": "http://localhost:9000",
            "ANTHROPIC_API_KEY": "test-key"
        },
        "requirements": ["anthropic"]
    },
    "gemini": {
        "name": "Gemini API",
        "script": "tests/test_gemini_streaming.py",
        "env_vars": {
            "GOOGLE_GEMINI_BASE_URL": "http://localhost:9000",
            "GOOGLE_API_KEY": "test-key"
        },
        "requirements": ["google-generativeai"]
    }
}


def check_dependencies(suite_name: str) -> Tuple[bool, List[str]]:
    """Check if required dependencies are installed."""
    suite = TEST_SUITES[suite_name]
    missing = []

    for req in suite["requirements"]:
        try:
            __import__(req.replace("-", "_"))
        except ImportError:
            missing.append(req)

    return len(missing) == 0, missing


def run_test_suite(suite_name: str, verbose: bool = False) -> Tuple[str, bool, str]:
    """Run a single test suite."""
    suite = TEST_SUITES[suite_name]

    print(f"\n{'=' * 70}")
    print(f"Running {suite['name']} Tests")
    print(f"{'=' * 70}")

    # Check dependencies
    deps_ok, missing = check_dependencies(suite_name)
    if not deps_ok:
        error_msg = f"Missing dependencies: {', '.join(missing)}\nInstall with: pip install {' '.join(missing)}"
        print(f"❌ SKIP: {error_msg}")
        return suite_name, False, error_msg

    # Set environment variables
    env = os.environ.copy()
    env.update(suite["env_vars"])

    # Run the test script
    try:
        result = subprocess.run(
            [sys.executable, suite["script"]],
            env=env,
            capture_output=True,
            text=True,
            timeout=300  # 5 minute timeout
        )

        output = result.stdout + result.stderr

        if verbose:
            print(output)

        success = result.returncode == 0

        if success:
            print(f"✅ {suite['name']} tests PASSED")
        else:
            print(f"❌ {suite['name']} tests FAILED")
            if not verbose:
                print("\nError output:")
                print(result.stderr[-1000:] if len(result.stderr) > 1000 else result.stderr)

        return suite_name, success, output

    except subprocess.TimeoutExpired:
        error_msg = f"Test suite timed out after 5 minutes"
        print(f"❌ {suite['name']}: {error_msg}")
        return suite_name, False, error_msg
    except Exception as e:
        error_msg = f"Error running test suite: {str(e)}"
        print(f"❌ {suite['name']}: {error_msg}")
        return suite_name, False, error_msg


def run_tests_sequential(suites: List[str], verbose: bool = False) -> Dict[str, Tuple[bool, str]]:
    """Run test suites sequentially."""
    results = {}

    for suite_name in suites:
        name, success, output = run_test_suite(suite_name, verbose)
        results[name] = (success, output)

    return results


def run_tests_parallel(suites: List[str], verbose: bool = False) -> Dict[str, Tuple[bool, str]]:
    """Run test suites in parallel."""
    results = {}

    print(f"\nRunning {len(suites)} test suites in parallel...")

    with ThreadPoolExecutor(max_workers=len(suites)) as executor:
        futures = {
            executor.submit(run_test_suite, suite_name, verbose): suite_name
            for suite_name in suites
        }

        for future in as_completed(futures):
            suite_name = futures[future]
            try:
                name, success, output = future.result()
                results[name] = (success, output)
            except Exception as e:
                print(f"❌ {suite_name} crashed: {e}")
                results[suite_name] = (False, str(e))

    return results


def print_summary(results: Dict[str, Tuple[bool, str]]):
    """Print test summary."""
    print("\n" + "=" * 70)
    print("Test Summary")
    print("=" * 70)

    total = len(results)
    passed = sum(1 for success, _ in results.values() if success)
    failed = total - passed

    for suite_name, (success, _) in results.items():
        suite = TEST_SUITES[suite_name]
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"{status}: {suite['name']}")

    print(f"\n{passed}/{total} test suites passed")

    if failed > 0:
        print(f"\n⚠️  {failed} test suite(s) failed. Review output above for details.")
        return 1
    else:
        print(f"\n✅ All test suites passed!")
        return 0


def main():
    parser = argparse.ArgumentParser(
        description="Run comprehensive protocol compliance tests",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s                    # Run all tests sequentially
  %(prog)s --parallel         # Run all tests in parallel
  %(prog)s --openai           # Run OpenAI tests only
  %(prog)s --anthropic        # Run Anthropic tests only
  %(prog)s --gemini           # Run Gemini tests only
  %(prog)s --verbose          # Show detailed test output
  %(prog)s --openai --gemini  # Run specific test suites
        """
    )

    parser.add_argument(
        "--openai",
        action="store_true",
        help="Run OpenAI API tests"
    )
    parser.add_argument(
        "--anthropic",
        action="store_true",
        help="Run Anthropic API tests"
    )
    parser.add_argument(
        "--gemini",
        action="store_true",
        help="Run Gemini API tests"
    )
    parser.add_argument(
        "--parallel",
        action="store_true",
        help="Run test suites in parallel"
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Show detailed test output"
    )
    parser.add_argument(
        "--check-deps",
        action="store_true",
        help="Check dependencies and exit"
    )

    args = parser.parse_args()

    # Determine which suites to run
    suites_to_run = []
    if args.openai:
        suites_to_run.append("openai")
    if args.anthropic:
        suites_to_run.append("anthropic")
    if args.gemini:
        suites_to_run.append("gemini")

    # If no specific suites specified, run all
    if not suites_to_run:
        suites_to_run = list(TEST_SUITES.keys())

    # Check dependencies only
    if args.check_deps:
        print("Checking dependencies...")
        all_ok = True
        for suite_name in suites_to_run:
            suite = TEST_SUITES[suite_name]
            deps_ok, missing = check_dependencies(suite_name)

            status = "✅" if deps_ok else "❌"
            print(f"{status} {suite['name']}: ", end="")

            if deps_ok:
                print("All dependencies installed")
            else:
                print(f"Missing: {', '.join(missing)}")
                print(f"   Install with: pip install {' '.join(missing)}")
                all_ok = False

        return 0 if all_ok else 1

    # Print header
    print("=" * 70)
    print("Multi-Protocol LLM Proxy Test Runner")
    print("=" * 70)
    print(f"Test suites: {', '.join(suite['name'] for suite in [TEST_SUITES[s] for s in suites_to_run])}")
    print(f"Mode: {'Parallel' if args.parallel else 'Sequential'}")
    print(f"Verbose: {args.verbose}")

    # Run tests
    if args.parallel:
        results = run_tests_parallel(suites_to_run, args.verbose)
    else:
        results = run_tests_sequential(suites_to_run, args.verbose)

    # Print summary and return exit code
    return print_summary(results)


if __name__ == "__main__":
    sys.exit(main())
