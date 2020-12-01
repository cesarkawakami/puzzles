#!/usr/bin/env python3

import json
import subprocess
import sys
from argparse import ArgumentParser
from pathlib import Path
from typing import Any, Dict, List, NamedTuple


class Options(NamedTuple):
    release: bool
    test_only: List[str]


DEFAULT_TOOL_CONFIG = {"cpp_std": "c++17", "type": "generic"}


def _merge_tool_config(base: Dict[str, Any], local: Dict[str, Any]) -> Dict[str, Any]:
    rv = dict(base)
    rv.update(local)
    return rv


def compute_tool_config(file_path: Path) -> Dict[str, str]:
    root_dir = Path(__file__).parent.resolve()
    found_config_paths = []
    current_dir = file_path.resolve()
    while True:
        current_dir = current_dir.parent
        candidate_path = current_dir / "toolconfig.json"
        if candidate_path.is_file():
            found_config_paths.append(candidate_path)
        if current_dir in root_dir.parents:
            break

    found_config_paths = found_config_paths[::-1]
    tool_config = DEFAULT_TOOL_CONFIG
    for config_path in found_config_paths:
        with open(config_path, "r") as f:
            local_config = json.load(f)
        tool_config = _merge_tool_config(tool_config, local_config)

    return tool_config


def autofix_usaco_header(file_path: Path) -> None:
    contents = file_path.read_text("utf-8")
    problem_name = _get_base_path(file_path).name
    if "ID:" not in contents[:30]:
        if file_path.suffix == ".py":
            contents = (
                f'''\
"""
ID: cesarka2
LANG: PYTHON3
TASK: {problem_name}
"""
'''
                + contents
            )
        elif file_path.suffix == ".cpp":
            contents = (
                f"""\
/*
ID: cesarka2
LANG: C++14
TASK: {problem_name}
*/
"""
                + contents
            )
        file_path.write_text(contents, "utf-8")


def autofix_usaco_input(file_path: Path) -> None:
    problem_name = _get_base_path(file_path).name

    magic_string = problem_name + ".in"

    if magic_string not in file_path.read_text("utf-8"):
        print(
            f"Looks like {magic_string} was found nowhere in {file_path}, but this is usaco!"
        )
        print(
            """\
Monkey:

#ifndef LOCAL
    std::freopen("PROBLEM.in", "r", stdin);
    std::freopen("PROBLEM.out", "w", stdout);
#endif
"""
        )
        sys.exit(1)


def autofix(options: Options, file_path: Path) -> None:
    tool_config = compute_tool_config(file_path)

    if tool_config["type"] == "usaco":
        autofix_usaco_header(file_path)
        autofix_usaco_input(file_path)


def build(options: Options, file_path: Path) -> None:
    tool_config = compute_tool_config(file_path)

    if file_path.suffix == ".py":
        return
    elif file_path.suffix == ".cpp":
        mode_args = (
            ["-O3"]
            if options.release == "release"
            else ["-fsanitize=address,undefined"]
        )
        output = file_path.with_suffix(".run")
        p = subprocess.run(
            [
                "clang++",
                "-DLOCAL",
                "-Wall",
                "-g",
                f"-std={tool_config['cpp_std']}",
                *mode_args,
                "-o",
                str(output),
                str(file_path),
            ],
        )
        if p.returncode != 0:
            print(f"Compiler failed with code: {p.returncode}")
            sys.exit(1)
    else:
        print(f"Unrecognized suffix: {file_path.suffix}")
        sys.exit(1)

    print("Compilation successful!")


def _get_base_path(file_path: Path) -> Path:
    base_path = file_path.with_suffix("")
    if "-" in base_path.name:
        new_name, _ = base_path.name.split("-", 1)
        base_path = base_path.with_name(new_name)
    return base_path


def test(options: Options, file_path: Path) -> None:
    base_path = _get_base_path(file_path)

    for path in base_path.parent.glob(base_path.name + ".cand*"):
        path.unlink()

    if not options.test_only:
        input_files = list(base_path.parent.glob(base_path.name + ".in*"))
        input_files = [x for x in input_files if not x.name.endswith("debug")]
    else:
        input_files = [
            base_path.parent / (base_path.name + ".in" + x) for x in options.test_only
        ]
    input_files.sort()

    if not input_files:
        print("No input files found!")
        sys.exit(1)

    for input_file in input_files:
        output_file = input_file.with_name(input_file.name.replace(".in", ".cand"))
        with open(input_file, "r") as fin, open(output_file, "w") as fout:
            timeout_prefix = ["timeout", "-s9", "5"]
            if file_path.suffix == ".py":
                p = subprocess.run(
                    [*timeout_prefix, "python3", str(file_path)], stdin=fin, stdout=fout
                )
            elif file_path.suffix == ".cpp":
                p = subprocess.run(
                    [*timeout_prefix, str(file_path.with_suffix(".run"))],
                    stdin=fin,
                    stdout=fout,
                )
            else:
                print(f"Unrecognized suffix: {file_path.suffix}")
                sys.exit(1)
            if p.returncode != 0:
                print(f">>> Failure exit code: {p.returncode} ! <<<")


def compare(options: Options, file_path: Path) -> None:
    base_path = _get_base_path(file_path)
    candidates = list(base_path.parent.glob(base_path.name + ".cand*"))
    candidates = [x for x in candidates if not x.name.endswith('debug')]
    candidates.sort()
    different_files_count = 0
    for candidate in candidates:
        output = candidate.with_name(candidate.name.replace(".cand", ".out"))
        print()
        print(f">>> {candidate} vs {output} <<<")
        p = subprocess.run(
            ["diff", "--color=always", "-ysN", str(candidate), str(output)]
        )
        if p.returncode != 0:
            different_files_count += 1
    print()
    if different_files_count:
        print(
            f">>> {different_files_count} out of {len(candidates)} files different <<<"
        )
    else:
        print(">>> All files identical! <<<")


def _generate_compilation_database_entry(
    path: Path, tool_config: Dict[str, Any]
) -> Dict[str, str]:
    std = tool_config["cpp_std"]
    return {
        "directory": str(Path(__file__).parent.resolve()),
        "command": f"clang++ -DLOCAL -std={std} {str(path)}",
        "file": str(path),
    }


def _recursive_generate_compilation_database(
    dir: Path, parent_tool_config: Dict[str, Any]
) -> List[Dict[str, str]]:
    local_tool_config_path = dir / "toolconfig.json"
    current_tool_config = parent_tool_config
    if local_tool_config_path.exists():
        with open(local_tool_config_path, "r") as f:
            local_tool_config = json.load(f)
        current_tool_config = _merge_tool_config(parent_tool_config, local_tool_config)

    rv = []
    for path in dir.iterdir():
        if path.is_file() and path.suffix == ".cpp":
            entry = _generate_compilation_database_entry(path, current_tool_config)
            rv.append(entry)
        elif path.is_dir():
            sub_entries = _recursive_generate_compilation_database(
                path, current_tool_config
            )
            rv.extend(sub_entries)

    return rv


def generate_compilation_database(options: Options, file_path: Path) -> None:
    base_dir = Path(__file__).parent

    compilation_database = _recursive_generate_compilation_database(
        base_dir, DEFAULT_TOOL_CONFIG
    )

    with open(base_dir / "compile_commands.json", "w") as f:
        json.dump(compilation_database, f, indent=4)

    print(f"Generated {len(compilation_database)} compilation database entries.")


MODES = {
    "autofix": autofix,
    "build": build,
    "test": test,
    "compare": compare,
    "gen_cmds": generate_compilation_database,
}


def main() -> None:
    parser = ArgumentParser()
    parser.add_argument(
        "-r", "--release", action="store_true", help="compile in release mode"
    )
    parser.add_argument(
        "-t",
        "--test-only",
        help="comma-separated list of tests to run, e.g. -t ,2 runs .in, .in2",
    )
    parser.add_argument(
        "modes",
        help=f"comma-separated list of modes to run. Available: {','.join(MODES)}",
    )
    parser.add_argument("source", help="path to source code")
    args = parser.parse_args()

    if not args.test_only:
        args.test_only = []
    else:
        args.test_only = args.test_only.split(",")

    options = Options(
        release=args.release,
        test_only=args.test_only,
    )

    modes = args.modes.split(",")
    for mode in modes:
        if mode not in MODES:
            print(f"Unrecognized mode: {mode}")
            sys.exit(1)

    file_path = Path(args.source)
    if not file_path.is_file() and modes != ["gen_cmds"]:
        print(f"File not found: {file_path}")
        sys.exit(1)

    for mode in modes:
        MODES[mode](options, file_path)


if __name__ == "__main__":
    main()
