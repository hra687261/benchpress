# Benchpress ![build](https://github.com/sneeuwballen/benchpress/workflows/build/badge.svg)

Tool to run one or more logic programs, on a set of files, and collect the
results.

License: BSD.

## Basic usage

```sh
$ benchpress run -c foo.sexp dir_a/ dir_b/ -p z3
…
```

this tells `benchpress` to run the prover `z3` on directories `dir_a`
and `dir_b`. `foo.sexp` contains additional configuration parameters
as described below.

## System dependencies

Logitest relies on a bunch of utilities, besides OCaml libraries:

- `sqlite3` (with development headers)
- `time`, `ulimit`, etc
- ~~`gzip`/`zcat` for compressing files~~
- (optional) `grep` + access to `/proc/cpuinfo` for guessing number of cores
- (optional) `git` for tagging solvers from their repository

**TODO** use cgroups or similar for CPU affinity

## Options

### CLI options

Most of the commands accept `-c <config file>` to specify which config files to use.

- `benchpress --help` to list options
- `benchpress run`:
  * `-t <time>` timeout (in seconds) for each run
  * `-m <memory>` memory limit in MB
  * `-F <file>` read list of problems from given file
  * `-p <prover1,prover2>` list of provers to use
  * `--task <profile>` specify which task to use
- `benchpress slurm`:
  * `-t <time>` timeout (in seconds) for each run
  * `-m <memory>` memory limit in MB
  * `-F <file>` read list of problems from given file
  * `-p <prover1,prover2>` list of provers to use
  * `--task <profile>` specify which task to use
  * `--nodes`: number of nodes to allocate for the execution
  * `--ntasks`: number of parallel instances of benchpress to run
  * `--cpus-per-task`: number of cpus to use on each benchpress instance
- `benchpress dir config` shows the configuration directory
- `benchpress dir state` shows the directory where the state (benchmark results) is stored
- `benchpress check-config <file>` to check that the file is valid configuration
- `benchpress prover-list` to list available provers
- `benchpress prover-show <prover>` to show the definition of a prover
- `benchpress list-files` to list the results
- `benchpress show <result>` to show the content of the result file

- `-v` and `-vv` can be used to get more verbose output.
- if the environment variable `LOGS_FILE` is set to a filename, logs will be
  written to that file.

### ENV var options

Some internal parameters of benchpress can be set using environment variables:

- "BENCHPRESS_BUSY_TIMEOUT" controls the busy timeout of the sql database used
  by benchpress, in miliseconds. Default is 3000.



### Web interface

- `benchpress-server` is a daemon listening on a local port (default `8080`),
  which provides a basic web UI.

## Config File

Benchpress ships with a builtin config, which is imported by default
unless `(import-prelude false)` is specified. It contains, roughly:

### Builtin config

```sexp
; read smtlib status
(prover
  (name smtlib-read-status)
  (cmd "grep :status $file")
  (unknown ":status unknown")
  (sat ":status sat")
  (unsat ":status unsat"))

(prover
  (name minisat)
  (unsat "UNSATISFIABLE")
  (sat "^SATISFIABLE")
  (cmd "minisat -cpu-lim=$timeout $file"))

(prover
  (name z3)
  (cmd "z3 $file")
  (version "cmd:z3 --version")
  (unsat "unsat")
  (sat "^sat"))
```

The configuration is based on _stanzas_ that define available provers, available
sets of benchmarks (based on directories that contain  them), and _tasks_.
For now the only kind of supported task is to run provers on problems,
but it should get richer as we go (e.g. run proof checkers, do some basic CI,
run a task daily, etc.).

In this default file we also define a pseudo-prover, "smtlib-read-status",
which is used to parse SMTLIB benchmarks and find an annotation
`(set-info :status <…>)`. This is useful when running provers later
because it makes it easy to find bugs (if a prover reports a wrong answer).

We also define provers `minisat` and `z3` as common reference points,
providing info on how to run them (with `cmd …`) and how to parse their
results using regexes.

### Example of config file

A more complete example, taken from [mc2](https://github.com/c-cube/mc2):

```sexp

; from https://github.com/c-cube/mc2

(prover
  (name mc2)
  (cmd "ulimit -t $timeout; mc2 --time $timeout $file")
  (unsat "^Unsat")
  (sat "^Sat")
  (unknown "Unknown")
  (timeout "Timeout"))

(dir
  (path "$HOME/workspace/smtlib")
  (pattern ".*.smt2")
  (expect (run smtlib-read-status)))

(task
  (name glob-all-smtlib)
  (synopsis "run all SMT solvers on smtlib")
  (action
   (run_provers
    (dirs ("$HOME/workspace/smtlib"))
    (provers (mc2 z3))
    ;(memory 100000000)  ; TODO: parse "10G"
    (timeout 10))))

(task
  (name glob-all-smtlib-QF_UF)
  (synopsis "run all SMT solvers on QF_UF")
  (action
    (run_provers
      (dirs ("$HOME/workspace/smtlib/QF_UF"))
      (provers (mc2 z3))
      (timeout 10))))
```

Such a configuration file can be validated using:

```sh
$ benchpress check-config the_file.sexp
```

Then one can run a task, like so:
```sh
$ benchpress run -c the_file.sexp --task glob-all-smtlib-QF_UF -t 30
```
to run mc2 and z3 on the QF_UF problems in the SMTLIB directory.
The `task` stanza defines a pre-packaged task that can be launched easily
from the command line or the embedded web server (a bit like a makefile target).

Note that tasks are not necessary, they're just shortcuts. You can also
pass directly the prover list and directory:

```sh
$ benchpress run -c the_file.sexp -p mc2 some/path/ -t 30
```

### List of stanzas

The variable `$cur_dir` evaluates to the config file's directory. This allows
the config file to refer to provers that are installed locally (e.g. in the
same repository).

- `(prover …)` defines a new prover. The name should be unique.
  * `name`: unique name, used to refer to this prover in results, on the command line, etc
  * `cmd`: how to run the prover. Variables `$timeout`, `$file`, `$memory` are
    available and will refer to parameters used to run the prover on a file.
  * `sat`, `unsat`, `unknown`, `timeout`, `memory` are (perl) regex used to recognize
    the result (or reason for failure by timeout or memory exhaustion) of the prover.
  * custom tags can be used with `(tag foo regex)`: a tag named `foo` will be
    used when `regex` matches the prover's output.
- `(dir …)` defines a directory:
  * `(path …)` defines the path. The rules below apply to any file within this directory.
  * `(pattern ".*.smt2")` means only files matching the (perl) regex will be considered.
  * `(expect …)` defines how to find the expected result of each file (which will
    be compared to the actual result to detect bugs).
- `(custom-tag (name t))` makes a custom tag `t` available
- `(task …)` defines a task that can be run from the command line.
  * `name` should be unique (used to refer to the task)
  * `action` defines what the task should do, see [the action section](#actions)
    For now there's only `(run_provers …)` to run provers on files locally.
- `(set-options…)` defines global options:
  * `j` integer for number of parallel tasks in `run`
  * `progress` boolean for progress bar in `run`

### Actions

- `(run_provers fields)` to run some provers on some benchmarks. Fields are:
  * `(provers (p1 … pn))` list of (names of) provers defined in other stanzas
  * `(dirs (p1 … pn))` paths containing benchmarks. The paths must be subdirectories
    of already defined directories (see the `dir` stanza above)
  * `(timeout n)` (optional) defines a timeout in seconds
  * `(memory n)` (optional) defines a memory limit in MB
  * `(pattern regex)` (optional) an additional regex for files to consider in `dirs`
  * `(db_file n)` (optional) defines a file in which to store the database
  * `(j n)` (optional) defines the number of concurrent threads to use when running the provers
- `(run_provers_slurm fields)` to run some provers on some benchmarks using the computing power of a slurm cluster. The fields are mainly the same as those of the `run_provers` except that they apply to each the instances on benchpress that will be lunched on the cluster's nodes. There are also additional fields:
  * `(nodes n)`: (optional) the number of nodes to allocate to the action (default is 1).
  * `(ntasks n)`: (optional) the number of benchpress instances to lunch in parallel (default is the number of `nodes`).
  * `(cpus_per_task n)`: (optional) the number of cpus to use on each parallel instance of benchpress (default is 1).
  * `(j n)` (optional) defines the number of concurrent threads to use for each parallel instance of benchpress (default is the number of `cpus_per_task`).
- `(progn a1 … an)` runs actions in sequence. Fails if any action fails.
- `(run_cmd "the command")` runs the given command.
- `(git_checkout (dir d) (ref r) [(fetch_first fetch|pull)])` specifies
  a directory in which to go (`(dir d)`), a git reference to checkout (`(ref r)`)
  and optionally a tag to indicate whether to fetch/pull the repo first.

## An example of a task running with slurm

```
(task
  (name testrun-slurm)
  (action
    (run_provers_slurm
      (dirs ($PATHS))
      (provers (z3 cvc4))
      (timeout 2)
      (nodes 2)
      (ntasks 2)
      (cpus_per_task 1))))
```

assuming that $PATHS are paths to directories containing smt-lib2 benchmarks which were previously defined in the config file.
