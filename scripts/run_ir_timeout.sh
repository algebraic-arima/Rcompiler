#!/usr/bin/env bash
set -euo pipefail

# Run IR tests one by one with a per-step timeout. Use COMPILER and TIMEOUT env vars to override defaults.
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
COMPILER="${COMPILER:-$ROOT/cmake-build-debug/code}"
TIMEOUT="${TIMEOUT:-12}"
FILTER="${1:-}"

red() { printf "\033[31m%s\033[0m\n" "$*"; }
green() { printf "\033[32m%s\033[0m\n" "$*"; }
yellow() { printf "\033[33m%s\033[0m\n" "$*"; }

run_one() {
  local rx="$1"
  local dir="$(dirname "$rx")"
  local base="$(basename "$rx" .rx)"
  local ll="$dir/$base.ll"
  local s="$dir/$base.s"
  local exe="$dir/$base"
  local infile="$dir/$base.in"
  local outfile="$dir/$base.out"

  printf "\n== %s ==\n" "$base"

  if ! timeout "${TIMEOUT}s" "$COMPILER" "$rx" --emit-llvm >"$dir/$base.compile.log" 2>&1; then
    red "compile failed or timed out"
    return
  fi

  if ! timeout "${TIMEOUT}s" llc "$ll" >"$dir/$base.llc.log" 2>&1; then
    red "llc failed or timed out"
    return
  fi

  if ! timeout "${TIMEOUT}s" clang -no-pie "$s" -o "$exe" >"$dir/$base.clang.log" 2>&1; then
    red "clang failed or timed out"
    return
  fi

  local run_output
  if [ -f "$infile" ]; then
    if ! run_output=$(timeout "${TIMEOUT}s" bash -lc "\"$exe\" < \"$infile\""); then
      red "program run failed or timed out"
      return
    fi
  else
    if ! run_output=$(timeout "${TIMEOUT}s" "$exe"); then
      red "program run failed or timed out"
      return
    fi
  fi

  if [ -f "$outfile" ]; then
    if diff -u <(cat "$outfile"; if [ -n "$(tail -c1 "$outfile")" ]; then echo; fi) <(printf "%s\n" "$run_output") >"$dir/$base.diff.log"; then
      green "PASS"
    else
      yellow "FAIL (see $dir/$base.diff.log)"
      printf "%s\n" "$run_output" >"$dir/$base.actual.log"
    fi
  else
    yellow "no .out file; program output:\n$run_output"
  fi
}

mapfile -t tests < <(find "$ROOT/test_case/semantic-2" -name '*.rx' | sort)

if [ -n "$FILTER" ]; then
  tmp=()
  for t in "${tests[@]}"; do
    if [[ "$(basename "$t")" == *"$FILTER"* ]]; then
      tmp+=("$t")
    fi
  done
  tests=("${tmp[@]}")
fi

if [ "${#tests[@]}" -eq 0 ]; then
  yellow "no tests matched"
  exit 0
fi

echo "Using compiler: $COMPILER"
echo "Timeout (s): $TIMEOUT"

for t in "${tests[@]}"; do
  run_one "$t"
done
