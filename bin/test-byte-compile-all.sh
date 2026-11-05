#!/usr/bin/env bash
set -eu -o pipefail

mapfile -t devshells < \
    <(nix flake show --json \
          | jq --raw-output '.devShells."x86_64-linux" | keys.[]')

for devshell in "${devshells[@]}"; do
    nix develop ".#$devshell" --command bin/test-byte-compile.sh
done
