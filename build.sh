#!/usr/bin/env bash

set -eu
set -o pipefail

dotnet tool restore
dotnet run -v:m --project ./src/build/build.fsproj -- "$@"
