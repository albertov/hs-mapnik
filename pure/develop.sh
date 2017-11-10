#!/usr/bin/env bash
exec ghcid --command='ghci' --warnings --test=main "$@"
