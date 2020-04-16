#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2034

app_usage() {
        cat <<EOF

Run this like:

   scripts/mainnet-via-fetcher.sh --epochs 3 --nix --profile

This works in two phases, prefetch and benchmark:

   Phase 1:  preload the prefetcher's ChainDB, up to desired limit.

   Phase 2:  run the second node as the benchmark, up to desired slot limit,
             while feeding it blocks from the preloaded local prefetcher,
             while also preventing said prefetcher from syncing mainnet further.

  App options (going BEFORE common options OR IGNORED):

    --node-config-default / --node-config-silent
                                enable/disable tracing
    --node-config-both          two benchmark runs -- silent and not
    --epochs / --slots          at which point to stop
    --skip-prefetch             skip the prefetch phase
    --prebench-pause            request the user to hit 'Enter' between phases
    --server-wait SECONDS       delay the benchmarked (client) side to start,
                                  for this long -- to avoid profile disturbance.
                                  Defaults to ${server_wait} seconds.
    --no-server-wait            same as --server-wait 0
    --skip-benchmark            skip the benchmark phase
    --repeats COUNT             repeat the chosen benchmark runs, default=1

  Common options (going AFTER common options or LATTER ARE IGNORED):

    --profile MODE              MODE profiling of the benchmarked node:
       time space space-module space-closure space-type space-retainer space-bio
    --nix / --cabal / --stack   pick your poison
    --help                      see for more common options

EOF
}

epoch_limit=3
mainnet_k=2160
mainnet_epoch_slots=$((10 * mainnet_k))
slot_limit=
skip_prefetch=
prebench_pause=
benchmark_configs=('mainnet-via-fetcher')
server_wait=$((epoch_limit*18+10))
repeats=1

while test -n "$1"
do case "$1" in
           --node-config-normal )
                               benchmark_configs=('mainnet-via-fetcher');;
           --node-config-silent )
                               benchmark_configs=('mainnet-silent');;
           --node-config-both )
                               benchmark_configs=('mainnet-via-fetcher'
                                                  'mainnet-silent');;
           ## The server node takes a while to validate its
           --server-wait )     server_wait=$2; shift;;
           --no-server-wait | --no-wait )
                               server_wait=0;;
           --epochs )          epoch_limit=$2; shift;;
           --slots )           slot_limit=$2; shift;;
           --prebench-pause )  prebench_pause=t;;
           --skip-prefetch )   skip_prefetch=t;;
           --skip-benchmark )  benchmark_configs=();;
           --repeats )         repeats=$2; shift;;
           --app-help )        app_usage; exit 1;;
           ## Remaining options handled in 'lib/common.sh'
           * )                 break;; esac; shift; done

. "$(dirname "$0")"/common.sh
. "$(dirname "$0")"/lib-cli.sh
. "$(dirname "$0")"/lib-node.sh

REMAINING_TOP_ARGS=("$@")

shutdown() {
        pkill --pgroup $$ 2>/dev/null
}
## Terminate children, whatever they may be..
trap shutdown EXIT

phase_prefetch()
{
        oprint "prefetching up to ${limit_desc} worth of mainnet to local ChainDB.."
        local RUN_NODE_ARGS=(
                --no-profile
                --no-stats
                --config-name     'mainnet-silent'
                --topology-name   'mainnet'
                --state           'mainnet'
                ## non-run_node args follow:
                "${REMAINING_TOP_ARGS[@]}"
                --shutdown-on-slot-synced "${slot_limit}"
        )
        if run_node_quiet "${RUN_NODE_ARGS[@]}"
        then oprint "local ChainDB preloaded with mainnet chain up to ${limit_desc}"
        else
                 echo -e '\n' >&2
                 pgrep -fal 'cardano-node'
                 echo -e '\nMainnet prefetch failed, perhaps some unexpected "cardano-node" processes are running (see above)?\nIf so, you may consider:  pkill cardano-node' >&2
                 exit 1
        fi
}

## Launch the server side of the local chainsync benchmark.
start_preloaded_server_node()
{
        local SERVER_RUN_NODE_ARGS=(
                --no-profile
                --no-stats
                --config-name     'mainnet-silent'
                --topology-name   'excommunicated'
                --state           'mainnet'
                --port            3002
                "${REMAINING_TOP_ARGS[@]}"
        )
        oprint "starting local ChainSync server.."
        run_node_quiet "${SERVER_RUN_NODE_ARGS[@]}"
}

## Run a chainsync benchmark for:
##  1. the specified configuration
##  2. using the 'mainnet-via-fetcher' topology
benchmark_config()
{
        local benchmarked_config="$1"
        local iteration="$2"

        rm -rf "db/${benchmarked_config}"

        BENCHMARKED_RUN_NODE_ARGS=(
                --config-name     "${benchmarked_config}"
                --topology-name   'mainnet-via-fetcher'
                --state           "${benchmarked_config}"
                --profile-suffix  "${slot_limit}slots.${benchmarked_config}.run${iteration}"
                "${REMAINING_TOP_ARGS[@]}"
                ## non-run_node args follow:
                --shutdown-on-slot-synced "${slot_limit}"
        )

        oprint "starting local ChainSync benchmark for ${limit_desc}, mode ${benchmarked_config}.."
        run_node_quiet "${BENCHMARKED_RUN_NODE_ARGS[@]}"
}

###
### Main
###

if test -n "${REMAINING_TOP_ARGS[*]}"
then oprint "WARNING:"
     oprint "WARNING:  unhandled top-level args:  ${REMAINING_TOP_ARGS[*]}"
     oprint "WARNING:  they will be passed to run_node, which can break things"
     oprint "WARNING:                                   ^^^^^^^^^^^^^^^^^^^^^^"; fi

if test -z "${slot_limit}"
then slot_limit=$((epoch_limit * mainnet_epoch_slots))
     limit_desc=" epoch ${epoch_limit} (${slot_limit} slots)"
else limit_desc=${slot_limit}' slots'
fi

oprint "benchmark schedule:  ${repeats} of configs ${benchmark_configs[*]}"

#
#  Phase 1:  preload the prefetcher's ChainDB, up to desired limit.
#
if test -z "${skip_prefetch}"
then time -p phase_prefetch
else prebuild "cardano-node"; fi

test -n "${prebench_pause}" &&
  read -rp "Mainnet prefetch complete. Press Enter to continue..." foo

start_preloaded_server_node &

sleep 0.3
if test "${server_wait}" != 0
then oprint "waiting for ${server_wait} seconds, to let the server process ChainDB.."
     sleep "${server_wait}"; fi

#
#  Phase 2:  run the second node as the benchmark, up to desired slot limit,
#            while feeding it blocks from the local prefetcher,
#            while also preventing said prefetcher from syncing mainnet further.
#
for i in $(seq 1 "${repeats}")
do for bench_conf in "${benchmark_configs[@]}"
   do time -p benchmark_config "${bench_conf}" "${i}"; done; done
