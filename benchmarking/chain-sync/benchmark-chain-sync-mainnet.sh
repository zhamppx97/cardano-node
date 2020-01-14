#!/usr/bin/env bash

# >> time limit in seconds
TIME_LIMIT=$((10*60))

BASEDIR=$(realpath $(dirname $0))

DATADIR=state-node-mainnet
mkdir -p $DATADIR
cd $DATADIR

# remove blockchain
if [ -d db-mainnet-0 ]; then
  rm -rf db-mainnet-0
fi

# remove old log files
rm node-0*

#set -euo pipefail

# adjust for nicer colors
TERM=xterm-256color

date --iso-8601=seconds > STARTTIME

NODE="cabal v2-run exe:cardano-node -- "
NODE="stack --nix --profile exec cardano-node -- "
NODE="${BASEDIR}/../../../../bin/cardano-node "

exec timeout $TIME_LIMIT ${NODE} \
  --genesis-file ${BASEDIR}/../../configuration/mainnet-genesis.json \
  --genesis-hash "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb" \
  --config ${BASEDIR}/configuration/log-configuration.yaml \
  --database-path .//db-mainnet \
  --socket-dir /tmp/socket-bm-chain-sync \
  --topology ${BASEDIR}/configuration/topology-local.yaml \
  --host-addr 127.0.0.1 \
  --port 7778 \
  --tracing-verbosity-maximal \
  --trace-block-fetch-decisions \
  --trace-mempool \
  --trace-chain-db \
  --trace-forge \
  \
  +RTS -hc -L90 -p -N1 -A10m -qg -qb -M3G -RTS \
  \
 $@ & 

#  +RTS -hc -N2 -A10m -qg -qb -M3G -RTS \

sleep 2
ps x | grep cardano-node | grep log-configuration.yaml | grep -v timeout
sleep 2
ps x | grep cardano-node | grep log-configuration.yaml | grep -v timeout
sleep 1
CARDANOPID=$(ps x | grep cardano-node | grep log-configuration.yaml | grep -v timeout | sed -ne 's/^ *\([0-9]\+\) .*/\1/p;')
echo $CARDANOPID

set -e
echo "memory (MB) every 3 seconds"
while [ TRUE ]; do PAGES=`cat /proc/${CARDANOPID}/statm | cut -d ' ' -f 2`; echo $((PAGES * 4096 / 1024 / 1024)) ; sleep 3; done

#  +RTS -s -N2 -G3 -A10m -qg2 -qb -M1G -RTS \
#  +RTS -h  -RTS \
#  +RTS -xc  -RTS \
#  --socket-dir ${BASEDIR}/${DATADIR}/socket \
# this will render the events in textual format
#  --trace-chain-db \

#  --trace-block-fetch-client \
#  --trace-chain-sync-protocol \
#  --trace-block-fetch-protocol \
#  --trace-block-fetch-decisions \
#  --trace-block-fetch-server \
#  --trace-chain-sync-header-server \
#  --trace-tx-inbound \
#  --trace-tx-outbound \
#  --trace-local-tx-submission-server \
#  --trace-local-chain-sync-protocol \
#  --trace-tx-submission-protocol \
#  --trace-local-tx-submission-protocol \
#  --trace-ip-subscription \
#  --trace-dns-subscription \
#  --trace-dns-resolver \

../analyse-logs.sh
