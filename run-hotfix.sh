ulimit -n 1048576
LD_LIBRARY_PATH="/usr/lib64" RUST_MIN_STACK=8192000 ./ic-starter --log-level debug --replica-path ./replica-${1} --create-funds-whitelist '*' --consensus-pool-backend rocksdb --subnet-type application --initial-notary-delay-millis 600 --http-port 8080 &> replica_${1}.txt &
PID=$!
sleep 7
LD_LIBRARY_PATH="" nix-shell --run 'cabal run ic-ref-test -- -j16 --endpoint http://localhost:8080/ -p '\''/hotfix/'\' &> test_${1}.txt
kill -9 ${PID} || true
