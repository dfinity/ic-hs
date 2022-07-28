#before fix
wget https://download.dfinity.systems/ic/63c5693ba56960389e88e3814866e7c99a939f73/release/replica.gz
gzip -d replica.gz
chmod +x replica
mv replica replica-63c5693ba56960389e88e3814866e7c99a939f73

# after fix
wget https://download.dfinity.systems/ic/50e34926c12aab19d131b8d43b8921881a2a325a/release/replica.gz
gzip -d replica.gz
chmod +x replica
mv replica replica-50e34926c12aab19d131b8d43b8921881a2a325a

wget https://download.dfinity.systems/ic/50e34926c12aab19d131b8d43b8921881a2a325a/release/ic-starter.gz
gzip -d ic-starter.gz
chmod +x ic-starter

