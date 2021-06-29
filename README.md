### kadena-crypto-ghcjs

A ghcjs wrapper around cardano-crypto.js (containing kadena-specific changes)


##### Running the test exe
```
nix-build
cd result/bin/test.jsexe/
chromium index.html
```

##### Hacking
The underlying js library is actually a wrapper around C-code that gets compiled to nodejs with
emscripten. Browserify then converts the nodejs code to code that can run in the browser 
```
nix-thunk unpack dep/cardano-crypto.js
cd dep/cardano-crypto.js
nix-shell
npm install
./dev
cd ../../
cp dep/cardano-crypto.js/kadena-crypto.js js-bits/
```
