PDFCACHE=./cache \
    ghcid \
    --test Main.runServer \
    --reload=providers.yaml \
    --restart=fg-efb-server.cabal \
    --restart=cabal.project \
    -c cabal v2-repl fg-efb-server.cabal
