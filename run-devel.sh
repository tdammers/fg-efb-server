CACHE_DIR=./cache \
    ghcid \
    --test Main.main \
    --reload=providers.yaml \
    --restart=fg-efb-server.cabal \
    --restart=cabal.project \
    --restart=static/style.xsl \
    --restart=static/style.css \
    -c cabal v2-repl fg-efb-server.cabal
