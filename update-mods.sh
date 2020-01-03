find geopolitik-backend/src geopolitik-backend/app -name "*.hs" | xargs graphmod --no-cluster -q | dot -Tpng -o mods.png -Gdpi=500
