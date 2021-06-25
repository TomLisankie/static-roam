# See https://github.com/artpi/roam-research-private-api

# To install the proper version:
# npm i -g git@github.com:dimfeld/roam-research-private-api.git#download-extra-formats --save

echo "Fetching $ROAM_API_GRAPH"
roam-api export --format json --removezip false ~/Downloads

