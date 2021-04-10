source bin/setup.sh
export PORT=1777
cd $ROAM_OUTPUT_DIR		# TODO needs to default
python3 -m http.server $PORT &
open http://localhost:$PORT/index.html
