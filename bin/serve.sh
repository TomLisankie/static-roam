export PORT=1779
cd $ROAM_OUTPUT_DIR		# TODO needs to default
python3 -m http.server $PORT &
open http://localhost:$PORT/index.html
