source bin/setup.sh

export PORT=$1

bin/simple_server.sh   $PORT $ROAM_OUTPUT_DIR &
open http://localhost:$PORT/index.html
