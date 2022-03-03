
export PORT=$1
export OUTPUT_DIR=$2

bin/simple_server.sh $PORT $OUTPUT_DIR &
open http://localhost:$PORT/index.html
