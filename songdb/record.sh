#!/bin/bash

# Record audio from microphone and find matching in the database,
# e.g. a poor man's Shazam for Amiga and tracker music

# Requires at least scala-cli, sox and chromaprint (fpcalc)
# on macOS you also need to give microphone permission to the terminal app

# see audio_match.sc for more details

# NOTE: run ./audio_match.sc once before running ./record.sh

if [ $# -eq 0 ]; then
    echo "Usage: $0 [timeout] [threshold] [max_results]"
    echo ""
    echo "Record audio from microphone and find matching songs in the database"
    echo ""
    echo "Parameters:"
    echo "  timeout     - seconds to record (0 for interactive mode)"
    echo "  threshold   - similarity threshold (default: 0.67)"
    echo "  max_results - maximum number of results to return (default: 10)"
    echo ""
    echo "Examples:"
    echo "  $0 0            # Interactive recording with defaults"
    echo "  $0 30           # Record for 30 seconds"
    echo "  $0 30 0.8 5     # Record for 30 seconds, threshold 0.8, max 5 results"
    exit 0
fi

TIMEOUT=${1:-0}
THRESHOLD=${2:-0.67}
MAX_RESULTS=${3:-10}

if ! command -v sox &> /dev/null; then
    echo "sox could not be found in PATH, please install it first."
    exit 1
fi

if ! command -v fpcalc &> /dev/null; then
    echo "fpcalc could not be found in PATH, please install chromaprint first."
    exit 1
fi

if [ "$TIMEOUT" -ge 3 ]; then
    echo "Starting recording for $TIMEOUT seconds..."
elif [ "$TIMEOUT" -lt 3 ] && [ "$TIMEOUT" -ne 0 ]; then
    echo "Timeout must be at least 3 seconds or use 0 for interactive recording."
    exit 1
else
    echo "Press ENTER to start recording..."
    read
fi

mkdir -p /tmp/audacious-uade-tools
TEMP_FILE=/tmp/audacious-uade-tools/audio_XXXXXX.wav
rm -f "$TEMP_FILE"

if [ "$TIMEOUT" -gt 0 ]; then
    echo "Recording for $TIMEOUT seconds... Press Ctrl+C to stop early"

    sox -d "$TEMP_FILE" &
    SOX_PID=$!
    
    (sleep "$TIMEOUT" && kill $SOX_PID 2>/dev/null) &
    wait $SOX_PID 2>/dev/null
    
    echo "Recording finished. Processing fingerprint..."
    fpcalc -length 9999 -plain "$TEMP_FILE" | ./audio_match.sc - $THRESHOLD $MAX_RESULTS
else
    echo "Recording... Press Ctrl+C to stop and process"
    trap 'echo "Processing fingerprint..."; fpcalc -length 9999 -plain "$TEMP_FILE" \
                                          | ./audio_match.sc - $THRESHOLD $MAX_RESULTS; \
                                          rm "$TEMP_FILE"; exit' INT
 
    sox -d "$TEMP_FILE" &
    SOX_PID=$!
    
    wait $SOX_PID
fi
rm "$TEMP_FILE"
