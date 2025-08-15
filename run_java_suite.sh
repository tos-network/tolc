#!/bin/bash

# Java Suite Test Runner - Segmented Execution
# This script helps run the Java test suite in segments to avoid timeouts

set -e

# Configuration
SEGMENT_SIZE=${JAVA_SUITE_SEGMENT_SIZE:-36}
TOTAL_SEGMENTS=${JAVA_SUITE_TOTAL_SEGMENTS:-10}
FILTER=${JAVA_SUITE_FILTER:-""}
FIRST_ONLY=${JAVA_SUITE_FIRST_ONLY:-""}
MAX_FILES=${TOLC_MAXFILES:-""}
TIMEOUT=${JAVA_SUITE_TIMEOUT:-300}

echo "=== Java Suite Test Runner ==="
echo "Segment size: $SEGMENT_SIZE"
echo "Filter: ${FILTER:-none}"
echo "First only: ${FIRST_ONLY:-false}"
echo "Max files: ${MAX_FILES:-unlimited}"
echo "Timeout: ${TIMEOUT}s"
echo ""

# Function to run a single segment
run_segment() {
    local segment_num=$1
    echo "=== Running Segment $segment_num ==="
    
    export JAVA_SUITE_SEGMENT=$segment_num
    export JAVA_SUITE_SEGMENT_SIZE=$SEGMENT_SIZE
    export JAVA_SUITE_FILTER=$FILTER
    export JAVA_SUITE_FIRST_ONLY=$FIRST_ONLY
    export TOLC_MAXFILES=$MAX_FILES
    export JAVA_SUITE_TIMEOUT=$TIMEOUT
    
    echo "Environment:"
    echo "  JAVA_SUITE_SEGMENT=$JAVA_SUITE_SEGMENT"
    echo "  JAVA_SUITE_SEGMENT_SIZE=$JAVA_SUITE_SEGMENT_SIZE"
    echo "  JAVA_SUITE_FILTER=$JAVA_SUITE_FILTER"
    echo "  JAVA_SUITE_FIRST_ONLY=$JAVA_SUITE_FIRST_ONLY"
    echo "  TOLC_MAXFILES=$TOLC_MAXFILES"
    echo "  JAVA_SUITE_TIMEOUT=$JAVA_SUITE_TIMEOUT"
    echo ""
    
    # Run the test with timeout (macOS compatible)
    if command -v timeout >/dev/null 2>&1; then
        timeout $TIMEOUT cargo test --test java_suite -- --nocapture
    else
        # macOS doesn't have timeout, use gtimeout if available, otherwise no timeout
        if command -v gtimeout >/dev/null 2>&1; then
            gtimeout $TIMEOUT cargo test --test java_suite -- --nocapture
        else
            echo "Warning: timeout command not available, running without timeout"
            cargo test --test java_suite -- --nocapture
        fi
    fi
    
    local exit_code=$?
    if [ $exit_code -eq 124 ]; then
        echo "❌ Segment $segment_num timed out after ${TIMEOUT}s"
        return 1
    elif [ $exit_code -eq 0 ]; then
        echo "✅ Segment $segment_num completed successfully"
        return 0
    else
        echo "❌ Segment $segment_num failed with exit code $exit_code"
        return $exit_code
    fi
}

# Function to count total Java files
count_java_files() {
    local count=$(find tests/java -name "*.java" | wc -l)
    echo $count
}

# Function to calculate total segments needed
calculate_total_segments() {
    local total_files=$(count_java_files)
    local segments=$(( ($total_files + $SEGMENT_SIZE - 1) / $SEGMENT_SIZE ))
    echo $segments
}

# Main execution
if [ "$1" = "count" ]; then
    # Just count files and segments
    total_files=$(count_java_files)
    total_segments=$(calculate_total_segments)
    echo "Total Java files: $total_files"
    echo "Total segments needed: $total_segments"
    echo "Segment size: $SEGMENT_SIZE"
    exit 0
fi

if [ "$1" = "list" ]; then
    # List all Java files
    echo "Java files in tests/java:"
    find tests/java -name "*.java" | sort
    exit 0
fi

if [ "$1" = "segment" ] && [ -n "$2" ]; then
    # Run specific segment
    segment_num=$2
    run_segment $segment_num
    exit $?
fi

if [ "$1" = "all" ]; then
    # Run all segments
    total_segments=$(calculate_total_segments)
    echo "Running all $total_segments segments..."
    
    failed_segments=()
    for ((i=0; i<$total_segments; i++)); do
        if ! run_segment $i; then
            failed_segments+=($i)
        fi
        echo ""
    done
    
    if [ ${#failed_segments[@]} -eq 0 ]; then
        echo "🎉 All segments completed successfully!"
        exit 0
    else
        echo "❌ Failed segments: ${failed_segments[*]}"
        exit 1
    fi
fi

if [ "$1" = "range" ] && [ -n "$2" ] && [ -n "$3" ]; then
    # Run range of segments
    start_segment=$2
    end_segment=$3
    echo "Running segments $start_segment to $end_segment..."
    
    failed_segments=()
    for ((i=$start_segment; i<=$end_segment; i++)); do
        if ! run_segment $i; then
            failed_segments+=($i)
        fi
        echo ""
    done
    
    if [ ${#failed_segments[@]} -eq 0 ]; then
        echo "🎉 Range segments completed successfully!"
        exit 0
    else
        echo "❌ Failed segments: ${failed_segments[*]}"
        exit 1
    fi
fi

# Default: run first segment
if [ -z "$1" ]; then
    echo "Running first segment (default)..."
    run_segment 0
    exit $?
fi

# Help
echo "Usage: $0 [command] [args...]"
echo ""
echo "Commands:"
echo "  (no args)     Run first segment (default)"
echo "  count         Count total files and segments"
echo "  list          List all Java files"
echo "  segment N     Run specific segment N"
echo "  all           Run all segments"
echo "  range N M     Run segments N through M"
echo ""
echo "Environment variables:"
echo "  JAVA_SUITE_SEGMENT_SIZE    Files per segment (default: 10)"
echo "  JAVA_SUITE_FILTER          Filter files by name"
echo "  JAVA_SUITE_FIRST_ONLY      Run only first file in segment"
echo "  TOLC_MAXFILES              Maximum files to process"
echo "  JAVA_SUITE_TIMEOUT         Timeout per segment in seconds (default: 300)"
echo ""
echo "Examples:"
echo "  $0                           # Run first segment"
echo "  $0 segment 2                # Run segment 2"
echo "  $0 range 0 2                # Run segments 0, 1, 2"
echo "  $0 all                      # Run all segments"
echo "  JAVA_SUITE_SEGMENT_SIZE=5 $0 segment 1  # Run segment 1 with 5 files"
echo "  JAVA_SUITE_FILTER=Byte $0   # Run first segment with Byte filter"
