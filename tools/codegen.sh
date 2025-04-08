#!/bin/bash

ELM_CODEGEN="npx --node-options='--max-old-space-size=32768' elm-codegen run --debug"
REFRESH=0
FIGMA_WHITELIST_FRAMES=[]

source .env

for i in "$@"
do
case $i in
    -f=*|--file-id=*)
    FIGMA_FILE_ID="${i#*=}"
    ;;

    -a=*|--api-token=*)
    FIGMA_API_TOKEN="${i#*=}"
    ;;

    -w=*|--whitelist=*)
    FIGMA_WHITELIST_FRAMES="${i#*=}"

    ;;
    --refresh)
    REFRESH=1
    ;;

    -p=*|--plugin=*)
    PLUGIN_NAME="${i#*=}"
    ;;

    *)
            # unknown option
    ;;
esac
done

if [ $REFRESH -eq 1 ]; then
    if [ -z "$FIGMA_FILE_ID" ]; then
        echo "-f <figma file id> required"
        exit 1
    fi

    if [ -z "$FIGMA_API_TOKEN" ]; then
        echo "-a <figma api token> required"
        exit 1
    fi

    echo "Refreshing figma file from $FIGMA_FILE_ID ..."
    if [ -z "$PLUGIN_NAME" ]; then
	    mkdir -p theme
        output=`$ELM_CODEGEN --flags="{\"figma_file\": \"$FIGMA_FILE_ID\", \"api_key\": \"$FIGMA_API_TOKEN\"}" --output theme 2>&1`
    else
        PLUGIN_FIGMA="./plugins/$PLUGIN_NAME/theme/figma.json"
        if [ -e "$PLUGIN_FIGMA" ]; then
	        output=`$ELM_CODEGEN --flags="{\"plugin_name\": \"$PLUGIN_NAME\", \"figma_file\": \"$FIGMA_FILE_ID\", \"api_key\": \"$FIGMA_API_TOKEN\"}" --output plugins/$PLUGIN_NAME/theme 2>&1`
        else
            echo "No $PLUGIN_FIGMA found to refresh. Exiting."
            exit 0
        fi
    fi
    if [ $? -eq 0 ]; then
        if [[ ! $output =~ "generated" ]]; then
            echo "$output"
            exit 1
        fi
    fi
    echo "$output" | grep "generated"
else
    echo "Running codegen ..."
    tmp=`mktemp`.json
    if [ -z "$PLUGIN_NAME" ]; then
        { echo "{\"whitelist\": {\"frames\": $FIGMA_WHITELIST_FRAMES}, \"theme\":"; \
          cat ./theme/figma.json; 
          echo '}'; \
        } > $tmp
    else
        PLUGIN_FIGMA="./plugins/$PLUGIN_NAME/theme/figma.json"
        if [ -e "$PLUGIN_FIGMA" ]; then
            echo "{\"colormaps\": `cat ./generated/theme/colormaps.json`, \"theme\": `cat $PLUGIN_FIGMA`}" > $tmp
        else
            echo "No $PLUGIN_FIGMA found. Skipping."
            exit 0
        fi
    fi
    cmd="$ELM_CODEGEN --output ./generated/theme --flags-from=$tmp"
    echo $cmd
    out=`mktemp`.out
    $cmd 2>&1 > $out
    found=`grep "files generated in" $out`
    # surprisingly elm-codegen yield exit code 0 if error
    if [ -z "$found" ]; then
        echo "Printing the first 100 lines of output:"
        head -n 100 $out
        echo "The full output can be inspected in $out"
        echo "Input file: $tmp"
        exit 1
    fi
    echo "$found"
    rm $tmp $out
fi

exit 0
