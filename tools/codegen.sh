#!/bin/bash

ELM_CODEGEN="node --max-old-space-size=8192 ./node_modules/.bin/elm-codegen run --debug"
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
	    mkdir -p plugins/$PLUGIN_NAME/theme
	    output=`$ELM_CODEGEN --flags="{\"plugin_name\": \"$PLUGIN_NAME\", \"figma_file\": \"$FIGMA_FILE_ID\", \"api_key\": \"$FIGMA_API_TOKEN\"}" --output plugins/$PLUGIN_NAME/theme 2>&1`
    fi
    if [ $? -eq 0 ]; then
        if [[ ! $output =~ "generated" ]]; then
            echo "$output"
            exit 1
        fi
    fi
    echo "$output" | grep "files generated in"
else
    echo "Running codegen ..."
    tmp=`mktemp`.json
    if [ -z "$PLUGIN_NAME" ]; then
        { echo "{\"whitelist\": {\"frames\": $FIGMA_WHITELIST_FRAMES}, \"theme\":"; \
          cat ./theme/figma.json; 
          echo '}'; \
        } > $tmp
    else
        echo "{\"colormaps\": `cat ./theme/colormaps.json`, \"theme\": `cat ./plugins/$PLUGIN_NAME/theme/figma.json`}" > $tmp
    fi
    output=`$ELM_CODEGEN --output theme --flags-from=$tmp 2>&1`
    # surprisingly elm-codegen yield exit code 0 if error
    if [[ ! $output =~ "generated" ]]; then
        echo "$output"
        echo "Input file: $tmp"
        exit 1
    fi
    echo "$output" | grep "files generated in"
    rm $tmp
fi

exit 0
