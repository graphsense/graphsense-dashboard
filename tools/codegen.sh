#!/bin/bash

#ELM_CODEGEN="node --max-old-space-size=8192 ./node_modules/.bin/elm-codegen run"
ELM_CODEGEN="npx --node-options='--max-old-space-size=16384' elm-codegen run"
REFRESH=0
FIGMA_WHITELIST_FRAMES=[]
PLUGIN_NAME=""

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

tmp=`mktemp -u`
tmp_json=$tmp.json
tmp_out=$tmp.out
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
        echo "{\"figma_file\": \"$FIGMA_FILE_ID\", \"api_key\": \"$FIGMA_API_TOKEN\"}" > $tmp_json
        cmd="$ELM_CODEGEN --flags-from=$tmp_json --output theme"
    else
        PLUGIN_FIGMA="./plugins/$PLUGIN_NAME/theme/figma.json"
        if [ -e "$PLUGIN_FIGMA" ]; then
            echo "{\"plugin_name\": \"$PLUGIN_NAME\", \"figma_file\": \"$FIGMA_FILE_ID\", \"api_key\": \"$FIGMA_API_TOKEN\"}" > $tmp_json
	        cmd="$ELM_CODEGEN --flags-from=$tmp_json --output plugins/$PLUGIN_NAME/theme" 
        else
            echo "No $PLUGIN_FIGMA found to refresh. Exiting."
            exit 0
        fi
    fi
else
    echo "Running codegen ..."
    if [ -z "$PLUGIN_NAME" ]; then
        { echo "{\"whitelist\": {\"frames\": $FIGMA_WHITELIST_FRAMES, \"components\":[]}, \"theme\":"; \
          cat ./theme/figma.json; 
          echo '}'; \
        } > $tmp_json
    else
        PLUGIN_FIGMA="./plugins/$PLUGIN_NAME/theme/figma.json"
        if [ -e "$PLUGIN_FIGMA" ]; then
            echo "{\"colormaps\": `cat ./generated/theme/colormaps.json`, \"theme\": `cat $PLUGIN_FIGMA`}" > $tmp_json
        else
            echo "No $PLUGIN_FIGMA found. Skipping."
            exit 0
        fi
    fi
    cmd="$ELM_CODEGEN --output ./generated/theme --flags-from=$tmp_json"
fi
echo $cmd
$cmd 2>&1 > $tmp_out
grep DEBUG $tmp_out
found=`grep -E '(files generated in|was generated!)' $tmp_out`
if [ -z "$found" ]; then
    echo "Printing the first 100 lines of output:"
    head -n 100 $tmp_out
    echo "The full output can be inspected in $tmp_out"
    echo "Input file: $tmp_json"
    exit 1
fi
echo "$found"
rm $tmp_json $tmp_out

exit 0
