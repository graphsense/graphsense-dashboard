#!/bin/bash
set -euo pipefail

usage() {
    cat <<USAGE
Usage: $0 [--test] [--port PORT]

  --test       proxy api.test.iknaio.com instead of api.iknaio.com, on port 8081
               by default; uses GS_TEST_API_KEY if set, else GS_API_KEY
  --port PORT  local port to listen on (default: 8080, or 8081 with --test)

GS_UPSTREAM overrides the upstream host in either mode.
USAGE
}

test_mode=false
port=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        --test) test_mode=true; shift ;;
        --port) port="${2:?--port needs a value}"; shift 2 ;;
        -h|--help) usage; exit 0 ;;
        *) echo "Error: unknown argument: $1" >&2; usage >&2; exit 1 ;;
    esac
done

if $test_mode; then
    upstream="${GS_UPSTREAM:-api.test.iknaio.com}"
    port="${port:-8081}"
    api_key="${GS_TEST_API_KEY:-${GS_API_KEY:-}}"
    key_var=GS_TEST_API_KEY
    container=nginx-proxy-iknaio-test-api
else
    upstream="${GS_UPSTREAM:-api.iknaio.com}"
    port="${port:-8080}"
    api_key="${GS_API_KEY:-}"
    key_var=GS_API_KEY
    container=nginx-proxy-iknaio-prod-api
fi

if [[ -z "$api_key" ]]; then
    echo "Error: $key_var is not set. Add to your .bashrc:" >&2
    echo "  export $key_var=\"your_key_here\"" >&2
    exit 1
fi

echo "Proxying https://$upstream on http://localhost:$port (container $container)"

docker run --rm -d --name "$container" -p "$port:80" nginx /bin/bash -c '
cat << "EOF" > /etc/nginx/conf.d/default.conf

map $http_origin $cors_origin {
    default "";
    "~^https?://([^/]+\.)?(.+)$" $http_origin;
}

server {
    listen 80;

    location / {
        # Handle CORS preflight requests directly
        if ($request_method = OPTIONS) {
            add_header Access-Control-Allow-Origin $cors_origin always;
            add_header Access-Control-Allow-Methods "GET, POST, PUT, DELETE, OPTIONS" always;
            add_header Access-Control-Allow-Headers "Authorization, Content-Type, Accept, Origin, X-Requested-With" always;
            add_header Access-Control-Allow-Credentials "true" always;
            add_header Access-Control-Max-Age 86400 always;
            add_header Content-Length 0;
            add_header Content-Type "text/plain";
            return 204;
        }

        proxy_pass https://'"$upstream"';
        proxy_ssl_server_name on;
        proxy_set_header Host '"$upstream"';
        proxy_set_header Authorization "'"$api_key"'";

        # Strip any CORS headers the upstream already sends to avoid duplicates
        proxy_hide_header Access-Control-Allow-Origin;
        proxy_hide_header Access-Control-Allow-Credentials;
        proxy_hide_header Access-Control-Allow-Methods;
        proxy_hide_header Access-Control-Allow-Headers;

        # Set our own CORS headers
        add_header Access-Control-Allow-Origin $cors_origin always;
        add_header Access-Control-Allow-Credentials "true" always;
    }
}
EOF
exec nginx -g "daemon off;"
'
