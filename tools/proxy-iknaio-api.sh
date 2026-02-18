#!/bin/bash
set -euo pipefail

if [[ -z "${GS_API_KEY:-}" ]]; then
    echo "Error: GS_API_KEY is not set. Add to your .bashrc:" >&2
    echo '  export GS_API_KEY="your_key_here"' >&2
    exit 1
fi

docker run --replace -d --name nginx-proxy-iknaio-prod-api -p 8080:80 nginx /bin/bash -c '
cat << "EOF" > /etc/nginx/conf.d/default.conf
server {
    listen 80;

    location / {
        # Handle CORS preflight requests directly
        if ($request_method = OPTIONS) {
            add_header Access-Control-Allow-Origin "http://localhost:3000" always;
            add_header Access-Control-Allow-Methods "GET, POST, PUT, DELETE, OPTIONS" always;
            add_header Access-Control-Allow-Headers "Authorization, Content-Type, Accept, Origin, X-Requested-With" always;
            add_header Access-Control-Allow-Credentials "true" always;
            add_header Access-Control-Max-Age 86400 always;
            add_header Content-Length 0;
            add_header Content-Type "text/plain";
            return 204;
        }

        proxy_pass https://api.iknaio.com;
        proxy_ssl_server_name on;
        proxy_set_header Host api.iknaio.com;
        proxy_set_header Authorization "'"$GS_API_KEY"'";

        # Strip any CORS headers the upstream already sends to avoid duplicates
        proxy_hide_header Access-Control-Allow-Origin;
        proxy_hide_header Access-Control-Allow-Credentials;
        proxy_hide_header Access-Control-Allow-Methods;
        proxy_hide_header Access-Control-Allow-Headers;

        # Set our own CORS headers
        add_header Access-Control-Allow-Origin "http://localhost:3000" always;
        add_header Access-Control-Allow-Credentials "true" always;
    }
}
EOF
exec nginx -g "daemon off;"
'
