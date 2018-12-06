FROM alpine:3.7
LABEL maintainer="rainer.stuetz@ait.ac.at"

RUN mkdir -p /srv/graphsense-dashboard/
COPY requirements.txt /srv/graphsense-dashboard/

RUN apk --no-cache --update add bash python3 uwsgi-python3 nginx supervisor && \
    apk --no-cache --update --virtual build-dependendencies add \
    gcc \
    linux-headers \
    musl-dev \
    pcre-dev \
    python3-dev && \
    python3 -m ensurepip && \
    rm -r /usr/lib/python*/ensurepip && \
    rm /etc/nginx/conf.d/default.conf && \
    pip3 install --upgrade pip setuptools && \
    pip3 install -r /srv/graphsense-dashboard/requirements.txt && \
    apk del build-dependendencies && \
    rm -rf /root/.cache

COPY conf/nginx.conf /etc/nginx/
COPY conf/graphsense-dashboard.conf /etc/nginx/conf.d/graphsense-dashboard.conf
COPY conf/supervisor-app.conf /etc/supervisor/conf.d/
COPY conf/graphsense-dashboard.ini *.py /srv/graphsense-dashboard/
COPY static /srv/graphsense-dashboard/static
COPY templates /srv/graphsense-dashboard/templates

# connect dashboard to docker bridge network
RUN sed -ie 's/localhost/172.17.0.1/g' /srv/graphsense-dashboard/dashboard.py

CMD ["supervisord", "-n", "-c", "/etc/supervisor/conf.d/supervisor-app.conf"]
