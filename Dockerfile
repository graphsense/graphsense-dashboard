FROM debian:8
LABEL maintainer=mihai.bartha@ait.ac.at

################## BEGIN INSTALLATION ######################
RUN apt-get update --fix-missing && \
    apt-get install -y python3 python3-pip nginx upstart
RUN pip3 install --upgrade pip
RUN mkdir -p /srv/graphsense-dashboard
ADD ./requirements.txt /srv/graphsense-dashboard
RUN cd /srv/graphsense-dashboard; pip3 install -r requirements.txt
ADD ./ /srv/graphsense-dashboard/
# connect dashboard to docker bridge network
RUN sed -ie 's/localhost/172.17.0.1/g' /srv/graphsense-dashboard/dashboard.py
ADD ./uwsgi /etc/init.d/uwsgi
ADD ./dashboard /etc/nginx/sites-available/dashboard
RUN ln -s /etc/nginx/sites-available/dashboard /etc/nginx/sites-enabled
RUN rm /etc/nginx/sites-enabled/default

CMD /etc/init.d/uwsgi start && /etc/init.d/nginx start && bash
##################### INSTALLATION END #####################
