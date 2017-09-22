FROM ubuntu:14.04
MAINTAINER ivan.zderadicka@gmail.com

ENV http_proxy=http://emea-proxy.uk.oracle.com:80
ENV https_proxy=http://emea-proxy.uk.oracle.com:80

RUN apt-get update &&\
    apt-get install -y software-properties-common &&\
    apt-get install -y pkg-config libgdbm-dev libpcre3-dev libcairo2-dev libsqlite3-dev m4 make libssl-dev libgmp-dev  &&\
    apt-add-repository -y ppa:avsm/ppa &&\
    apt-get update &&\
    apt-get install -y opam

RUN adduser iching &&\
    mkdir /tmp/iching &&\
    chown iching:0 /tmp/iching &&\
    mkdir /opt/iching &&\
    chown iching:0 /opt/iching

ENV TMPDIR=/tmp
USER iching

RUN opam init --compiler=4.02.3 &&\
    eval `opam config env` &&\
    opam install -y camlp4 "cairo2<0.5" uuidm sqlite3 "eliom=5.0.0"


COPY / /tmp/iching
USER root
RUN chown -R iching:iching /tmp/iching
USER iching

RUN cd /tmp/iching &&\
    export PREFIX=/opt/iching/ &&\
    eval `opam config env` &&\
    make clean &&\
    make opt &&\
    make install.opt &&\
    rm -rf /tmp/iching

RUN chmod -R a+rw /opt/iching/var

EXPOSE 8088

VOLUME ["/opt/iching/var/log/iching", "/opt/iching/var/data/iching/"]

CMD CAML_LD_LIBRARY_PATH="/home/iching/.opam/4.02.3/lib/stublibs" /home/iching/.opam/4.02.3/bin/ocsigenserver.opt -c /opt/iching/etc/iching/iching.conf 

