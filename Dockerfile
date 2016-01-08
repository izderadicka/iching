FROM ubuntu:14.04
MAINTAINER ivan.zderadicka@gmail.com

RUN apt-get install -y software-properties-common &&\
    apt-add-repository -y ppa:avsm/ppa &&\
    apt-get update &&\
    apt-get install -y ocaml opam camlp4 camlp4-extra pkg-config libgdbm-dev libpcre3-dev libcairo2-dev libsqlite3-dev m4 make libssl-dev   

RUN opam init -a &&\
    opam install -y cairo2 uuidm sqlite3 &&\
    opam install -y eliom 

RUN git clone --depth 1 https://github.com/izderadicka/iching.git /tmp/iching &&\
    cd /tmp/iching &&\
    . /root/.opam/opam-init/init.sh > /dev/null 2> /dev/null &&\
    make opt &&\
    make install.opt &&\
    rm -r /tmp/iching

EXPOSE 8088

VOLUME ["/usr/local/var/log/iching", "/usr/local/var/data/iching/"]

CMD . /root/.opam/opam-init/init.sh && ocsigenserver.opt -c /usr/local/etc/iching/iching.conf

