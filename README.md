Interactive web application to present I Ching - book of changes.
Using not so common technology stack Ocaml + Ocsigen. 
I made it as a learnig exercise to get acknowledged with Ocsigen framework.

You can check it online [here](http://iching.zderadicka.eu).



Prerequisites
-------------

```
apt-get install -y software-properties-common
apt-add-repository -y ppa:avsm/ppa 
apt-get update
apt-get install -y ocaml opam camlp4 camlp4-extra pkg-config libgdbm-dev libpcre3-dev libcairo2-dev libsqlite3-dev m4 make libssl-dev   

opam init -a
opam install -y cairo2 uuidm sqlite3
opam install -y eliom


```

Install And Run 
---------------

```
#Check and edit Makefile.options to customize build and deployment
make all
make install.opt

ocsigenserver -c /usr/local/etc/iching/iching.conf
```

Docker
------
There is also [Dockerfile](https://github.com/izderadicka/iching/blob/master/Dockerfile) - so you can easily run in docker container, download it to some directory and run :
```
 docker build -t iching .
 docker run --name iching -p 8088:8088 iching
```

Openshift
---------
You can also easily deploy on openshift:
```
#set rhc as described in openshift docs
rhc app create --no-git iching https://raw.githubusercontent.com/izderadicka/ocaml-openshift2/master/metadata/manifest.yml
# note the app git repo url
git add remote openshift app_openshift_git_repo_url
git pull openshift
# resolve eventual conflicts
git push openshift
```

License
-------

Code is [GPL v3 license](http://www.gnu.org/copyleft/gpl.html),  I Ching text in Wilhems-Baynes translation is publick domain to my best knowledge.
