# Readme


## Requeriments

* Docker
* Docker-compose

## Overview

* Database used: PostgreSQL
* This include the following python libraries:
  ** Flask: web server
  ** request: to consume a REST API
  ** Sqlalchemy: ORM

* Includes the folllowing libraries for NLP:
  ** TextBlob: https://textblob.readthedocs.io/en/dev/index.html
  **           http://rwet.decontextualize.com/book/textblob/

* Includes the following libraries for Machine Learning:
  ** Scipy
  ** scikit-learn
  ** Numpy

* Includes the following libraries for data science and cleaning data:
  ** Pandas

## How to run

* First, build the image for download the pip packages in requeriments.txt.

``` bash
$ docker-compose build
```

* Finally, to run 

``` bash
$ docker-compose up
```

* The webservice is run on localhost:8082