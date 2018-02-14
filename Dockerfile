FROM python:2.7
ADD . /code

# Deps
RUN apt update && apt install swi-prolog-nox libpq-dev -y
RUN curl -sSL https://get.haskellstack.org/ | sh 

# This is after the apt install to cache the previous step
WORKDIR /code/

RUN pip install -r requirements.txt

# Haskell
RUN cd bussines/ && stack build --fast

EXPOSE 8082
