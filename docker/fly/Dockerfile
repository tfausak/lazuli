FROM ubuntu:23.04

RUN \
  set -o errexit -o xtrace; \
  apt-get update; \
  apt-get install --assume-yes --no-install-recommends ca-certificates

ARG USER_NAME=lazuli
RUN useradd --create-home "$USER_NAME"
USER "$USER_NAME"
WORKDIR "/home/$USER_NAME"

ARG LAZULI_COMMIT
ENV LAZULI_COMMIT="$LAZULI_COMMIT"

COPY data data
ENV LAZULI_DATA_DIRECTORY=/home/lazuli/data

COPY lazuli /usr/local/bin

CMD lazuli
