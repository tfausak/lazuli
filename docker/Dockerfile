FROM ubuntu:22.04

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

COPY lazuli /usr/local/bin

CMD lazuli
