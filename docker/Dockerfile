FROM ubuntu:22.04

ARG USER_NAME=lazuli
RUN useradd --create-home "$USER_NAME"
USER "$USER_NAME"
WORKDIR "/home/$USER_NAME"

COPY lazuli /usr/local/bin

CMD lazuli
