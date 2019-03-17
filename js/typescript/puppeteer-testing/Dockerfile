FROM ubuntu:18.04

RUN apt-get update \
  && apt-get install -y curl git rsync

RUN addgroup ubuntu \
  && useradd -d /home/ubuntu -ms /bin/bash -g root -G sudo -G ubuntu -p ubuntu ubuntu \
  && echo 'ubuntu ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers \
  && mkdir -p /home/ubuntu/app \
  && chown -R ubuntu:ubuntu /home/ubuntu/app

# https://github.com/Googlechrome/puppeteer/issues/290#issuecomment-451471338
RUN apt-get update \
  && apt-get install -y \
    libx11-xcb1 \
    libxrandr2 \
    libasound2 \
    libpangocairo-1.0-0 \
    libatk1.0-0 \
    libatk-bridge2.0-0 \
    libgtk-3-0 \
    libnss3 \
    libxss1

USER ubuntu

WORKDIR /home/ubuntu/

# nodenv
ENV PATH="/home/ubuntu/.nodenv/bin:/home/ubuntu/.nodenv/shims:${PATH}"
RUN curl -fsSL https://github.com/nodenv/nodenv-installer/raw/master/bin/nodenv-installer | bash
WORKDIR /home/ubuntu/app
COPY .node-version .node-version
RUN nodenv install

COPY package.json .
COPY package-lock.json .

RUN npm i

CMD rsync --delete -r --exclude node_modules /code/ /home/ubuntu/app/ \
  && npm test \
  && rsync --delete -r /home/ubuntu/app/test-assets/ /code/test-assets/
