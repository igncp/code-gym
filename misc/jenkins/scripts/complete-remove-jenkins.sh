#!/usr/bin/env bash

sudo apt-get purge --auto-remove -y jenkins
sudo rm -rf /var/cache/jenkins /var/lib/jenkins /var/log/jenkins/
