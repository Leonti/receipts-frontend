#!/bin/bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

docker login -e="$DOCKER_EMAIL" -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
docker build -t leonti/receipts-frontend:$version .
docker push leonti/receipts-frontend
git tag -a v$version -m 'new version $version'

echo "Released version "$version
