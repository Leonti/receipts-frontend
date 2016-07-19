#!/bin/bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

docker login -e="$DOCKER_EMAIL" -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
docker build -t leonti/receipts-frontend:$version .
docker push leonti/receipts-frontend
git tag -a v$version -m 'new version $version'

git push --quiet "https://${TAG_TOKEN}@github.com/Leonti/receipts-frontend" --follow-tags > /dev/null 2>&1

echo "Released version $version"
