#!/usr/bin/env bash
curl \
    -T "build/libs/deepstream.io.*.jar" \
    -H "X-Bintray-Publish:1" \
    -u yasserf:$1 \
    "https://api.bintray.com/content/deepstreamio/maven/deepstream.io-client-java/${1}/deepstream.io-client-java_${1}.jar"