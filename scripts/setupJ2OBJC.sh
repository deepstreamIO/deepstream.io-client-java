#!/usr/bin/env bash
J2OBJCVersion=1.2

# Paths and Stuff
DIRECTORY=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd );
if [ -z ${TRAVIS_BUILD_DIR} ]; then
	export TRAVIS_BUILD_DIR=$DIRECTORY/..;
fi
J2OBJC_ROOT=$TRAVIS_BUILD_DIR/j2objcDist

# Create local.properties for gradlew
echo '# J2OBJC Start' >> $DIRECTORY/../local.properties;
echo j2objc.home=$J2OBJC_ROOT/j2objc-$J2OBJCVersion >> $DIRECTORY/../local.properties
if [[ "$TRAVIS_OS_NAME" == "osx" || "$OSTYPE" == "darwin"* ]]; then
	echo 'Wooho! Mac! j2objc compilation!';
else
	echo 'Only transpiling will occur';
	echo j2objc.translateOnlyMode=true >> $DIRECTORY/../local.properties;
fi
echo '# J2OBJC End' >> $DIRECTORY/../local.properties;

# Download J2OBJC
mkdir -p $J2OBJC_ROOT;
pushd $J2OBJC_ROOT;
if [ ! -f "$J2OBJC_ROOT/j2objc-$J2OBJCVersion.zip" ]; then
    echo "wget -q --retry-connrefused --waitretry=1 https://github.com/google/j2objc/releases/download/$J2OBJCVersion/j2objc-$J2OBJCVersion.zip"
    wget -q --retry-connrefused --waitretry=1 https://github.com/google/j2objc/releases/download/$J2OBJCVersion/j2objc-$J2OBJCVersion.zip
else
    rm -rf $J2OBJC_ROOT/j2objc-$J2OBJCVersion
	echo 'j2objc already downloaded';
fi

echo "Looking at dir contents"
pwd
ls

echo 'unzipping';
unzip -q j2objc-$J2OBJCVersion.zip; popd

pwd
ls

if [ $1 ]; then
 rm -rf j2objc-$J2OBJCVersion.zip;
fi
