#!/usr/bin/env sh -e
#
# Script used by Travis CI build to prepare DeepstreamIO SDK zip package

J2OBJCVersion=1.2

echo 'Preparing DeepstreamIO.zip for iOS Release';

DIRECTORY=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd );
if [ -z ${TRAVIS_BUILD_DIR} ]; then
	export TRAVIS_BUILD_DIR=$DIRECTORY/..;
fi

DS_SRC="$TRAVIS_BUILD_DIR/build/j2objcOutputs/src/main"
DS_HEADERS="$DS_SRC/main/objc/"
DS_IOS_LIB="$TRAVIS_BUILD_DIR/build/j2objcOutputs/lib/iosRelease/libdeepstream.io-client-java-j2objc.a"
DS_OSX_LIB="$TRAVIS_BUILD_DIR/build/j2objcOutputs/lib/x86_64Release/libdeepstream.io-client-java-j2objc.a"
DS_LICENSE="$TRAVIS_BUILD_DIR/LICENSE"

J2OBJC_ROOT="$TRAVIS_BUILD_DIR/j2objcDist/j2objc-$J2OBJCVersion"
J2OBJC_INCLUDE_HEADERS="$J2OBJC_ROOT/include/"
J2OBJC_INCLUDE_JRE_EMUL_LIB="$J2OBJC_ROOT/lib/libjre_emul.a"
J2OBJC_INCLUDE_JRE_EMUL_LIB_MAC_OS_X="$J2OBJC_ROOT/lib/macosx/libjre_emul.a"

# Create temp dir
DS_TMP_DIR="$TRAVIS_BUILD_DIR/DeepstreamIO"

mkdir -p $DS_TMP_DIR/lib/iosRelease
mkdir -p $DS_TMP_DIR/lib/x86_64Release
mkdir -p $DS_TMP_DIR/src
mkdir -p $DS_TMP_DIR/j2objc/lib
mkdir -p $DS_TMP_DIR/j2objc/lib/macosx

echo 'Copying Deepstream files to temp dir';

cp $DS_LICENSE $DS_TMP_DIR
cp -r $DS_SRC $DS_TMP_DIR/src
cp $DS_IOS_LIB $DS_TMP_DIR/lib/iosRelease
cp $DS_OSX_LIB $DS_TMP_DIR/lib/x86_64Release

# Create DeepstreamIO.h
touch $DS_TMP_DIR/src/DeepstreamIO.h
echo '//
//  DeepstreamIO.h
//  DeepstreamIO
//
//  Created by deepstreamHub GmbH.
//  Copyright © 2016 deepstreamHub GmbH. All rights reserved.
//

#ifndef DeepstreamIO_h
#define DeepstreamIO_h

// J2ObjC requirement for Java Runtime Environment
#import "JreEmulation.h"
#import "java/util/ArrayList.h"\n' >> $DS_TMP_DIR/src/DeepstreamIO.h

pushd $TRAVIS_BUILD_DIR/build/j2objcOutputs/src/main/objc
for f in *.h; do
	echo "#import \"$f\"" >> $DS_TMP_DIR/src/DeepstreamIO.h
done
popd

echo "\n#endif /* DeepstreamIO_h */" >> $DS_TMP_DIR/src/DeepstreamIO.h

echo 'Copying J2OBJC files to temp dir';
cp -r $J2OBJC_INCLUDE_HEADERS $DS_TMP_DIR/j2objc/include
cp $J2OBJC_INCLUDE_JRE_EMUL_LIB $DS_TMP_DIR/j2objc/lib
cp $J2OBJC_INCLUDE_JRE_EMUL_LIB_MAC_OS_X $DS_TMP_DIR/j2objc/lib/macosx

echo 'Zipping contents';
pushd $DS_TMP_DIR
zip -r DeepstreamIO.zip ./
popd
mv $DS_TMP_DIR/DeepstreamIO.zip $TRAVIS_BUILD_DIR

echo 'Cleaning up...';
rm -rf $DS_TMP_DIR/

echo 'Finished preparing DeepstreamIO.zip';
