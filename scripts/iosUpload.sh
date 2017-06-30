#!/usr/bin/env bash
VERSION=$1
if [ -z $VERSION ]
    then
        echo "Version missing, exiting"
        exit
fi

echo "Packaging iOS client version" $1

echo "build-prod.gradle"
J2OBJC_ROOT=../j2objcDist
bash ./gradlew -b build-prod.gradle
echo "build-prod.gradle exited"

chmod a+x ./scripts/prepareiOS.sh && ./scripts/prepareiOS.sh

## Append the version string to the .zip package, where X.Y.Z represent the semantic version of the package

mv ./DeepstreamIO.zip ./DeepstreamIO-$1.zip

echo "Uploading to s3"
aws s3 cp ./DeepstreamIO-$1.zip s3://deepstream.io-client-ios/ --region=eu-west-2 --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers


#####
##### Update the Podspec
#####

git clone git@github.com:deepstreamIO/deepstream.io-client-ios.git

cd deepstream.io-client-ios

sed -i '' 's/s.version                   = "[0-9]*\.[0-9]*\.[0-9]*"/s.version                   = "'$VERSION'"/' 'DeepstreamIO.podspec'

pod spec lint

git add .
git commit -m 'v'$VERSION

git tag 'v'$VERSION
git push origin master && git push origin --tags

echo '
// All done! Now you just need to do
// $ pod trunk register
// Click the link in your email and
// $ pod trunk push DeepstreamIO.podspec
'

