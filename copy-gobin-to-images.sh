#! /bin/bash
if [ -z $GOPATH ]; then
    echo 'You must set GOPATH environment variable first.'
    exit 1
fi
imgdirs=$(find . -maxdepth 1 -type d -not -regex '^\(\.\|\./\(\.git\|tests\|jail\|intra-jail\|jail-check\|intra-jail-check\)\)$')
for dest in $imgdirs; do
    cp $GOPATH/bin/jail $dest/
    cp $GOPATH/bin/intra-jail $dest/
done
