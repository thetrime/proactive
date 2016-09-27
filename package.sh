#!/bin/sh

VERSION=$(npm list --depth -1 | sed -e "s/proactive@\([^ ]*\) .*/\1/g" | head -1)
mkdir proactivejs-${VERSION}
browserify --standalone Proactive -t brfs src/core.js | uglifyjs -cm > proactivejs-${VERSION}/proactive.js
cp node_modules/proscript/proscript.js.mem proactivejs-${VERSION}/
cp proactive.css proactivejs-${VERSION}/
zip -r proactivejs-${VERSION}.zip proactivejs-${VERSION}/
rm -rf proactivejs-${VERSION}
