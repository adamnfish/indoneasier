elm-app build || exit
rm -r docs/*
cp -r build/* docs/

echo "commit and push to complete deployment"
