elm-package install --yes
rm -r public
mkdir public
elm-make Main.elm --output public/elm.js
cp index.html public/index.html
cp style.css public/style.css
