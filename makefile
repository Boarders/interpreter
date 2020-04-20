default:
	yarn parcel web/index.html

deps:
	yarn add --dev parcel-bundler


ps:
	spago bundle-module --main TopLevel --to 'web/toplevel.js'
