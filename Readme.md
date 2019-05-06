# How to create a new PetriAppLand

GitHub will be the "main" directory from which we will be working for this example. Any
folder is fine, as long as this repo and the one you create are in the same place.

## Prerequisites
- Haskell stack
- rsync utility

## Specifying your app and generating the code

1. From a directory of your choosing, type `stack new YourProjectName github:cschank/petriappland`
to create a new PetriAppLand project (remember to change `YourProjectName` to whatever you 
want). This will create a folder `YourProjectName`.
2. `cd YourProjectName`.
3. Open `src/ClientServerSpec.hs`
	- Modify your specification how you please, adding types, places, transitions, etc.
	- Types for the specification are in `src/Types.hs`
	- Type helper functions are in `src/TypeHelpers.hs`
4. From `YourProjectName`, run `stack build`. Fix any compiler errors you may have with your
specification, and rebuild.
5. When it compiled successfully (you get no errors), run `stack exec pal-exe`.
	- This will attempt to generate code based on your specification.
	- If errors are found, you'll have to fix them, rebuild and execute again.
6. Once the specification generates successfully, `cd YourProjectName` into the output folder 
you set in your ClientServerSpec.hs in step 3. Inside you'll see `client` and `server`.

## Completing the generated client
1. Go into the client directory with `cd client`.
2. For each Net that you generated, you'll need to complete `Init.elm`, `Update.elm` and all of the views
in the `View` folder. 
	- _All files in `Static` directories can be used for reference but should not be edited!_
	- _No generated imports should be modified, but you can add new modules and import them,
or import new libraries._
	- Helper functions for models can be found in `Static/Helpers`.
3. From the `client` directory, run `make` to build, then `make launch` to open your app in `elm 
reactor`. Running `build/index.html` directly will not work from your local machine due to Elm's
handling of URLs.
4. To publish live on a server, copy all files in `build` into the directory you want them on your
webserver.

## Completing the generated server
1. Go into the server directory with `cd ../server`
2. For each Net generated, you'll need to complete `Init.hs` and `Update.hs`.
	- _All files in `Static` directories can be used for reference but should not be edited!_
	- _No generated imports should be modified, but you can add new modules and import them,
or import new libraries._
	- Helper functions for models can be found in `Static/Helpers`.
3. From the `server` directory, use `stack build` to build your client. Then use `stack exec pal-server-exe`
to launch the server.
4. To launch live on a server, clone the repository on your server, build the server there and run on an
open port. You'll have to update the client's URL to point to the correct one.

## Editing your specification
To edit your specification, make changes to the spec in `ClientServerSpec.hs` and then rebuild and execute the program. Note that any files the user is supposed to edit (views, updates) will NOT be rebuilt by the system if they already exist. If you haven't edited a certain file, you can delete it or move it before regenerating and it will be generated again. Otherwise, you can use the compiler errors to help you fix your code. S

_Static files will be rebuilt if there are changes, so don't edit them as you will lose work from them._

#Updating your version of PAL
1. Ensure that you have run `stack build` for your project.
2. Run `stack exec pal-update`
3. The newest version of PAL will be installed.
4. Rebuild your project using `stack build` to use the newest version of PAL.

## Compiling Net Graphs
When you generate your code, you will be given the code to generate codegraphs with `dot`. Either run them through dot, or use the
following website: https://dreampuf.github.io/GraphvizOnline/. Copy the code from the dot file in `MyNewPetriProject/Diagrams/` into 
the website to preview it.
