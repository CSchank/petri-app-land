Server File Structure

AppHome/
	app/
		Main.hs 		-- entry point (static)
	Setup.hs    		-- cabal / stack setup file
	src/
		utils/			--static utilities that need to be compiled into the server
			Decode.hs   --some helper functions for decoding
		static/         --generated files, user should not change these
			Types.hs	--generated types for state, server and client messages
			Encode.hs	--generated outgoing message encoders
			Decode.hs	--generated inocoming messgae decoders
			Lib.hs		--thread manager
		userApp/		--users can change these files
			Init.hs		--user specified initial state
			Update.hs	--update functions (stubs are generated, user fills them out)
			View.hs		--user-specified functions for logging messages and / or server state
			Types.hs    --internal user types not included in the state diagram