Server File Structure

server/
	app/
		Main.hs 		-- entry point (static)
	Setup.hs    		-- cabal / stack setup file
	src/
		utils/			--static utilities that need to be compiled into the server  			
			Decode.hs   --some helper functions for decoding									DONE
		static/         --generated files, user should not change these
			Types.hs	--generated types for state, server and client messages					DONE
			Encode.hs	--generated outgoing message encoders									DONE
			Decode.hs	--generated inocoming messgae decoders									DONE
			Update.hs   --hidden update function which calls user functions						
			Lib.hs		--thread manager 														
		userApp/		--users can change these files
			Init.hs		--user specified initial state											DONE
			Update.hs	--update functions (stubs are generated, user fills them out)			DONE
			View.hs		--user-specified functions for logging messages and / or server state	
			Types.hs    --internal user types not included in the state diagram					DONE


client/
	app/
		Main.elm 		-- entry point (static)
	Setup.hs    		-- cabal / stack setup file
	src/
		utils/			--static utilities that need to be compiled into the server  			
			Decode.elm   --some helper functions for decoding									
		static/         --generated files, user should not change these
			Types.elm	--generated types for state, server and client messages					
			Encode.elm	--generated outgoing message encoders									
			Decode.elm	--generated inocoming messgae decoders									
			Update.elm   --hidden update function which calls user functions						
			Lib.elm		--thread manager 														
		userApp/		--users can change these files
			Init.elm	--user specified initial state											
			Update.elm	--update functions (stubs are generated, user fills them out)			
			View.elm	--user-specified functions for logging messages and / or server state	
			Types.elm 	--internal user types not included in the state diagram