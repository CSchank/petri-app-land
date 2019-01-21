module NewspaperExample.Init where
import NewspaperExample.Static.Types

-- the initial states of each place in this net
initMainStreet :: MainStreet
initMainStreet = MainStreet 

initReadingRoom :: ReadingRoom
initReadingRoom = ReadingRoom [Article "title" "author" 0 "A long story." ]

initEditingRoom :: EditingRoom
initEditingRoom = EditingRoom [DraftArticle "title" "author" 0 "A long story." [(10000,"Needs more words.")]]


