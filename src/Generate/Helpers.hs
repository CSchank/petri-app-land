{-# LANGUAGE OverloadedStrings #-}


module Generate.Helpers where
{-
import ClientServerSpec
import Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S
import qualified Data.Map as M
import                  System.FilePath.Posix   ((</>),(<.>))
import Generate.Types
import Data.Char (toUpper)

generateHelpers :: FilePath -> Bool -> [Constructor] -> IO ()
generateHelpers fp h states =
    mapM_ (\(sn,edt) -> TIO.writeFile (fp </> "server" </> "src" </> "Static" </> "Helpers" </> sn <.> "hs") $ generateHelper h (sn,edt)) states



generateHelper :: Bool -> Constructor -> T.Text
generateHelper h (sn,edts) =
    let
        snTxt = T.pack sn

        generateGetter (et,n,d) =
            let
                nTxt = T.pack n
                fnName = T.concat["get",capitalize nTxt]
            in T.unlines
                [
                    T.concat [fnName," :: Model -> ",et2Txt h False et]
                ,   T.concat [fnName," (",generatePattern (sn,edts),") =",nTxt]
                ]
        generateSetter (et,n,d) =
            let
                nTxt = T.pack n
                fnName = T.concat["set",capitalize nTxt]
            in T.unlines
                [
                    T.concat [fnName," :: Model -> ",et2Txt h False et]
                ,   T.concat [fnName," (",generatePattern (sn,edts),") =",nTxt]
                ]
        {-generateUpdater (et,n,d) =
            let
                nTxt = T.pack n
                fnName = T.concat["update",capitalize nTxt]
            in T.unlines
                [
                    T.concat [fnName," : Model -> ",et2Txt h False et]
                ,   T.concat [fnName," (",generatePattern (sn,edts),") =",nTxt]
                ]-}

    in
        T.unlines 
            [
                T.concat ["module Helpers.",snTxt," where"]
            ,   T.concat ["import Static.Types"]
            ,   T.unlines $ map generateGetter edts
            ,   T.unlines $ map generateSetter edts
            ]

capitalize :: T.Text -> T.Text
capitalize txt =
    case T.unpack txt of
        h:rest -> T.pack $ toUpper h : rest
        _ -> txt-}