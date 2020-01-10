import Network.Wai                           -- from wai
import Network.Wai.Handler.Warp (run)        -- from warp
import Network.HTTP.Types                    -- from http-types

import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.Functor
import Data.Function
import Data.Bifunctor


import System.IO
import System.Process                        -- from process

import Data.Char (isSpace)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString (ByteString)          -- from bytestring
import Data.ByteString.Char8 (unpack, pack)  -- from bytestring
import qualified Data.Text as Text           -- from text

import System.Directory                      -- from directory
import System.Directory.Internal.Prelude     -- from directory
import System.FilePath                       -- from FilePath

import qualified Data.List as List
import Data.List.Split (splitOn)             -- from split

import Data.Map.Strict (Map)                 -- from containers
import qualified Data.Map.Strict as Map      -- from containers

import Data.Time.Clock                       -- from time
import Data.Time.Format                      -- from time
import Data.Time.Format.Human                -- from friendly-time
import Data.FileEmbed                        -- from file-embed

import Text.Printf

import Prelude hiding (delete, update)

import Debug.Trace


------------------------------------------------------------
-- Constants

postFolder           = "posts"
templateFolder       = "templates"
staticFolder         = "static"

-- Only consider these file extensions when looking for posts.
validExtensions      = [".md", ".tex", ".txt"]


--------------------------------------------------------------------------------
-- Types

data PostData = Post
    { date :: UTCTime
    , title :: String
    , content :: String
    }


--------------------------------------------------------------------------------
-- Main app

main :: IO ()
main = do
    initialise
    port <- defaultIO 8080 $ read <$> getEnv "MC_PORT"
    run port app

-- Generate all the default files and folders.
initialise :: IO ()
initialise = do
    putStr "Initialising directories ..."

    havePosts <- doesDirectoryExist postFolder
    unless havePosts $ do
        createDirectory postFolder
        writeFile (postFolder </> "home.md")
                 $(embedStringFile "defaults/posts/home.md")

    haveTemplates <- doesDirectoryExist templateFolder
    unless haveTemplates $ do
        createDirectory templateFolder
        writeFile (templateFolder </> "dir.html")
                 $(embedStringFile "defaults/templates/dir.html" )
        writeFile (templateFolder </> "post.html")
                 $(embedStringFile "defaults/templates/post.html")

    haveStatic <- doesDirectoryExist staticFolder
    unless haveStatic $ do
        createDirectory staticFolder
        createDirectory $ staticFolder </> "css"
        writeFile (staticFolder </> "css" </> "main.css")
                 $(embedStringFile "defaults/static/css/main.css")

    putStrLn " done."


------------------------------------------------------------
-- Respond to web requests

notFound = responseLBS status404 [("Content-Type", "text/plain")] "Not found"
naughty  = responseLBS status403 [("Content-Type", "text/plain")] "Unauthorized"

app :: Application
app req respond = app' (Text.unpack <$> pathInfo req) respond

-- Resolve a root (if possible / safe) and generate a response.
app' :: [FilePath] -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app' pathSegments respond = do
    let fullPath     = joinPath pathSegments

    cwd        <- getCurrentDirectory
    unsafe     <- not <$> fullPath `isIn` cwd
    let root   = pathSegments == [] || pathSegments == [""]
        static = head pathSegments == "static"

    if | unsafe    -> respond naughty
       | root      -> app' ["home"] respond
       | static    -> respond =<< fromMaybe notFound
                              <$> respondStatic fullPath
       | otherwise -> respond =<< fromMaybe notFound
                              <$> respondNormal pathSegments

-- Generate a response for a static file.
respondStatic :: FilePath -> IO (Maybe Response)
respondStatic fp = do
    exists <- doesFileExist fp
    return $ if exists
        then Just (responseFile status200 mempty fp Nothing)
        else Nothing

-- Generate a response for either a post or a directory.
respondNormal :: [FilePath] -> IO (Maybe Response)
respondNormal pathSegments = do
    pathM <- resolvePosts pathSegments
    case pathM of
        Nothing -> return Nothing
        Just path -> do
            isDir <- doesDirectoryExist path
            resp <- (if isDir then respondDir else respondPost) path
            return $ Just resp

-- Generate a response for a post.
respondPost :: FilePath -> IO Response
respondPost path = do
    Post{..} <- parsePost path

    relDate <- humanReadableTime date
    parentTitle <- getDirName $ takeDirectory path

    let dataPairs =
            [ ("content", content)
            , ("title"  , title)
            , ("date"   , relDate)
            , ("parent-dir" , relParent path)
            , ("parent-title", parentTitle)
            ]

    renderedPost <- fillTemplate "post" dataPairs

    return $ responseLBS status200 [("Content-Type", "text/html")]
           $ LBS.pack renderedPost

-- Generate a response for a directory.
respondDir :: FilePath -> IO Response
respondDir path = do
    dirname  <- getDirName path
    contents <- listDirectory path
        <&> filter (\x -> head x /= '.')
        <&> fmap (path </>)

    now <- getCurrentTime

    let
        listDir :: FilePath -> String -> String
        listDir = printf
            "<li class='dir'><a href='%s'><p class='name'>%s</p></a></li>"

        listPost :: FilePath -> PostData -> String
        listPost postPath Post{..} = printf
            ("<li class='post'><a href='%s'>"
            ++ "<p class='name'>%s</p>"
            ++ "<p class='date' title='%s'>%s</p>"
            ++ "</a></li>"
            )
            ("/" </> postPath)
            title
            (formatTime defaultTimeLocale "%F" date)
            (humanReadableTime' now date)


    dirPaths  <- filterM doesDirectoryExist contents
    postPaths <- filterM doesFileExist      contents

    posts    <- mapM parsePost postPaths
    dirNames <- mapM getDirName dirPaths

    let
        dirListings  = zipWith listDir
                        (dropRoot <$> dirPaths) dirNames
        postListings = zipWith listPost
                        (dropExtension . dropRoot <$> postPaths) posts
        sortedPostListings = fmap snd
                           $ List.sortOn fst
                           $ zip (date <$> posts) postListings
        listing = printf
            "<ul class='listing'>\n%s\n%s\n%s\n</ul>\n"
            (listDir (relParent path) "[ Up a level ]")
            (unlines dirListings)
            (unlines sortedPostListings)

        dataPairs = [ ("title", dirname), ("listing", listing) ]

    renderedDir <- fillTemplate "dir" dataPairs

    return $ responseLBS
        status200
        [("Content-Type", "text/html")]
        $ LBS.pack renderedDir

-- Get the (possibly pretty) name of a directory.
-- Poll the ".dirname" file if it exists.
getDirName :: FilePath -> IO String
getDirName path = fmap trim
                $ defaultIO (takeBaseName path)
                $ readFile $ path </> ".dirname"


--------------------------------------------------------------------------------
-- Resolving filenames

-- Given a path which does not escape the root, try and find a matching
-- file in the posts folder.
resolvePosts :: [FilePath] -> IO (Maybe FilePath)
resolvePosts pathSegments = do
    allPaths <- allPathsFrom postFolder

    let
        basePairs   = Map.fromList . fmap (\x -> (takeBaseName x, x))
        allSlugs    = basePairs allPaths
        fullPath    = joinPath pathSegments
        parent      = postFolder </> takeDirectory fullPath

    dirContents <- defaultIO [] $ listDirectory parent

    let
        allContents = dirContents
                    & fmap (parent </>)
                    & fmap (\x -> (dropExtension $ dropRoot x, x))
                    & Map.fromList

        resolveSlug = flip Map.lookup allSlugs
        resolvePath = Map.lookup fullPath allContents

    case pathSegments of
        [] -> resolvePosts ["home"]
        [single] -> return $ resolveSlug single <|> resolvePath
        multiple -> return $ resolvePath

-- List all VALID filepaths from some root.
allPathsFrom :: FilePath -> IO [FilePath]
allPathsFrom path = do
    isDir <- doesDirectoryExist path
    if isDir
    then do
        children <- (listDirectory path) >>= mapM (allPathsFrom . (path </>))
        return $ path : concat children
    else
        if takeExtension path `notElem` validExtensions
        then return []
        else return [path]


--------------------------------------------------------------------------------
-- Dealing with Templates

getTemplatePath :: String -> String
getTemplatePath name = templateFolder </> name <.> "html"

fillTemplate :: String -> [(String,String)] -> IO String
fillTemplate templateName params = do
    let templatePath = getTemplatePath templateName
        placeholderMap = first (printf "<PLACEHOLDER name=\"%s\">") <$> params

    rawTemplate <- readFile $ templatePath

    return $ foldr (uncurry replace) rawTemplate placeholderMap


--------------------------------------------------------------------------------
-- Dealing with Posts

-- Get the rendered version of the content from pandoc.
-- Why not use the Pandoc library directly? Because it weighs a metric ton!
renderContent :: String -> IO String
renderContent raw = do
    (pandocIn, pandocOut, _, _) <- runInteractiveCommand "pandoc -t html"

    rendered <- hGetContents pandocOut

    hPutStrLn pandocIn raw
    hClose pandocIn

    return rendered

-- Retrieve the contents of a post from its full relative file path.
parsePost :: FilePath -> IO PostData
parsePost fp = do

    fileContent <- readFile fp

    let
        isFence = all (== '-')
        isBlank = all isSpace

        fileName  = takeBaseName fp
        fileLines = lines fileContent

        noPreamble = (Nothing, fileContent)

        readTime = parseTimeM True defaultTimeLocale "%Y-%-m-%-d"

        (preamble, body) = fromMaybe noPreamble $ do
            (first, rest) <- List.uncons $ dropWhile isBlank fileLines
            guard $ isFence first

            (vars, body) <- splat isFence rest
            let pre = vars
                    & fmap (splat (==':'))
                    & catMaybes
                    & fmap (bimap trim trim)
                    & Map.fromList

            return (Just pre, unlines body)

    mtime       <- getModificationTime fp
    content     <- renderContent body

    let date  = fromMaybe mtime    $ preamble >>= Map.lookup "date" >>= readTime
        title = fromMaybe fileName $ preamble >>= Map.lookup "title"

    return $ Post date title content


--------------------------------------------------------------------------------
-- Helper functions

-- Perform an IO action, but if it fails use the default value.
defaultIO :: a -> IO a -> IO a
defaultIO def action = catchIOError action (const $ return def)

-- Replace all instances in a string (list).
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = List.intercalate new . splitOn old

-- Trim whitespace from a string.
trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- Find the first occurrence in a list fulfilling predicate p, and take
-- the pre and post of that occurrence.
splat :: (a -> Bool) -> [a] -> Maybe ([a], [a])
splat pred ls = let
    (prefix, rest) = break pred ls
    in case rest of
        []     -> Nothing           -- not found
        (_:ss) -> Just (prefix, ss) -- found

-- Check if one filepath is contained within another.
isIn :: FilePath -> FilePath -> IO Bool
pathA `isIn` pathB = do
    pathACanonical <- splitDirectories <$> canonicalizePath pathA
    pathBCanonical <- splitDirectories <$> canonicalizePath pathB
    return $ pathBCanonical `List.isPrefixOf` pathACanonical

-- Remove the outermoth path segment.
dropRoot :: FilePath -> FilePath
dropRoot fp = case splitDirectories fp of
    []  -> fp
    (x:xs) -> joinPath xs

-- Switch a serverside path for a clientside one and take its parent.
relParent :: FilePath -> FilePath
relParent = ("/" </>) . dropRoot . takeDirectory

