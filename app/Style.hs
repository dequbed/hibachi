module Style where

import           Prelude        hiding (not, rem, span, (**), display)

import           Data.Text.Lazy (Text, toStrict)

import           Clay
import qualified Clay.Flexbox   as Flexbox
import           Clay.FontFace
import qualified Clay.Media     as Media

import           Skylighting    (pygments, styleToCss)

--styleText :: Text
styleText = toStrict $ renderWith pretty [] ourStyle

styleCode :: String
styleCode = styleToCss pygments

ourStyle :: Css
ourStyle = do
    resets
    fonts
    typesetting
    navbar
    mainElement
    post
    footerElement
    links
    sourceCode
    lists
    logo
    pagination
    project

fonts :: Css
fonts = mf
       [ ("Vollkorn", "/fonts/Vollkorn-Regular.woff2", normal, weight 400)
       , ("Hack", "/fonts/Hack-Regular.woff2", normal, weight 400)
       , ("Vollkorn", "/fonts/Vollkorn-Italic.woff2", italic, weight 400)
       , ("Vollkorn", "/fonts/Vollkorn-Semibold.woff2", normal, weight 500)
       , ("Vollkorn", "/fonts/Vollkorn-SemiboldItalic.woff2", italic, weight 500)
       , ("Vollkorn", "/fonts/Vollkorn-Bold.woff2", normal, weight 700)
       , ("Vollkorn", "/fonts/Vollkorn-BoldItalic.woff2", italic, weight 700)
       , ("Vollkorn", "/fonts/Vollkorn-Black.woff2", normal, weight 900)
       , ("Vollkorn", "/fonts/Vollkorn-BlackItalic.woff2", italic, weight 900)
       , ("Hack", "/fonts/Hack-Italic.woff2", italic, weight 400)
       , ("Hack", "/fonts/Hack-Bold.woff2", normal, weight 700)
       , ("Hack", "/fonts/Hack-BoldItalic.woff2", italic, weight 700)
       , ("Dearest", "/fonts/Dearest-webfont.woff", normal, normal)
       , ("FontAwesome", "/fonts/fontawesome-webfont.woff2", normal, normal)
       ]
  where
    mf = mapM_ (fontFace . ff)
    ff (a,b,c,d) = do
        fontFamily [a] []
        fontFaceSrc [FontFaceSrcUrl b $ Just WOFF2]
        fontStyle c
        fontWeight d

typesetting :: Css
typesetting = do
    html <> body ? do
        "font-feature-settings" -: "'zero', 'ss11', 'ss14', 'ss17', 'onum', 'pnum'"
        fontSize (pt 14)
        fontFamily ["Vollkorn"] [serif]
        backgroundColor "#3a3e4f"
    h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
        "font-variant-caps" -: "small-caps"
    h2 <> h3 <> h4 <> h5 <> h6 ? do
        marginTop (em 1)
        textIndent $ indent (em 1)


    h1 ? do
        fontSize (pt 21)
        lineHeight $ unitless 1
        marginBottom $ unitless 0.125
        marginTop $ unitless 0.125
    h2 ? do
        fontSize (pt 17.5)
        lineHeight $ unitless 1
        marginTop (pt 16.8)
        marginBottom (pt 4.2)

    p ? do
        lineHeight $ unitless 1.5
        marginBottom nil

    p |+ p ? do
        textIndent $ indent (em 1)
        marginTop nil

    ".abstract" ?
        minHeight (rem 2.1)
    ".abstract" |> p#firstLetter#nthOfType "1" ? do
        color myViolet
        float floatLeft
        fontFamily ["Dearest"] [cursive]
        fontSize (rem 3)
        lineHeight (rem 1.5)
        textTransform uppercase
        marginTop (rem 0.25)
        marginRight (rem 0.3)

    blockquote ? margin (em 1.5) (em 1.5) (em 1.5) (em 1.5)

    code ? do
        fontFamily ["Hack"] [monospace]
        fontSize (rem 0.8)
        lineHeight (rem 0)

    article |> code ?  lineHeight (rem 1.5)

    li ? lineHeight (rem 1.5)

    ".smallcaps" ? ("font-variant-caps" -: "small-caps")
    ".lining" ? ("font-variant-numeric" -: "lining-nums")

navbar :: Css
navbar = do
    ".toggle-nav" ? display none
    ".mobile-bar" ? do
        zIndex 5
        position fixed
        width (pct 100)
        height (rem 3)
        backgroundColor "#50556c"
        boxShadow . pure $ bsColor (rgba 0 0 0 0.3) $ shadowWithBlur (px 0) (px 1) (px 5)
        "will-change" -: "transform"
        transition "transform" (ms 200) ease none
        justifyContent center
        display flex
        flexDirection column

    ".mobile-bar" |> label ? do
        position absolute
        top nil
        left (px 10)
        width (rem 3)
        height (rem 3)
        textAlign center
        cursor pointer
        transition "transform" (ms 300) ease none

    ".mobile-bar" |> label#after ? do
        content $ stringContent "\61524"
        fontFamily ["FontAwesome"] [monospace]
        fontSize (em 2)
        color "#eee"
        lineHeight (rem 3)
        "backface-visibility" -: "hidden"
        transition "transform" (ms 500) ease none

    ".mobile-bar" ** ".navlink" ? do
        "align-self" -: "right"

    query Media.screen [Media.minWidth (rem 49)] $
        ".mobile-bar" ? display none

    ".navlink" ? do
        "font-variant-caps" -: "small-caps"
        display flex
        justifyContent center
    ".navlink" ** ".navicon" ?
        marginRight (em 0.4)

    "#navbar" ? zIndex 10

    "#navleft" ? do
        float floatLeft
        "flex" -: "4"
    "#navright" ? do
        float floatRight
        "flex" -: "6"

    nav ? do
        "will-change" -: "transform"
        transition "transform" (ms 300) ease none
        height (pct 100)
        position fixed
        top nil
        marginLeft (pct (-70))
        width (pct 70)
        backgroundColor "#50556c"
        overflowY auto
        zIndex 10
        display flex
        flexDirection column
        alignItems center
        justifyContent center
    nav ** header ? do
        width (pct 100)
        "flex" -: "2"
    nav ** ul ? do
        fontSize (rem 1.2)
        textAlign center
        "list-style" -: "none none"
        display flex
        width (pct 100)
        height (pct 100)
        flexDirection column
        padding nil nil nil nil
    nav ** ul ** a ? do
        "flex" -: "none"
        height (em 3)
        display flex
        flexDirection column
        justifyContent center
        color "#f2f2f0"
    nav ** ul ** a#hover ? do
        border none none none
        backgroundColor "#454a5e"

    nav ** ".spacer" ? do
        "flex" -: "1"

    query Media.screen [Media.minWidth (rem 49)] $ nav ? do
        position fixed
        left nil
        right nil
        width (pct 100)
        height (rem 5.5)
        marginTop nil
        marginLeft nil
        flexDirection row

    query Media.screen [Media.minWidth (rem 49)] $ do
        nav ** ul ?
            flexDirection row
        nav ** ul ** a ? do
            "flex" -: "1"
            height (rem 5.5)

    ".toggle-nav:checked ~ nav" ? do
        "will-change" -: "transform"
        "transform" -: "translateX(100%)"
    query Media.screen [Media.minWidth (rem 49)] $
        ".toggle-nav:checked ~ nav" ?
            (transform $ translateX nil) :: Css

    ".toggle-nav:checked ~ main" ? do
        "will-change" -: "transform"
        "transform" -: "translateX(70%)"
    query Media.screen [Media.minWidth (rem 49)] $
        ".toggle-nav:checked ~ nav" ?
            (transform $ translateX nil) :: Css

    ".toggle-nav:checked ~ #footer" ? do
        "will-change" -: "transform"
        "transform" -: "translateX(70%)"
    query Media.screen [Media.minWidth (rem 49)] $
        ".toggle-nav:checked ~ #footer" ? do
            (transform $ translateX nil) :: Css

    ".toggle-nav"#checked |+ ".mobile-bar" ? do
        "will-change" -: "transform"
        "transform" -: "translateX(70%)"
    query Media.screen [Media.minWidth (rem 49)] $
        ".toggle-nav"#checked |+ ".mobile-bar" ?
            (transform $ rotate (deg 180)) :: Css

mainElement :: Css
mainElement = do
    main_ ? do
        "flex" -: "1"
        position relative
        paddingTop (rem 5.6)
        margin nil auto auto auto
        transition "transform" (ms 300) ease none
        width (pct 100)
        maxWidth (em 50)

    query Media.screen [Media.minWidth (px 768)] $
        main_ ? do
            paddingLeft (px 20)
            paddingRight (px 20)

myFontColor :: Color
myFontColor = parse "#0e0e0e"
myViolet :: Color
myViolet = parse "#8e16a6"
myBackgroundColor :: Color
myBackgroundColor = parse "#f0f0f0"

violetOutline :: Css
violetOutline = outline solid (px 2) myViolet


post :: Css
post = do
    ".postlink" ? do
        textDecoration none
        display block

    ".postlink"#hover ?
        border none none none

    query Media.screen [Media.minWidth (px 768)] $
        ".postlink"#hover ?  violetOutline

    ".post" ? do
        marginBottom (px 20)
        padding (px 20) (px 20) (px 10) (px 20)
        backgroundColor myBackgroundColor
        color myFontColor

    ".post" |> header ** ".readtime" ? do
        paddingLeft (px 5)
        paddingRight (px 5)
        fontSize (rem 0.7)
        lineHeight (unitless 1)
        marginBottom (pt 11.2)

    ".post-footer" ? do
        display block
        marginTop (rem 0.6)
        fontSize (rem 0.8)

    ".tags" ? do
        marginRight (em 0.3)
        padding nil nil nil nil
        "list-style" -: "none"
        display inlineFlex

    ".tag" ?
        marginLeft (px 5)

footerElement :: Css
footerElement = do
    "#footer" ? do
        display grid
        width (pct 100)
        padding (px 10) (px 10) (px 10) (px 10)
        fontSize (rem 0.6)
        color "#abab9e"
        borderTop solid (px 2) white
        transition "transform" (ms 300) ease none
    "#footer" ** span ? do
        alignSelf center
        textAlign center
    "#footer" ** a ? do
        textDecoration none
        color "#cecec7"

links :: Css
links = do
    a ? do
        textDecoration none
        color "#8e16a6"
    a#focus <> a#hover ?
        borderBottom solid (px 1) none
    a#active ?
        color "#410a4c"
    a#("href"*="http")#before#(not ".navlink") ? do
        content $ stringContent "\61582"
        paddingLeft (em 0.4)
        fontSize (em 0.6)
        fontFamily ["FontAwesome"] [monospace]


sourceCode :: Css
sourceCode = do
    ".sourceLine" ** a ? do
        color myFontColor
        textDecoration none
    (".sourceLine" ** a#focus) <>
        (".sourceLine" ** a#hover) ? do
            backgroundColor "#d7d7d7"
            border none none none

lists :: Css
lists = do
    ul <> ol ?
        paddingLeft (rem 1)
    ul ? ("list-style" -: "disc inside")
    ol ? ("list-style" -: "decimal inside")

pagination :: Css
pagination =
    ".pagination" ? do
        display inlineBlock
        "list-style" -: "none"
        a ? do
            float floatLeft
            textDecoration none
            border solid (px 1) black
            margin nil nil nil nil

logo :: Css
logo = ".logo" ?
    height (rem 5.5)

project :: Css
project = do
    ".project" ? do
        display flex
        flexDirection row
        marginBottom (px 20)
    ".project"#hover ? violetOutline
    -- Switch around every other listing (image on the right, text on the left)
    -- This breaks the monotony and hides that pictures aren't all the same proportions
    ".project"#nthChild "2n+0" ? flexDirection rowReverse
    
    ".project-image" ? do
        width (pct 100)
        minWidth (em 10)
    ".project-header" ? 
        color myViolet
    ".project-information" ? do
        width (pct 100)
        background myBackgroundColor
        padding (px 20) (px 20) (px 10) (px 20)
        display flex
        flexDirection column
    ".project-description" ? do
        "flex" -: "max-content"
        marginTop (em 1)
        marginBottom (em 1)

resets :: Css
resets = do
    geometryreset
    fontreset
    listreset
    inputreset
    boxreset
    mediareset
    tablereset

geometryreset :: Css
geometryreset = html <> body <> p <> ol <> ul <> li <> dl <> dt <> dd <> blockquote <>
    figure <> fieldset <> legend <> textarea <> pre <> iframe <> hr <>
    h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
            margin nil nil nil nil
            padding nil nil nil nil

fontreset :: Css
fontreset = h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
        fontSize (pct 100)
        fontWeight normal

listreset :: Css
listreset = ul ? ("list-style" -: "none none")

inputreset :: Css
inputreset = button <> input <> select <> textarea ? margin nil nil nil nil

boxreset :: Css
boxreset = do
    html ? boxSizing borderBox
    star <> star#before <> star#after ? boxSizing inherit

mediareset :: Css
mediareset =
    img <> embed <> iframe <> object <> audio <> video ? do
        height auto
        maxWidth (pct 100)

tablereset :: Css
tablereset = do
    table ? do
        borderCollapse collapse
        borderSpacing nil
    td <> th ? do
        padding nil nil nil nil
        textAlign $ alignSide sideLeft
