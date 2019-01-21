module Hibachi.Style
    ( styleText
    , ourStyle
    , putCss
    )
    where

import Prelude hiding (span, rem, (**))

import Data.Text.Lazy (Text)

import Clay
import Clay.FontFace
import qualified Clay.Media as Media
import qualified Clay.Flexbox as Flexbox

styleText :: Text
styleText = renderWith compact [] ourStyle

ourStyle :: Css
ourStyle = do
    resets
    fonts
    typesetting
    navbar
    mainElement

fonts :: Css
fonts = do
    mf [ ("Dearest", "/fonts/Dearest-webfont.woff", normal, normal)
       , ("Vollkorn", "/fonts/Vollkorn-Regular.woff2", normal, weight 400)
       , ("Vollkorn", "/fonts/Vollkorn-Italic.woff2", italic, weight 400)
       , ("Vollkorn", "/fonts/Vollkorn-Semibold.woff2", normal, weight 500)
       , ("Vollkorn", "/fonts/Vollkorn-SemiboldItalic.woff2", italic, weight 500)
       , ("Vollkorn", "/fonts/Vollkorn-Bold.woff2", normal, weight 700)
       , ("Vollkorn", "/fonts/Vollkorn-BoldItalic.woff2", italic, weight 700)
       , ("Vollkorn", "/fonts/Vollkorn-Black.woff2", normal, weight 900)
       , ("Vollkorn", "/fonts/Vollkorn-BlackItalic.woff2", italic, weight 900)
       , ("Hack", "/fonts/Hack-Regular.woff2", normal, weight 400)
       , ("Hack", "/fonts/Hack-Italic.woff2", italic, weight 400)
       , ("Hack", "/fonts/Hack-Bold.woff2", normal, weight 700)
       , ("Hack", "/fonts/Hack-BoldItalic.woff2", italic, weight 700)
       , ("FontAwesome", "/fonts/fontawesome-webfont.woff2", normal, normal)
       ]
  where
    mf = mapM_ fontFace . Prelude.map ff
    ff (a,b,c,d) = do
        fontFamily [a] []
        fontFaceSrc $ [FontFaceSrcUrl b $ Just WOFF2]
        fontStyle c
        fontWeight d

typesetting :: Css
typesetting = do
    html <> body ? do
        "font-feature-settings" -: "'zero', 'ss11', 'ss14', 'ss17', 'onum', 'pnum'"
    h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
        "font-variant-caps" -: "small-caps"

    h1 ? do
        fontSize (pt 21)
        lineHeight $ unitless 1
        marginBottom $ unitless 0.125
        marginTop $ unitless 0.125
    h2 ? do
        fontSize (pt 17.5)
        lineHeight $ unitless 1
        marginTop $ (pt 16.8)
        marginBottom $ (pt 4.2)

    p ? do
        lineHeight $ unitless 1.5
        marginBottom nil

    p |+ p ? do
        textIndent $ indent (em 1)
        marginTop nil

    article |> ( p#firstLetter#nthOfType "1") ? do
        color "#8e16a6"
        float floatLeft
        fontFamily ["Dearest"] [cursive]
        fontSize (rem 3)
        lineHeight (rem 1.5)
        textTransform uppercase
        marginTop (rem 0.25)

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
        "willChange" -: "transform"
        transition "transform" (ms 300) ease none
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
        "willChange" -: "transform"
        transition "transform" (ms 300) ease none

    ".mobile-bar" |> label#after ? do
        content $ stringContent "\61524"
        fontFamily ["FontAwesome"] [monospace]
        fontSize (em 2)
        color "#eee"
        lineHeight (rem 3)
        "backfaceVisibility" -: "hidden"
        transition "transform" (ms 500) ease none

    ".mobile-bar" ** ".navlink" ? do
        "align-self" -: "right"

    query Media.screen [(Media.minWidth (rem 49))] $ do
        ".mobile-bar" ? display none

    ".navlink" ? do
        "font-variant-caps" -: "small-caps"
        display flex
        justifyContent center
    ".navlink" ** ".navicon" ? do
        marginRight (em 0.4)

    "#navbar" ? zIndex 10

    "#navleft" ? do
        float floatLeft
        flexGrow 4
    "#navright" ? do
        float floatRight
        flexGrow 6

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
        flexGrow 2
    nav ** ul ? do
        fontSize (rem 1.2)
        textAlign center
        listStyle none none none
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

    nav ** ".spacer" ? flexGrow 0

    ".toggle-nav:checked ~ nav" ? do
        "will-change" -: "transform"
        "transform" -: "translateX(100%)"
    query Media.screen [(Media.minWidth (rem 49))] $ do
        ".toggle-nav:checked ~ nav" ? do
            transform $ translateX nil

    ".toggle-nav:checked ~ main" ? do
        "will-change" -: "transform"
        "transform" -: "translateX(70%)"
    query Media.screen [(Media.minWidth (rem 49))] $ do
        ".toggle-nav:checked ~ nav" ? do
            transform $ translateX nil

    ".toggle-nav:checked ~ #footer" ? do
        "will-change" -: "transform"
        "transform" -: "translateX(70%)"
    query Media.screen [(Media.minWidth (rem 49))] $ do
        ".toggle-nav:checked ~ #footer" ? do
            transform $ translateX nil

    ".toggle-nav"#checked |+ ".mobile-bar" ? do
        "will-change" -: "transform"
        "transform" -: "translateX(70%)"
    query Media.screen [(Media.minWidth (rem 49))] $ do
        (".toggle-nav"#checked) |+ ".mobile-bar" ? do
            transform $ rotate (deg 180)

mainElement :: Css
mainElement = do
    main_ ? do
        flexGrow 1
        position relative
        paddingTop (rem 5.5)
        margin nil auto auto auto
        transition "transform" (ms 300) ease none
        width (pct 100)
        maxWidth (em 50)

    query Media.screen [(Media.minWidth (px 768))] $ do
        main_ ? do
            paddingLeft (px 20)
            paddingRight (px 20)

post :: Css
post = do
    ".postlink" ? do
        textDecoration none
        display block

    ".postlink"#hover ?
        border none none none

    query Media.screen [(Media.minWidth (px 768))] $ do
        ".postlink"#hover ?
            outline solid (px 2) "#8e16a6"

    ".post" ? do
        marginBottom (px 20)
        padding (px 20) (px 20) (px 20) (px 20)
        backgroundColor "#f0f0f0"
        color "#0e0e0e"

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
        padding nil nil nil nil
        listStyle none none none
        display inlineFlex

    ".tag" ? do
        marginLeft (px 5)

footer :: Css
footer = do
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
listreset = ul ? listStyle none none none

inputreset :: Css
inputreset = button <> input <> select <> textarea ? margin nil nil nil nil

boxreset :: Css
boxreset = do
    html ? boxSizing borderBox
    star <> star#before <> star#after ? boxSizing inherit

mediareset :: Css
mediareset = do
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
