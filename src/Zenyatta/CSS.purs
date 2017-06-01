module Zenyatta.CSS
    ( main
    ) where


-------------------------------------------------------------------------------
import CSS
import CSS.Background as BG
import CSS.TextAlign as TA
import CSS.Common (class None, baseline, inherit, none, unset)
import CSS.Geometry (lineHeight)
import CSS.ListStyle (listStyle)
import CSS.TextAlign (textAlign)
import CSS.VerticalAlign (verticalAlign)
import Zenyatta.Prelude hiding (div,sub)
-------------------------------------------------------------------------------


--TODO: BEM
main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = putStyleSheet css


-------------------------------------------------------------------------------
css :: CSS
css = do
  reset
  html5DisplayRoleReset
  appCSS


-------------------------------------------------------------------------------
reset :: CSS
reset = do
  --TODO: all of it
  sels [html, body, div, span, applet, object, iframe,
     h1, h2, h3, h4, h5, h6, p, blockquote, pre,
     a, abbr, acronym, address, big, cite, code,
     em', del, dfn, img, ins, kbd, q, s, samp,
     small, strike, strong, sub, sup, tt, var,
     b, u, i, center,
     dl, dt, dd, ol, ul, li,
     fieldset, form, label, legend,
     table', caption, tbody, tfoot, thead, tr, th, td,
     article, aside, canvas, details, embed,
     figure, figcaption, footer, header, hgroup,
     menu, nav, output, ruby, section, summary,
     time, mark, audio, video] do
    margin z z z z
    padding z z z z
    fontSize (pct 100.0)
    fontWeight inherit
    --fontStyle inherit
    verticalAlign baseline


-------------------------------------------------------------------------------
applet = fromString "applet"
acronym = fromString "acronym"
big = fromString "big"
strike = fromString "strike"
tt = fromString "tt"
center = fromString "center"
hgroup = fromString "hgroup"
menu = fromString "menu"
ruby = fromString "ruby"
time = fromString "time"
video = fromString "video"
table' = fromString "table"
em' = fromString "em"


-------------------------------------------------------------------------------
quotes :: Quotes -> CSS
quotes = key (fromString "quotes")


-------------------------------------------------------------------------------
data Quotes = QNone


instance noneQuotes :: None Quotes where
  none = QNone

instance valQuotes :: Val Quotes where
  value QNone = Value (Plain "none")


-------------------------------------------------------------------------------
z :: Size Abs
z = pt 0.0


-------------------------------------------------------------------------------
before :: Refinement
before = fromString ":before"


after :: Refinement
after = fromString ":after"


active :: Refinement
active = fromString ":active"


-------------------------------------------------------------------------------
html5DisplayRoleReset :: CSS
html5DisplayRoleReset = do
  sels [article, aside, details, figcaption, figure,
        footer, header, hgroup, menu, nav, section
       ] do
    display block
  body ? do
    lineHeight (unitless 1.0)
  sels [ol, ul] do
    listStyle none unset unset
  sels [blockquote, q] do
    quotes none
  sels [ blockquote `with` before
       , blockquote `with` after
       , q `with` before
       , q `with` after
       ] do
    content (ContentString "")
    content none
  table' ? do
    borderCollapse BorderCollapseCollapse
    borderSpacing (px 0.0)


-------------------------------------------------------------------------------
unitless :: forall a. Number -> Size a
unitless = Size <<< value


-------------------------------------------------------------------------------
content :: Content -> CSS
content = key (fromString "content")


-------------------------------------------------------------------------------
borderCollapse :: BorderCollapse -> CSS
borderCollapse = key (fromString "border-collapse")

data BorderCollapse = BorderCollapseCollapse


instance valBorderCollapse :: Val BorderCollapse where
  value BorderCollapseCollapse = Value (Plain "collapse")


-------------------------------------------------------------------------------
borderSpacing :: forall a. Size a -> CSS
borderSpacing = key (fromString "border-spacing")


-------------------------------------------------------------------------------
data Content = ContentString String
             | ContentNone


instance valContent :: Val Content where
  value ContentNone = Value (Plain "none")
  value (ContentString s) = Value (Plain (quote s))


instance noneContent :: None Content where
  none = ContentNone


-------------------------------------------------------------------------------
--TODO: would be better if we could comma the values, otherwise css is
--way bigger. perhaps there's a minifier
sels ss props = for_ ss $ \s -> s ? props

--TODO: line height wrong

-------------------------------------------------------------------------------
cursor :: Cursor -> CSS
cursor = key (fromString "cursor")


data Cursor = Pointer

instance valCursor :: Val Cursor where
  value Pointer = Value (Plain "pointer")


pointer = Pointer

-------------------------------------------------------------------------------
--TODO: capture color constants
appCSS :: CSS
appCSS = do
  fromString ".app" ? do
    fontFamily ["Roboto"] (sansSerif :| [])
    display flex
    flexDirection row
    height (vh 100.0)
    width (vw 100.0)
    backgroundColor (fromInt 0xFFFFFF)

  fromString ".gutter" ? do
    flexGrow 1

  fromString ".main" ? do
    flexGrow 8
    display flex
    flexDirection column

  fromString ".header" ? do
    backgroundColor (fromInt 0x00BCD4)
    color (fromInt 0xFFFFFF)
    flexGrow 1
    display flex
    flexDirection row

  fromString ".tab-button" ? do
    cursor pointer
    fontSize (vh 5.0)
    flexGrow 1
    textAlign TA.center
    display flex
    flexDirection column
    justifyContent center

  --TODO: .tab-button hover

  fromString ".tab-button" `with` active ? do
    fontWeight bold

  fromString ".footer" ? do
    backgroundColor (fromInt 0xFFFFFF)

  fromString ".content" ? do
    flexGrow 8
    display flex
    flexDirection column

  fromString ".timer-block" ? do
     flexGrow 1
     flexDirection row
     justifyContent center
     display flex

  fromString ".timer-title" ? do
    display flex
    fontSize (vh 4.0)
    fontWeight bold
    justifyContent center

  fromString ".timer" ? do
    display flex
    flexDirection column
    justifyContent center
    flexGrow 2

  fromString ".timer .center-block" ? do
    display flex
    flexDirection row
    justifyContent center

  fromString ".progress-bar" ? do
    display flex
    flexDirection row
    border solid (px 3.0) (fromInt 0x000000)
    height (px 30.0)
    margin (px 10.0) z (px 10.0) z

  fromString ".progress-bar-fill" ? do
    backgroundColor (fromInt 0x000000)

  fromString ".minutes" ? do
    display flex
    flexDirection column

  fromString ".minutes-control" ? do
    flexGrow 1
    display flex
    justifyContent center
    alignItems center
    cursor pointer

  fromString ".minutes-display" ? do
    flexGrow 6
    display flex
    justifyContent center
    alignItems center
    fontSize (vh 10.0)

  fromString ".seconds" ? do
    display flex

  fromString ".seconds-display" ? do
    display flex
    justifyContent center
    alignItems center
    fontSize (vh 10.0)

  fromString ".chime-timer" ? do
    flexGrow 1

  fromString ".controls" ? do
    flexGrow 1
    display flex
    flexDirection row

  fromString ".control" ? do
    display flex
    flexGrow 1
    justifyContent center
    alignItems center

  fromString ".timer .control" ? do
    height (vh 4.0)

  -- TODO credit https://thenounproject.com/chinnaking012/collection/material-basic-ui/?i=1050659
  fromString ".control" ? do
    backgroundSize contain
    backgroundRepeat noRepeat
    backgroundPosition center

  fromString ".control.plus" ? do
    backgroundImage (BG.url "plus.svg")

  fromString ".control.minus"? do
    backgroundImage (BG.url "minus.svg")

  fromString ".control.undo" ? do
    backgroundImage (BG.url "undo.svg")

  fromString ".control.ff" ? do
    backgroundImage (BG.url "ff.svg")

  fromString ".control.pause" ? do
    backgroundImage (BG.url "pause.svg")

  fromString ".control.pause" `with` active ? do
    backgroundImage (BG.url "pause-down.svg")

  fromString ".control.play" ? do
    backgroundImage (BG.url "play.svg")

  fromString ".control.play" `with` active ? do
    backgroundImage (BG.url "play-down.svg")

  --TODO: go back to default on control disabled

-------------------------------------------------------------------------------
opacity :: Number -> CSS
opacity = key (fromString "opacity")
