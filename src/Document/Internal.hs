{-|
  Module      : Document.Internal
  Description : The internals of 'Document'

  This modules contains all the necessary data types and functions to work with
  the type 'Document', as well as all the tag mappings between the language and
  HTML.
-}
module Document.Internal where

import           Data.List
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.Blaze.Html5            (Attribute, AttributeValue, Html)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           ToHtml

------------ Custom types

-- | A 'Document' represents the entire document in the language,
-- comprised of a 'Config' and a list of 'Content'.
data Document = Document Config [Content]
  deriving (Show, Eq)

-- | The 'Config' of a 'Document' contains some metadata
-- about the document, like for example the page title or
-- the layout.
data Config = Config
  { pageTitle       :: Text
  , configCustomCss :: Maybe Text
  , configLayout    :: Text
  } deriving (Show, Eq)

-- | A 'Content' is essentialy a list or a single element ('Unquote' or 'Document.Internal.String').
data Content
  = List Text AttrList [Content]
  | Unquote Text
  | String Text
  deriving (Show, Eq)

-- | A list of attributes, represented as tuples.
type AttrList = [(Text, Content)]

------------ Html conversion

instance ToHtml Content where
  toHtml (String s)          = H.text s
  toHtml (List x as content) = fromMaybe (H.text "") $ M.lookup x tagMapping <*> pure as <*> pure (foldMap toHtml content)
  toHtml _                   = H.text ""

-- | A utility type that takes an 'AttrList' and some 'Html' body
-- to return a 'Html' object.
type HtmlMapping = AttrList -> Html -> Html

-- | The possible list names
defaultListNames :: [Text]
defaultListNames = M.keys tagMapping

-- | Associations between custom list names and their respective HTML tag.
tagMapping :: Map Text HtmlMapping
tagMapping = M.fromList
  [ ("par",       tag  H.p)
  , ("title",     tag  H.h1)
  , ("subtitle",  tag  H.h2)
  , ("image",     tag' H.img)
  , ("b",         tag  H.strong)
  , ("i",         tag  H.i)
  , ("nl",        tag' H.br)
  , ("table",     tag  H.table)
  , ("trow",      tag  H.tr)
  , ("tcel",      tag  H.td)
  , ("link",      tag  H.a)
  , ("html",      tag  H.html)
  , ("head",      tag  H.head)
  , ("pagetitle", tag  H.title)
  , ("body",      tag  H.body)
  , ("div",       tag  H.div)
  , ("style",     tag  H.style)
  , ("link_",     tag' H.link)
  ]

-- | A tag with some content.
tag :: (Html -> Html) -> HtmlMapping
tag t as = foldl' (H.!) t (mapMaybe toAttr as)

-- | A tag without content.
tag' :: Html -> HtmlMapping
tag' t as _ = foldl' (H.!) t (mapMaybe toAttr as)

-- | Converts a tuple into an 'Attribute'.
toAttr :: (Text, Content) -> Maybe Attribute
toAttr (k, String v) = M.lookup k attrs <*> pure (fromString (T.unpack v))
toAttr _             = Nothing

-- | The possible attribute names.
defaultAttrNames :: [Text]
defaultAttrNames = M.keys attrs

-- | Mapping between attribute names and their constructor function.
attrs :: Map Text (AttributeValue -> Attribute)
attrs = M.fromList
  [ ("accept", A.accept)
  , ("acceptCharset", A.acceptCharset)
  , ("accesskey", A.accesskey)
  , ("action", A.action)
  , ("alt", A.alt)
  , ("async", A.async)
  , ("autocomplete", A.autocomplete)
  , ("autofocus", A.autofocus)
  , ("autoplay", A.autoplay)
  , ("challenge", A.challenge)
  , ("charset", A.charset)
  , ("checked", A.checked)
  , ("cite", A.cite)
  , ("class", A.class_)
  , ("cols", A.cols)
  , ("colspan", A.colspan)
  , ("content", A.content)
  , ("contenteditable", A.contenteditable)
  , ("contextmenu", A.contextmenu)
  , ("controls", A.controls)
  , ("coords", A.coords)
  , ("data", A.data_)
  , ("datetime", A.datetime)
  , ("defer", A.defer)
  , ("dir", A.dir)
  , ("disabled", A.disabled)
  , ("draggable", A.draggable)
  , ("enctype", A.enctype)
  , ("for", A.for)
  , ("form", A.form)
  , ("formaction", A.formaction)
  , ("formenctype", A.formenctype)
  , ("formmethod", A.formmethod)
  , ("formnovalidate", A.formnovalidate)
  , ("formtarget", A.formtarget)
  , ("headers", A.headers)
  , ("height", A.height)
  , ("hidden", A.hidden)
  , ("high", A.high)
  , ("href", A.href)
  , ("hreflang", A.hreflang)
  , ("httpEquiv", A.httpEquiv)
  , ("icon", A.icon)
  , ("id", A.id)
  , ("ismap", A.ismap)
  , ("item", A.item)
  , ("itemprop", A.itemprop)
  , ("itemscope", A.itemscope)
  , ("itemtype", A.itemtype)
  , ("keytype", A.keytype)
  , ("label", A.label)
  , ("lang", A.lang)
  , ("list", A.list)
  , ("loop", A.loop)
  , ("low", A.low)
  , ("manifest", A.manifest)
  , ("max", A.max)
  , ("maxlength", A.maxlength)
  , ("media", A.media)
  , ("method", A.method)
  , ("min", A.min)
  , ("multiple", A.multiple)
  , ("name", A.name)
  , ("novalidate", A.novalidate)
  , ("onbeforeonload", A.onbeforeonload)
  , ("onbeforeprint", A.onbeforeprint)
  , ("onblur", A.onblur)
  , ("oncanplay", A.oncanplay)
  , ("oncanplaythrough", A.oncanplaythrough)
  , ("onchange", A.onchange)
  , ("onclick", A.onclick)
  , ("oncontextmenu", A.oncontextmenu)
  , ("ondblclick", A.ondblclick)
  , ("ondrag", A.ondrag)
  , ("ondragend", A.ondragend)
  , ("ondragenter", A.ondragenter)
  , ("ondragleave", A.ondragleave)
  , ("ondragover", A.ondragover)
  , ("ondragstart", A.ondragstart)
  , ("ondrop", A.ondrop)
  , ("ondurationchange", A.ondurationchange)
  , ("onemptied", A.onemptied)
  , ("onended", A.onended)
  , ("onerror", A.onerror)
  , ("onfocus", A.onfocus)
  , ("onformchange", A.onformchange)
  , ("onforminput", A.onforminput)
  , ("onhaschange", A.onhaschange)
  , ("oninput", A.oninput)
  , ("oninvalid", A.oninvalid)
  , ("onkeydown", A.onkeydown)
  , ("onkeyup", A.onkeyup)
  , ("onload", A.onload)
  , ("onloadeddata", A.onloadeddata)
  , ("onloadedmetadata", A.onloadedmetadata)
  , ("onloadstart", A.onloadstart)
  , ("onmessage", A.onmessage)
  , ("onmousedown", A.onmousedown)
  , ("onmousemove", A.onmousemove)
  , ("onmouseout", A.onmouseout)
  , ("onmouseover", A.onmouseover)
  , ("onmouseup", A.onmouseup)
  , ("onmousewheel", A.onmousewheel)
  , ("ononline", A.ononline)
  , ("onpagehide", A.onpagehide)
  , ("onpageshow", A.onpageshow)
  , ("onpause", A.onpause)
  , ("onplay", A.onplay)
  , ("onplaying", A.onplaying)
  , ("onprogress", A.onprogress)
  , ("onpropstate", A.onpropstate)
  , ("onratechange", A.onratechange)
  , ("onreadystatechange", A.onreadystatechange)
  , ("onredo", A.onredo)
  , ("onresize", A.onresize)
  , ("onscroll", A.onscroll)
  , ("onseeked", A.onseeked)
  , ("onseeking", A.onseeking)
  , ("onselect", A.onselect)
  , ("onstalled", A.onstalled)
  , ("onstorage", A.onstorage)
  , ("onsubmit", A.onsubmit)
  , ("onsuspend", A.onsuspend)
  , ("ontimeupdate", A.ontimeupdate)
  , ("onundo", A.onundo)
  , ("onunload", A.onunload)
  , ("onvolumechange", A.onvolumechange)
  , ("onwaiting", A.onwaiting)
  , ("open", A.open)
  , ("optimum", A.optimum)
  , ("pattern", A.pattern)
  , ("ping", A.ping)
  , ("placeholder", A.placeholder)
  , ("preload", A.preload)
  , ("pubdate", A.pubdate)
  , ("radiogroup", A.radiogroup)
  , ("readonly", A.readonly)
  , ("rel", A.rel)
  , ("required", A.required)
  , ("reversed", A.reversed)
  , ("role", A.role)
  , ("rows", A.rows)
  , ("rowspan", A.rowspan)
  , ("sandbox", A.sandbox)
  , ("scope", A.scope)
  , ("scoped", A.scoped)
  , ("seamless", A.seamless)
  , ("selected", A.selected)
  , ("shape", A.shape)
  , ("size", A.size)
  , ("sizes", A.sizes)
  , ("span", A.span)
  , ("spellcheck", A.spellcheck)
  , ("src", A.src)
  , ("srcdoc", A.srcdoc)
  , ("start", A.start)
  , ("step", A.step)
  , ("style", A.style)
  , ("subject", A.subject)
  , ("summary", A.summary)
  , ("tabindex", A.tabindex)
  , ("target", A.target)
  , ("title", A.title)
  , ("type", A.type_)
  , ("usemap", A.usemap)
  , ("value", A.value)
  , ("width", A.width)
  , ("wrap", A.wrap)
  , ("xmlns", A.xmlns)
  ]
