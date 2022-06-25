<TeXmacs|2.1.1>

<style|<tuple|article|british|doc|cite-author-year>>

<\body>
  \;

  <doc-data|<doc-title|Static Site Generator>|<doc-author|<author-data|<author-name|Beretta
  Michele, Crippa Bianca, Toure Pape Alpha>|<\author-affiliation>
    University of Bergamo

    School of Engineering

    Formal Languages & Compilers

    2021-2022
  </author-affiliation>>>>

  <chapter|Project description>

  This project is a <with|font-shape|italic|Static Site Generator> written in
  the <with|font-shape|italic|Haskell> programming language. As the name
  suggests, a static site generator (SSG for here on) is a tool that allows
  the creation static web pages from templates and raw data. More
  specifically, it consumes files written in a custom-defined language and
  outputs HTML5 files.

  Such tools have long attracted the attention of various players in the web
  space as they provide an abstraction that achieves two things: the first is
  a degree of automation in the domain of web development, specifically in
  making web pages; the second is transpilation as there is a source to
  source compilation. Combined together, they prove to be an effective
  approach to either building user interfaces for web site building used by
  non technical users or styling the same website in vastly different ways.

  <chapter|Design goals>

  The main non functional requirements to be pursued are the following:

  \;

  <\itemize>
    <item><with|font-shape|italic|Simplicity>: the input language ought to
    have an easy learning curve, prioritizing minimal static sites, valuing
    user productivity;

    <item><with|font-shape|italic|Extensibility>: with future change in mind,
    it should be possible to define your custom layouts to be applied in the
    source files;

    <item><with|font-shape|italic|Correctness>: as any compilation,
    consistency in the output produced is of utmost importance;

    <item><with|font-shape|italic|Security>: attacker controlled arbitrarily
    long inputs is an intrinsically hard threat-model to handle;

    <item><with|font-shape|italic|Maintainability>: the resulting code should
    be easily modifiable;
  </itemize>

  The programming language chosen (Haskell) aims to address several of the
  aforementioned points by providing an extremely strong type system, type
  safety, immutability, memory safety and strong and verifiable formal
  guarantees about correctness.

  <chapter|Usage>

  Usage is quite straightforward, after installing the binary provide a
  folder containing the source files written in the DSL. Here is an example
  of running <verbatim|haskell-ssg> for a <verbatim|website> folder, with a
  <verbatim|build> output folder. Once the file are parsed and rendered, a
  web server is spawn at <verbatim|localhost:4000> so that you can preview
  the output live.

  \;

  <\verbatim>
    $ ./haskell-ssg website --output build

    Reading macros...

    Reading source files...

    Building...

    Server running on port 4000...

    \<gtr\>\ 
  </verbatim>

  \;

  In Table <reference|table:cli-args> are visible all available command line
  options. Once the web server is running, the program presents a very simple
  prompt and accept 3 commands:

  <\itemize>
    <item><verbatim|r> or <verbatim|reload>, which allows the user to
    re-parse the files to update the preview;

    <item><verbatim|q> or <verbatim|quit>, to exit the program;

    <item><verbatim|h> or <verbatim|help>, to view which commands are
    available.
  </itemize>

  \;

  <\big-table|<tabular|<tformat|<table|<row|<cell|<block|<tformat|<table|<row|<cell|<with|font-shape|italic|Argument>>|<cell|<with|font-shape|italic|Description>>|<cell|<with|font-shape|italic|Default>>>|<row|<cell|<verbatim|SRC>>|<cell|The
  source folder, containing all the main DSL
  files>|<cell|<verbatim|_src>>>|<row|<cell|<verbatim|--output>>|<cell|The
  output folder, which will contain all HTML
  files>|<cell|<verbatim|_build>>>|<row|<cell|<verbatim|--layouts>>|<cell|A
  folder with <with|font-shape|italic|layout>
  macros>|<cell|<verbatim|_layouts>>>|<row|<cell|<verbatim|--macros>>|<cell|A
  folder with <with|font-shape|italic|generic> macros, not to be used as
  layouts>|<cell|<verbatim|_macros>>>|<row|<cell|<verbatim|--static>>|<cell|A
  folder with static files>|<cell|<verbatim|_static>>>|<row|<cell|<verbatim|--out-static>>|<cell|Where
  to copy the static files>|<cell|<verbatim|_build/static>>>>>>>>>>>>
    <label|table:cli-args>All command line arguments and their defaults.
  </big-table>

  <page-break*><chapter|DSL>

  The DSL (Domain Specific Language) used as source language to design the
  web pages is a LISP-esque formatting language, where each element has a 1
  to 1 correspondance to an HTML attributes or tags. It is also possible to
  define <with|font-shape|italic|custom macros> so that custom components can
  be re-used, or even <with|font-shape|italic|custom layouts> for entire
  pages.

  <subsection|Grammar>

  Here is the abstract syntax of the language for a document definition and a
  macro definition.

  <center|<tabular|<tformat|<table|<row|<cell|<with|font-shape|italic|document>>|<cell|::=>|<cell|<with|font-shape|italic|config>
  <with|font-shape|italic|content*>>>|<row|<cell|<with|font-shape|italic|config>>|<cell|::=>|<cell|<verbatim|{>
  <with|font-series|bold|title> <with|font-shape|italic|string>
  (<with|font-series|bold|custom-css> <with|font-shape|italic|string>)?
  (<with|font-series|bold|layout> <with|font-shape|italic|string>)?
  <verbatim|}>>>|<row|<cell|<with|font-shape|italic|content>>|<cell|::=>|<cell|<with|font-shape|italic|unquote>
  \| <with|font-shape|italic|string> \| <with|font-shape|italic|list> \|
  <with|font-shape|italic|macro-call>>>|<row|<cell|<with|font-shape|italic|unquote>>|<cell|::=>|<cell|<verbatim|@>
  <with|font-shape|italic|identifier>>>|<row|<cell|<with|font-shape|italic|list>>|<cell|::=>|<cell|<verbatim|(>
  <with|font-shape|italic|identifier> <with|font-shape|italic|attr-list>?
  <with|font-shape|italic|content*> <verbatim|)>>>|<row|<cell|<with|font-shape|italic|attr-list>>|<cell|::=>|<cell|<verbatim|[>
  <with|font-shape|italic|tuple*> <verbatim|]>>>|<row|<cell|<with|font-shape|italic|tuple>>|<cell|::=>|<cell|<verbatim|(>
  <with|font-shape|italic|identifier> <with|font-shape|italic|string>?
  <verbatim|)>>>|<row|<cell|<with|font-shape|italic|macro-call>>|<cell|::=>|<cell|<verbatim|(>
  <with|font-shape|italic|identifier> <with|font-shape|italic|macro-arg*>
  <verbatim|)>>>|<row|<cell|<with|font-shape|italic|macro-arg>>|<cell|::=>|<cell|<verbatim|(>
  <with|font-shape|italic|identifier> <with|font-shape|italic|content*>
  )>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|font-shape|italic|string>>|<cell|::=>|<cell|<verbatim|">
  <with|font-shape|italic|s-char*> <verbatim|">>>|<row|<cell|<with|font-shape|italic|s-char>>|<cell|::=>|<cell|<verbatim|\\>
  any \| any but <verbatim|">>>|<row|<cell|<with|font-shape|italic|identifier>>|<cell|::=>|<cell|<with|font-shape|italic|i-char>+>>|<row|<cell|<with|font-shape|italic|i-char>>|<cell|::=>|<cell|none
  of <verbatim|()[]"@>, blank or newline>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|font-shape|italic|macro-def>>|<cell|::=>|<cell|<verbatim|'(>
  <with|font-shape|italic|identifier> <with|font-shape|italic|content*>
  <verbatim|)>>>>>>>

  If the <with|font-shape|italic|layout> property in the
  <with|font-shape|italic|config> section of a document is missing,
  <verbatim|"default"> will be used.

  <subsection|Tags and attributes>

  All identifiers that are accepted as <with|font-shape|italic|list> names
  are visible in Table <reference|table:list-ids>, while all identifiers that
  are accepted as <with|font-shape|italic|tuple> keys are visible in Table
  <reference|table:tuple-key-ids>.

  <center|<big-table|<block|<tformat|<cwith|1|1|1|2|font-series|bold>|<table|<row|<cell|<with|font-series|bold|<with|font-shape|italic|Identifier>>>|<cell|<with|font-shape|italic|HTML
  tag>>>|<row|<cell|par>|<cell|p>>|<row|<cell|title>|<cell|h1>>|<row|<cell|subtitle>|<cell|h2>>|<row|<cell|image>|<cell|img>>|<row|<cell|b>|<cell|strong>>|<row|<cell|i>|<cell|i>>|<row|<cell|nl>|<cell|br>>|<row|<cell|table>|<cell|table>>|<row|<cell|trow>|<cell|tr>>|<row|<cell|tcel>|<cell|td>>|<row|<cell|link>|<cell|a>>|<row|<cell|html>|<cell|html>>|<row|<cell|head>|<cell|head>>|<row|<cell|pagetitle>|<cell|title>>|<row|<cell|body>|<cell|body>>|<row|<cell|div>|<cell|div>>|<row|<cell|style>|<cell|style>>|<row|<cell|link_>|<cell|link>>|<row|<cell|list>|<cell|ul>>|<row|<cell|enumerate>|<cell|ol>>|<row|<cell|item>|<cell|li>>>>>|<label|table:list-ids>Identifiers
  accepted as list names>>

  <big-table|<block|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|39|2|2|cell-halign|c>|<cwith|1|38|2|2|cell-halign|l>|<cwith|1|21|3|3|cell-halign|l>|<cwith|22|39|3|3|cell-halign|c>|<cwith|22|38|3|3|cell-halign|l>|<cwith|1|30|4|4|cell-halign|l>|<cwith|31|39|4|4|cell-halign|l>|<cwith|1|8|5|5|cell-halign|l>|<table|<row|<cell|accept>|<cell|id>|<cell|onended>|<cell|onsubmit>|<cell|summary>>|<row|<cell|acceptCharset>|<cell|icon>|<cell|onerror>|<cell|onsuspend>|<cell|tabindex>>|<row|<cell|accesskey>|<cell|ismap>|<cell|onfocus>|<cell|ontimeupdate>|<cell|target>>|<row|<cell|action>|<cell|itemprop>|<cell|onformchange>|<cell|onundo>|<cell|title>>|<row|<cell|alt>|<cell|itemscope>|<cell|onforminput>|<cell|onunload>|<cell|type>>|<row|<cell|async>|<cell|keytype>|<cell|onhaschange>|<cell|onvolumechange>|<cell|usemap>>|<row|<cell|autocomplete>|<cell|label>|<cell|oninput>|<cell|onwaiting>|<cell|value>>|<row|<cell|autofocus>|<cell|lang>|<cell|oninvalid>|<cell|open>|<cell|width>>|<row|<cell|autoplay>|<cell|list>|<cell|onkeydown>|<cell|optimum>|<cell|wrap>>|<row|<cell|challenge>|<cell|loop>|<cell|onkeyup>|<cell|pattern>|<cell|xmlns>>|<row|<cell|charset>|<cell|low>|<cell|onload>|<cell|ping>|<cell|>>|<row|<cell|checked>|<cell|manifest>|<cell|onloadeddata>|<cell|placeholder>|<cell|>>|<row|<cell|cite>|<cell|max>|<cell|onloadedmetadata>|<cell|preload>|<cell|>>|<row|<cell|class>|<cell|maxlength>|<cell|onloadstart>|<cell|pubdate>|<cell|>>|<row|<cell|cols>|<cell|media>|<cell|onmessage>|<cell|radiogroup>|<cell|>>|<row|<cell|colspan>|<cell|method>|<cell|onmousedown>|<cell|readonly>|<cell|>>|<row|<cell|content>|<cell|min>|<cell|onmousemove>|<cell|rel>|<cell|>>|<row|<cell|contenteditable>|<cell|multiple>|<cell|onmouseout>|<cell|required>|<cell|>>|<row|<cell|contextmenu>|<cell|name>|<cell|onmouseover>|<cell|reversed>|<cell|>>|<row|<cell|controls>|<cell|novalidate>|<cell|onmouseup>|<cell|role>|<cell|>>|<row|<cell|coords>|<cell|onbeforeonload>|<cell|onmousewheel>|<cell|rows>|<cell|>>|<row|<cell|data>|<cell|onbeforeprint>|<cell|ononline>|<cell|rowspan>|<cell|>>|<row|<cell|datetime>|<cell|onblur>|<cell|onpagehide>|<cell|sandbox>|<cell|>>|<row|<cell|defer>|<cell|oncanplay>|<cell|onpause>|<cell|scope>|<cell|>>|<row|<cell|dir>|<cell|oncanplaythrough>|<cell|onplay>|<cell|scoped>|<cell|>>|<row|<cell|disabled>|<cell|onchange>|<cell|onplaying>|<cell|seamless>|<cell|>>|<row|<cell|draggable>|<cell|onclick>|<cell|onprogress>|<cell|selected>|<cell|>>|<row|<cell|enctype>|<cell|oncontextmenu>|<cell|onpropstate>|<cell|shape>|<cell|>>|<row|<cell|for>|<cell|ondblclick>|<cell|onratechange>|<cell|size>|<cell|>>|<row|<cell|formaction>|<cell|ondrag>|<cell|onreadystatechange>|<cell|sizes>|<cell|>>|<row|<cell|formenctype>|<cell|ondragend>|<cell|onredo>|<cell|span>|<cell|>>|<row|<cell|formmethod>|<cell|ondragenter>|<cell|onresize>|<cell|spellcheck>|<cell|>>|<row|<cell|formnovalidate>|<cell|ondragleave>|<cell|onscroll>|<cell|src>|<cell|>>|<row|<cell|formtarget>|<cell|ondragover>|<cell|onseeked>|<cell|srcdoc>|<cell|>>|<row|<cell|headers>|<cell|ondragstart>|<cell|onseeking>|<cell|start>|<cell|>>|<row|<cell|height>|<cell|ondrop>|<cell|onselect>|<cell|step>|<cell|>>|<row|<cell|hidden>|<cell|ondurationchange>|<cell|onstalled>|<cell|style>|<cell|>>|<row|<cell|high>|<cell|onemptied>|<cell|onstorage>|<cell|subject>|<cell|>>>>>|<label|table:tuple-key-ids>Identifiers
  accepted as attributes lists' tuple key names>

  <subsection|Example>

  What follows is a simple example of a <with|font-shape|italic|document>
  written in the DSL.

  <\render-code>
    <\verbatim>
      { title "Home page" custom-css "/static/custom.css" layout "default" }

      \;

      (title "Home page")

      \;

      (subtitle "Section 1")

      (par\ 

      \ \ (list [(class "custom-css-class")]

      \ \ \ \ (item "First item")

      \ \ \ \ (item "Second item"))
    </verbatim>
  </render-code>

  <chapter|Parser's details>

  Haskell SSG makes uses of <with|font-shape|italic|Megaparsec>, an
  industrial strength monadic parser combinator library derived from the
  simpler <with|font-shape|italic|Parsec> library, bundled with GHC. Proper
  installation of the library requires a working installation of
  <verbatim|cabal> and <verbatim|ghc>, which are part of any Haskell
  installation.

  <subsection|Monad>

  In functional programming, a <with|font-series|bold|monad> is a software
  design pattern with a structure that combines functions and wraps their
  return values in a type with additional information (usually something
  about computation).

  In addition to defining a wrapping <with|font-series|bold|monadic type>,
  monads define two operators: one to wrap a value in the monad type, called
  <verbatim|return> in Haskell, and another to compose together functions
  that output values of the monad type, called <verbatim|bind> or
  <verbatim|\<gtr\>\<gtr\>=> in Haskell - these are known as
  <with|font-series|bold|monadic functions>. These functions must also obey
  some laws, namely <verbatim|return> must act as an identity and
  <verbatim|\<gtr\>\<gtr\>=> must be \Passociative\Q.

  Functional languages use monads to turn complicated sequences of functions
  into succinct pipelines that abstract away control flow and side-effects.

  In <with|font-shape|italic|Megaparsec>, parsers are monad, and as such can
  be combined using a uniform abstraction, common throughout all Haskell
  code. Moreover, parser can also be <with|font-shape|italic|monad
  transformers>, which means they can combine with other monads to enhance
  functionality. For example, in this project the parser is combined with a
  <verbatim|State Env>, another monad used to keep track of eventual macro
  declarations.

  <subsection|Parser combinators>

  A <with|font-shape|italic|parser combinator> is a higher-order function
  that takes one or more parsers as input and produces a new parser as its
  output.

  To put it simply, combining parsers means that given two distinct parsers a
  third one can be made that is the sum of its part, namely, can parse both
  grammars parsed by the two smaller parsers.\ 

  Compared to Bison/yacc/ANTLR which are parsers
  <with|font-series|bold|generators>, <with|font-shape|italic|Megaparsec> is
  a parser combinator library. In functional terms, parsing combination is
  akin to treating parsers as first class values and making use of higher
  order functions to compose them.

  Therefore, parser combinators offer a universal and flexible approach to
  parsing. They follow the structure of an underlying grammar, are modular,
  well-structured, easy to maintain, and can recognize a large variety of
  languages including context-sensitive ones.

  However, these advantages generally introduce a performance overhead as the
  same powerful parsing algorithm is used to recognize every language, even
  the simplest one. Specifically, a parser combinator uses the full power of
  a Turing-equivalent formalism to recognize even simple languages that could
  be recognized by finite state machines or pushdown automata. Time-wise,
  parser combinators cannot compete with parsers generated by well-performing
  parser generators or optimized hand-written code. Meta-programming
  approaches such as macros and staging have been applied to Scala parser
  combinators with significant performance improvements. In general, these
  approaches remove composition overhead and intermediate representations.
  Moreover, \ parser generators can give static guarantees about termination
  and non-ambiguity which, depending on the specific implementation, parser
  combinator libraries are not able to give.

  <page-break*><chapter|Errors>

  Errors are managed entirely by <with|font-shape|italic|Megaparsec>.
  <with|font-shape|italic|Megaparsec>, upon parsing failure, returns a
  <with|font-shape|italic|ParseError s e>, which is an abstract data type
  that represents error messages on a stream of type <verbatim|s> for errors
  of type <verbatim|e>, and is made as follows

  <\scm-code>
    <\code>
      \ data ParseError s e

      \ \ \ \ = TrivialError Int (Maybe (ErrorItem (Token s))) (Set
      (ErrorItem (Token s)))

      \ \ \ \ \| FancyError Int (Set (ErrorFancy e))
    </code>
  </scm-code>

  More specifically, a <verbatim|TrivialError> is generated by Megaparsec's
  machinery and includes an offset, an unexpected <verbatim|Token s> (if any)
  and a set of expected <verbatim|Token s>. A <verbatim|FancyError>, on the
  other way, is used for custom errors, represented by <verbatim|ErrorFancy
  e>.

  <\scm-code>
    <\code>
      data ErrorItem t

      \ \ \ \ = Tokens (NonEmpty t)

      \ \ \ \ \| Label (NonEmtpy Char)

      \ \ \ \ \| EndOfInput

      \ \ \ \ 

      data ErrorFancy e

      \ \ \ \ = ErrorFail String

      \ \ \ \ \| ErrorIndentation Ordering Pos Pos

      \ \ \ \ \| ErrorCustom e
    </code>
  </scm-code>

  \;

  In order to enrich error reporting, three semantic errors have been added
  to clarify the nature of the error the user, defined by the following type.

  <\scm-code>
    <\code>
      data CustomError

      \ \ \ \ = InvalidListName Text

      \ \ \ \ \| InvalidAttrName Text

      \ \ \ \ \| IdentifierAlreadyTaken Text
    </code>
  </scm-code>

  These are represented by the following error messages.

  \;

  <block|<tformat|<cwith|1|1|1|-1|font-series|bold>|<cwith|1|-1|1|-1|cell-halign|l>|<twith|table-width|1par>|<twith|table-hmode|exact>|<table|<row|<cell|<with|font-shape|italic|Error>>|<cell|<with|font-shape|italic|Message>>>|<row|<cell|<verbatim|InvalidListName
  name>>|<cell|\P<verbatim|name>\Q is not a valid list
  name>>|<row|<cell|<verbatim|InvalidAttrName
  name>>|<cell|\P<verbatim|name>\Q is not a valid attribute
  name>>|<row|<cell|<verbatim|IdentifierAlreadyTaken
  name>>|<cell|\P<verbatim|name>\Q is already declared and cannot be used as
  a macro>>>>>

  \;

  \;

  Here an example of a syntax error:

  <center|<image|syntax-error.png|0.8par|||>>

  \;

  And here an example of some semantic errors:

  <center|<image|semantic-error.png|362pt|151pt||>>
</body>

<\initial>
  <\collection>
    <associate|page-crop-marks|a4>
    <associate|page-medium|paper>
    <associate|preamble|false>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-10|<tuple|3|4>>
    <associate|auto-11|<tuple|5|5>>
    <associate|auto-12|<tuple|1|5>>
    <associate|auto-13|<tuple|2|5>>
    <associate|auto-14|<tuple|6|6>>
    <associate|auto-2|<tuple|2|1>>
    <associate|auto-3|<tuple|3|2>>
    <associate|auto-4|<tuple|1|2>>
    <associate|auto-5|<tuple|4|3>>
    <associate|auto-6|<tuple|1|3>>
    <associate|auto-7|<tuple|2|3>>
    <associate|auto-8|<tuple|2|3>>
    <associate|auto-9|<tuple|3|4>>
    <associate|table:cli-args|<tuple|1|2>>
    <associate|table:list-ids|<tuple|2|3>>
    <associate|table:tuple-key-ids|<tuple|3|4>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|<\surround|<hidden-binding|<tuple>|1>|>
        All command line arguments and their defaults.
      </surround>|<pageref|auto-4>>

      <tuple|normal|<surround|<hidden-binding|<tuple>|2>||Identifiers
      accepted as list names>|<pageref|auto-8>>

      <tuple|normal|<surround|<hidden-binding|<tuple>|3>||Identifiers
      accepted as attributes lists' tuple key names>|<pageref|auto-9>>
    </associate>
    <\associate|toc>
      <vspace*|2fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|font-size|<quote|1.19>|1<space|2spc>Project
      description> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|1fn>

      <vspace*|2fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|font-size|<quote|1.19>|2<space|2spc>Design
      goals> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|1fn>

      <vspace*|2fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|font-size|<quote|1.19>|3<space|2spc>Usage>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|1fn>

      <vspace*|2fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|font-size|<quote|1.19>|4<space|2spc>DSL>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5><vspace|1fn>

      <with|par-left|<quote|1tab>|1<space|2spc>Grammar
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-6>>

      <with|par-left|<quote|1tab>|2<space|2spc>Tags and attributes
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-7>>

      <with|par-left|<quote|1tab>|3<space|2spc>Example
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-10>>

      <vspace*|2fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|font-size|<quote|1.19>|5<space|2spc>Parser's
      details> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-11><vspace|1fn>

      <with|par-left|<quote|1tab>|1<space|2spc>Monad
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-12>>

      <with|par-left|<quote|1tab>|2<space|2spc>Parser combinators
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-13>>

      <vspace*|2fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|font-size|<quote|1.19>|6<space|2spc>Errors>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-14><vspace|1fn>
    </associate>
  </collection>
</auxiliary>