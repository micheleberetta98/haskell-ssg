<TeXmacs|2.1>

<style|generic>

<\body>
  <doc-data|<doc-title|Haskell SSG>|<doc-author|<author-data|<author-name|Beretta
  Michele, Crippa Bianca, Toure Pape Alpha>>>>

  <section|Project description>

  This project is a <with|font-shape|italic|Static Site Generator> written in
  the <with|font-shape|italic|Haskell> language. It consumes some files in a
  custom-defined language and outputs traditional HTML5 files, while handling
  eventual assets files that must not be modified nor treated as source.

  The main objectives are the following

  <\itemize>
    <item><with|font-shape|italic|Simplicity>: the input language has to have
    an easy learning curve, and the project should be aimed at minimal static
    sites, without any unnecessary cognitive overhead;

    <item><with|font-shape|italic|Customization>: it should be possible to
    define your own layouts and apply them in the source files;

    <item><with|font-shape|italic|Code quality>: the resulting code should be
    easily modifiable and as type-safe as possible.
  </itemize>

  <section|The language>

  <section|Parser's details>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-2|<tuple|2|1>>
    <associate|auto-3|<tuple|3|1>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Descrizione
      del progetto> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Linguaggio
      e grammatica> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>Funzionamento
      del parser> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>