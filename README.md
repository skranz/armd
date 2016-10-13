# armd: Academic RMarkdown

## Desired features of armd

- Useful markdown blocks, like notes, propositions, footnotes, ...

    #< note Remark
    This is a small note that can be opened in the html file.
    #>

- Create full offline HTML in a single document (including offline Mathjax math). No need to have internet connection.

- Extensible: new block types or widgets can be defined in packages

- Different output format: html, rmd, latex-rmd, ...

- Smart cache. Take account of dependencies.

## Interactive RTutor Widgets

- RTutor package extends armd for interactive tasks in form of widgets. E.g. RTutor chunks or quizes.

- All shiny based interactivity is delegated to RTutor.

- Server hosted problemsets. Students can log in and solve homework.

- Server Hosted Clicker: Use armd presentation live in lecture and push tasks (mainly quizes) to students' mobile phone and let them solve in class.