%%
%% This is file `faofactbook.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% faofactbook.dtx  (with options: `class')
%% Copyright (c) 2011, Food and Agriculture Organization of the  United Nations
%% 
%% All rights reserved.
%% \CharacterTable
%%  {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%   Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%   Digits        \0\1\2\3\4\5\6\7\8\9
%%   Exclamation   \!     Double quote  \"     Hash (number) \#
%%   Dollar        \$     Percent       \%     Ampersand     \&
%%   Acute accent  \'     Left paren    \(     Right paren   \)
%%   Asterisk      \*     Plus          \+     Comma         \,
%%   Minus         \-     Point         \.     Solidus       \/
%%   Colon         \:     Semicolon     \;     Less than     \<
%%   Equals        \=     Greater than  \>     Question mark \?
%%   Commercial at \@     Left bracket  \[     Backslash     \\
%%   Right bracket \]     Circumflex    \^     Underscore    \_
%%   Grave accent  \`     Left brace    \{     Vertical bar  \|
%%   Right brace   \}     Tilde         \~}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{faofactbook}
[2012/03/25 v0.14 Typesetting FAO Factbook]
\PassOptionsToPackage{papersize={10cm,21cm},top=1cm,bottom=1cm,
left=1cm, right=1cm, twoside}{geometry}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{faoyearbook}}
\ProcessOptions\relax
\LoadClass{faoyearbook}
\RequirePackage{makeidx}
\RequirePackage[clockwise]{rotating}
\hypersetup{breaklinks,colorlinks=false,pdfborder=0 0 0,
  pdfauthor={FAO},
  pdfsubject={Statistical Pocket Book of the Food And Agricultural Organization for the United Nations},
  pdftitle={Statistical Pocket Book of the Food And Agricultural Organization for the United Nations},
  pdfkeywords={FAO, Food Security, Undernourishment, Sustainable agriculture},
  pdfpagelayout=TwoColumnRight,
  pdfnewwindow=true
}
% ---------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------
% New Font sizes 
% ---------------------------------------------------------------------------------
\renewcommand\normalsize{%
   \@setfontsize\normalsize{6\p@}{7}% Affects caption sizes and texts size within Chatpage environments
   \abovedisplayskip 1\p@ \@plus1\p@ \@minus1\p@
   \abovedisplayshortskip \z@ \@plus2\p@
   \belowdisplayshortskip 1\p@ \@plus2\p@ \@minus2\p@
   \belowdisplayskip \abovedisplayskip
   \let\@listi\@listI}
\normalsize
\renewcommand\small{%
   \@setfontsize\small{5.5\p@}{6}% Affects at least the small tables within ChartPage envisonments
   \abovedisplayskip 4\p@ \@plus1\p@ \@minus1\p@
   \abovedisplayshortskip \z@ \@plus1\p@
   \belowdisplayshortskip 2\p@ \@plus\p@ \@minus1\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 2\p@ \@plus\p@ \@minus\p@
               \parsep 1\p@ \@plus\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
\renewcommand\footnotesize{\small}
\renewcommand\scriptsize{\@setfontsize\scriptsize\@vpt\@vipt}
\renewcommand\tiny{\@setfontsize\tiny\@ivpt\@vpt}
\renewcommand\large{\@setfontsize\large\@viipt\@viiipt}
\renewcommand\Large{\@setfontsize\Large\@xipt{13}} % Section are size large
\renewcommand\LARGE{\@setfontsize\LARGE\@xivpt{16}}
\renewcommand\huge{\@setfontsize\huge\@xvpt{18}}
\renewcommand\Huge{\@setfontsize\Huge\@xviipt{20}}
\setlength\parskip{\z@}
\AtBeginDocument{\geometry{papersize={10cm,21cm}, layoutsize={10cm,21cm},
  left=1cm,right=1cm,bottom=1cm,top=1cm,twoside}%
  \setlength\columnsep{20pt}%
\savegeometry{standard}}
\newlength\@stripwidth
\define@key{fao}{stripwidth}{\setlength{\@stripwidth}{#1}}
\faoset{stripwidth=7.2mm}
% ---------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------
% Headers & Footers
% ---------------------------------------------------------------------------------
\fancypagestyle{plain}{%
  \fancyhf{}%
  \fancyfoot[RO,LE]{\thepage}%
  \fancyhead[LO]{%
    \begin{picture}(0,0)%
      \put(268,-157){\raisebox{-0.729\Gm@layoutheight}{%
          \color{@bgcolor}\hspace*{-\@stripwidth}%
          \rule{\@stripwidth}{1.05\Gm@layoutheight}}}%
    \end{picture}}
  \fancyhead[LE]{%
    \begin{picture}(0,0)%
      \put(-39,-157){\raisebox{-0.729\Gm@layoutheight}{%
          \color{@bgcolor}\rule{\@stripwidth}{1.05\Gm@layoutheight}}}%
    \end{picture}}
  \renewcommand{\headrulewidth}{\z@}%
  \renewcommand{\footrulewidth}{\z@}}
\AtBeginDocument{\pagestyle{plain}}
\footskip15\p@
\definecolor{FAOblue}{cmyk}{0.69,0.34,0,0}
% ---------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------
% New Commands
% ---------------------------------------------------------------------------------
\newcommand\CountryData[1]{\clearpage
  \phantomsection
  \addcontentsline{loc}{country}{#1}%
  \index{#1}%
  \colorbox{FAOblue}{\parbox{\dimexpr\columnwidth-2ex}{\Large\color{white}\hypertarget{#1}{#1}}}\par}
% ---------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------
% Define the DataUnit environment
% ---------------------------------------------------------------------------------
\newenvironment{DataUnit}[1]{%
  \addvspace{-0.5\baselineskip}
  \begin{minipage}{\columnwidth}\begin{multicols}{2}%
      {\raggedright\bfseries#1\par}}{\end{multicols}\addvspace{-2\baselineskip}%
    \rule{\columnwidth}{0.1pt}%
  \end{minipage}\par\nointerlineskip}
\newcommand\DataField[2]{\par\hangindent=2ex\relax
  \raggedright#1\unskip~\hfill#2\par}
\def\@makeschapterhead#1{%
  {\parindent \z@ \raggedright
    \normalfont
    \interlinepenalty\@M
    \Huge \bfseries  #1\par\nobreak
    \vskip 20\p@
  }}

% ---------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------
% Define the Table of Contents
% ---------------------------------------------------------------------------------                                   

\def\newpart#1{\clearpage
  \arrayrulecolor{#1}%
  \faoset{bgcolor=#1}\selectcolor
  \addtocontents{toc}{\string\@newpart}}
\def\@newpart{\leavevmode\par\addvspace{5\p@}%
  \@fao@tocrule@start=\pagetotal
  \advance\@fao@tocrule@start by 10\p@\relax}
\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {1sp}%
                                   {1sp}%
                                   {\normalfont\large\bfseries}} % Section font size in introduc
\renewcommand\subsection{\@startsection {subsection}{1}{\z@}%
                                   {8\p@}%
                                   {1sp}%
                                   {\normalfont\bfseries}}

\def\tableofcontents{\cleardoublepage
  \@makeschapterhead{Contents}%
  \@mkboth{\MakeUppercase\contentsname}%
  {\MakeUppercase\contentsname}%
  \@starttoc{toc}}

\renewcommand*\l@section[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty{-\@highpenalty}%
    \setlength\@tempdima{3em}%
    \begingroup
     \parskip\z@
      \parindent \z@ \rightskip -100pt
      \parfillskip -\@pnumwidth
      {\leavevmode\strut
       #1\hfil \hb@xt@\@pnumwidth{\hss #2}}\par\@draw@tocrule@section
       \nobreak
         \global\@nobreaktrue
         \everypar{\global\@nobreakfalse\everypar{}}%
    \endgroup
  \fi}

\def\@draw@tocrule@section{\@fao@tocrule@height=\pagetotal
  \advance\@fao@tocrule@height-\@fao@tocrule@start
  \advance\@fao@tocrule@height5\p@
  \bgroup\parskip\z@\small
  \raisebox{\baselineskip}[\z@][\z@]{\parbox[b][\z@]{\z@}{\hspace*{240\p@}\color{@bgcolor}\rule{21\p@}{\@fao@tocrule@height}}}%
  \par\vspace{-\baselineskip}\egroup}

\def\listofcountries{{\twocolumn[\@makeschapterhead{List of
      Countries}]}%
  \@mkboth{\MakeUppercase\indexname}%
  {\MakeUppercase\indexname}%
  \@starttoc{loc}}

\newcommand*\l@country[2]{
    \addpenalty\@secpenalty
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
    \endgroup}

% ---------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------
% Define the ChartPage spread
% ---------------------------------------------------------------------------------
\newenvironment{ChartPage}[1]{%
  \faoset{floatwidth=0.52\textwidth}
  \gdef\@chartname{\ignorespaces#1}%
  \LeftText{To Be Done}%
  \RightText{To Be Done}%
  \LeftChart{To Be Done}%
  \RightChart{To Be Done}%
  \BottomChart{To Be Done}}{%
  \clearpage\ifodd\c@page\hbox{}\newpage\fi
  \phantomsection\addcontentsline{toc}{section}{\@chartname}%
  \raisebox{-\height}{\strut\parbox{0.50\textwidth}{\@LeftText}}%
  \hspace{0.05\textwidth}%
  \raisebox{-\height}{\strut\parbox{0.40\textwidth}{\@RightText}}\par\medskip
  \raisebox{-\height}{\strut\parbox{0.47\textwidth}{\@LeftChart}}%
  \hspace{0.05\textwidth}%
  \raisebox{-\height}{\strut\parbox{0.47\textwidth}{\@RightChart}}\par\medskip
  \faoset{floatwidth=\columnwidth}
  {\centering\@BottomChart\par}}
\long\def\LeftText#1{\gdef\@LeftText{{\bfseries\large\@chartname\par}#1}}
\long\def\RightText#1{\gdef\@RightText{{\bfseries\large\vphantom{\@chartname}\par}#1}}
\long\def\LeftChart#1{\gdef\@LeftChart{#1}}
\long\def\RightChart#1{\gdef\@RightChart{#1}}
\long\gdef\BottomChart#1{\gdef\@BottomChart{#1}}%

% ---------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------
% Define the Chart2Page spread
% ---------------------------------------------------------------------------------
\newenvironment{Chart2Page}[1]{%
  \faoset{floatwidth=0.50\textwidth}
  \gdef\@chartname{\ignorespaces#1}%
  \LeftText{To Be Done}%
  \RightText{To Be Done}%
  \LeftChart{To Be Done}%
  \RightChart{To Be Done}%
  \BottomLeftChart{To Be Done}%
  \BottomLeftChart{To Be Done}}{%
  \clearpage\ifodd\c@page\hbox{}\newpage\fi
  \phantomsection\addcontentsline{toc}{section}{\@chartname}%
  \raisebox{-\height}{\strut\parbox{0.50\textwidth}{\@LeftText}}%
  \hspace{0.05\textwidth}%
  \raisebox{-\height}{\strut\parbox{0.40\textwidth}{\@RightText}}\par\medskip
  \raisebox{-\height}{\strut\parbox{0.48\textwidth}{\@LeftChart}}%
  \hspace{0.09\textwidth}%
  \raisebox{-\height}{\strut\parbox{0.48\textwidth}{\@RightChart}}\par\medskip
  \vspace{0.09\textwidth}%
  \raisebox{-\height}{\strut\parbox{0.50\textwidth}{\@BottomLeftChart}}%
  \hspace{0.09\textwidth}%
  \raisebox{-\height}{\strut\parbox{0.50\textwidth}{\@BottomRightChart}}\par\medskip}
\long\def\LeftText#1{\gdef\@LeftText{{\bfseries\large\@chartname\par}#1}}
\long\def\RightText#1{\gdef\@RightText{{\bfseries\large\vphantom{\@chartname}\par}#1}}
\long\def\LeftChart#1{\gdef\@LeftChart{#1}}
\long\def\RightChart#1{\gdef\@RightChart{#1}}
\long\def\BottomLeftChart#1{\gdef\@BottomLeftChart{#1}}
\long\def\BottomRightChart#1{\gdef\@BottomRightChart{#1}}

% ---------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------
% Define the Chart3Page spread
% ---------------------------------------------------------------------------------
\newenvironment{Chart3Page}[1]{%
  \faoset{floatwidth=0.50\textwidth}
  \gdef\@chartname{\ignorespaces#1}%
  \LeftText{To Be Done}%
  \RightText{To Be Done}%
  \LeftChart{To Be Done}%
  \RightChart{To Be Done}}{%
  \clearpage\ifodd\c@page\hbox{}\newpage\fi
  \phantomsection\addcontentsline{toc}{section}{\@chartname}%
  \raisebox{-\height}{\strut\parbox{0.50\textwidth}{\@LeftText}}%
  \hspace{0.09\textwidth}%
  \raisebox{-\height}{\strut\parbox{0.50\textwidth}{\@RightText}}\par\medskip
  \raisebox{-\height}{\strut\parbox{0.50\textwidth}{\@LeftChart}}%
  \hspace{0.09\textwidth}%
  \raisebox{-\height}{\strut\parbox{0.50\textwidth}{\@RightChart}}\par\medskip}
\long\def\LeftText#1{\gdef\@LeftText{{\bfseries\large\@chartname\par}#1}}
\long\def\RightText#1{\gdef\@RightText{{\bfseries\large\vphantom{\@chartname}\par}#1}}
\long\def\LeftChart#1{\gdef\@LeftChart{#1}}
\long\def\RightChart#1{\gdef\@RightChart{#1}}
\newcommand\indexgroup[1]{{\bfseries#1\par}}
\renewcommand\@idxitem{\par\hangindent2ex}
\onecolumn
\normalfont
\endinput
%%
%% End of file `faofactbook.cls'.
