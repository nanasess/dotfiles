;; -*- coding: utf-8 -*-
(when (require 'muse-mode nil t)     ; load authoring mode

  (require 'muse-html)     ; load publishing styles I use
  (require 'muse-latex)
  (require 'muse-texinfo)
  (require 'muse-docbook)

  (require 'muse-project)
  (require 'muse-wiki)

  ;(setq muse-file-extension nil muse-mode-auto-p t)

  (setq muse-xhtml-encoding-default 'utf-8-unix)
  (setq muse-latexjck-encoding-default
	(cdr (assoc 'japanese-iso-8bit muse-latexcjk-encoding-map)))
  
  (setq my-project-path "~/workspace/svn.ebis.ne.jp/branches/")
  
  ;; Project settings
  (add-to-list 'muse-project-alist
	       '("top"
		 ("~/workspace/svn.ebis.ne.jp/docs" :default "index.muse")
		 (:base "xhtml" :path "~/workspace/svn.ebis.ne.jp/docs")))
  (add-to-list 'muse-project-alist
	       '("hotei"
		 ("~/workspace/svn.ebis.ne.jp/docs/hotei" :default "index.muse")
		 (:base "xhtml" :path "~/workspace/svn.ebis.ne.jp/docs/hotei")))
  (add-to-list 'muse-project-alist
	       '("framework"
		 ("~/workspace/svn.ebis.ne.jp/docs/framework" :default "index.muse")
		 (:base "xhtml" :path "~/workspace/svn.ebis.ne.jp/docs/framework")))
  (add-to-list 'muse-project-alist
	       '("autobid"
		 ("~/workspace/svn.ebis.ne.jp/docs/autobid" :default "index.muse")
		 (:base "xhtml" :path "~/workspace/svn.ebis.ne.jp/docs/autobid")))


  (setq muse-xhtml-style-sheet
	(concat "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"common.css\">\n"
		"<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"screen\" href=\"screen.css\">\n"
		"<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"print\" href=\"print.css\">\n"))
  
  (setq muse-latex-header
	(concat
	 "%% -*- mode: japanese-Latex; coding: euc-jp-unix -*-\n"
	 "\\documentclass[11pt,a4paper,oneside]{jarticle}\n"
	 "\\usepackage{mathpazo}\n"
	 "\\usepackage[scaled]{helvet}\n"
	 "%%\\usepackage[expert,deluxe,multi]{otf}\n"
	 "\\usepackage[dvipdfmx]{graphicx,color}\n"
	 "\\usepackage{tabularx}\n"
	 "%%\\renewcommand{\\figurename}{Fig.}\n\n"
	 "\\usepackage{makeidx}\n"
	 "\\usepackage{float}\n\n"
	 "%% hyperref を使う\n"
	 "\\usepackage[dvipdfm,\n"
	 "  colorlinks=true,\n"
	 "  bookmarks=true,\n"
	 "  bookmarksnumbered=true,\n"
	 "  bookmarkstype=toc]{hyperref}\n\n"
	 "% 日本語しおりが字化けしないために\n"
	 "\\ifnum \n"
	 "  42146=\\euc\"A4A2 \\AtBeginDvi{\\special{pdf:tounicode EUC-UCS2}}\n"
	 "\\else\n"
	 "  \\AtBeginDvi{\\special{pdf:tounicode 90ms-RKSJ-UCS2}}\n"
	 "\\fi\n\n"
	 "% 余白の調整\n"
	 "\\setlength{\\paperwidth}{210mm}      % 紙の幅\n"
	 "\\setlength{\\paperheight}{297mm}     % 紙の高さ\n"
	 "\\setlength{\\textwidth}{170mm}       % テキストの幅\n"
	 "\\setlength{\\oddsidemargin}{20mm}    % 偶数ページの左マージン\n"
	 "\\setlength{\\evensidemargin}{20mm}   % 奇数ページの左マージン\n"
	 "\\addtolength{\\oddsidemargin}{-1in}  % 元の 1in の空白を削除\n"
	 "\\addtolength{\\evensidemargin}{-1in} % 元の 1in の空白を削除\n"
       "\\setlength{\\textheight}{232mm}      % テキストの高さ\n"
       "\\setlength{\\headheight}{0mm}        % ヘッダの高さ\n"
       "\\setlength{\\headsep}{10mm}           % テキストの最上部とヘッダの最下部との間隔\n"
       "\\setlength{\\footskip}{10mm}         % テキストの最下部とフッタの最下部との間隔\n"
       "\\setlength{\\topmargin}{25mm}        % 上のマージン\n"
       "\\addtolength{\\topmargin}{-1in}      % 元の 1in の空白を削除\n"
       "\\kanjiskip=.20pt plus.05pt minus.07pt  % 全角文字の隙間の調整\n"
       "\\newcommand{\\comment}[1]{}\n"
       "\\makeindex"
       "\\begin{document}\n"
       "%% 表紙作製\n"
       "\\title{<lisp>(muse-publishing-directive \"title\")</lisp>}\n"
       "\\author{<lisp>(muse-publishing-directive \"author\")</lisp>}\n"
       "\\date{<lisp>(muse-publishing-directive \"date\")</lisp>}\n\n"
       "\\maketitle\n"
       "% \\pagebreak\n\n"
       "\\def\\museincludegraphics{%\n"
       "  \\begingroup\n"
       "  \\catcode`\\|=0\n"
       "  \\catcode`\\\=12\n"
       "  \\catcode`\\#=12\n"
       "  \\includegraphics[width=0.5\\textwidth]\n"
       "}\n"
       "%% 目次ページ作製\n"
       "% \\Large\n"
       "% \\hypertarget{mokuji}{}\n"
       "% \\begin{center}\\LARGE\\bf\n"
       "% 目次のタイトル\n"
       "% \\end{center}\n"
       "% \\tableofcontents\n"
       "% \\pagebreak\n\n"
       "<lisp>(and muse-publish-generate-contents\n           \"\\\\tableofcontents\n\\\\newpage\")</lisp>\n")))

(provide 'muse-settings)
