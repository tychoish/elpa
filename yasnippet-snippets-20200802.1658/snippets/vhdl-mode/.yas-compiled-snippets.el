;;; Compiled snippets and support files for `vhdl-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'vhdl-mode
		     '(("when" "when ${1:Value} =>\n    $0\n" "when" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/when" nil nil)
		       ("type" "type ${1:Name} is (${2:Value list});\n" "type" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/type" nil nil)
		       ("to" "${1:name}(${2:start} to ${3:end})$0" "to" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/to" nil nil)
		       ("signal" "signal ${1:Names}: ${2:Type};" "signal" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/signal" nil nil)
		       ("process" "${1:Name}: process(${2:Sensitivity List})\nbegin\n  $0\nend process $1;\n" "process" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/process" nil nil)
		       ("port" "port(${1:name}: ${2:IO} ${3:type});" "port" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/port" nil nil)
		       ("lib" "library IEEE;\nuse IEEE.std_logic_1164.all;\n" "library" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/lib" nil nil)
		       ("ifel" "if ${1:cond1} then\n   $0\nelse\n   \nend if;" "ifelse" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/ifelse" nil nil)
		       ("ifelif" "if ${1:cond1} then\n   $0\nelsif ${2:cond2} then\n      \nend if;" "ifelif" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/ifelif" nil nil)
		       ("if" "if ${1:cond} then\n   $0\nend if;" "if" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/if" nil nil)
		       ("ent" "entity ${1:Name} is\n    $0\nend $1;" "entity" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/entity" nil nil)
		       ("dto" "${1:name}(${2:start} downto ${3:end})$0" "downto" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/downto" nil nil)
		       ("const" "constant ${1:Name}: ${2:Type} := ${3:Value};" "constant" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/constant" nil nil)
		       ("comp" "component ${1:Name}\n  $0\nend component;\n" "component" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/component" nil nil)
		       ("case" "case ${1:cond} is\n    when ${2:Value} =>\n        $0\n\nend case;" "case" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/case" nil nil)
		       ("asg" "${1:variable} <= ${2:value};\n" "assignation" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/assignation" nil nil)
		       ("arch" "architecture ${1:Type} of ${2:Name} is\nbegin\n  $0\nend $1;\n" "architecture" nil nil nil "/home/tychoish/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets/vhdl-mode/architecture" nil nil)))


;;; Do not edit! File generated at Tue Aug 18 10:07:28 2020
