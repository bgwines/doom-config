;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("wt" "async with data.task.WriteTask(proto.db.$1) as task:\n    await task.connect_to_id($2_id)\n    $2 = await data.$2s.read_in_task($2_id)\n" "wt" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/wt" nil nil)
                       ("vv" "core.varz.write(\"$1\", 1, tags={\"$2\": $3})" "vv" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/vv" nil nil)
                       ("np" "# NOPUSH" "np" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/np" nil nil)
                       ("lwf" "logging.warn(f\"$1: {$1}\")" "lwf" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/lwf" nil nil)
                       ("lif" "logging.info(f\"$1: {$1}\")\n" "lif" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/lif" nil nil)
                       ("lcf" "logging.NOCOMMIT(f\"$1: {$1}\")" "lcf" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/lcf" nil nil)
                       ("cog" "import core.options\n\noptions, define = core.options.group()" "cog" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/cog" nil nil)))


;;; Do not edit! File generated at Sun Jan 17 23:43:02 2021
