;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("wt" "async with data.task.WriteTask(proto.db.$1) as task:\n    await task.connect_to_id($2_id)\n    $2 = await data.$2s.read_in_task(task, $2_id)\n" "wt" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/wt" nil nil)
                       ("vv" "core.varz.write(\"$1\", 1, tags={\"$2\": $3})" "vv" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/vv" nil nil)
                       ("np" "# NOPUSH" "np" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/np" nil nil)
                       ("lwf" "logging.warning(f\"$1: {$1}\")" "lwf" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/lwf" nil nil)
                       ("lif" "logging.info(f\"$1: {$1}\")\n" "lif" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/lif" nil nil)
                       ("lcf" "logging.NOCOMMIT(f\"$1: {$1}\")" "lcf" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/lcf" nil nil)
                       ("crg" "await core.runtime.gather($1)" "crg" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/crg" nil nil)
                       ("cog" "import core.options\n\noptions, define = core.options.group()" "cog" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/cog" nil nil)
                       ("cods" "define(\"$1\", default=$2, type=$3, dynamic=$4, category=\"$5\", longevity=proto.options.SHORTLIVED, owner=\"bgwines\")" "cods" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/cods" nil nil)
                       ("codl" "define(\"$1\", default=$2, type=$3, dynamic=$4, category=\"$5\", longevity=proto.options.LONGLIVED, description=\"$6\")" "codl" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/codl" nil nil)
                       ("cccd" "async with core.coro.concurrent_dict() as batch:\n    $1\n" "cccd" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/cccd" nil nil)
                       ("ccc" "async with core.coro.concurrent() as batch:\n<<<<<<< HEAD\n    batch.$1 = $2\n    batch.$3 = $4\n=======\n    $1\n>>>>>>> ab0dab4 (yasnippet)\n" "ccc" nil nil nil "/Users/bwines/doom-config/yasnippets/python-mode/ccc" nil nil)))


;;; Do not edit! File generated at Wed Feb 28 16:23:14 2024
