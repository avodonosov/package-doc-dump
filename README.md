Simple API doc generator - dumps to an HTML file
the names, docstrings, lambda lists of public
symbols in the specified packages.
In the order the symbols are listed in the defpackage forms.

It is similar to other API doc generators, like CL-API,
but I want the symbols to be listed in the order they 
are listed in the `defpackage` forms, assuming the authros
order them logically and conveniently for readrs.

Uses [docparser](https://github.com/eudoxia0/docparser).

Maturity is low - I only made it to generate an OK ducument
for cl+ssl. If you try it for another library, you may need
to improve something.


Example:


```common-lisp


(pushnew "/home/anton/prj/package-doc-dump/" asdf:*central-registry* :test #'equal)
(ql:quickload "package-doc-dump")

(pushnew "/home/anton/prj/cl+ssl/cl-plus-ssl/" asdf:*central-registry* :test #'equal)
(ql:quickload "cl+ssl")

(package-doc-dump:dump-html "cl+ssl"
                            (asdf:system-relative-pathname "cl+ssl"
                                                           "src/package.lisp")
                            ;; Remove the implementation of cl+ssl:stream-fd,
                            ;; like on CCL `stream-fd ((stream ccl::basic-stream))`,
                            ;; so only the generic function reamins.
                            :doc-node-filter (lambda (doc-node)
                                               (not (and (typep doc-node 'docparser:method-node)
                                                         (string-equal (docparser:node-name doc-node)
                                                                       "stream-fd")))))

=> #P"/home/anton/prj/cl+ssl/cl-plus-ssl/src/package.html"

```

Usage from GitHub Actions:
https://github.com/cl-plus-ssl/cl-plus-ssl/blob/master/.github/workflows/api-doc.yml
It generates and publishes the following doc: https://cl-plus-ssl.github.io/cl-plus-ssl/cl-plus-ssl-api.html
