Simple API doc generator - dumps docstrings, names, lambda lists
of all external symbols in specified packages to an HTML file.
In the order the symbols are specified in the defpackage forms.

It is similar to other API doc generators, like CL-API,
but I want the symbols to be listed in the order they 
are listed in the `defpackage` forms,
assuming the authros order them logically and conveniently
for readrs.

Uses [docparser](https://github.com/eudoxia0/docparser).

Maturity is low - I only made it to generate an OK doc
for cl+ssl. If you try it for other library, you may need
to improve something.
