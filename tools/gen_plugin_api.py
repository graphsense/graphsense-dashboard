import os,re,collections,sys
def walk(base):
    for dp,_,fs in os.walk(base,followlinks=True):
        for f in fs:
            if f.endswith('.elm'): yield os.path.join(dp,f)
def body_of(text,pat):
    m=re.search(pat,text,re.M)
    if not m: return None
    i=m.end()-1; d=0
    for j in range(i,len(text)):
        if text[j]=='(': d+=1
        elif text[j]==')':
            d-=1
            if d==0: return text[i+1:j]
    return None
def split_top(b): return [p.strip() for p in re.split(r',(?![^()]*\))',b) if p.strip()]

core={}
for p in walk('src'):
    core[os.path.relpath(p,'src')[:-4].replace('/','.')]=open(p,encoding='utf8',errors='ignore').read()

types={}; ctors={}; values={}
for m,t in core.items():
    ty={}; ct={}; va=set()
    for x in re.finditer(r'^type alias\s+(\w+)((?:\s+[a-z]\w*)*)',t,re.M):
        ty[x.group(1)]=len(x.group(2).split())
    for x in re.finditer(r'^type\s+(\w+)((?:\s+[a-z]\w*)*)',t,re.M):
        if x.group(1) not in ty: ty[x.group(1)]=len(x.group(2).split())
    for x in re.finditer(r'^type\s+(\w+)',t,re.M):
        tn=x.group(1)
        # the block runs until the next top-level declaration (column-0, non-comment)
        nxt=re.search(r'^\S',t[x.end():],re.M)
        seg=t[x.end(): x.end()+(nxt.start() if nxt else len(t))]
        for c in re.finditer(r'^\s*[=|]\s*([A-Z]\w*)',seg,re.M): ct[c.group(1)]=tn
    for x in re.finditer(r'^(\w+)\s*:',t,re.M): va.add(x.group(1))
    types[m]=ty; ctors[m]=ct; values[m]=va

def exports(mod):
    b=body_of(core[mod],r'^module\s+[\w.]+\s+exposing\s*\(')
    if b is None: return set()
    if b.strip()=='..': return set(types[mod])|set(ctors[mod])|values[mod]
    out=set()
    for p in split_top(b):
        if p.endswith('(..)'):
            tn=p[:-4]; out.add(tn)
            out|={c for c,t2 in ctors[mod].items() if t2==tn}
        else: out.add(p)
    return out


def ctor_exposed(mod):
    b=body_of(core[mod],r'^module\s+[\w.]+\s+exposing\s*\(')
    if b is None: return set()
    if b.strip()=='..': return set(ctors[mod])
    out=set()
    for p in split_top(b):
        if p.endswith('(..)'):
            tn=p[:-4]
            out|={c for c,t2 in ctors[mod].items() if t2==tn}
    return out

used=collections.defaultdict(set)
for p in walk('plugins'):
    t=open(p,encoding='utf8',errors='ignore').read()
    for im in re.finditer(r'^import\s+([A-Z][\w.]*)(?:\s+as\s+([A-Z][\w.]*))?',t,re.M):
        mod,alias=im.group(1),im.group(2)
        if mod not in core: continue
        av=exports(mod); rest=t[im.end():]
        me=re.match(r'\s+exposing\s*\(',rest)
        if me:
            i=me.end()-1; d=0
            for j in range(i,len(rest)):
                if rest[j]=='(': d+=1
                elif rest[j]==')':
                    d-=1
                    if d==0:
                        for q in split_top(rest[i+1:j]):
                            if q=='..': used[mod]|=av
                            elif q.endswith('(..)'):
                                tn=q[:-4]; used[mod].add(tn)
                                used[mod]|={c for c,t2 in ctors[mod].items() if t2==tn}
                            else: used[mod].add(q)
                        break
        for nm in [x for x in (mod,alias) if x]:
            for q in set(re.findall(r'(?<![\w.])'+re.escape(nm)+r'\.(\w+)',t)):
                if f"{nm}.{q}" in core or f"{mod}.{q}" in core: continue
                if q in av: used[mod].add(q)

mods=sorted(m for m in used if used[m] and not m.startswith('PluginInterface'))
type_items=[]; ref_items=[]
for mod in mods:
    av=exports(mod)
    for s in sorted(used[mod]):
        if s not in av: continue
        if s in types[mod]:
            type_items.append((mod,s,types[mod][s]))
            # `type Row = Row ...` exposes one name that is both a type and a
            # constructor; the alias above pins only the type.
            if s in ctors[mod] and s in ctor_exposed(mod):
                ref_items.append((mod,s))
            continue
        if s in ctors[mod] or s in values[mod]:
            ref_items.append((mod,s))


CANONICAL = 'src/PluginApi.elm'
PRUNE = '--prune' in sys.argv

def parse_existing(path):
    """Recover (refs, types) from a previously generated module."""
    try: t = open(path, encoding='utf8').read()
    except OSError: return set(), set()
    refs = set()
    for m in re.finditer(r'^\s*[\[,]\s*ref\s+([\w.]+)\.(\w+)\s*$', t, re.M):
        refs.add((m.group(1), m.group(2)))
    tys = set()
    for m in re.finditer(r'^type alias\s+\w+((?:\s+[a-z]\w*)*)\s*=\s*\n\s+([\w.]+)\.(\w+)', t, re.M):
        tys.add((m.group(2), m.group(3), len(m.group(1).split())))
    return refs, tys

if not PRUNE:
    # A plugin missing from the working tree must never shrink the list: devs
    # routinely build without one, and its symbols are still used in production.
    # Regeneration therefore only ever adds. `--prune` drops what is no longer
    # referenced, and needs every plugin present -- see `make plugin-api-prune`.
    old_refs, old_types = parse_existing(CANONICAL)
    ref_items = sorted(set(ref_items) | old_refs)
    type_items = sorted(set(type_items) | old_types)
else:
    ref_items = sorted(set(ref_items))
    type_items = sorted(set(type_items))


# Core modules that cannot be referenced from here because they themselves depend
# on plugin-provided code: src/Util/Nullable.elm imports OpenApi.Common, which
# lives in a plugin's generated api directory. Importing it here would drag it
# into the test build and make `make test` fail in every checkout without that
# plugin -- including CI. Those files are exempt from the dead-code rules by name
# instead (see review/src/ReviewConfig.elm). The real fix is to move such a module
# into the plugin that owns it; nothing in core uses this one.
EXCLUDED_MODULES = {'Util.Nullable'}
ref_items = [x for x in ref_items if x[0] not in EXCLUDED_MODULES]
type_items = [x for x in type_items if x[0] not in EXCLUDED_MODULES]

def alias_name(mod,s): return mod.replace('.','')+s
o=[]
# The type aliases are exposed rather than kept private so that NoUnused.Variables
# does not report them; NoUnused.Exports is already exempt for this file.
exposed=', '.join(['surface']+[alias_name(m,s) for m,s,_ in type_items])
o.append(f'module PluginApi exposing ({exposed})')
o.append('')
o.append('''{-| The core API that the dashboard plugins depend on.

This module creates no behaviour. It exists so that dead-code analysis is
correct **without a plugin checkout**.

`elm.json` is generated and gains one `source-directories` entry per plugin
registered in `config/Config.elm`, so elm-review only sees the plugins that
happen to be present. CI builds from `config/Config.elm.tmp`, which registers
none, and in that checkout every core export that only a plugin uses looks
dead. Referencing those exports here keeps them alive for the analyser
whichever plugins are checked out.

So this doubles as the plugin contract: if a symbol appears below, a plugin
repository may depend on it. `tests/PluginApiTest.elm` imports this module, so
`make test` fails if one of them is deleted or renamed. Signature changes are
*not* caught -- `ref` accepts any type.

`PluginInterface.*` is deliberately absent -- it is the designated plugin
interface already, and the dead-code config exempts it by name.

`make plugin-api` only ever *adds* to this list. A plugin missing from the
working tree -- routine, since it is faster to develop without the ones you are
not touching -- therefore cannot shrink it. Removing entries is `make
plugin-api-prune`, which requires every registered plugin to be present.

-}''')
o.append('')
# Import exactly the modules actually referenced below, not every module a
# plugin touches -- an unused import would still pull an excluded module in.
for m in sorted({x[0] for x in ref_items} | {x[0] for x in type_items}):
    o.append('import '+m)
o.append('')
o.append('')
o.append('-- TYPES USED BY PLUGINS')
o.append('--')
o.append('-- Re-declared rather than referenced: a type cannot be passed as a value.')
o.append('-- Names are module-qualified because `Model`, `Msg` and `Config` collide.')
for mod,s,n in type_items:
    ps=' '.join(chr(97+i) for i in range(n))
    o.append('')
    o.append('')
    o.append(f'type alias {alias_name(mod,s)}{(" "+ps) if n else ""} =')
    o.append(f'    {mod}.{s}{(" "+ps) if n else ""}')
o.append('')
o.append('')
o.append('{-| Mentions a value without caring what its type is. -}')
o.append('ref : a -> ()')
o.append('ref _ =')
o.append('    ()')
o.append('')
o.append('')
o.append('{-| Every core value, function and type constructor a plugin references. -}')
o.append('surface : List ()')
o.append('surface =')
cur=None; first=True
for mod,s in ref_items:
    if mod!=cur:
        o.append(f'    -- {mod}' if first else f'\n    -- {mod}')
        cur=mod
    o.append(('    [ ' if first else '    , ')+f'ref {mod}.{s}')
    first=False
o.append('    ]')
out = next((a for a in sys.argv[1:] if not a.startswith('-')), 'src/PluginApi.elm')
open(out,'w').write('\n'.join(o)+'\n')
print(f"{len(ref_items)} refs, {len(type_items)} types, {len(mods)} modules")
