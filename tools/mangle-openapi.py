# /usr/bin/python3

import yaml
import sys
import os

input_filename = sys.argv[1]
yaml.Dumper.ignore_aliases = lambda self, data: True

def log(str):
    #print(str)
    return

def dereference(input, ref):
    if not isinstance(ref, dict):
        return
    path = ref.get('$ref', None)
    if not path:
        return
    log(f'ref path {path}')
    path = path[2:].split('/')
    target = input
    for p in path:
        target = target.get(p, None)
        log(f'target {target}')
        if not target:
            return
    log(f'target {target}')
    if target.get('type', '') not in ['string', 'integer', 'float', 'boolean',
                                      'number']:
        return
    ref.pop('$ref')
    for k in target:
        ref[k] = target[k]


def traverse(input, tree):
    if not isinstance(tree, list) and not isinstance(tree, dict):
        return
    for t in tree:
        if isinstance(tree, dict):
            t = tree[t]
        dereference(input, t)
        traverse(input, t)


with open(input_filename, 'r') as input_file:
    log('read file')
    input = yaml.safe_load(input_file)

    traverse(input, input['components']['parameters'])
    traverse(input, input['components']['schemas'])

    input['paths'] = {p: input['paths'][p] for p in input['paths']
                      if 'bulk' not in p}

    # remove unused references:

    new_file = yaml.dump(input, Dumper=yaml.Dumper)
    new_schemas = {}
    for schema in input['components']['schemas']:
        ref = f"'#/components/schemas/{schema}'"
        if ref not in new_file:
            log('clean ' + ref)
            continue
        log(f'keep {ref}')
        new_schemas[schema] = input['components']['schemas'][schema]

    input['components']['schemas'] = new_schemas

    print(yaml.dump(input, Dumper=yaml.Dumper))


