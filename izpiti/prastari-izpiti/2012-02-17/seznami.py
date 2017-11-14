##########################################################################
# To je datoteka, s katero pripravite nalogo.
# Vsebina naloge je spodaj, za definicijo razreda Check.
##########################################################################


import json, os, re, sys, shutil
from urllib.error import HTTPError
from urllib.parse import urlencode
from urllib.request import urlopen
class Check:
    @staticmethod
    def initialize(parts):
        Check.parts = parts
        for part in Check.parts:
            part['errors'] = []
            part['challenge'] = []
        Check.current = None
        Check.part_counter = None

    @staticmethod
    def part():
        if Check.part_counter is None:
            Check.part_counter = 0
        else:
            Check.part_counter += 1
        Check.current = Check.parts[Check.part_counter]
        return Check.current.get('solution', '').strip() != ''

    @staticmethod
    def error(msg, *args, **kwargs):
        Check.current['errors'].append(msg.format(*args, **kwargs))

    @staticmethod
    def challenge(x, k=None):
        pair = (str(k), str(Check.canonize(x)))
        Check.current['challenge'].append(pair)

    @staticmethod
    def run(example, state, message=None, env={}, clean=lambda x: x):
        code = "\n".join(example)
        example = "  >>> " + "\n  >>> ".join(example)
        s = {}
        s.update(env)
        exec (code, globals(), s)
        errors = []
        for (x,v) in state.items():
            if x not in s:
                errors.append('morajo nastaviti spremenljivko {0}, vendar je ne'.format(x))
            elif clean(s[x]) != clean(v):
                errors.append('morajo nastaviti {0} na {1},\nvendar nastavijo {0} na {2}'.format(x, v, s[x]))
        if errors:
            Check.error('Ukazi\n{0}\n{1}.', example,  ";\n".join(errors))

    @staticmethod
    def canonize(x, digits=6):
        if   type(x) is float:
            x = round(x, digits)
            # We want to canonize -0.0 and similar small negative numbers to 0.0
            # Since -0.0 still behaves as False, we can use the following
            return x if x else 0.0
        elif type(x) is complex: return complex(Check.canonize(x.real, digits), Check.canonize(x.imag, digits))
        elif type(x) is list: return list([Check.canonize(y, digits) for y in x])
        elif type(x) is tuple: return tuple([Check.canonize(y, digits) for y in x])
        elif type(x) is dict: return sorted([(Check.canonize(k, digits), Check.canonize(v, digits)) for (k,v) in x.items()])
        elif type(x) is set: return sorted([Check.canonize(y, digits) for y in x])
        else: return x

    @staticmethod
    def equal(example, value=None, exception=None,
                clean=lambda x: x, env={},
                precision=1.0e-6, strict_float=False, strict_list=True):
        def difference(x, y):
            if x == y: return None
            elif (type(x) != type(y) and
                 (strict_float or not (type(y) in [int, float, complex] and type(x) in [int, float, complex])) and
                 (strict_list or not (type(y) in [list, tuple] and type(x) in [list, tuple]))):
                return "različna tipa"
            elif type(y) in [int, float, complex]:
                return ("numerična napaka" if abs(x - y) > precision else None)
            elif type(y) in [tuple,list]:
                if len(y) != len(x): return "napačna dolžina seznama"
                else:
                    for (u, v) in zip(x, y):
                        msg = difference(u, v)
                        if msg: return msg
                    return None
            elif type(y) is dict:
                if len(y) != len(x): return "napačna dolžina slovarja"
                else:
                    for (k, v) in y.items():
                        if k not in x: return "manjkajoči ključ v slovarju"
                        msg = difference(x[k], v)
                        if msg: return msg
                    return None
            else: return "različni vrednosti"

        local = locals()
        local.update(env)

        if exception:
            try:
                eval(example, globals(), local)
            except Exception as e:
                if e.__class__ != exception.__class__ or e.args != exception.args:
                    Check.error("Izraz {0} sproži izjemo {1!r} namesto {2!r}.",
                                example, e, exception)
            else:
                Check.error("Izraz {0} vrne {1!r} namesto da bi sprožil izjemo {2}.",
                            example, returned, exception)

        else:
            returned = eval(example, globals(), local)
            reason = difference(clean(returned), clean(value))
            if reason:
                Check.error("Izraz {0} vrne {1!r} namesto {2!r} ({3}).",
                            example, returned, value, reason)

    @staticmethod
    def summarize():
        for i, part in enumerate(Check.parts):
            if not part['solution'].strip():
                print('Podnaloga {0} je brez rešitve.'.format(i + 1))
            elif part['errors']:
                print('Podnaloga {0} ni prestala vseh testov:'.format(i + 1))
                for e in part['errors']:
                    print("- {0}".format("\n  ".join(e.splitlines())))
            elif 'rejection' in part:
                print('Podnaloga {0} je zavrnjena. ({1})'.format(i + 1, part['rejection']))
            else:
                print('Podnaloga {0} je pravilno rešena.'.format(i + 1))


_filename = os.path.abspath(sys.argv[0])
with open(_filename, encoding='utf-8') as f:
    source = f.read()

Check.initialize([
    {
        'part': int(match.group('part')),
        'description': "\n".join(s[2:] for s in match.group('description').strip().splitlines()),
        'solution': match.group('solution').strip(),
        'validation': match.group('validation').strip(),
    } for match in re.compile(
        r'^#+@(?P<part>\d+)#\n'               # beginning of header
        r'(?P<description>(^#( [^\n]*)?\n)*)' # description
        r'^#+(?P=part)@#\n'                   # end of header
        r'(?P<solution>.*?)'                  # solution
        r'^Check\.part\(\)\n'                 # beginning of validation
        r'(?P<validation>.*?)'                # validation
        r'^(# )?(?=#+@)',                     # beginning of next part
        flags=re.DOTALL|re.MULTILINE
    ).finditer(source)
])

problem_match = re.search(
    r'^#+@@#\n'                           # beginning of header
    r'^# (?P<title>[^\n]*)\n'             # title
    r'^(#\s*\n)*'                         # empty rows
    r'(?P<description>(^#( [^\n]*)?\n)*)' # description
    r'^#+@@#\n'                           # end of header
    r'(?P<preamble>.*?)'                  # preamble
    r'^(# )?(?=#+@)',                     # beginning of first part
    source, flags=re.DOTALL|re.MULTILINE)

if not problem_match:
    print("NAPAKA: datoteka ni pravilno oblikovana")
    sys.exit(1)

title = problem_match.group('title').strip()
description = "\n".join(s[2:] for s in problem_match.group('description').strip().splitlines())
preamble = problem_match.group('preamble').strip()

##########################################################################
# Od tu naprej je navodilo naloge

#######################################################################@@#
# Seznami 
#######################################################################@@#




##################################################################@000521#
# Sestavite funkcijo `naloga1a(a, k)`, kot je zapisano v navodilih.
##################################################################000521@#
def naloga1a(a,k):
    return [sum(a[i:i+k])/k for i in range(0, len(a)-k+1)]

Check.part()
Check.equal("naloga1a([1, 2, 3, 4, 5, 6], 2)", [1.5, 2.5, 3.5, 4.5, 5.5])
Check.equal("naloga1a([1, 2, 3, 4, 5, 6], 1)", [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
Check.equal("naloga1a([1, 2, 3, 4, 5, 6], 6)", [3.5])


##################################################################@000524#
# Sestavite funkcijo `naloga1b(a, b)`, kot je zapisano v navodilih.
##################################################################000524@#
def naloga1b(sez, povprecja):
    k = len(sez) + 1
    skoraj_povprecje = sum(sez)
    for i, povprecje in enumerate(povprecja):
        naslednji = k * povprecje - skoraj_povprecje
        skoraj_povprecje += naslednji - sez[i]
        sez.append(naslednji)
    return sez

Check.part()
Check.equal("naloga1b([1.0], [1.5, 2.5, 3.5, 4.5, 5.5])", [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
Check.equal("naloga1b([1.0, 2.0, 3.0, 4.0, 5.0], [3.5])", [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])



# ##################################################################@000000#
# # To je predloga za novo podnalogo. Tu vpisite besedilo podnaloge.
# ##################################################################000000@#
#
# sem napisite resitev
#
# Check.part()
#
# Check.equal("""testni_primer""", resitev)
#
# Check.challenge(testni_primer_1)
# Check.challenge(testni_primer_2)
# Check.challenge(testni_primer_3)
# ...

#######################################################################@@#
# Od tu naprej ničesar ne spreminjajte.

Check.summarize()
if any(part.get('errors') for part in Check.parts):
    print('Naloge so napačno sestavljene.')
else:
    print('Naloge so pravilno sestavljene.')
    if input('Ali jih shranim na strežnik? [da/NE]') == 'da':
        print('Shranjujem naloge...')
        post = json.dumps({
            'data': '{"timestamp": "2012-02-17 10:04:32.252060", "problem": 170, "user": 1}',
            'signature': 'c68289faa8e6a5bb8ede482e9af9b20b',
            'title': title,
            'description': description,
            'preamble': preamble,
            'parts': Check.parts,
        }).encode('utf-8')
        try:
            r = urlopen('http://tomo.fmf.uni-lj.si:80/problem/upload/teacher/', post)
            response = json.loads(r.read().decode('utf-8'))
            print(response['message'])
            if 'update' in response:
                r = urlopen(response['update'])
                shutil.copy(_filename, _filename + ".orig")
                with open(_filename, 'w', encoding='utf-8') as f:
                    f.write(r.read().decode('utf-8'))
        except HTTPError:
            print('Pri shranjevanju je prišlo do napake. Poskusite znova.')
    else:
        print('Naloge niso bile shranjene.')
