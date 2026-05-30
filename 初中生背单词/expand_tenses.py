#!/usr/bin/env python3
"""
扩展时态题库：从 296 题扩展到 400-600 题
覆盖：一般现在时、现在进行时、一般过去时、过去进行时、
      现在完成时、过去完成时、一般将来时、将来进行时
"""
import json, random, os

random.seed(42)

OUT = os.path.join(os.path.dirname(__file__), "data")
os.makedirs(OUT, exist_ok=True)

existing_file = os.path.join(OUT, "tenses.json")
existing = []
max_id = 0
if os.path.exists(existing_file):
    with open(existing_file, 'r', encoding='utf-8') as f:
        existing = json.load(f)
    max_id = max(q['id'] for q in existing) if existing else 0

_id_counter = [max_id]

def nid():
    _id_counter[0] += 1
    return _id_counter[0]

def mc(q, hint, options, answer_idx, explain, examples, mnemonic, cat, diff='easy'):
    return {
        "id": nid(), "cat": cat, "diff": diff, "type": "mc",
        "q": q, "hint": hint, "options": options, "answer": answer_idx,
        "explain": explain, "examples": examples, "mnemonic": mnemonic
    }

def fl(q, hint, answer, explain, examples, mnemonic, cat, diff='easy'):
    return {
        "id": nid(), "cat": cat, "diff": diff, "type": "fill",
        "q": q, "hint": hint, "answer": answer,
        "explain": explain, "examples": examples, "mnemonic": mnemonic
    }

def gen_all_tenses():
    Q = []

    # ================================================================
    # 1. 一般现在时 (Simple Present) — 第三人称单数 + 频率副词
    # ================================================================
    verbs_3ps = [
        ("go", "goes"), ("do", "does"), ("have", "has"), ("watch", "watches"),
        ("study", "studies"), ("play", "plays"), ("read", "reads"), ("write", "writes"),
        ("speak", "speaks"), ("teach", "teaches"), ("fly", "flies"), ("cry", "cries"),
        ("try", "tries"), ("carry", "carries"), ("wash", "washes"), ("fix", "fixes"),
        ("miss", "misses"), ("kiss", "kisses"), ("push", "pushes"), ("catch", "catches"),
        ("enjoy", "enjoys"), ("stay", "stays"), ("buy", "buys"), ("say", "says"),
        ("eat", "eats"), ("drink", "drinks"), ("sleep", "sleeps"), ("walk", "walks"),
        ("run", "runs"), ("swim", "swims"), ("sing", "sings"), ("dance", "dances"),
    ]

    subjects = ["He", "She", "My father", "My mother", "The teacher", "The student",
                "My brother", "My sister", "Tom", "Mary"]

    for v_base, v_3ps in verbs_3ps:
        subj = random.choice(subjects)
        # MC
        wrongs = [v_base, v_base+"ing", v_3ps+"ed" if not v_3ps.endswith('es') else v_base+"ed"]
        opts = [v_3ps] + [w for w in wrongs if w != v_3ps][:3]
        random.shuffle(opts)
        Q.append(mc(
            f"{subj} ___ English every day.", '一般现在时第三人称单数',
            opts, opts.index(v_3ps),
            f'{subj} 是第三人称单数，{v_base} 需变成 {v_3ps}。',
            [f'{subj} <strong>{v_3ps}</strong> English every day.'],
            f'{v_base} → {v_3ps}', 'tense_present', 'easy'))

        # Fill
        Q.append(fl(
            f"{subj} ___ to school by bus. ({v_base})", '一般现在时三单',
            v_3ps,
            f'{subj} 是第三人称单数，{v_base} → {v_3ps}。',
            [f'{subj} <strong>{v_3ps}</strong> to school by bus.'],
            f'{v_base} → {v_3ps}', 'tense_present', 'easy'))

    # 频率副词
    adverbs = ["always", "usually", "often", "sometimes", "rarely", "never", "seldom"]
    for adv in adverbs:
        for _ in range(3):
            v = random.choice(verbs_3ps)
            Q.append(mc(
                f"She ___ {adv} drinks coffee in the morning.",
                f'频率副词 {adv} + 一般现在时',
                ["drink", "drinks", "drinking", "drank"],
                1,
                f'{adv} 表示习惯性动作，用一般现在时。she 三单加 -s。',
                [f'She <strong>drinks</strong> coffee in the morning.'],
                f'频率副词 + 一般现在时', 'tense_present', 'easy'))

    # ================================================================
    # 2. 现在进行时 (Present Continuous) — be + V-ing
    # ================================================================
    ing_verbs = [
        ("play", "playing"), ("read", "reading"), ("write", "writing"), ("swim", "swimming"),
        ("run", "running"), ("sit", "sitting"), ("cut", "cutting"), ("put", "putting"),
        ("make", "making"), ("take", "taking"), ("dance", "dancing"), ("come", "coming"),
        ("lie", "lying"), ("die", "dying"), ("tie", "tying"), ("study", "studying"),
        ("cook", "cooking"), ("sing", "singing"), ("draw", "drawing"), ("eat", "eating"),
        ("drink", "drinking"), ("sleep", "sleeping"), ("talk", "talking"), ("walk", "walking"),
        ("fly", "flying"), ("cry", "crying"), ("try", "trying"), ("shop", "shopping"),
    ]

    now_signals = ["now", "right now", "at the moment", "Look!", "Listen!", "currently"]

    for v_base, v_ing in ing_verbs:
        sig = random.choice(now_signals)
        subj = random.choice(["I", "You", "He", "She", "They", "We", "The children", "My friend"])
        be_form = "am" if subj == "I" else ("is" if subj in ("He","She","My friend") else "are")

        # MC
        wrong_forms = [v_base, v_base+"s", v_base+"ed"]
        opts = [v_ing] + [w for w in wrong_forms if w != v_ing][:3]
        random.shuffle(opts)
        Q.append(mc(
            f"{subj} ___ {v_ing} {sig}.", '现在进行时 be + V-ing',
            opts, opts.index(v_ing),
            f'{sig} 是现在进行时信号词。{subj} {be_form} + {v_ing}。',
            [f'{subj} <strong>{be_form} {v_ing}</strong> {sig}.'],
            f'{sig} → be + V-ing', 'tense_present', 'easy'))

        # Fill
        Q.append(fl(
            f"{subj} is ___ ({v_base}) right now.", '现在进行时',
            v_ing,
            f'right now 是现在进行时信号词。{v_base} → {v_ing}。',
            [f'{subj} is <strong>{v_ing}</strong> right now.'],
            f'{v_base} → {v_ing}', 'tense_present', 'easy'))

    # ================================================================
    # 3. 一般过去时 (Simple Past) — 规则 + 不规则
    # ================================================================
    irr_past = [
        ("go", "went"), ("come", "came"), ("see", "saw"), ("take", "took"),
        ("give", "gave"), ("make", "made"), ("do", "did"), ("have", "had"),
        ("get", "got"), ("say", "said"), ("tell", "told"), ("find", "found"),
        ("know", "knew"), ("think", "thought"), ("feel", "felt"),
        ("leave", "left"), ("keep", "kept"), ("sleep", "slept"),
        ("meet", "met"), ("read", "read"), ("write", "wrote"),
        ("speak", "spoke"), ("break", "broke"), ("choose", "chose"),
        ("drive", "drove"), ("eat", "ate"), ("drink", "drank"),
        ("sing", "sang"), ("swim", "swam"), ("run", "ran"),
        ("begin", "began"), ("fly", "flew"), ("grow", "grew"),
        ("throw", "threw"), ("draw", "drew"), ("fall", "fell"),
        ("buy", "bought"), ("bring", "brought"), ("catch", "caught"), ("teach", "taught"),
        ("sell", "sold"), ("stand", "stood"), ("build", "built"), ("send", "sent"),
        ("spend", "spent"), ("lose", "lost"), ("lead", "led"), ("hold", "held"),
        ("win", "won"), ("pay", "paid"), ("hear", "heard"),
        ("put", "put"), ("cut", "cut"), ("let", "let"), ("hit", "hit"),
        ("hurt", "hurt"), ("cost", "cost"), ("shut", "shut"),
        ("become", "became"), ("forget", "forgot"), ("hide", "hid"),
        ("ride", "rode"), ("steal", "stole"), ("wake", "woke"),
    ]

    past_signals = ["yesterday", "last night", "last week", "last month", "last year",
                    "two days ago", "in 2010", "when I was young", "just now"]

    for v_base, v_past in irr_past:
        sig = random.choice(past_signals)
        subj = random.choice(["I", "He", "She", "They", "We", "My friend"])

        other_pasts = random.sample([p for b,p in irr_past if p != v_past], 3)
        opts = [v_past] + other_pasts
        random.shuffle(opts)
        Q.append(mc(
            f"{subj} ___ to the park {sig}.", f'{sig} → 一般过去时',
            opts, opts.index(v_past),
            f'{sig} 是过去时间状语，用一般过去时。{v_base} 的过去式是 {v_past}。',
            [f'{subj} <strong>{v_past}</strong> to the park {sig}.'],
            f'{v_base} → {v_past}', 'tense_past', 'medium'))

    # 规则动词过去式
    reg_past = [
        ("play", "played"), ("stay", "stayed"), ("enjoy", "enjoyed"), ("watch", "watched"),
        ("wash", "washed"), ("finish", "finished"), ("work", "worked"), ("talk", "talked"),
        ("walk", "walked"), ("look", "looked"), ("help", "helped"), ("ask", "asked"),
        ("love", "loved"), ("like", "liked"), ("hope", "hoped"), ("live", "lived"),
        ("study", "studied"), ("try", "tried"), ("cry", "cried"), ("carry", "carried"),
        ("stop", "stopped"), ("plan", "planned"), ("drop", "dropped"), ("prefer", "preferred"),
    ]

    for v_base, v_past in reg_past:
        sig = random.choice(past_signals)
        subj = random.choice(["I", "He", "She", "They", "We"])
        Q.append(fl(
            f"{subj} ___ ({v_base}) TV {sig}.", '规则动词过去式',
            v_past,
            f'{sig} 是过去时间状语。{v_base} 的过去式是 {v_past}（规则变化）。',
            [f'{subj} <strong>{v_past}</strong> TV {sig}.'],
            f'{v_base} + ed → {v_past}', 'tense_past', 'easy'))

    # ================================================================
    # 4. 过去进行时 (Past Continuous) — was/were + V-ing
    # ================================================================
    past_cont_signals = ["at 8 p.m. yesterday", "at that time", "when I called",
                         "while", "all day yesterday", "at this time last week"]

    for v_base, v_ing in ing_verbs[:20]:
        sig = random.choice(past_cont_signals)
        subj = random.choice(["I", "He", "She", "They", "We"])
        was_were = "was" if subj in ("I","He","She") else "were"

        Q.append(mc(
            f"{subj} ___ {v_ing} {sig}.", '过去进行时 was/were + V-ing',
            [v_ing, v_base, v_base+"ed", v_base+"s"],
            0,
            f'{sig} 是过去进行时信号。{subj} {was_were} + {v_ing}。',
            [f'{subj} <strong>{was_were} {v_ing}</strong> {sig}.'],
            f'was/were + V-ing', 'tense_past', 'medium'))

    # ================================================================
    # 5. 现在完成时 (Present Perfect) — have/has + 过去分词
    # ================================================================
    pp_verbs = [
        ("go", "gone"), ("see", "seen"), ("take", "taken"), ("give", "given"),
        ("make", "made"), ("do", "done"), ("have", "had"), ("get", "got"),
        ("write", "written"), ("speak", "spoken"), ("break", "broken"), ("choose", "chosen"),
        ("drive", "driven"), ("eat", "eaten"), ("drink", "drunk"),
        ("sing", "sung"), ("swim", "swum"), ("run", "run"),
        ("begin", "begun"), ("fly", "flown"), ("grow", "grown"),
        ("throw", "thrown"), ("draw", "drawn"), ("fall", "fallen"),
        ("buy", "bought"), ("bring", "brought"), ("catch", "caught"), ("teach", "taught"),
        ("sell", "sold"), ("build", "built"), ("send", "sent"), ("spend", "spent"),
        ("lose", "lost"), ("win", "won"), ("pay", "paid"), ("hear", "heard"),
        ("become", "become"), ("forget", "forgotten"), ("hide", "hidden"),
        ("ride", "ridden"), ("steal", "stolen"), ("wake", "woken"),
    ]

    pp_signals = ["already", "just", "yet", "ever", "never", "so far",
                  "since 2020", "for three years", "recently", "lately"]

    for v_base, v_pp in pp_verbs:
        sig = random.choice(pp_signals)
        subj = random.choice(["I", "You", "He", "She", "They", "We"])
        have_has = "has" if subj in ("He","She") else "have"

        other_pps = random.sample([p for b,p in pp_verbs if p != v_pp], 3)
        opts = [v_pp] + other_pps
        random.shuffle(opts)
        Q.append(mc(
            f"{subj} have {sig} ___ the movie.", f'现在完成时 {sig}',
            opts, opts.index(v_pp),
            f'{sig} 是现在完成时信号词。{have_has} + {v_pp}（过去分词）。',
            [f'{subj} {have_has} <strong>{v_pp}</strong> the movie.'],
            f'{v_base} → {v_pp} (过去分词)', 'tense_past', 'medium'))

    # since/for 辨析
    for _ in range(30):
        use_since = random.choice([True, False])
        time_expr = random.choice(["2020", "three years", "last week", "two hours",
                                   "childhood", "a long time", "Monday", "five days",
                                   "I was born", "ten minutes", "graduation", "months"])
        if use_since:
            ans = "since"
            hint = "since + 时间点"
            explain = "since 后接时间点（如 2020、Monday、childhood）"
        else:
            ans = "for"
            hint = "for + 时间段"
            explain = "for 后接时间段（如 three years、two hours）"
        Q.append(mc(
            f"I have lived here ___ {time_expr}.", hint,
            ["since", "for", "from", "in"],
            0 if ans == "since" else 1,
            explain,
            [f'I have lived here <strong>{ans}</strong> {time_expr}.'],
            'since = 时间点, for = 时间段', 'tense_past', 'medium'))

    # have been to vs have gone to
    for _ in range(15):
        q = "She has ___ to Beijing three times."
        opts = ["been", "gone", "went", "go"]
        random.shuffle(opts)
        Q.append(mc(q, 'have been to 去过（已回来）', opts, opts.index("been"),
            'have been to = 去过已回；have gone to = 去了未回。',
            ['She has <strong>been</strong> to Beijing three times.'],
            'been to = 去过已回', 'tense_past', 'medium'))
    for _ in range(15):
        q = "Tom isn't here. He has ___ to the library."
        opts = ["gone", "been", "went", "go"]
        random.shuffle(opts)
        Q.append(mc(q, 'have gone to 去了（未回来）', opts, opts.index("gone"),
            'have gone to = 去了还没回来。人不在说明还没回来。',
            ['He has <strong>gone</strong> to the library.'],
            'gone to = 去了未回', 'tense_past', 'hard'))

    # ================================================================
    # 6. 过去完成时 (Past Perfect) — had + 过去分词
    # ================================================================
    pp_signals2 = ["before I arrived", "by the time", "after she had", "already"]
    for v_base, v_pp in pp_verbs[:20]:
        sig = random.choice(pp_signals2)
        Q.append(mc(
            f"She ___ already {v_pp} the work {sig}.", '过去完成时 had + 过去分词',
            ["had", "has", "have", "was"],
            0,
            f'过去完成时表示"过去的过去"，用 had + 过去分词。',
            [f'She <strong>had</strong> already {v_pp} the work.'],
            f'had + 过去分词 = 过去的过去', 'tense_past', 'hard'))

    # ================================================================
    # 7. 一般将来时 (Simple Future) — will + 动词原形 / be going to
    # ================================================================
    future_signals = ["tomorrow", "next week", "next month", "next year",
                      "in the future", "soon", "in 2030", "later"]

    for v_base, _ in verbs_3ps[:25]:
        sig = random.choice(future_signals)
        Q.append(mc(
            f"I ___ visit my grandparents {sig}.", '一般将来时 will + 动词原形',
            ["will", "am", "have", "was"],
            0,
            f'{sig} 是将来时间状语，用 will + 动词原形。',
            [f'I <strong>will</strong> visit my grandparents {sig}.'],
            'will + 动词原形', 'tense_future', 'easy'))

        Q.append(fl(
            f"She ___ (go) to college {sig}.", '一般将来时',
            'will go',
            f'{sig} 是将来时间状语，用 will + 动词原形。',
            [f'She <strong>will go</strong> to college {sig}.'],
            'will + 动词原形', 'tense_future', 'easy'))

    # be going to
    for _ in range(30):
        plan_verb = random.choice(["visit", "buy", "study", "learn", "build", "start", "open", "join", "watch", "cook"])
        Q.append(mc(
            f"They ___ going to {plan_verb} a new house.", 'be going to 打算做',
            ["are", "is", "will", "have"],
            0,
            'be going to 表示计划/打算。They 用 are。',
            [f'They <strong>are</strong> going to {plan_verb} a new house.'],
            'be going to = 打算', 'tense_future', 'easy'))

    # ================================================================
    # 8. 时态综合辨析
    # ================================================================
    mixed = [
        ("She ___ (go) to school every day.", "goes", "every day → 一般现在时三单", 'tense_present'),
        ("She ___ (go) to school now.", "is going", "now → 现在进行时", 'tense_present'),
        ("She ___ (go) to school yesterday.", "went", "yesterday → 一般过去时", 'tense_past'),
        ("She ___ (go) to school when I called.", "was going", "when I called → 过去进行时", 'tense_past'),
        ("She ___ already ___ (go) to school.", "has ... gone", "already → 现在完成时", 'tense_past'),
        ("She ___ (go) to school tomorrow.", "will go", "tomorrow → 一般将来时", 'tense_future'),
        ("Look! The bus ___ (come).", "is coming", "Look! → 现在进行时", 'tense_present'),
        ("I ___ never ___ (be) to Japan.", "have ... been", "never → 现在完成时", 'tense_past'),
        ("He ___ (sleep) when the phone rang.", "was sleeping", "when + 过去时 → 过去进行时", 'tense_past'),
        ("We ___ (live) here since 2015.", "have lived", "since → 现在完成时", 'tense_past'),
        ("She ___ (finish) her homework before dinner.", "had finished", "before dinner（过去） → 过去完成时", 'tense_past'),
        ("They ___ (play) football every weekend.", "play", "every weekend → 一般现在时", 'tense_present'),
        ("I ___ (read) a book at this time yesterday.", "was reading", "at this time yesterday → 过去进行时", 'tense_past'),
        ("He ___ (write) three books so far.", "has written", "so far → 现在完成时", 'tense_past'),
        ("By 2030, I ___ (graduate) from college.", "will have graduated", "By 2030 → 将来完成时", 'tense_future'),
        ("She ___ (cook) dinner when I ___ (arrive).", "was cooking ... arrived", "when 从句过去时 → 过去进行时", 'tense_past'),
        ("I ___ (not see) him since last Monday.", "haven't seen", "since → 现在完成时否定", 'tense_past'),
        ("He ___ (just finish) his work.", "has just finished", "just → 现在完成时", 'tense_past'),
        ("They ___ (travel) to Europe next summer.", "will travel", "next summer → 一般将来时", 'tense_future'),
        ("The train ___ (leave) at 8 every morning.", "leaves", "时刻表 → 一般现在时", 'tense_present'),
    ]

    for q_text, answer, explain, cat in mixed:
        if '...' in answer:
            parts = answer.split(' ... ')
            Q.append(fl(q_text, explain, answer, explain,
                [q_text.replace('___', f'<strong>{parts[0]}</strong>', 1).replace('___', f'<strong>{parts[1]}</strong>', 1)],
                explain, cat, 'hard'))
        else:
            Q.append(fl(q_text, explain, answer, explain,
                [q_text.replace('___', f'<strong>{answer}</strong>')],
                explain, cat, 'medium'))

    return Q

new_qs = gen_all_tenses()
all_qs = existing + new_qs

# 去重
seen = set()
unique = []
for q in all_qs:
    key = q['q'] + str(q.get('answer', ''))
    if key not in seen:
        seen.add(key)
        unique.append(q)

print(f"原有: {len(existing)} 题")
print(f"新增: {len(new_qs)} 题")
print(f"去重后: {len(unique)} 题")

with open(existing_file, 'w', encoding='utf-8') as f:
    json.dump(unique, f, ensure_ascii=False, indent=2)

print(f"✅ tenses.json: {len(unique)} 题")
