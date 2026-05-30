#!/usr/bin/env python3
"""
初中英语名词题库生成器
目标：500 题，涵盖 10 个维度
"""

import json
import random
import os

random.seed(42)

OUTPUT_DIR = os.path.join(os.path.dirname(__file__), "data")
os.makedirs(OUTPUT_DIR, exist_ok=True)

# ============================================================
# 全局题目 ID 计数器
# ============================================================
id_counter = [1]

def next_id():
    val = id_counter[0]
    id_counter[0] += 1
    return val

# ============================================================
# 工具函数
# ============================================================
def make_mc(q, hint, options, answer_idx, explain, examples, mnemonic, diff):
    return {
        "id": next_id(),
        "cat": "noun",
        "diff": diff,
        "type": "mc",
        "q": q,
        "hint": hint,
        "options": options,
        "answer": answer_idx,
        "explain": explain,
        "examples": examples,
        "mnemonic": mnemonic
    }

def make_fill(q, hint, answer, explain, examples, mnemonic, diff):
    return {
        "id": next_id(),
        "cat": "noun",
        "diff": diff,
        "type": "fill",
        "q": q,
        "hint": hint,
        "answer": answer,
        "explain": explain,
        "examples": examples,
        "mnemonic": mnemonic
    }

# ============================================================
# 1. 可数/不可数名词 (约 60 题)
# ============================================================
def gen_uncountable():
    questions = []

    uncountable_nouns = {
        "advice": {"zh": "建议", "quantifier": "a piece of", "example_objects": ["from your teacher", "from my parents", "on studying", "on health"]},
        "information": {"zh": "信息", "quantifier": "a piece of", "example_objects": ["about the train", "on the website", "from the library", "for travelers"]},
        "furniture": {"zh": "家具", "quantifier": "a piece of", "example_objects": ["in the living room", "from IKEA", "for the bedroom", "made of wood"]},
        "news": {"zh": "新闻/消息", "quantifier": "a piece of", "example_objects": ["about the election", "on TV", "from abroad", "on the radio"]},
        "equipment": {"zh": "设备", "quantifier": "a piece of", "example_objects": ["for the lab", "in the gym", "for camping", "for the kitchen"]},
        "luggage": {"zh": "行李", "quantifier": "a piece of", "example_objects": ["at the airport", "in the hotel room", "on the conveyor belt", "in the trunk"]},
        "progress": {"zh": "进步", "quantifier": "much/a lot of", "example_objects": ["in English", "in science", "this semester", "since last year"]},
        "knowledge": {"zh": "知识", "quantifier": "a lot of", "example_objects": ["of history", "about computers", "of the world", "about space"]},
        "evidence": {"zh": "证据", "quantifier": "a piece of", "example_objects": ["at the crime scene", "in court", "from the experiment", "in the report"]},
        "homework": {"zh": "作业", "quantifier": "a piece of / a lot of", "example_objects": ["from math class", "for tonight", "on the desk", "for the weekend"]},
        "music": {"zh": "音乐", "quantifier": "a piece of", "example_objects": ["by Mozart", "from the movie", "on the radio", "for dancing"]},
        "work": {"zh": "工作（不可数）", "quantifier": "a lot of", "example_objects": ["to do today", "in the office", "after school", "at home"]},
        "money": {"zh": "钱", "quantifier": "a lot of/some", "example_objects": ["in the bank", "for the trip", "from his job", "for charity"]},
        "water": {"zh": "水", "quantifier": "a glass of/a bottle of", "example_objects": ["on the table", "from the tap", "in the fridge", "for the plants"]},
        "paper": {"zh": "纸", "quantifier": "a piece of/a sheet of", "example_objects": ["on the desk", "for the printer", "from the notebook", "for drawing"]},
        "rice": {"zh": "米饭/大米", "quantifier": "a bowl of/a bag of", "example_objects": ["for dinner", "in the bowl", "from the market", "on the plate"]},
        "bread": {"zh": "面包", "quantifier": "a loaf of/a slice of", "example_objects": ["for breakfast", "from the bakery", "on the table", "with butter"]},
        "hair": {"zh": "头发（不可数）", "quantifier": "a strand of", "example_objects": ["on the pillow", "in the comb", "on the floor", "in the sink"]},
        "weather": {"zh": "天气", "quantifier": "—", "example_objects": ["today", "in summer", "for the picnic", "this week"]},
        "traffic": {"zh": "交通", "quantifier": "a lot of/heavy", "example_objects": ["on the highway", "in the city", "at rush hour", "during the festival"]},
    }

    # 选择题：选正确用法
    for noun, info in uncountable_nouns.items():
        qf = info["quantifier"].split("/")[0].strip()
        # MC: 选正确表达
        wrong_choices = [
            f"a {noun}",
            f"many {noun}s",
            f"a few {noun}s",
            f"two {noun}s",
            f"{noun}s",
            f"a {noun} of",
        ]
        wrong = random.sample(wrong_choices, min(3, len(wrong_choices)))
        correct = f"{qf} {noun}"
        options = [correct] + wrong
        random.shuffle(options)
        ans_idx = options.index(correct)

        questions.append(make_mc(
            q=f"以下哪个表达是正确的？",
            hint=f'{info["zh"]} 是不可数名词',
            options=options,
            answer_idx=ans_idx,
            explain=f'{noun} 是不可数名词，不能用 a/an 或复数形式。用量词：{qf} {noun}。',
            examples=[
                f'She gave me <strong>{qf} {noun}</strong>. 她给了我{info["zh"]}。',
                f'{noun.capitalize()} is important. {info["zh"]}很重要。',
            ],
            mnemonic=f'{noun} 不可数 → {qf} + {noun}',
            diff='easy'
        ))

        # 填空题
        obj = random.choice(info["example_objects"])
        questions.append(make_fill(
            q=f"She gave me ___ {noun} {obj}.",
            hint=f'{info["zh"]} 不可数，需要量词',
            answer=qf,
            explain=f'{noun} 是不可数名词，需要量词 {qf}。',
            examples=[
                f'She gave me <strong>{qf} {noun}</strong> {obj}.',
                f'I need <strong>{qf} {noun}</strong> for my project.',
            ],
            mnemonic=f'{noun} 不可数 → {qf} + {noun}',
            diff='easy'
        ))

    # 不可数名词 MC：选不可数名词
    all_nouns_list = list(uncountable_nouns.keys())
    countable_foils = ["apple", "book", "student", "chair", "dog", "cat", "car", "pen", "teacher", "school"]

    for _ in range(10):
        unc = random.choice(all_nouns_list)
        foils = random.sample(countable_foils, 3)
        options = [unc] + foils
        random.shuffle(options)
        ans_idx = options.index(unc)
        questions.append(make_mc(
            q="以下哪个是不可数名词？",
            hint="不可数名词不能直接加 a/an，也没有复数形式",
            options=options,
            answer_idx=ans_idx,
            explain=f'{unc} 是不可数名词，不能直接用 a/an 修饰，通常用 a piece of 等量词。其他选项都是可数名词。',
            examples=[
                f'I need <strong>{uncountable_nouns[unc]["quantifier"].split("/")[0]} {unc}</strong>.',
                f'There is a/an <strong>{foils[0]}</strong> on the table.',
            ],
            mnemonic=f'{unc} 不可数 → 量词 + {unc}',
            diff='medium'
        ))

    return questions

# ============================================================
# 2. 单复数规则 (约 70 题)
# ============================================================
def gen_plural_rules():
    questions = []

    # -s 规则
    s_nouns = [
        ("book", "books", "书"),
        ("cat", "cats", "猫"),
        ("dog", "dogs", "狗"),
        ("house", "houses", "房子"),
        ("table", "tables", "桌子"),
        ("pen", "pens", "笔"),
        ("student", "students", "学生"),
        ("teacher", "teachers", "老师"),
        ("flower", "flowers", "花"),
        ("river", "rivers", "河流"),
        ("cup", "cups", "杯子"),
        ("door", "doors", "门"),
    ]

    for sg, pl, zh in s_nouns:
        if random.random() < 0.5:
            options = [pl, f"{sg}es", f"{sg}s's", f"{sg}ies"]
            random.shuffle(options)
            ans_idx = options.index(pl)
            questions.append(make_mc(
                q=f"{sg} 的复数形式是什么？",
                hint=f"一般情况直接加 -s",
                options=options,
                answer_idx=ans_idx,
                explain=f'一般可数名词复数直接加 -s。{sg} → {pl}。',
                examples=[
                    f'I have three <strong>{pl}</strong>. 我有三{zh}。',
                    f'This <strong>{sg}</strong> is mine. 这个{zh}是我的。',
                ],
                mnemonic=f'一般名词 → 直接 +s → {pl}',
                diff='easy'
            ))
        else:
            questions.append(make_fill(
                q=f"One {sg}, two ___.",
                hint=f"{sg} 的复数",
                answer=pl,
                explain=f'{sg} 的复数是 {pl}，直接加 -s。',
                examples=[
                    f'One <strong>{sg}</strong>, two <strong>{pl}</strong>.',
                    f'There are many <strong>{pl}</strong> in the room.',
                ],
                mnemonic=f'{sg} + s = {pl}',
                diff='easy'
            ))

    # -es 规则（s, x, ch, sh, o 结尾）
    es_nouns = [
        ("bus", "buses", "公共汽车"),
        ("box", "boxes", "盒子"),
        ("watch", "watches", "手表"),
        ("dish", "dishes", "盘子"),
        ("brush", "brushes", "刷子"),
        ("class", "classes", "班级/课"),
        ("match", "matches", "比赛/火柴"),
        ("beach", "beaches", "海滩"),
        ("fox", "foxes", "狐狸"),
        ("glass", "glasses", "玻璃杯/眼镜"),
        ("tomato", "tomatoes", "西红柿"),
        ("potato", "potatoes", "土豆"),
        ("hero", "heroes", "英雄"),
        ("echo", "echoes", "回声"),
    ]

    for sg, pl, zh in es_nouns:
        if random.random() < 0.5:
            options = [pl, f"{sg}s", f"{sg}ies", f"{sg}ves"]
            random.shuffle(options)
            ans_idx = options.index(pl)
            questions.append(make_mc(
                q=f"{sg} 的复数形式是什么？",
                hint=f"以 -s/-x/-ch/-sh/-o 结尾，加 -es",
                options=options,
                answer_idx=ans_idx,
                explain=f'{sg} 以 {"-s/-x/-ch/-sh" if not sg.endswith("o") else "-o"} 结尾，复数加 -es：{pl}。',
                examples=[
                    f'Two <strong>{pl}</strong> are coming. 两辆{zh}来了。',
                    f'There is a <strong>{sg}</strong> here. 这里有一个{zh}。',
                ],
                mnemonic=f'{sg} 结尾特殊 → +es → {pl}',
                diff='easy' if sg in ("bus", "box", "watch") else 'medium'
            ))
        else:
            questions.append(make_fill(
                q=f"One {sg}, two ___.",
                hint=f"{sg} 的复数",
                answer=pl,
                explain=f'{sg} 以 {"-s/-x/-ch/-sh" if not sg.endswith("o") else "-o"} 结尾，复数加 -es。',
                examples=[
                    f'One <strong>{sg}</strong>, two <strong>{pl}</strong>.',
                    f'There are many <strong>{pl}</strong> here.',
                ],
                mnemonic=f'{sg} + es = {pl}',
                diff='easy' if sg in ("bus", "box", "watch") else 'medium'
            ))

    # -ies 规则（辅音 + y）
    ies_nouns = [
        ("baby", "babies", "婴儿"),
        ("city", "cities", "城市"),
        ("story", "stories", "故事"),
        ("family", "families", "家庭"),
        ("party", "parties", "聚会"),
        ("country", "countries", "国家"),
        ("lady", "ladies", "女士"),
        ("dictionary", "dictionaries", "字典"),
        ("library", "libraries", "图书馆"),
        ("hobby", "hobbies", "爱好"),
    ]

    for sg, pl, zh in ies_nouns:
        if random.random() < 0.5:
            options = [pl, f"{sg}s", f"{sg}es", f"{sg}ves"]
            random.shuffle(options)
            ans_idx = options.index(pl)
            questions.append(make_mc(
                q=f"{sg} 的复数形式是什么？",
                hint=f"以辅音 + y 结尾，变 y 为 i 加 -es",
                options=options,
                answer_idx=ans_idx,
                explain=f'{sg} 以辅音 + y 结尾，变 y 为 i 再加 -es：{pl}。注意：如果是元音 + y（如 boy → boys），直接加 s。',
                examples=[
                    f'Many <strong>{pl}</strong> have this problem. 许多{zh}有这个问题。',
                    f'This <strong>{sg}</strong> is happy. 这个{zh}很快乐。',
                ],
                mnemonic=f'{sg} 辅音+y → 去y+ies → {pl}',
                diff='medium'
            ))
        else:
            questions.append(make_fill(
                q=f"One {sg}, two ___.",
                hint=f"辅音 + y 结尾，变 y 为 i 加 -es",
                answer=pl,
                explain=f'{sg} → {pl}（变 y 为 i + es）。',
                examples=[
                    f'One <strong>{sg}</strong>, two <strong>{pl}</strong>.',
                    f'Many <strong>{pl}</strong> in this {sg[:-1]} area.',
                ],
                mnemonic=f'{sg} → 去y+ies → {pl}',
                diff='medium'
            ))

    # -ves 规则（f/fe 结尾）
    ves_nouns = [
        ("leaf", "leaves", "树叶"),
        ("wolf", "wolves", "狼"),
        ("thief", "thieves", "小偷"),
        ("knife", "knives", "刀"),
        ("wife", "wives", "妻子"),
        ("life", "lives", "生命"),
        ("half", "halves", "一半"),
        ("shelf", "shelves", "架子"),
    ]

    for sg, pl, zh in ves_nouns:
        options = [pl, f"{sg}s", f"{sg}es", f"{sg}ies"]
        random.shuffle(options)
        ans_idx = options.index(pl)
        questions.append(make_mc(
            q=f"{sg} 的复数形式是什么？",
            hint=f"以 -f/-fe 结尾，变 f 为 v 加 -es",
            options=options,
            answer_idx=ans_idx,
            explain=f'{sg} 以 -f/-fe 结尾，变 f 为 v 再加 -es：{pl}。注意：roof → roofs（例外）。',
            examples=[
                f'Autumn <strong>{pl}</strong> are beautiful. 秋天的{zh}很美。',
                f'A single <strong>{sg}</strong> fell. 一片{zh}落了下来。',
            ],
            mnemonic=f'{sg} -f/-fe → 变f为v+es → {pl}',
            diff='medium'
        ))

    # 不规则复数
    irregular_plurals = [
        ("child", "children", "孩子"),
        ("mouse", "mice", "老鼠"),
        ("foot", "feet", "脚"),
        ("tooth", "teeth", "牙齿"),
        ("person", "people", "人"),
        ("man", "men", "男人"),
        ("woman", "women", "女人"),
        ("sheep", "sheep", "绵羊"),
        ("fish", "fish", "鱼"),
        ("deer", "deer", "鹿"),
        ("goose", "geese", "鹅"),
        ("ox", "oxen", "公牛"),
        ("crisis", "crises", "危机"),
        ("phenomenon", "phenomena", "现象"),
        ("medium", "media", "媒介"),
    ]

    for sg, pl, zh in irregular_plurals:
        if sg == pl:
            # 单复数同形
            options = [pl, f"{sg}s", f"{sg}es", f"{sg}ies"]
            random.shuffle(options)
            ans_idx = options.index(pl)
            questions.append(make_mc(
                q=f"{sg} 的复数形式是什么？",
                hint=f"{sg} 单复数同形",
                options=options,
                answer_idx=ans_idx,
                explain=f'{sg} 是单复数同形的名词，一只{zh}是 {sg}，多只{zh}也是 {sg}。',
                examples=[
                    f'There is one <strong>{sg}</strong>. 有一只{zh}。',
                    f'There are three <strong>{sg}</strong>. 有三只{zh}。',
                ],
                mnemonic=f'{sg} 单复数同形！不变！',
                diff='medium'
            ))
        else:
            options = [pl, f"{sg}s", f"{sg}es", f"{sg}ies"]
            random.shuffle(options)
            ans_idx = options.index(pl)
            questions.append(make_mc(
                q=f"{sg} 的复数形式是什么？",
                hint=f"不规则复数，需要特别记忆",
                options=options,
                answer_idx=ans_idx,
                explain=f'{sg} 是不规则名词，复数是 {pl}，需要单独记忆。',
                examples=[
                    f'One <strong>{sg}</strong>, many <strong>{pl}</strong>. 一个{zh}，许多{zh}。',
                    f'The <strong>{pl}</strong> are playing. {zh}们在玩耍。',
                ],
                mnemonic=f'{sg} → {pl}（不规则，死记！）',
                diff='hard' if sg in ("phenomenon", "crisis", "ox") else 'medium'
            ))

    return questions

# ============================================================
# 3. 集合名词 (约 40 题)
# ============================================================
def gen_collective_nouns():
    questions = []

    collective_nouns = [
        ("family", "家庭/家人"),
        ("team", "团队"),
        ("audience", "观众"),
        ("committee", "委员会"),
        ("class", "班级/全班同学"),
        ("government", "政府"),
        ("staff", "员工/工作人员"),
        ("crew", "全体船员/机组人员"),
        ("jury", "陪审团"),
        ("band", "乐队"),
        ("public", "公众"),
        ("police", "警察（总称）"),
    ]

    for noun, zh in collective_nouns:
        # MC: 选正确谓语形式
        if noun == "police":
            correct_sentence = f"The {noun} are investigating the case."
            wrong_sentences = [f"The {noun} is investigating the case."]
            explain = f'{noun}（警察）作主语时，谓语永远用复数。The police are...'
            mnemonic = f'{noun} → 永远复数！The {noun} are...'
        else:
            correct_sentence = f"The {noun} is/are having a meeting."
            explain = f'{noun}（{zh}）强调整体时用单数谓语，强调个体成员时用复数谓语。具体看语境。'
            mnemonic = f'{noun} → 整体单数/个体复数'

        # 选择题
        options = [
            f"The {noun} is having a meeting.",
            f"The {noun} are having a meeting.",
            f"The {noun} were having a meeting.",
            f"The {noun} have a meeting.",
        ]
        random.shuffle(options)

        if noun == "police":
            ans_idx = options.index(f"The {noun} are having a meeting.") if f"The {noun} are having a meeting." in options else 0
        else:
            ans_idx = options.index(f"The {noun} is having a meeting.")

        questions.append(make_mc(
            q=f"选择正确的句子：",
            hint=f'{noun} 是集合名词',
            options=options,
            answer_idx=ans_idx,
            explain=explain,
            examples=[
                f'The <strong>{noun}</strong> is/are discussing the plan. {zh}正在讨论计划。',
                f'My <strong>{noun}</strong> supports/support me. 我的{zh}支持我。',
            ],
            mnemonic=mnemonic,
            diff='medium' if noun != "police" else 'easy'
        ))

        # 填空题
        questions.append(make_fill(
            q=f"The {noun} ___ (be) having a discussion.",
            hint=f'{noun} 的谓语形式',
            answer="is" if noun != "police" else "are",
            explain=f'{noun} 作主语，{"强调整体用单数 is" if noun != "police" else "police 永远用复数 are"}。',
            examples=[
                f'The <strong>{noun}</strong> {"is" if noun != "police" else "are"} having a discussion.',
                f'My <strong>{noun}</strong> {"is" if noun != "police" else "are"} very supportive.',
            ],
            mnemonic=f'{noun} → {"整体=is" if noun != "police" else "永远=are"}',
            diff='medium'
        ))

    return questions

# ============================================================
# 4. 名词所有格 (约 50 题)
# ============================================================
def gen_possessive():
    questions = []

    # 's 所有格
    s_possessive = [
        ("Tom", "book", "Tom的书"),
        ("my sister", "room", "我姐姐的房间"),
        ("the teacher", "desk", "老师的桌子"),
        ("the dog", "tail", "狗的尾巴"),
        ("Lucy", "bag", "Lucy的包"),
        ("Mr. Smith", "car", "Smith先生的车"),
        ("my father", "friend", "我爸爸的朋友"),
        ("the cat", "food", "猫的食物"),
        ("the children", "toys", "孩子们的玩具"),
        ("women", "rights", "女性权利"),
        ("the boss", "office", "老板的办公室"),
        ("James", "phone", "James的手机"),
        ("the earth", "surface", "地球的表面"),
        ("today", "newspaper", "今天的报纸"),
    ]

    for owner, thing, zh in s_possessive:
        if "'" not in owner:
            poss = f"{owner}'s {thing}"
        else:
            poss = f"{owner}'s {thing}"

        # MC
        wrong_forms = [
            f"{owner}s {thing}",
            f"{owner} of {thing}",
            f"{thing} of {owner}",
            f"{owner}es {thing}",
        ]
        options = [poss] + random.sample(wrong_forms, 3)
        random.shuffle(options)
        ans_idx = options.index(poss)

        questions.append(make_mc(
            q=f"选择正确的表达：{zh}",
            hint=f"有生命名词用 's 所有格",
            options=options,
            answer_idx=ans_idx,
            explain=f'有生命名词（人/动物）的所有格用 \'s。{owner}\'s {thing} = {zh}。',
            examples=[
                f'This is <strong>{poss}</strong>. 这是{zh}。',
                f"I found <strong>{poss}</strong> on the floor. 我在地板上找到了{zh}。",
            ],
            mnemonic=f'有生命 → \'s → {poss}',
            diff='easy'
        ))

    # s' 所有格（复数名词以 s 结尾）
    s_apostrophe = [
        ("the students", "classroom", "学生们的教室"),
        ("my parents", "bedroom", "我父母的卧室"),
        ("the teachers", "office", "老师们的办公室"),
        ("the boys", "bathroom", "男生们的浴室"),
        ("the dogs", "bowls", "狗们的碗"),
        ("the Smiths", "house", "Smith一家的房子"),
        ("the players", "locker room", "球员的更衣室"),
        ("the workers", "rights", "工人们的权利"),
    ]

    for owner, thing, zh in s_apostrophe:
        poss = f"{owner}' {thing}"

        wrong_forms = [
            f"{owner}'s {thing}",
            f"{owner}{thing}",
            f"{thing} of {owner}",
        ]
        options = [poss] + random.sample(wrong_forms, 3)
        random.shuffle(options)
        ans_idx = options.index(poss)

        questions.append(make_mc(
            q=f"选择正确的表达：{zh}",
            hint=f"复数名词以 s 结尾，只加 '",
            options=options,
            answer_idx=ans_idx,
            explain=f'以 s 结尾的复数名词，所有格只加 \'（不加 s）。{owner}\' {thing} = {zh}。',
            examples=[
                f'This is <strong>{poss}</strong>. 这是{zh}。',
                f"We cleaned <strong>{poss}</strong> yesterday.",
            ],
            mnemonic=f'复数 s 结尾 → 只加 \' → {poss}',
            diff='medium'
        ))

    # of 所有格（无生命名词）
    of_possessive = [
        ("the door", "the room", "房间的门"),
        ("the cover", "the book", "书的封面"),
        ("the name", "the city", "城市的名字"),
        ("the end", "the road", "路的尽头"),
        ("the color", "the sky", "天空的颜色"),
        ("the capital", "China", "中国的首都"),
        ("the title", "the song", "歌曲的名字"),
        ("the roof", "the house", "房子的屋顶"),
        ("the leg", "the table", "桌子的腿"),
        ("the window", "the car", "车的窗户"),
    ]

    for part, whole, zh in of_possessive:
        poss = f"{part} of {whole}"

        wrong_forms = [
            f"{whole}'s {part}",
            f"{part}'s {whole}",
            f"{part} {whole}",
        ]
        options = [poss] + random.sample(wrong_forms, 3)
        random.shuffle(options)
        ans_idx = options.index(poss)

        questions.append(make_mc(
            q=f"选择正确的表达：{zh}",
            hint=f"无生命名词用 of 所有格",
            options=options,
            answer_idx=ans_idx,
            explain=f'无生命名词的所有格用 of 结构。{part} of {whole} = {zh}。',
            examples=[
                f'<strong>{poss}</strong> is broken. {zh}坏了。',
                f'We reached <strong>{poss}</strong>. 我们到达了{zh}。',
            ],
            mnemonic=f'无生命 → of 结构 → {part} of {whole}',
            diff='easy'
        ))

    # 双重所有格
    double_possessive = [
        ("a friend of my father's", "我爸爸的一个朋友"),
        ("a classmate of Tom's", "Tom的一个同学"),
        ("a painting of Picasso's", "毕加索的一幅画"),
        ("a book of my sister's", "我姐姐的一本书"),
        ("an idea of his", "他的一个想法"),
    ]

    for expr, zh in double_possessive:
        wrong_forms = [
            expr.replace("'s", ""),
            expr.replace("'s", "s"),
            f"a friend of my father",
        ]
        options = [expr] + random.sample(wrong_forms, 3)
        random.shuffle(options)
        ans_idx = options.index(expr)

        questions.append(make_mc(
            q=f"选择正确的表达：{zh}",
            hint=f"双重所有格：of + 's",
            options=options,
            answer_idx=ans_idx,
            explain=f'双重所有格 = of + 名词所有格（\'s / 名词性物主代词）。{expr} = {zh}。',
            examples=[
                f'<strong>{expr}</strong> is coming to visit. {zh}要来拜访。',
                f"I borrowed <strong>{expr}</strong> yesterday.",
            ],
            mnemonic=f'双重所有格 → of + \'s → {expr}',
            diff='hard'
        ))

    return questions

# ============================================================
# 5. 复合名词 (约 50 题)
# ============================================================
def gen_compound_nouns():
    questions = []

    compounds = [
        ("bedroom", "卧室", "bed + room", "睡觉的房间"),
        ("basketball", "篮球", "basket + ball", "篮筐+球"),
        ("notebook", "笔记本", "note + book", "记笔记的本子"),
        ("airport", "机场", "air + port", "航空港口"),
        ("classroom", "教室", "class + room", "上课的房间"),
        ("homework", "家庭作业", "home + work", "在家做的工作"),
        ("newspaper", "报纸", "news + paper", "新闻纸"),
        ("sunrise", "日出", "sun + rise", "太阳升起"),
        ("password", "密码", "pass + word", "通行的暗号"),
        ("football", "足球", "foot + ball", "用脚踢的球"),
        ("rainbow", "彩虹", "rain + bow", "雨后弓形"),
        ("birthday", "生日", "birth + day", "出生的日子"),
        ("weekend", "周末", "week + end", "一周的末尾"),
        ("railway", "铁路", "rail + way", "铁轨之路"),
        ("lifetime", "一生", "life + time", "生命的时间"),
        ("earthquake", "地震", "earth + quake", "大地震动"),
        ("sunlight", "阳光", "sun + light", "太阳的光"),
        ("moonlight", "月光", "moon + light", "月亮的光"),
        ("waterfall", "瀑布", "water + fall", "水落下"),
        ("seashell", "贝壳", "sea + shell", "海里的壳"),
        ("toothbrush", "牙刷", "tooth + brush", "刷牙的刷子"),
        ("snowman", "雪人", "snow + man", "雪做的人"),
        ("bookstore", "书店", "book + store", "卖书的店"),
        ("sunglasses", "太阳镜", "sun + glasses", "遮阳眼镜"),
        ("haircut", "理发", "hair + cut", "剪头发"),
    ]

    for comp, zh, breakdown, meaning in compounds:
        if random.random() < 0.5:
            # MC: 选正确的复合词
            wrong_forms = [
                comp.replace("room", " room") if "room" in comp else comp + " room",
                comp.replace("ball", " ball") if "ball" in comp else comp + " ball",
                comp.replace("work", " work") if "work" in comp else comp + " work",
                comp.replace("book", " book") if "book" in comp else comp + " book",
            ]
            # 确保错误选项看起来合理
            parts = breakdown.split(" + ")
            if len(parts) >= 2:
                wrong_forms = [
                    f"{parts[0]} {parts[1]}",
                    f"{parts[0]}-{parts[1]}",
                    f"{parts[1]}{parts[0]}",
                ]
            options = [comp] + wrong_forms
            random.shuffle(options)
            ans_idx = options.index(comp)

            questions.append(make_mc(
                q=f"{zh} 的英文是什么？",
                hint=f'{meaning} → {breakdown}',
                options=options,
                answer_idx=ans_idx,
                explain=f'{comp} = {breakdown}（{meaning}）。复合名词由两个词组合而成。',
                examples=[
                    f'I have my own <strong>{comp}</strong>. 我有自己的{zh}。',
                    f'The <strong>{comp}</strong> is very clean. {zh}很干净。',
                ],
                mnemonic=f'{comp} = {breakdown}',
                diff='easy'
            ))
        else:
            questions.append(make_fill(
                q=f"I need a ___ ({zh}) to write down my ideas.",
                hint=f'合成词：{breakdown}',
                answer=comp,
                explain=f'{comp} = {breakdown}（{meaning}）。',
                examples=[
                    f'I bought a new <strong>{comp}</strong>. 我买了一个新的{zh}。',
                    f'This <strong>{comp}</strong> is very useful. 这个{zh}很有用。',
                ],
                mnemonic=f'{comp} = {breakdown}',
                diff='easy'
            ))

    return questions

# ============================================================
# 6. 名词后缀 (约 50 题)
# ============================================================
def gen_noun_suffixes():
    questions = []

    suffixes = [
        ("-tion", "表示动作/状态/结果", [
            ("educate", "education", "教育"),
            ("inform", "information", "信息"),
            ("celebrate", "celebration", "庆祝"),
            ("communicate", "communication", "交流"),
            ("invite", "invitation", "邀请"),
            ("graduate", "graduation", "毕业"),
            ("imagine", "imagination", "想象"),
            ("organize", "organization", "组织"),
            ("prepare", "preparation", "准备"),
            ("translate", "translation", "翻译"),
        ]),
        ("-sion", "表示动作/状态", [
            ("decide", "decision", "决定"),
            ("discuss", "discussion", "讨论"),
            ("express", "expression", "表达"),
            ("confuse", "confusion", "困惑"),
            ("conclude", "conclusion", "结论"),
            ("admit", "admission", "承认/准入"),
            ("permit", "permission", "许可"),
        ]),
        ("-ment", "表示动作/结果", [
            ("develop", "development", "发展"),
            ("achieve", "achievement", "成就"),
            ("agree", "agreement", "协议"),
            ("move", "movement", "运动/移动"),
            ("improve", "improvement", "改进"),
            ("argue", "argument", "争论"),
            ("govern", "government", "政府"),
            ("entertain", "entertainment", "娱乐"),
            ("treat", "treatment", "治疗/对待"),
        ]),
        ("-ness", "将形容词变为名词", [
            ("happy", "happiness", "幸福"),
            ("kind", "kindness", "善良"),
            ("dark", "darkness", "黑暗"),
            ("sad", "sadness", "悲伤"),
            ("weak", "weakness", "弱点"),
            ("ill", "illness", "疾病"),
            ("lonely", "loneliness", "孤独"),
            ("careless", "carelessness", "粗心"),
        ]),
        ("-ity", "表示性质/状态", [
            ("able", "ability", "能力"),
            ("active", "activity", "活动"),
            ("possible", "possibility", "可能性"),
            ("real", "reality", "现实"),
            ("equal", "equality", "平等"),
            ("popular", "popularity", "受欢迎"),
            ("curious", "curiosity", "好奇心"),
        ]),
        ("-ance/-ence", "表示状态/性质", [
            ("important", "importance", "重要性"),
            ("appear", "appearance", "外貌/出现"),
            ("different", "difference", "不同"),
            ("distant", "distance", "距离"),
            ("exist", "existence", "存在"),
            ("silent", "silence", "沉默"),
            ("patient", "patience", "耐心"),
        ]),
        ("-ship", "表示关系/状态/技能", [
            ("friend", "friendship", "友谊"),
            ("leader", "leadership", "领导力"),
            ("relation", "relationship", "关系"),
            ("member", "membership", "会员资格"),
            ("champion", "championship", "冠军地位"),
            ("hard", "hardship", "艰难"),
        ]),
        ("-hood", "表示时期/状态", [
            ("child", "childhood", "童年"),
            ("neighbor", "neighborhood", "邻里"),
            ("brother", "brotherhood", "兄弟情谊"),
            ("adult", "adulthood", "成年"),
            ("likely", "likelihood", "可能性"),
        ]),
        ("-dom", "表示领域/状态", [
            ("free", "freedom", "自由"),
            ("king", "kingdom", "王国"),
            ("wise", "wisdom", "智慧"),
            ("bore", "boredom", "无聊"),
        ]),
    ]

    for suffix, meaning, examples in suffixes:
        for base, noun, zh in examples:
            # MC: 选正确的名词形式
            wrong_endings = [s for s, _, _ in suffixes if s != suffix]
            wrong_choices = []
            for we in random.sample(wrong_endings, min(3, len(wrong_endings))):
                if we == "-tion":
                    wrong_choices.append(f"{base}tion")
                elif we == "-sion":
                    wrong_choices.append(f"{base}sion")
                elif we == "-ment":
                    wrong_choices.append(f"{base}ment")
                elif we == "-ness":
                    wrong_choices.append(f"{base}ness")
                elif we == "-ity":
                    wrong_choices.append(f"{base}ity")
                elif we == "-ance/-ence":
                    wrong_choices.append(f"{base}ance")
                else:
                    wrong_choices.append(f"{base}{we.replace('-', '')}")

            options = [noun] + wrong_choices[:3]
            random.shuffle(options)
            ans_idx = options.index(noun)

            questions.append(make_mc(
                q=f"{base}（{base}）的名词形式是？",
                hint=f'后缀 {suffix} {meaning}',
                options=options,
                answer_idx=ans_idx,
                explain=f'{base} + {suffix} = {noun}（{zh}）。{suffix} {meaning}。',
                examples=[
                    f'<strong>{noun}</strong> is important. {zh}很重要。',
                    f'We need more <strong>{noun}</strong> in this area.',
                ],
                mnemonic=f'{base} + {suffix} = {noun}',
                diff='medium' if len(base) <= 5 else 'hard'
            ))

            # 填空题
            questions.append(make_fill(
                q=f"The ___ ({zh}) of this city is impressive.",
                hint=f'{base} 加后缀 {suffix}',
                answer=noun,
                explain=f'{base} + {suffix} = {noun}（{zh}）。',
                examples=[
                    f'The <strong>{noun}</strong> of this city is impressive.',
                    f'<strong>{noun}</strong> comes from the verb {base}.',
                ],
                mnemonic=f'{base} + {suffix} = {noun}',
                diff='medium' if len(base) <= 5 else 'hard'
            ))

    return questions

# ============================================================
# 7. 同义名词辨析 (约 60 题)
# ============================================================
def gen_synonym_nouns():
    questions = []

    # problem / question / issue / matter
    problem_question = [
        ("problem", "问题（需要解决的问题/麻烦）", "solve a problem", "解决问题"),
        ("question", "问题（需要回答的问题）", "ask a question", "提问"),
        ("issue", "议题/问题（有争议的话题）", "raise an issue", "提出问题"),
        ("matter", "事情/问题（泛指）", "a personal matter", "私事"),
    ]

    for noun, zh, collocation, coll_zh in problem_question:
        others = [n for n, _, _, _ in problem_question if n != noun]
        options = [noun] + others
        random.shuffle(options)
        ans_idx = options.index(noun)

        questions.append(make_mc(
            q=f"选择正确的词：This is a serious ___. We need to {collocation.split()[-2]} it.",
            hint=f'{zh}',
            options=options,
            answer_idx=ans_idx,
            explain=f'{noun} 表示"{zh}"。常用搭配：{collocation}。',
            examples=[
                f'This is a serious <strong>{noun}</strong>. 这是一个严重的{zh}。',
                f'We need to <strong>{collocation}</strong>. 我们需要{coll_zh}。',
            ],
            mnemonic=f'{noun} = {zh} | {collocation}',
            diff='medium'
        ))

    # journey / trip / travel / tour / voyage
    travel_words = [
        ("journey", "旅程（长途，通常一次性的）", "a long journey", "长途旅程"),
        ("trip", "旅行（短途往返）", "a business trip", "出差"),
        ("travel", "旅行（泛指，不可数）", "air travel", "航空旅行"),
        ("tour", "游览/巡回（参观多个地方）", "a city tour", "城市游览"),
        ("voyage", "航行（海上/太空）", "a sea voyage", "海上航行"),
    ]

    for noun, zh, collocation, coll_zh in travel_words:
        others = [n for n, _, _, _ in travel_words if n != noun]
        options = [noun] + random.sample(others, min(3, len(others)))
        random.shuffle(options)
        ans_idx = options.index(noun)

        questions.append(make_mc(
            q=f"选择正确的词：They went on a ___. It was {collocation}.",
            hint=f'{zh}',
            options=options,
            answer_idx=ans_idx,
            explain=f'{noun} 表示"{zh}"。{collocation} = {coll_zh}。',
            examples=[
                f'They went on <strong>{collocation}</strong>. 他们进行了一次{coll_zh}。',
                f'The <strong>{noun}</strong> was wonderful. 这次{zh}很棒。',
            ],
            mnemonic=f'{noun} = {zh}',
            diff='medium'
        ))

    # view / scene / scenery / sight
    sight_words = [
        ("view", "视野/景色（从某处看到的）", "a beautiful view", "美丽的景色"),
        ("scene", "场景/景象（眼前看到的）", "a touching scene", "感人的一幕"),
        ("scenery", "风景（整体自然风光，不可数）", "beautiful scenery", "美丽的风景"),
        ("sight", "景象/名胜（值得看的）", "see the sights", "观光"),
    ]

    for noun, zh, collocation, coll_zh in sight_words:
        others = [n for n, _, _, _ in sight_words if n != noun]
        options = [noun] + random.sample(others, min(3, len(others)))
        random.shuffle(options)
        ans_idx = options.index(noun)

        questions.append(make_mc(
            q=f"选择正确的词：From the top, the ___ was breathtaking.",
            hint=f'{zh}',
            options=options,
            answer_idx=ans_idx,
            explain=f'{noun} 表示"{zh}"。{collocation} = {coll_zh}。',
            examples=[
                f'The <strong>{noun}</strong> was breathtaking. {zh}令人叹为观止。',
                f'We enjoyed the <strong>{collocation}</strong>. 我们欣赏了{coll_zh}。',
            ],
            mnemonic=f'{noun} = {zh}',
            diff='medium'
        ))

    # work / job / career / occupation
    work_job = [
        ("work", "工作（不可数，泛指）", "hard work", "努力工作"),
        ("job", "工作（可数，具体职位）", "find a job", "找工作"),
        ("career", "职业/生涯（长期）", "a successful career", "成功的职业"),
        ("occupation", "职业（正式用语）", "What's your occupation?", "你的职业是什么？"),
    ]

    for noun, zh, collocation, coll_zh in work_job:
        others = [n for n, _, _, _ in work_job if n != noun]
        options = [noun] + random.sample(others, min(3, len(others)))
        random.shuffle(options)
        ans_idx = options.index(noun)

        questions.append(make_mc(
            q=f"选择正确的词：He is looking for a ___.",
            hint=f'{zh}',
            options=options,
            answer_idx=ans_idx,
            explain=f'{noun} 表示"{zh}"。{collocation} = {coll_zh}。注意：work 不可数，job 可数。',
            examples=[
                f'He found a good <strong>{noun}</strong>. 他找到了一份好的{zh}。',
                f'{collocation.capitalize()} is important. {coll_zh}很重要。',
            ],
            mnemonic=f'{noun} = {zh} | {collocation}',
            diff='easy' if noun in ("work", "job") else 'medium'
        ))

    # error / mistake / fault
    error_mistake = [
        ("error", "错误（正式/技术用语）", "a computer error", "电脑错误"),
        ("mistake", "错误（日常用语）", "make a mistake", "犯错误"),
        ("fault", "过错/责任", "It's my fault.", "是我的错。"),
    ]

    for noun, zh, collocation, coll_zh in error_mistake:
        others = [n for n, _, _, _ in error_mistake if n != noun]
        options = [noun] + others
        random.shuffle(options)
        ans_idx = options.index(noun)

        questions.append(make_mc(
            q=f"选择正确的词：There is a spelling ___ in the text.",
            hint=f'{zh}',
            options=options,
            answer_idx=ans_idx,
            explain=f'{noun} 表示"{zh}"。{collocation} = {coll_zh}。',
            examples=[
                f'There is a spelling <strong>{noun}</strong>. 有一个拼写{zh}。',
                f'Everyone makes <strong>mistakes</strong>. 每个人都会犯错。',
            ],
            mnemonic=f'{noun} = {zh}',
            diff='medium'
        ))

    # chance / opportunity
    chance_opportunity = [
        ("chance", "机会（偶然/可能性）", "by chance", "偶然"),
        ("opportunity", "机会（有利时机）", "a great opportunity", "绝佳机会"),
    ]

    for noun, zh, collocation, coll_zh in chance_opportunity:
        other = "opportunity" if noun == "chance" else "chance"
        options = [noun, other, f"a {other}", f"the {other}"]
        random.shuffle(options)
        ans_idx = options.index(noun)

        questions.append(make_mc(
            q=f"选择正确的词：Don't miss this ___ to study abroad!",
            hint=f'{zh}',
            options=options,
            answer_idx=ans_idx,
            explain=f'{noun} 表示"{zh}"。{collocation} = {coll_zh}。',
            examples=[
                f"Don't miss this <strong>{noun}</strong>! 别错过这个{zh}！",
                f'I met her <strong>by chance</strong>. 我偶然遇到了她。',
            ],
            mnemonic=f'{noun} = {zh}',
            diff='medium'
        ))

    return questions

# ============================================================
# 8. 常见名词搭配 (约 50 题)
# ============================================================
def gen_noun_collocations():
    questions = []

    collocations = [
        ("make a decision", "做决定", "It's time to make a decision.", "是时候做决定了。"),
        ("do homework", "做作业", "I do my homework every evening.", "我每天晚上做作业。"),
        ("take a break", "休息一下", "Let's take a break.", "我们休息一下吧。"),
        ("have a meeting", "开会", "We have a meeting at 3 p.m.", "我们下午三点有个会。"),
        ("pay attention", "注意", "Please pay attention to the teacher.", "请注意听老师讲。"),
        ("keep a secret", "保守秘密", "Can you keep a secret?", "你能保守秘密吗？"),
        ("catch a cold", "感冒", "I caught a cold last week.", "我上周感冒了。"),
        ("tell a lie", "说谎", "It's wrong to tell a lie.", "说谎是不对的。"),
        ("tell the truth", "说实话", "You should tell the truth.", "你应该说实话。"),
        ("take a photo", "拍照", "Let me take a photo of you.", "让我给你拍张照。"),
        ("make a mistake", "犯错", "Everyone makes mistakes.", "每个人都会犯错。"),
        ("give advice", "给建议", "Can you give me some advice?", "你能给我一些建议吗？"),
        ("take medicine", "吃药", "Remember to take your medicine.", "记得吃药。"),
        ("make progress", "取得进步", "She is making good progress.", "她取得了很好的进步。"),
        ("take a shower", "洗澡", "I take a shower every morning.", "我每天早上洗澡。"),
        ("have a party", "举办聚会", "We're having a party this weekend.", "我们这个周末有聚会。"),
        ("make friends", "交朋友", "It's easy to make friends here.", "在这里很容易交朋友。"),
        ("do business", "做生意", "They do business with many countries.", "他们和许多国家做生意。"),
        ("take a risk", "冒险", "Sometimes you need to take a risk.", "有时候你需要冒险。"),
        ("have a look", "看一看", "Can I have a look at your book?", "我能看看你的书吗？"),
        ("make a difference", "有影响/起作用", "Your help made a difference.", "你的帮助起了作用。"),
        ("do research", "做研究", "He is doing research on cancer.", "他在做癌症研究。"),
        ("take place", "发生/举行", "The event will take place tomorrow.", "活动明天举行。"),
        ("have fun", "玩得开心", "We had fun at the park.", "我们在公园玩得很开心。"),
        ("pay a visit", "拜访", "We paid a visit to our grandparents.", "我们去拜访了祖父母。"),
        ("make an effort", "努力", "You need to make an effort.", "你需要努力。"),
        ("take care of", "照顾", "Please take care of my dog.", "请照顾我的狗。"),
        ("have a problem", "有问题/麻烦", "I have a problem with my computer.", "我的电脑有问题。"),
        ("make sure", "确保", "Make sure the door is locked.", "确保门锁好了。"),
        ("do the dishes", "洗碗", "Who will do the dishes tonight?", "今晚谁洗碗？"),
        ("take a walk", "散步", "Let's take a walk after dinner.", "晚饭后我们散步吧。"),
        ("have a rest", "休息", "You should have a rest.", "你应该休息一下。"),
        ("make money", "赚钱", "He wants to make money quickly.", "他想快速赚钱。"),
        ("do a favor", "帮忙", "Can you do me a favor?", "你能帮我一个忙吗？"),
        ("take notes", "记笔记", "Don't forget to take notes.", "别忘了记笔记。"),
        ("have an idea", "有想法", "I have an idea!", "我有个主意！"),
        ("make a plan", "制定计划", "Let's make a plan first.", "我们先制定计划。"),
        ("take a seat", "坐下", "Please take a seat.", "请坐。"),
        ("have a chance", "有机会", "I hope I have a chance to go.", "我希望我有机会去。"),
        ("make noise", "制造噪音", "Don't make noise in the library.", "别在图书馆制造噪音。"),
        ("do harm", "造成伤害", "Smoking does harm to your health.", "吸烟对健康有害。"),
        ("take action", "采取行动", "We need to take action now.", "我们现在需要采取行动。"),
        ("have a dream", "做梦/有梦想", "I had a dream last night.", "我昨晚做了一个梦。"),
        ("make tea", "泡茶", "Let me make you a cup of tea.", "让我给你泡杯茶。"),
        ("take advantage of", "利用", "Take advantage of this opportunity.", "利用这个机会。"),
        ("have a word with", "和…说句话", "Can I have a word with you?", "我能和你说句话吗？"),
        ("make room for", "给…腾地方", "Make room for the new books.", "给新书腾地方。"),
        ("take part in", "参加", "She took part in the competition.", "她参加了比赛。"),
        ("have no idea", "不知道", "I have no idea what to do.", "我不知道该怎么办。"),
        ("do one's best", "尽力", "I'll do my best to help you.", "我会尽力帮你。"),
    ]

    for coll, zh, example, ex_zh in collocations:
        verb, noun_phrase = coll.split(" ", 1)

        if random.random() < 0.4:
            # Fill: 填动词
            questions.append(make_fill(
                q=f"I need to ___ {noun_phrase}. ({zh})",
                hint=f'"{zh}"的动词搭配',
                answer=verb,
                explain=f'"{coll}" = {zh}。名词 {noun_phrase.split()[-1] if " " in noun_phrase else noun_phrase} 常与 {verb} 搭配。',
                examples=[example.replace(coll, f'<strong>{coll}</strong>'), ex_zh],
                mnemonic=f'{verb} + {noun_phrase} = {zh}',
                diff='easy'
            ))
        else:
            # MC: 选动词
            distractor_verbs = ["make", "do", "take", "have", "give", "pay", "catch", "keep", "tell"]
            distractor_verbs = [v for v in distractor_verbs if v != verb]
            distractors = random.sample(distractor_verbs, min(3, len(distractor_verbs)))
            options = [verb] + distractors
            random.shuffle(options)
            ans_idx = options.index(verb)

            questions.append(make_mc(
                q=f"选择正确的动词：___ {noun_phrase}",
                hint=f'意为"{zh}"',
                options=options,
                answer_idx=ans_idx,
                explain=f'"{coll}" = {zh}。{verb} 是正确搭配。',
                examples=[example.replace(coll, f'<strong>{coll}</strong>'), ex_zh],
                mnemonic=f'{verb} + {noun_phrase} = {zh}',
                diff='easy'
            ))

    return questions

# ============================================================
# 9. 抽象名词具体化 (约 30 题)
# ============================================================
def gen_abstract_nouns():
    questions = []

    abstract_concrete = [
        ("experience", "经验（不可数，抽象）", "经历（可数，具体）", "She has much experience. / It was an unforgettable experience.", "她经验丰富。/ 那是一次难忘的经历。"),
        ("beauty", "美（不可数，抽象）", "美人（可数，具体）", "She appreciates beauty. / She is a real beauty.", "她欣赏美。/ 她真是个美人。"),
        ("youth", "青春（不可数，抽象）", "年轻人（可数/集合）", "He wasted his youth. / The youth of today are confident.", "他浪费了青春。/ 今天的年轻人很自信。"),
        ("success", "成功（不可数，抽象）", "成功的人/事（可数）", "Hard work brings success. / The party was a great success.", "努力带来成功。/ 聚会非常成功。"),
        ("failure", "失败（不可数，抽象）", "失败的人/事（可数）", "Fear of failure stops many. / The project was a failure.", "对失败的恐惧阻碍了很多人。/ 这个项目是个失败。"),
        ("pleasure", "快乐（不可数，抽象）", "乐事（可数）", "Reading gives me pleasure. / It's a pleasure to meet you.", "阅读给我快乐。/ 很高兴认识你。"),
        ("honor", "荣誉（不可数，抽象）", "荣幸的事（可数）", "He fought for honor. / It's an honor to be here.", "他为荣誉而战。/ 来到这里是我的荣幸。"),
        ("surprise", "惊奇（不可数，抽象）", "惊喜（可数）", "He looked at me in surprise. / What a nice surprise!", "他惊讶地看着我。/ 真是个惊喜！"),
        ("difficulty", "困难（不可数，抽象）", "难事（可数）", "I have difficulty in math. / We faced many difficulties.", "我数学有困难。/ 我们面临很多困难。"),
        ("power", "力量/权力（不可数，抽象）", "大国/强权（可数）", "Knowledge is power. / China is a world power.", "知识就是力量。/ 中国是一个世界大国。"),
    ]

    for noun, abs_zh, conc_zh, example_pair, ex_pair_zh in abstract_concrete:
        # MC: 选不可数/可数用法
        if random.random() < 0.5:
            # 不可数用法
            questions.append(make_mc(
                q=f"选择正确的句子：",
                hint=f'{noun} 作"{abs_zh}"时不可数',
                options=[
                    f"She has much {noun}.",
                    f"She has a {noun}.",
                    f"She has many {noun}s.",
                    f"She has a few {noun}.",
                ],
                answer_idx=0,
                explain=f'{noun} 作"{abs_zh}"讲时是不可数名词，不用 a/an，没有复数。作"{conc_zh}"讲时可数。',
                examples=[example_pair, ex_pair_zh],
                mnemonic=f'{noun} → 抽象"{abs_zh}"不可数 / 具体"{conc_zh}"可数',
                diff='hard'
            ))
        else:
            questions.append(make_fill(
                q=f"Complete: It was ___ unforgettable ___ ({noun}).",
                hint=f'{noun} 作"{conc_zh}"讲时可数',
                answer="an",
                explain=f'{noun} 作"{conc_zh}"（可数）时可用 a/an。an unforgettable {noun} = 一次难忘的{conc_zh}。',
                examples=[example_pair, ex_pair_zh],
                mnemonic=f'{noun} 具体化 → 可数 → a/an {noun}',
                diff='hard'
            ))

        # 辨析填空
        questions.append(make_fill(
            q=f"{noun.capitalize()} is important. But it was a real ___.",
            hint=f'第一个 {noun} 是抽象"{abs_zh}"，第二个是具体"{conc_zh}"',
            answer=noun,
            explain=f'{noun} 的抽象含义"{abs_zh}"不可数，具体含义"{conc_zh}"可数。',
            examples=[example_pair, ex_pair_zh],
            mnemonic=f'{noun} 抽象→不可数，具体→可数',
            diff='hard'
        ))

    return questions

# ============================================================
# 10. 数量表达 (约 40 题)
# ============================================================
def gen_quantity_expressions():
    questions = []

    # a number of vs the number of
    number_expr = [
        ("a number of", "许多（+ 可数名词复数，谓语复数）", "A number of students are absent.", "许多学生缺席了。"),
        ("the number of", "…的数量（+ 可数名词复数，谓语单数）", "The number of students is 30.", "学生的数量是30。"),
    ]

    for expr, zh, example, ex_zh in number_expr:
        questions.append(make_mc(
            q=f"选择正确的句子：",
            hint=f'{zh}',
            options=[
                f"{expr} students are in the classroom.",
                f"{expr} students is in the classroom.",
                f"{expr} student is in the classroom.",
                f"{expr} student are in the classroom.",
            ],
            answer_idx=0 if "are" in example else 1,
            explain=f'{expr} {zh}。{expr.split()[-2]} 是主语核心，{expr.split()[-1]} 是修饰语。谓语形式取决于 {expr} 的含义。',
            examples=[f'<strong>{example}</strong>', ex_zh],
            mnemonic=f'a number of → 复数 / the number of → 单数',
            diff='hard'
        ))

    # many / much / a lot of / plenty of
    quantity_words = [
        ("many", "许多（+ 可数名词复数）", "many books", "许多书"),
        ("much", "许多（+ 不可数名词）", "much water", "许多水"),
        ("a lot of", "许多（+ 可数/不可数都可以）", "a lot of books/water", "许多书/水"),
        ("plenty of", "充足的（+ 可数/不可数都可以）", "plenty of time/friends", "充足的时间/很多朋友"),
        ("a great deal of", "大量的（+ 不可数名词，正式）", "a great deal of work", "大量工作"),
        ("a large number of", "大量的（+ 可数名词复数，正式）", "a large number of people", "大量的人"),
    ]

    for word, zh, example, ex_zh in quantity_words:
        # MC: 选正确搭配
        correct = f"{word} {'students' if '可数' in zh else 'information'}"
        wrong1 = f"{word} {'information' if '可数' in zh else 'students'}"
        wrong2 = f"{word} student" if "复数" in zh else f"{word} informations"
        wrong3 = f"a {word} student" if "复数" in zh else f"a {word} information"

        options = [correct, wrong1, wrong2, wrong3]
        random.shuffle(options)
        ans_idx = options.index(correct)

        questions.append(make_mc(
            q=f"选择正确的搭配：",
            hint=f'{word} {zh}',
            options=options,
            answer_idx=ans_idx,
            explain=f'{word} {zh}。例如：{example}。',
            examples=[
                f'There are/is <strong>{example}</strong>. {ex_zh}',
                f'{word} 表示"{zh}"',
            ],
            mnemonic=f'{word} → {zh}',
            diff='medium'
        ))

    # few / a few / little / a little
    few_little = [
        ("few", "几乎没有（否定，+ 可数名词复数）", "few friends", "几乎没有朋友"),
        ("a few", "有几个（肯定，+ 可数名词复数）", "a few friends", "有几个朋友"),
        ("little", "几乎没有（否定，+ 不可数名词）", "little money", "几乎没有钱"),
        ("a little", "有一点（肯定，+ 不可数名词）", "a little money", "有一点钱"),
    ]

    for word, zh, example, ex_zh in few_little:
        questions.append(make_mc(
            q=f"选择正确的句子：He has ___ friends, so he often feels lonely.",
            hint=f'{zh}',
            options=[
                "few",
                "a few",
                "little",
                "a little",
            ],
            answer_idx=0 if "几乎" in zh else 1 if word == "a few" else 2 if word == "little" else 3,
            explain=f'{word} = {zh}。few/little 含否定含义"几乎没有"，a few/a little 含肯定含义"有一些"。',
            examples=[
                f'He has <strong>{word}</strong> {"friends" if "可数" in zh else "money"}.',
                f'{"He has almost no friends." if "几乎" in zh else "He has some friends."}',
            ],
            mnemonic=f'{word} → {zh} | few+可数 / little+不可数',
            diff='hard'
        ))

    # a number of / the number of 填空
    for _ in range(5):
        questions.append(make_fill(
            q="A number of students ___ (be) absent today.",
            hint="a number of + 复数名词 → 谓语用复数",
            answer="are",
            explain='a number of = 许多，后接可数名词复数，谓语用复数。',
            examples=[
                '<strong>A number of</strong> students <strong>are</strong> absent.',
                '<strong>The number of</strong> students <strong>is</strong> 30.',
            ],
            mnemonic='a number of → 复数 / the number of → 单数',
            diff='hard'
        ))

        questions.append(make_fill(
            q="The number of students ___ (be) 30.",
            hint="the number of → 谓语用单数",
            answer="is",
            explain='the number of = ...的数量，主语是 number（单数），所以谓语用单数。',
            examples=[
                '<strong>The number of</strong> students <strong>is</strong> 30.',
                '<strong>A number of</strong> students <strong>are</strong> absent.',
            ],
            mnemonic='a number of → 复数 / the number of → 单数',
            diff='hard'
        ))

    return questions

# ============================================================
# 11. 补充题库 (约 70 题)
# ============================================================
def gen_supplement():
    questions = []

    # --- 不可数名词额外 MC 题 ---
    extra_uncountable = [
        ("furniture", "家具", "a piece of", "可数名词 foils: chair, table, desk, bed"),
        ("information", "信息", "a piece of", "可数名词 foils: book, message, letter, note"),
        ("advice", "建议", "a piece of", "可数名词 foils: suggestion, tip, idea, plan"),
        ("news", "新闻", "a piece of", "可数名词 foils: story, report, article, post"),
        ("equipment", "设备", "a piece of", "可数名词 foils: machine, tool, device, instrument"),
        ("luggage", "行李", "a piece of", "可数名词 foils: bag, suitcase, backpack, box"),
        ("knowledge", "知识", "a lot of", "可数名词 foils: fact, skill, lesson, subject"),
        ("progress", "进步", "much", "可数名词 foils: step, achievement, result, score"),
    ]

    for noun, zh, quantifier, foils_str in extra_uncountable:
        foils = foils_str.split(": ")[1].split(", ")
        # MC: 选不可数名词
        options_list = [noun] + random.sample(foils, 3)
        random.shuffle(options_list)
        ans_idx = options_list.index(noun)
        questions.append(make_mc(
            q="以下哪个是不可数名词？",
            hint=f'不可数名词不能加 s，不能用 a/an 修饰',
            options=options_list,
            answer_idx=ans_idx,
            explain=f'{noun}（{zh}）是不可数名词。其他选项都是可数名词，可以加 s 或用 a/an 修饰。',
            examples=[
                f'I need <strong>{quantifier} {noun}</strong>. 我需要{zh}。',
                f'There is a/an <strong>{foils[0]}</strong> on the table.',
            ],
            mnemonic=f'{noun} 不可数 → {quantifier} + {noun}',
            diff='medium'
        ))

    # --- 不规则复数额外题 ---
    extra_irregular = [
        ("child", "children", "The ___ are playing in the park."),
        ("mouse", "mice", "There are three ___ in the kitchen."),
        ("foot", "feet", "My ___ hurt after walking all day."),
        ("tooth", "teeth", "Brush your ___ twice a day."),
        ("person", "people", "Many ___ came to the concert."),
        ("man", "men", "The ___ are working outside."),
        ("woman", "women", "Three ___ are waiting in line."),
        ("goose", "geese", "The ___ are flying south."),
        ("sheep", "sheep", "There are ten ___ in the field."),
        ("fish", "fish", "I caught three ___ in the lake."),
        ("deer", "deer", "Several ___ crossed the road."),
        ("ox", "oxen", "The farmer has two ___."),
        ("crisis", "crises", "We are facing many ___."),
        ("phenomenon", "phenomena", "These natural ___ are amazing."),
    ]

    for sg, pl, sentence in extra_irregular:
        questions.append(make_fill(
            q=sentence,
            hint=f'{sg} 的复数形式',
            answer=pl,
            explain=f'{sg} 是不规则名词，复数是 {pl}。需要单独记忆。',
            examples=[
                f'One <strong>{sg}</strong>, many <strong>{pl}</strong>.',
                sentence.replace("___", f"<strong>{pl}</strong>"),
            ],
            mnemonic=f'{sg} → {pl}（不规则！）',
            diff='hard' if sg in ("crisis", "phenomenon", "ox") else 'medium'
        ))

    # --- 所有格综合 MC ---
    possessive_sentences = [
        ("This is ___ book.", "Tom's", "Tom的", ["Tom", "Toms", "Toms'"]),
        ("The ___ toys are on the floor.", "children's", "孩子们的", ["children", "childrens", "childrens'"]),
        ("I went to ___ house yesterday.", "my uncle's", "我叔叔的", ["my uncle", "my uncles", "my uncles'"]),
        ("___ room is on the second floor.", "The students'", "学生们的", ["The students", "The student's", "The students's"]),
        ("The ___ of the house is red.", "roof", "屋顶", ["roof's", "roofs", "rooves"]),
        ("___ is a good habit.", "Reading books", "读书", ["Read books", "To read book", "Reading book"]),
        ("The cover ___ the book is torn.", "of", "...的", ["'s", "for", "in"]),
        ("___ car is very expensive.", "Mr. Smith's", "Smith先生的", ["Mr. Smith", "Mr. Smiths", "Mr. Smiths'"]),
    ]

    for q_template, answer, zh, wrong_list in possessive_sentences:
        options = [answer] + wrong_list
        random.shuffle(options)
        ans_idx = options.index(answer)
        questions.append(make_mc(
            q=q_template,
            hint=f'表达"{zh}"',
            options=options,
            answer_idx=ans_idx,
            explain=f'正确答案是 {answer}。注意名词所有格的使用规则。',
            examples=[
                q_template.replace("___", f"<strong>{answer}</strong>"),
                f'This expresses: {zh}.',
            ],
            mnemonic=f'{zh} → {answer}',
            diff='medium'
        ))

    # --- 同义名词更多辨析 ---
    extra_synonyms = [
        ("cause", "原因（导致结果的）", "reason", "原因（解释的）", "The cause of the fire is unknown.", "火灾原因不明。"),
        ("custom", "风俗/习惯（社会）", "habit", "习惯（个人）", "It's a local custom. / He has a bad habit.", "这是当地风俗。/ 他有个坏习惯。"),
        ("damage", "损害/破坏（不可数）", "harm", "伤害（可数/不可数）", "The storm caused damage. / Smoking does harm.", "暴风雨造成了破坏。/ 吸烟有害。"),
        ("floor", "楼层/地板", "ground", "地面/土地", "on the second floor / on the ground", "在二楼 / 在地上"),
        ("house", "房子（建筑）", "home", "家（情感归属）", "a big house / go home", "大房子 / 回家"),
        ("street", "街道（城市）", "road", "路（连接两地）", "on Main Street / a long road", "在主街上 / 一条长路"),
        ("voice", "嗓音/声音（人的）", "sound", "声音（任何）", "a beautiful voice / a loud sound", "美妙的嗓音 / 响亮的声音"),
        ("cloth", "布/布料", "clothes", "衣服", "a piece of cloth / wear warm clothes", "一块布 / 穿暖和的衣服"),
    ]

    for noun1, zh1, noun2, zh2, examples_pair, ex_pair_zh in extra_synonyms:
        # MC
        options = [noun1, noun2, f"{noun1}s", f"{noun2}s"]
        random.shuffle(options)
        ans_idx = options.index(noun1)
        questions.append(make_mc(
            q=f"The ___ of the accident is still under investigation.",
            hint=f'{zh1} vs {zh2}',
            options=options,
            answer_idx=ans_idx,
            explain=f'{noun1} 表示"{zh1}"；{noun2} 表示"{zh2}"。',
            examples=[
                f'The <strong>{noun1}</strong> of the accident is unknown.',
                f'{noun1} = {zh1}, {noun2} = {zh2}',
            ],
            mnemonic=f'{noun1} = {zh1} | {noun2} = {zh2}',
            diff='hard'
        ))

        # 第二个词
        options2 = [noun2, noun1, f"{noun2}s", f"{noun1}s"]
        random.shuffle(options2)
        ans_idx2 = options2.index(noun2)
        questions.append(make_mc(
            q=f"Please tell me the ___ why you are late.",
            hint=f'{zh2} vs {zh1}',
            options=options2,
            answer_idx=ans_idx2,
            explain=f'{noun2} 表示"{zh2}"；{noun1} 表示"{zh1}"。',
            examples=[
                f'What is the <strong>{noun2}</strong> for being late?',
                f'{noun2} = {zh2}, {noun1} = {zh1}',
            ],
            mnemonic=f'{noun2} = {zh2} | {noun1} = {zh1}',
            diff='hard'
        ))

    # --- 数量表达更多题 ---
    extra_quantity = [
        ("There are ___ people in the park.", "many", "许多（+可数复数）"),
        ("I don't have ___ time left.", "much", "许多（+不可数）"),
        ("She has ___ friends at school.", "a few", "有几个（肯定）"),
        ("He has ___ money in his wallet.", "little", "几乎没有（否定）"),
        ("There is ___ water in the glass.", "a little", "有一点（肯定）"),
        ("___ students are in the library.", "A number of", "许多（+可数复数）"),
        ("___ students is 50.", "The number of", "…的数量（+单数谓语）"),
        ("I have ___ work to do today.", "a lot of", "许多（+可数/不可数）"),
        ("There is ___ time before the exam.", "plenty of", "充足的（+可数/不可数）"),
        ("She needs ___ patience to do this job.", "a great deal of", "大量的（+不可数）"),
    ]

    for q_text, answer, hint in extra_quantity:
        wrong_answers = ["many", "much", "a few", "few", "a little", "little",
                         "a lot of", "plenty of", "a number of", "the number of", "a great deal of"]
        wrong_answers = [w for w in wrong_answers if w != answer]
        distractors = random.sample(wrong_answers, min(3, len(wrong_answers)))
        options = [answer] + distractors
        random.shuffle(options)
        ans_idx = options.index(answer)
        questions.append(make_mc(
            q=q_text,
            hint=f'{hint}',
            options=options,
            answer_idx=ans_idx,
            explain=f'{answer} 表示"{hint}"。注意区分可数/不可数以及肯定/否定含义。',
            examples=[
                q_text.replace("___", f"<strong>{answer}</strong>"),
                f'{answer} = {hint}',
            ],
            mnemonic=f'{answer} → {hint}',
            diff='medium'
        ))

    # --- 额外补到 500+ ---
    extra_fill_irregular = [
        ("man", "men", "Two ___ are talking outside."),
        ("woman", "women", "The ___ are having coffee."),
        ("foot", "feet", "Wash your ___ before entering."),
        ("tooth", "teeth", "She has white ___."),
        ("child", "children", "The ___ are excited about the trip."),
        ("mouse", "mice", "The cat caught two ___."),
        ("person", "people", "How many ___ are coming?"),
        ("leaf", "leaves", "The ___ turn yellow in autumn."),
        ("knife", "knives", "Put the ___ in the drawer."),
        ("wolf", "wolves", "___ hunt in packs."),
        ("city", "cities", "Beijing and Shanghai are big ___."),
        ("baby", "babies", "The ___ are sleeping."),
        ("story", "stories", "Grandma tells interesting ___."),
        ("country", "countries", "How many ___ have you visited?"),
        ("family", "families", "Many ___ live in this neighborhood."),
        ("party", "parties", "I love birthday ___!"),
        ("potato", "potatoes", "We need more ___ for the soup."),
        ("tomato", "tomatoes", "These ___ are fresh."),
        ("hero", "heroes", "They are national ___."),
        ("bus", "buses", "Two ___ go to the city center."),
    ]

    for sg, pl, sentence in extra_fill_irregular:
        questions.append(make_fill(
            q=sentence,
            hint=f'{sg} 的复数形式',
            answer=pl,
            explain=f'{sg} 的复数是 {pl}。{"不规则变化，需单独记忆。" if pl not in (sg + "s", sg + "es") else "规则复数变化。"}',
            examples=[
                f'One <strong>{sg}</strong>, many <strong>{pl}</strong>.',
                sentence.replace("___", f"<strong>{pl}</strong>"),
            ],
            mnemonic=f'{sg} → {pl}',
            diff='easy' if pl.endswith("s") or pl.endswith("es") else 'medium'
        ))

    return questions

# ============================================================
# 主程序
# ============================================================
def main():
    print("开始生成名词题库...")

    generators = [
        ("可数/不可数名词", gen_uncountable),
        ("单复数规则", gen_plural_rules),
        ("集合名词", gen_collective_nouns),
        ("名词所有格", gen_possessive),
        ("复合名词", gen_compound_nouns),
        ("名词后缀", gen_noun_suffixes),
        ("同义名词辨析", gen_synonym_nouns),
        ("常见名词搭配", gen_noun_collocations),
        ("抽象名词具体化", gen_abstract_nouns),
        ("数量表达", gen_quantity_expressions),
        ("补充题库", gen_supplement),
    ]

    all_questions = []
    for name, gen_func in generators:
        qs = gen_func()
        all_questions.extend(qs)
        print(f"  {name}: {len(qs)} 题")

    # 确保难度分布：easy 30%, medium 40%, hard 30%
    easy_qs = [q for q in all_questions if q["diff"] == "easy"]
    medium_qs = [q for q in all_questions if q["diff"] == "medium"]
    hard_qs = [q for q in all_questions if q["diff"] == "hard"]

    total = len(all_questions)
    print(f"\n  总计生成: {total} 题")
    print(f"  Easy: {len(easy_qs)} ({len(easy_qs)/total*100:.1f}%)")
    print(f"  Medium: {len(medium_qs)} ({len(medium_qs)/total*100:.1f}%)")
    print(f"  Hard: {len(hard_qs)} ({len(hard_qs)/total*100:.1f}%)")

    # 统计题型分布
    mc_count = sum(1 for q in all_questions if q["type"] == "mc")
    fill_count = sum(1 for q in all_questions if q["type"] == "fill")
    print(f"  MC: {mc_count} ({mc_count/total*100:.1f}%)")
    print(f"  Fill: {fill_count} ({fill_count/total*100:.1f}%)")

    # 写入文件
    filepath = os.path.join(OUTPUT_DIR, "nouns.json")
    with open(filepath, 'w', encoding='utf-8') as f:
        json.dump(all_questions, f, ensure_ascii=False, indent=2)

    print(f"\n 已保存到: {filepath}")
    print(f" 文件大小: {os.path.getsize(filepath):,} bytes")

if __name__ == "__main__":
    main()
